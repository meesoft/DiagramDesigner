unit MultiCoreProcessing;

//{$DEFINE DisableMultiCoreProcess}

interface

uses Windows, Classes, SysUtils, Math;

type
  TSimpleThread = class; // Forward

  TSimpleThreadProc = procedure(Thread: TSimpleThread; Data: Pointer);
  TSimpleThreadMethod = procedure(Thread: TSimpleThread; Data: Pointer) of object;

  // A thread class ment for executing a simple process that requires no synchronization.
  // Exceptions are passed on to the calling thread.
  TSimpleThread = class(TThread)
  private
    ThreadProc : TSimpleThreadProc;
    ThreadMethod : TSimpleThreadMethod;
    Data : Pointer;
    Started : Boolean;
    ExceptionMessage : string;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadProc: TSimpleThreadProc; Data: Pointer=nil); overload;
    constructor Create(const ThreadMethod: TSimpleThreadMethod; Data: Pointer=nil); overload;
    destructor Destroy; override;
    procedure WaitFor;
    // Wait and free
    procedure Finalize;
  end;

// If Thread<>nil then wait for its completion and destroy it. Any thread exception are passed on.
procedure FinalizeAndNil(var Thread: TSimpleThread);

type
  TMultiCoreProcessThread = class; // Forward
  TMultiCoreProcess = class; // Forward

  TMultiCoreProcessMethod = procedure(Thread: TMultiCoreProcessThread; Data: Pointer) of object;

  TMultiCoreProcessThread = class(TThread)
  private
    ProcessMethod : TMultiCoreProcessMethod;
    Data : Pointer;
    Process : TMultiCoreProcess;
  public
    constructor Create(Process: TMultiCoreProcess; const ProcessMethod: TMultiCoreProcessMethod; Data: Pointer);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TThreadDataArray = array of Pointer;

  // Class for dividing a process in multiple threads. Inherit from this class and implement ProcessMethod.
  TMultiCoreProcess = class
  private
    Exceptions : Integer;
    ExceptionMessage : string;
  protected
    // Returns True when one of the parallel threads has been aborted or has raised an exception
    function ProcessAborted: Boolean;
    // The thread process. Exceptions are passed on to the calling thread.
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); virtual; abstract;
  public
    // Return number of CPU cores available
    class function NumberOfCores: Integer;
    class function OptimalNumberOfThreads(ThreadsPerCore: Integer=1; MaxThreads: Integer=16): Integer;
    // Execute a number of threads corresponding to the number of elements in the ThreadData array
    procedure ExecuteProcess(ThreadData: TThreadDataArray);
    // Divide the interval [Start,Stop] in parts and execute a thread for each part passing a PThreadDataStartStop
    procedure ExecuteStartStop(Start,Stop,MinSegmentSize: Integer; MaxNumberOfThreads: Integer=0);
  end;

  TThreadDataStartStop = record
                           Start, Stop : Integer;
                           ThreadIndex : Integer;
                         end;
  PThreadDataStartStop = ^TThreadDataStartStop;

// Return total number of TSimpleThread and TMultiCoreProcessThread instances running
function ProcessingThreadsRunning: Integer;

type TLoopProcedure = procedure(I: Integer);
// Parallel for loop. Note that LoopProc can be a local procudure if you put a @ before its name
procedure ParallelFor(LoopMin,LoopMax: Integer; LoopProc: TLoopProcedure; MinSegmentSize: Integer=1; ThreadsPerCore: Integer=1);
// Run a number of methods each taking a single 32 bit argument. Must be given as [@proc1,arg1,@proc2,arg2,@proc3,arg3]
procedure ParallelRun(const MethodsAndArguments: array of const);

var
  AllowThreads : Boolean = False; // Set in initialization (may fail in DLLs)

implementation

uses MathUtils;

var
  ThreadsRunning : Integer = 0;

function ProcessingThreadsRunning: Integer;
begin
  Result:=ThreadsRunning;
end;

//==============================================================================================================================
// TSimpleThread
//==============================================================================================================================

constructor TSimpleThread.Create(ThreadProc: TSimpleThreadProc; Data: Pointer);
begin
  Self.ThreadProc:=ThreadProc;
  Self.Data:=Data;
  inherited Create(False);
  InterlockedIncrement(ThreadsRunning);
end;

constructor TSimpleThread.Create(const ThreadMethod: TSimpleThreadMethod; Data: Pointer);
begin
  Self.ThreadMethod:=ThreadMethod;
  Self.Data:=Data;
  inherited Create(False);
  InterlockedIncrement(ThreadsRunning);
end;

destructor TSimpleThread.Destroy;
begin
  while not Started do Sleep(0); // Make sure that thread is actually started
  inherited;
  if ExceptionMessage<>'' then
    raise Exception.Create(ExceptionMessage); // This may be a memory leak...
end;

procedure TSimpleThread.WaitFor;
var
  Msg : string;
begin
  while not Started do Sleep(0); // Make sure that thread is actually started
  inherited WaitFor;
  if ExceptionMessage<>'' then
  begin
    Msg:=ExceptionMessage;
    ExceptionMessage:='';
    raise Exception.Create(Msg);
  end;
end;

procedure TSimpleThread.Execute;
begin
  Started:=True;
  try
    Set8087CW(DefaultFPUControlWord);
    if Assigned(ThreadProc) then ThreadProc(Self,Data)
    else ThreadMethod(Self,Data);
    Assert(ThreadsRunning>0);
  except
    on E: Exception do
      ExceptionMessage:=E.Message;
  end;
  InterlockedDecrement(ThreadsRunning);
end;

procedure TSimpleThread.Finalize;
begin
  try
    WaitFor;
  finally
    Destroy;
  end;
end;

procedure FinalizeAndNil(var Thread: TSimpleThread);
var
  Temp : TSimpleThread;
begin
  if Thread<>nil then
  begin
    Temp:=Thread;
    Pointer(Thread):=nil;
    Temp.Finalize;
  end;
end;

//==============================================================================================================================
// TMultiCoreProcessThread
//==============================================================================================================================

constructor TMultiCoreProcessThread.Create(Process: TMultiCoreProcess; const ProcessMethod: TMultiCoreProcessMethod; Data: Pointer);
begin
  Self.Process:=Process;
  Self.ProcessMethod:=ProcessMethod;
  Self.Data:=Data;
  inherited Create(False);
  InterlockedIncrement(ThreadsRunning);
end;

destructor TMultiCoreProcessThread.Destroy;
begin
  WaitFor;
  while Assigned(Process) do Sleep(0); // Make sure that thread was actually started
  inherited Destroy;
end;

procedure TMultiCoreProcessThread.Execute;
begin
  try
    Set8087CW(DefaultFPUControlWord);
    ProcessMethod(Self,Data);
  except
    on E: EAbort do
      if InterlockedExchange(Process.Exceptions,2)=1 then
        Process.Exceptions:=1;
    on E: Exception do
      if InterlockedExchange(Process.Exceptions,1)=0 then
        Process.ExceptionMessage:=E.Message;
  end;
  Process:=nil;
  InterlockedDecrement(ThreadsRunning);
  Assert(ThreadsRunning>=0);
end;

//==============================================================================================================================
// TMultiCoreProcess
//==============================================================================================================================

var
  CachedNumberOfCores : Integer = 0;

class function TMultiCoreProcess.NumberOfCores: Integer;
var
  SystemInfo : TSystemInfo;
begin
  if CachedNumberOfCores=0 then
  begin
    GetSystemInfo(SystemInfo);
    CachedNumberOfCores:=SystemInfo.dwNumberOfProcessors;
    {$IFDEF DisableMultiCoreProcess}
    CachedNumberOfCores:=1;
    {$ENDIF}
  end;
  Result:=CachedNumberOfCores;
end;

class function TMultiCoreProcess.OptimalNumberOfThreads(ThreadsPerCore,MaxThreads: Integer): Integer;
begin
  Assert(ThreadsPerCore>=1);
  if AllowThreads then Result:=NumberOfCores
  else Result:=1;
  if Result>1 then Result:=EnsureRange(Result*ThreadsPerCore-ThreadsRunning,1,MaxThreads);
end;

function TMultiCoreProcess.ProcessAborted: Boolean;
begin
  Result:=Exceptions<>0;
end;

procedure TMultiCoreProcess.ExecuteProcess(ThreadData: TThreadDataArray);
var
  Threads : array of TMultiCoreProcessThread;
  I : Integer;
begin
  Assert(Length(ThreadData)>=1,'No thread data');
  if NumberOfCores>1 then
  begin
    Exceptions:=0;
    SetLength(Threads,Length(ThreadData)-1);
    try
      for I:=0 to High(Threads) do
        Threads[I]:=TMultiCoreProcessThread.Create(Self,ProcessMethod,ThreadData[I+1]);
      try
        Set8087CW(DefaultFPUControlWord);
        ProcessMethod(nil,ThreadData[0]);
      except
        Exceptions:=1;
        raise;
      end;
    finally
      for I:=0 to High(Threads) do
        Threads[I].Free;
    end;
    if Exceptions=1 then raise Exception.Create(ExceptionMessage)
    else if Exceptions=2 then Abort;
  end
  else
  begin
    Set8087CW(DefaultFPUControlWord);
    for I:=0 to High(ThreadData) do
      ProcessMethod(nil,ThreadData[I]);
  end;
end;

procedure TMultiCoreProcess.ExecuteStartStop(Start,Stop,MinSegmentSize,MaxNumberOfThreads: Integer);
var
  I, SegmentSize : Integer;
  StartStopData : array of TThreadDataStartStop;
  ThreadData : TThreadDataArray;
begin
  Assert(Start<=Stop);
  if MaxNumberOfThreads=0 then MaxNumberOfThreads:=OptimalNumberOfThreads;
  {$IFDEF DisableMultiCoreProcess}
  MaxNumberOfThreads:=1;
  {$ENDIF}
  Assert(MaxNumberOfThreads>=1);
  //TODO: Count=11, MaxNumberOfThreads=6 will give SegmentSize=1 and 11 thraeds!
  SegmentSize:=Max(MinSegmentSize,(Stop-Start+1) div MaxNumberOfThreads);
  SetLength(StartStopData,Max(1,(Stop-Start+1) div SegmentSize));
  SetLength(ThreadData,Length(StartStopData));
  for I:=0 to High(StartStopData) do
  begin
    StartStopData[I].Start:=Start+SegmentSize*I;
    StartStopData[I].Stop:=Start+SegmentSize*(I+1)-1;
    StartStopData[I].ThreadIndex:=I;
    ThreadData[I]:=@StartStopData[I];
  end;
  StartStopData[High(StartStopData)].Stop:=Stop;
  ExecuteProcess(ThreadData);
end;

//==============================================================================================================================
// TParallelForProcess
//==============================================================================================================================
type
  TParallelForProcess = class(TMultiCoreProcess)
  protected
    FLoopProc, FCallerBP : Pointer;
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

{$O-} // We need control of CPU registers so turn off optimization
procedure TParallelForProcess.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  I : Integer;
  LoopProc, CallerBP : Pointer;
begin
  LoopProc:=FLoopProc;
  CallerBP:=FCallerBP;
  with PThreadDataStartStop(Data)^ do
    for I:=Start to Stop do
    asm
      mov eax, I // EAX holds procedure argument
      push CallerBP // Push caller's base pointer value onto stack and call
      Call LoopProc
      pop CallerBP
    end;
end;
{$O+}

// Parallel for loop. Note that LoopProc can be a local procudure if you put a @ before its name
procedure ParallelFor(LoopMin,LoopMax: Integer; LoopProc: TLoopProcedure; MinSegmentSize, ThreadsPerCore: Integer);
var
  CallerBP : Pointer;
begin
  // Get caller's base pointer value at the very beginning
  asm
    push dword ptr [ebp]
    pop CallerBP
  end;
  // Run loop in parallel
  with TParallelForProcess.Create do
  try
    FCallerBP:=CallerBP;
    FLoopProc:=@LoopProc;
    ExecuteStartStop(LoopMin,LoopMax,MinSegmentSize,OptimalNumberOfThreads(ThreadsPerCore));
  finally
    Free;
  end;
end;

//==============================================================================================================================
// TParallelForProcess
//==============================================================================================================================
type
  TParallelRunProcess = class(TMultiCoreProcess)
  protected
    FCallerBP : Pointer;
    FLoopProcs : array of record
                   Proc : Pointer;
                   Data : Integer;
                 end;
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

{$O-} // We need control of CPU registers so turn off optimization
procedure TParallelRunProcess.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  I, ProcData : Integer;
  LoopProc, CallerBP : Pointer;
begin
  CallerBP:=FCallerBP;
  with PThreadDataStartStop(Data)^ do
    for I:=Start to Stop do
    begin
      LoopProc:=FLoopProcs[I].Proc;
      ProcData:=FLoopProcs[I].Data;
      asm
        mov eax, ProcData // EAX holds procedure argument
        push CallerBP     // Push caller's base pointer value onto stack and call
        Call LoopProc
        pop CallerBP
      end;
    end;
end;
{$O+}

// Run a number of methods each taking a single 32 bit argument. Must be given as [@proc1,arg1,@proc2,arg2,@proc3,arg3]
procedure ParallelRun(const MethodsAndArguments: array of const);
var
  CallerBP : Pointer;
  I : Integer;
begin
  // Get caller's base pointer value at the very beginning
  asm
    push dword ptr [ebp]
    pop CallerBP
  end;
  // Run loop in parallel
  with TParallelRunProcess.Create do
  try
    FCallerBP:=CallerBP;
    SetLength(FLoopProcs,Length(MethodsAndArguments) div 2);
    for I:=0 to High(FLoopProcs) do
    begin
      FLoopProcs[I].Proc:=MethodsAndArguments[2*I+0].VPointer;
      FLoopProcs[I].Data:=MethodsAndArguments[2*I+1].VInteger;
    end;
    ExecuteStartStop(0,High(FLoopProcs),1);
  finally
    Free;
  end;
end;

//==============================================================================================================================

var
  OldDllProc : TDLLProc;

procedure DLLEntryPointFunc(Reason:integer);
begin
  DLLProc:=OldDllProc;
  if Assigned(DLLProc) then DllProc(Reason);
  AllowThreads:=True;
end;

initialization
  if IsLibrary then // Threads are not allowed until DLL initialization completed
  begin
    OldDllProc:=DLLProc;
    DLLProc:=@DLLEntryPointFunc;
  end
  else AllowThreads:=True;
finalization
  Assert(ThreadsRunning=0);
end.

