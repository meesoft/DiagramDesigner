///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Monitor.pas - Memory leak detector
// ----------------------------------
// Version:   2005-01-14
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//   Pointer lists changed to binary trees
//
unit Monitor;

{$IFOPT C+} {$IFOPT D+} {$IFNDEF CLR}
//  {$DEFINE MonitorObjects} // Auto-enable monitor if assertions and debugging enabled
{$ENDIF} {$ENDIF} {$ENDIF}

{DEFINE MonitorObjects}  // Define MonitorObjects to use object monitor
{DEFINE ZeroOnDestroy}   // Fill memory of destroyed objects with zeros
{DEFINE MonitorAll}      // Overload Delphi's memory manager to monitor all activity

{$IFOPT D-} {$IFOPT W+}
  Remember turning off stack frames!
{$ENDIF} {$ENDIF}

{$IFOPT D-}
  {$UNDEF MonitorObjects}
{$ENDIF}

{$D-} // Turn debug info off

interface

{$IFNDEF CLR}
uses Classes;
{$ENDIF}

{$IFDEF MonitorObjects}

const
  MemLogFile = 'memlog.txt';
  MaxGetMemBlock = 300*1024*1024; // Raise EOutOfMemory if allocating >300 MB
  StackTraceLevels = 1;

type
  TMonitorObject = class(TObject)
    private
      //ListItem : Pointer;
    public
      constructor Create;
      destructor Destroy; override;
      {$IFDEF ZeroOnDestroy}
      procedure FreeInstance; override;
      {$ENDIF}
    end;

  TPersistentMonitor = class(TPersistent)
    public
      constructor Create;
      destructor Destroy; override;
    end;

// Get return addresses from stack trace for the specified number of levels
function StackTrace(Levels: Cardinal): string;

{$IFNDEF MonitorAll}
procedure GetMem(var P; Size: Integer);
procedure ReallocMem(var P; Size: Integer);
procedure FreeMem(const P; Size: Integer=-1);
{$ENDIF}

{$ELSE}
type
  TMonitorObject = TObject;
  TPersistentMonitor = TPersistent;
{$ENDIF}

{$IFNDEF CLR}
// Return list of active objects and memory blocks
function GetMonitorLog(FreeObjects: Boolean=False): TStrings;

// Call example: Assert(MonitorComponent(Self)) in component constructor.
// This will ensure that the monitor is removed when assertions are turned off.
function MonitorComponent(Component: TComponent): Boolean;
{$ENDIF}

implementation

{$IFNDEF MonitorObjects}
{$IFNDEF CLR}

function GetMonitorLog(FreeObjects: Boolean): TStrings;
begin
  Result:=nil;
end;

function MonitorComponent(Component: TComponent): Boolean;
begin
  Result:=True;
end;

{$ENDIF}
{$ELSE}

uses Windows, MMSystem, SysUtils, FileUtils, MemUtils, SyncObjs, BinaryTrees;

function StackTrace(Levels: Cardinal): string;
var
  Addr, I : Integer;
begin
  asm
    mov eax,[ebp+0]
    mov Addr,eax
  end;
  Result:='$'+IntToHex(PInteger(Addr+4)^,8); // Return address
  for I:=2 to Levels do
  begin
    Addr:=PInteger(Addr)^; // Next level
    if Abs(Addr)<$10000 then Break;
    try
      Result:=Result+', $'+IntToHex(PInteger(Addr+4)^,8); // Return address
    except
      Break;
    end;
  end;
end;

var
  PointerList, ObjectList : TBinaryTree;
  Log : TStringList;
  Sync : TCriticalSection;

//==============================================================================================================================
// TComponentMonitor
//==============================================================================================================================

type
  TComponentMonitor = class(TComponent)
    public
      constructor Create(AOwner: TComponent; const Stack: string); reintroduce;
      destructor Destroy; override;
    end;

function MonitorComponent(Component: TComponent): Boolean;
begin
  Result:=True;
  TComponentMonitor.Create(Component,StackTrace(StackTraceLevels));
end;

constructor TComponentMonitor.Create(AOwner: TComponent; const Stack: string);
begin
  inherited Create(AOwner);
  Sync.Acquire;
  try
    ObjectList.AddTextObject(Stack+': '+Owner.ClassName,Self);
  finally
    Sync.Release;
  end;
end;

destructor TComponentMonitor.Destroy;
var
  Node : TRBTreeNode;
begin
  Sync.Acquire;
  try
    Node:=ObjectList.Find(TKey(Self));
    if Assigned(Node) then ObjectList.Delete(Node);
  finally
    Sync.Release;
  end;
  inherited;
end;

//==============================================================================================================================
// TMonitorObject
//==============================================================================================================================
constructor TMonitorObject.Create;
begin
  inherited;
  Sync.Acquire;
  try
    //ListItem:=
    ObjectList.AddTextObject(StackTrace(StackTraceLevels)+': '+ClassName,Self);
  finally
    Sync.Release;
  end;
end;

destructor TMonitorObject.Destroy;
var
  Node : TRBTreeNode;
begin
  Sync.Acquire;
  try
    Node:=ObjectList.Find(TKey(Self));
    //Node:=ListItem;
    if Assigned(Node) then ObjectList.Delete(Node)
    else Log.Add(ClassName+' at '+IntToHex(Integer(Self),8)+' already freed or inherited constructor/destructor not called')
                                                           // (or project needs rebuilding)
                                                           // Could also be because of exception in Create?
                                                           // Exception in Create results in call of Destroy
  finally
    Sync.Release;
  end;
  inherited;
end;

{$IFDEF ZeroOnDestroy}
procedure TMonitorObject.FreeInstance;
begin
  CleanupInstance;
  ZeroMem(Pointer(Integer(Self)+4)^,InstanceSize-4);
  inherited;
end;
{$ENDIF}

//==============================================================================================================================
// TPersistentMonitor
//==============================================================================================================================
constructor TPersistentMonitor.Create;
begin
  inherited;
  Sync.Acquire;
  try
    ObjectList.AddTextObject(StackTrace(StackTraceLevels)+': '+ClassName,Self);
  finally
    Sync.Release;
  end;
end;

destructor TPersistentMonitor.Destroy;
var
  Node : TRBTreeNode;
begin
  Sync.Acquire;
  try
    Node:=ObjectList.Find(TKey(Self));
    if Assigned(Node) then ObjectList.Delete(Node)
    else Log.Add(ClassName+' at '+IntToHex(Integer(Self),8)+' already freed or inherited constructor/destructor not called')
                                                           // (or project needs rebuilding)
                                                           // Could also be because of exception in Create?
                                                           // Exception in Create results in call of Destroy
  finally
    Sync.Release;
  end;
  inherited;
end;

//==============================================================================================================================
{$IFDEF MonitorAll}
function MonitorGetMem(Size: Integer): Pointer; forward;
function MonitorFreeMem(P: Pointer): Integer; forward;
function MonitorReallocMem(P: Pointer; Size: Integer): Pointer; forward;

const
  MonitorManager: TMemoryManager = (
    GetMem: MonitorGetMem;
    FreeMem: MonitorFreeMem;
    ReallocMem: MonitorReallocMem);

var
  OrgMemManager : TMemoryManager;

function MonitorGetMem(Size: Integer): Pointer;
var
  Addr : Cardinal;
begin
  asm
    mov eax,[ebp+4]
    mov Addr,eax
  end;
  if Size>MaxGetMemBlock then raise EOutOfMemory.CreateFmt('MonitorGetMem: %d MB memory block allocation',[Size div (1024*1024)]);

  Result:=OrgMemManager.GetMem(Size);
  Sync.Acquire;
  try
    SetMemoryManager(OrgMemManager);
    PointerList.AddTextObject({StackTrace(1)}IntToHex(Addr,8)+': '+IntToStr(Size),Result);
  finally
    SetMemoryManager(MonitorManager);
    Sync.Release;
  end;
end;

function MonitorFreeMem(P: Pointer): Integer;
var
  Node : TRBTreeNode;
begin
  Sync.Acquire;
  try
    SetMemoryManager(OrgMemManager);
    Node:=PointerList.Find(TKey(P));
    if Assigned(Node) then PointerList.Delete(Node);
  finally
    SetMemoryManager(MonitorManager);
    Sync.Release;
  end;
  FillChar(P^,(PInteger(Integer(P)-4))^ and not 3-4,$ff);
  Result:=OrgMemManager.FreeMem(P);
end;

function MonitorReallocMem(P: Pointer; Size: Integer): Pointer;
var
  Node : TRBTreeNode;
  Addr : Cardinal;
begin
  asm
    mov eax,[ebp+4]
    mov Addr,eax
  end;
  if Size>MaxGetMemBlock then raise EOutOfMemory.CreateFmt('MonitorReallocMem: %d MB memory block allocation',[Size div (1024*1024)]);

  Sync.Acquire;
  try
    SetMemoryManager(OrgMemManager);
    if Assigned(P) then
    begin
      Node:=PointerList.Find(TKey(P));
      if Assigned(Node) then PointerList.Delete(Node);
    end;
    Result:=OrgMemManager.ReallocMem(P,Size);
    PointerList.AddTextObject(IntToHex(Addr,8)+': '+IntToStr(Size),Result);
  finally
    SetMemoryManager(MonitorManager);
    Sync.Release;
  end;
end;

{$ELSE}

procedure GetMem(var P; Size: Integer);
begin
  if Size>MaxGetMemBlock then raise EOutOfMemory.CreateFmt('Monitor.GetMem: %d MB memory block allocation',[Size div (1024*1024)]);

  System.GetMem(Pointer(P),Size);
  Sync.Acquire;
  try
    PointerList.AddTextObject(StackTrace(StackTraceLevels)+': '+IntToStr(Size),Pointer(P));
  finally
    Sync.Release;
  end;
end;

procedure ReallocMem(var P; Size: Integer);
var
  Node : TRBTreeNode;
begin
  if Size>MaxGetMemBlock then raise EOutOfMemory.CreateFmt('Monitor.ReallocMem: %d MB memory block allocation',[Size div (1024*1024)]);

  Sync.Acquire;
  try
    if Assigned(Pointer(P)) then
    begin
      Node:=PointerList.Find(TKey(P));
      if Assigned(Node) then PointerList.Delete(Node);
    end;
    System.ReallocMem(Pointer(P),Size);
    if Size>0 then PointerList.AddTextObject(StackTrace(StackTraceLevels)+': '+IntToStr(Size),Pointer(P));
  finally
    Sync.Release;
  end;
end;

procedure FreeMem(const P; Size: Integer=-1);
var
  Node : TRBTreeNode;
begin
  Sync.Acquire;
  try
    Node:=PointerList.Find(TKey(P));
    if Node=nil then
    begin
      if Size<>-1 then Log.Add(Format('Data at $%p already freed',[Pointer(P)]))
    end
    else PointerList.Delete(Node);
  finally
    Sync.Release;
  end;

  if Size<>-1 then System.FreeMem(Pointer(P),Size)
  else System.FreeMem(Pointer(P));
end;
{$ENDIF}

function GetMonitorLog(FreeObjects: Boolean): TStrings;
var
  I, Count : Integer;
  Node : TRBTreeNode;
  Str : string;
begin
  Result:=TStringList.Create;
  Sync.Acquire;
  try
    Result.Assign(Log);
    I:=ObjectList.Count;
    while I>0 do
    begin
      Dec(I);
      try
        Str:='-';
        Node:=ObjectList.Nodes[I];
        Str:=TStringNode(Node).Text;
        if FreeObjects then TObject(Node.Key).Free;
        Result.Add('At '+Str+' not freed');
      except
        Result.Add('At '+Str+' inherited Destroy not called (or project needs rebuilding)');
      end;
      if I>ObjectList.Count then I:=ObjectList.Count;
    end;

    {$IFDEF MonitorAll}
    SetMemoryManager(OrgMemManager);
    {$ENDIF}
    for I:=0 to PointerList.Count-1 do
      Result.Add('At '+TStringNode(PointerList[I]).Text+' bytes not freed');
    {$IFDEF MonitorAll}
    SetMemoryManager(MonitorManager);
    {$ENDIF}
  finally
    Sync.Release;
  end;

  TStringList(Result).Sort;
  I:=0;
  while I<Result.Count-1 do
  begin
    Count:=1;
    while (I<Result.Count-1) and (Result[I]=Result[I+1]) do
    begin
      Result.Delete(I+1);
      Inc(Count);
    end;
    if Count>1 then Result[I]:=Result[I]+' * '+IntToStr(Count);
    Inc(I);
  end;
end;

initialization
  ObjectList:=TRedBlackTree.Create;
  PointerList:=TRedBlackTree.Create;
  Log:=TStringList.Create;
  Sync:=TCriticalSection.Create;

  {$IFDEF MonitorAll}
  GetMemoryManager(OrgMemManager);
  SetMemoryManager(MonitorManager);
  {$ENDIF}
finalization
  with GetMonitorLog(True) do
  begin
    if (Count>0) or (GetFileSize(ProgramPath+MemLogFile)>0) then
    try
      SaveToFile(ProgramPath+MemLogFile);
    except
    end;
    if Count>0 then
      MessageBox(0,PChar(Text),'Monitor has detected a memory leak',MB_OK or MB_ICONWARNING);
    Free;
  end;

  {$IFDEF MonitorAll}
  SetMemoryManager(OrgMemManager);
  {$ENDIF}

  Sync.Free;
  Log.Free;
  PointerList.Free;
  ObjectList.Free;
{$ENDIF}
end.

