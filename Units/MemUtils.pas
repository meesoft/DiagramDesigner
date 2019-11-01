///////////////////////////////////////////////////////////////////////////////////////////////
//
// MemUtils.pas
// --------------------------
// Changed:   2005-01-27
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Assembler optimized routines requires minimum 486 processor
//
// Last change: ReplaceChar moved to StringUtils
//
unit MemUtils;

interface

uses
  {$IFNDEF Package}
  FastMM4,
  {$ENDIF}
  
  Windows,
  SysUtils;

type
  TObjectArray = array [0..32767] of TObject;
  PObjectArray = ^TObjectArray;
  TCardinalArray = array[0..32767] of Cardinal;
  PCardinalArray = ^TCardinalArray;
  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..16383] of SmallInt;

  Int64Split = packed record
                 case Integer of
                   0 : (Lo : Cardinal;
                        Hi : Integer);
                   1 : (Value : Int64);
               end;
  PInt64Split = ^Int64Split;

  TAssignObject = class
    public
      // All descendants of Assign and AssignTo should call inherited if
      // supplied with an unsupported object type
      procedure Assign(Other: TObject); virtual;
      procedure AssignTo(Other: TObject); virtual;
    end;

  TMemoryMappedFile = class
    protected
      FHandle, FFileHandle : THandle;
      FMemory, FMappedAddress : Pointer;
      FAllocationGranularity : DWord;
      FSize : Int64;
      FMappedRangeFrom, FMappedRangeToExcl : Int64;
      FMappedRangeSize : DWord;
    public
      // Map in swap file
      constructor Create(Size: DWord; MapFullRange: Boolean=True); overload;
      // Map in specified file
      constructor Create(FileHandle: THandle; const Size: Int64; MapFullRange: Boolean=True); overload;
      destructor Destroy; override;
      property Size: Int64 read FSize;
      property Memory: Pointer read FMemory;
      property MappedRangeFrom : Int64 read FMappedRangeFrom;
      property MappedRangeToExcl : Int64 read FMappedRangeToExcl; // [MappedRangeFrom;MappedRangeToExcl[
      property MappedRangeSize : DWord read FMappedRangeSize;
      procedure SelectMappedRange(const RangeFrom: Int64; const RangeSize: DWord);
      procedure UnmapFile;
    end;

  TTemporaryStorageFile = class
    private
      FileHandle : THandle;
      MemoryPtrPtr : PPointer;
    public
      constructor Create(var MemoryPtr; Size: DWord);
      destructor Destroy; override;
      procedure Restore;
    end;

function ObjListAdd(var ObjList; Obj: TObject): Integer;
function ObjListIndexOf(var ObjList; Obj: TObject): Integer; overload;
function ObjListDeleteFast(var ObjList; Index: Integer): Boolean; overload;
function ObjListDeleteFast(var ObjList; Obj: TObject): Boolean; overload;
function ObjListDestroyFast(var ObjList; Index: Integer): Boolean; overload;
function ObjListDestroyFast(var ObjList; Obj: TObject): Boolean; overload;
procedure ObjListDestroyAll(var ObjList); overload;

// Find byte in buffer, return position or -1 if not found
function FastLocateByte(const Where; Start, BSize: Integer; What: Word): Integer; assembler; pascal;
// Find 2 bytes in buffer, return position or -1 if not found
function FastLocate2Bytes(const Where; Start, BSize: Integer; What: Word): Integer; assembler; pascal;
// Find 4 bytes in buffer at DWord boundaries. Return position or -1 if not found  >>>> UNTESTED <<<<
function FastLocateDWord(var Where; BSize: Integer; What: LongInt): Integer; assembler; register;

procedure FillDWord(var Dest; Count: Integer; Value: Cardinal); assembler; register;
// Fill Dest with zeros
procedure ZeroMem(var Dest; Count: Integer);

// Reverse byte order
function GetSwap2(A: Word): Word; assembler; register;
function GetSwap4(A: Cardinal): Cardinal; assembler; register;
procedure Swap4(var A: Cardinal); assembler; register;

procedure SwapDWords(var A,B); assembler; register;

// Like FreeMem, but checks for nil and set to nil after freeing
procedure FreeAndNilData(var P);

function CreateOrGetObject(var Obj: TObject; ObjectType: TClass): Pointer;

// Returns min(available physical memory,2GB-used memory)
function AvailablePhysicalMemory: DWord;

// Returns memory allocated from FastMM4
function TotalMemoryUsed: DWord;

implementation

uses FileUtils;

//==============================================================================================================================

// Returns memory allocated from FastMM4
function TotalMemoryUsed: DWord;
{$IFDEF Package}
begin
  Result:=1;
end;
{$ELSE}
var
  I : Integer;
  State : TMemoryManagerState;
begin
  FastMM4.GetMemoryManagerState(State);
  Result:=State.TotalAllocatedLargeBlockSize+State.TotalAllocatedMediumBlockSize;
  for I:=0 to High(State.SmallBlockTypeStates) do
    Inc(Result,State.SmallBlockTypeStates[I].ReservedAddressSpace);
end;
{$ENDIF}

// Returns min(available physical memory,2GB-used memory)
function AvailablePhysicalMemory: DWord;
const
  GB : DWord = 1 shl 30;
var
  Mem : TMemoryStatus;
  Used : DWord;
begin
  GlobalMemoryStatus(Mem);
  Result:=Mem.dwAvailPhys;

  Used:=TotalMemoryUsed;
  if Used>2*GB then
    Result:=0
  else if Result>2*GB-Used then
    Result:=2*GB-Used;
end;

function CreateOrGetObject(var Obj: TObject; ObjectType: TClass): Pointer;
begin
  if Cardinal(@Obj)<1024 then Result:=ObjectType.Create
  else
  begin
    if Obj=nil then Obj:=ObjectType.Create;
    Result:=Obj;
  end;
end;

procedure FreeAndNilData(var P);
var
  Ptr : Pointer;
begin
  if Assigned(Pointer(P)) then
  begin
    Ptr:=Pointer(P);
    Pointer(P):=nil;
    FreeMem(Ptr);
  end;
end;

// Find byte in buffer, return position or -1 if not found
function FastLocateByte(const Where; Start, BSize: Integer; What: Word): Integer; assembler; pascal;
asm
  push edi
  mov ecx, [bsize]
  sub ecx, [start]
  jz @notfound       // No data to search
  mov edi, [where]
  add edi, [start]
  mov ax, [what]
  @search:
  repne scasb
  je @found
  @notfound:
  mov eax, -1
  jmp @end
  @found:
  mov eax, edi
  dec eax
  sub eax, [where]
  @end:
  pop edi
end;

function FastLocate2Bytes(const where; start, bsize: integer; what: word):integer; assembler; pascal; far;
asm
 push edi
 mov ecx, [bsize]
 sub ecx, [start]
 jz @notfound       // No data to search
 mov edi, [where]
 add edi, [start]
 mov ax, [what]
 @search:
 repne scasb
 je @found
 @notfound:
 mov eax, -1
 jmp @end
 @found:
 cmp [edi], ah
 jne @search
 mov eax, edi
 dec eax
 sub eax, [where]
 @end:
 pop edi
end;

function FastLocateDWord(var Where; BSize: Integer; What: LongInt): Integer; assembler; register;
asm
 {eax=where; edx=bsize; ecx=what}
 push edi
 mov  edi, eax
 mov  eax, ecx
 mov  ecx, edx
 mov  edx, edi
 {edi=where; edx=where; eax=what; ecx=bsize}
 @search:
 repne scasd
 je @found
 @notfound:
 mov eax, -1
 jmp @end
 @found:
 mov eax, edi
 sub eax, edx
 shr eax, 2
 dec eax

 @end:
 pop  edi
end;

procedure ZeroMem(var Dest; Count: Integer);
begin
  FillChar(Dest,Count,0);
end;

// Like FillChar, just with DWords
procedure FillDWord(var Dest; Count: Integer; Value: Cardinal); assembler; register;
asm
  // eax=Dest; edx=Count; ecx=Value
  push edi  // protect edi
  mov  edi, eax  // edi=@dest
  mov  eax, ecx  // eax=Value
  mov  ecx, edx
  rep  stosd
  pop edi
end;

function GetSwap2(A: Word): Word; assembler; register;
asm
  mov cl, al
  mov al, ah
  mov ah, cl
end;

// Reverse byte order
procedure Swap4(var A: Cardinal); assembler; register;
asm
  mov   ecx, [eax]
  bswap ecx
  mov   [eax], ecx
end;

function GetSwap4(A: Cardinal): Cardinal; assembler; register;
asm
  bswap eax
end;

procedure SwapDWords(var A,B); assembler; register;
asm
  push  ebx
  mov   ebx, [eax]
  mov   ecx, [edx]
  mov   [eax], ecx
  mov   [edx], ebx
  pop   ebx
end;

//==============================================================================================================================

type
  TObjList = record
               List : array of TObject;
               Count : Integer;
             end;

// Add object to list and return its index
function ObjListAdd(var ObjList; Obj: TObject): Integer;
begin
  with TObjList(ObjList) do
  begin
    Assert(Cardinal(Count)<=Cardinal(Length(List)));
    if Length(List)=Count then
      if Count=0 then SetLength(List,4)
      else SetLength(List,Count*2);
    List[Count]:=Obj;
    Result:=Count;
    Inc(Count);
  end;
end;

// Return index of Obj in list or -1 if not found
function ObjListIndexOf(var ObjList; Obj: TObject): Integer; overload;
begin
  with TObjList(ObjList) do
  begin
    for Result:=0 to Count-1 do
      if List[Result]=Obj then Exit;
    Result:=-1;
  end;
end;

// Delete object from list by copying over last and return True if list is empty
function ObjListDeleteFast(var ObjList; Index: Integer): Boolean;
begin
  with TObjList(ObjList) do
  begin
    Assert(Index>=0);
    Assert(Index<Count);
    Dec(Count);
    List[Index]:=List[Count];
    Result:=Count=0;
  end;
end;

// Delete object from list by copying over last and return True if list is empty
function ObjListDeleteFast(var ObjList; Obj: TObject): Boolean; overload;
begin
  Result:=ObjListDeleteFast(ObjList,ObjListIndexOf(ObjList,Obj));
end;

// Delete and destroy object from list by copying over last and return True if list is empty
function ObjListDestroyFast(var ObjList; Index: Integer): Boolean;
begin
  with TObjList(ObjList) do
  begin
    Assert(Index>=0);
    Assert(Index<Count);
    List[Index].Destroy;
    Dec(Count);
    List[Index]:=List[Count];
    Result:=Count=0;
  end;
end;

// Delete and destroy object from list by copying over last and return True if list is empty
function ObjListDestroyFast(var ObjList; Obj: TObject): Boolean; overload;
begin
  Result:=ObjListDestroyFast(ObjList,ObjListIndexOf(ObjList,Obj));
end;

// Destroy all objects in list and set count=0
procedure ObjListDestroyAll(var ObjList); overload;
var
  I : Integer;
begin
  with TObjList(ObjList) do
  begin
    for I:=0 to Count-1 do List[I].Destroy;
    Count:=0;
  end;
end;

//==============================================================================================================================
// TAssignObject
//==============================================================================================================================
procedure TAssignObject.Assign(Other: TObject);
begin
  if Other is TAssignObject then TAssignObject(Other).AssignTo(Self)
  else raise Exception.Create('Cannot assign '+Other.ClassName+' to '+ClassName);
end;

procedure TAssignObject.AssignTo(Other: TObject);
begin
  if Other is TAssignObject then TAssignObject(Other).Assign(Self)
  else raise Exception.Create('Cannot assign '+ClassName+' to '+Other.ClassName);
end;

//==============================================================================================================================
// TMemoryMappedFile
//==============================================================================================================================
constructor TMemoryMappedFile.Create(FileHandle: THandle; const Size: Int64; MapFullRange: Boolean);
var
  SystemInfo: TSystemInfo;
begin
  inherited Create;
  FFileHandle:=FileHandle;
  FHandle:=CreateFileMapping(FFileHandle,nil,PAGE_READWRITE,Int64Split(Size).Hi,Int64Split(Size).Lo,nil);
  if FHandle=0 then RaiseLastOSError;
  FSize:=Size;
  if MapFullRange then
  begin
    Assert(Size<=High(DWord));
    FMappedAddress:=MapViewOfFile(FHandle,FILE_MAP_ALL_ACCESS,0,0,0);
    if FMappedAddress=nil then RaiseLastOSError;
    FMemory:=FMappedAddress;
    FMappedRangeSize:=Size;
    FMappedRangeToExcl:=Size;
  end
  else
  begin
    GetSystemInfo(SystemInfo);
    FAllocationGranularity:=SystemInfo.dwAllocationGranularity;
  end;
end;

constructor TMemoryMappedFile.Create(Size: DWord; MapFullRange: Boolean);
begin
  Create($FFFFFFFF,Size,MapFullRange); // Map to system swap file
end;

destructor TMemoryMappedFile.Destroy;
begin
  UnMapViewOfFile(FMappedAddress);
  CloseHandle(FHandle);
  CloseHandle(FFileHandle);
  inherited;
end;

procedure TMemoryMappedFile.SelectMappedRange(const RangeFrom: Int64; const RangeSize: DWord);
var
  I : Int64;
begin
  if FMappedAddress<>nil then
  begin
    UnMapViewOfFile(FMappedAddress);
    FMappedAddress:=nil;
  end;
  I:=RangeFrom mod FAllocationGranularity; // Must allocate on page boundary
  if I>0 then
  begin
    if I+RangeSize>High(RangeSize) then raise Exception.Create(ClassName+': Range overflow');
    FMappedRangeFrom:=RangeFrom-I;
    FMappedRangeSize:=RangeSize+I;
  end
  else
  begin
    FMappedRangeFrom:=RangeFrom;
    FMappedRangeSize:=RangeSize;
  end;
  FMappedAddress:=MapViewOfFile(FHandle,FILE_MAP_ALL_ACCESS,Int64Split(FMappedRangeFrom).Hi,Int64Split(FMappedRangeFrom).Lo,FMappedRangeSize);
  if FMappedAddress=nil then
  begin
    FMemory:=nil;
    FMappedRangeSize:=0;
    FMappedRangeToExcl:=0;
    RaiseLastOSError;
  end;
  FMappedRangeToExcl:=FMappedRangeFrom+FMappedRangeSize;
  FMemory:=Pointer(DWord(FMappedAddress)-DWord(FMappedRangeFrom));
end;

procedure TMemoryMappedFile.UnmapFile;
begin
  FMemory:=nil;
  if FMappedAddress<>nil then
  begin
    UnMapViewOfFile(FMappedAddress);
    FMappedAddress:=nil;
    FMappedRangeToExcl:=0;
    FMappedRangeSize:=0;
  end;
end;

//==============================================================================================================================
// TTemporaryStorageFile
//==============================================================================================================================

constructor TTemporaryStorageFile.Create(var MemoryPtr; Size: DWord);
begin
  FileHandle:=CreateFile(PChar(GetTempFileName),GENERIC_READ or GENERIC_WRITE,0,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE,0);
  if FileHandle=0 then RaiseLastOSError;

  MemoryPtrPtr:=@MemoryPtr;
  if not WriteFile(FileHandle,MemoryPtrPtr^^,Size,Size,nil) then RaiseLastOSError;

  FreeMem(MemoryPtrPtr^);
  MemoryPtrPtr^:=nil;
end;

destructor TTemporaryStorageFile.Destroy;
begin
  Restore;
  inherited;
end;

procedure TTemporaryStorageFile.Restore;
var
  Size : DWord;
begin
  Assert(MemoryPtrPtr^=nil);
  SetFilePointer(FileHandle,0,nil,FILE_BEGIN);
  Size:=Windows.GetFileSize(FileHandle,nil);
  GetMem(MemoryPtrPtr^,Size);
  if not ReadFile(FileHandle,MemoryPtrPtr^^,Size,Size,nil) then RaiseLastOSError;
end;

end.

