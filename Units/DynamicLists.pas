////////////////////////////////////////////////////////////////////////////////
//
// DynamicLists.pas - Dynamic list class
// -------------------------------------
// Version:   2004-08-09
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
// Last changes:
//   TDynamicIntegerList added
//
// Suggested improvements:
//
unit DynamicLists;

interface

uses Windows, Classes, SysUtils, Streams, StreamUtils, MemUtils, SortUtils;

type
  TDynamicList = class(TStreamClass)
    private
      FMirrorPointer : ^Pointer;
      FListSize : Integer;
      FRecordSize : Integer;
      procedure SetListSize(NewSize: Integer);
      procedure SetCount(NewCount: Integer);
    protected
      FData : PByteArray;
      FCount : Integer;
      property ListSize: Integer read FListSize write SetListSize;
      procedure Initialize(RecordSize: Integer; var MirrorPointer);
    public
      // FListSize increase in IncCount when list too small, 0 mean double
      GrowBy : Integer;

      constructor Create(RecordSize: Integer; var MirrorPointer);
      destructor Destroy; override;

      procedure Assign(Other: TObject); override;
      procedure Append(Next: TDynamicList);
      procedure Clear; virtual;
      function Add(const Item): Integer;
      procedure Move(Index,NewIndex: Integer);
      // Delete Index and preserve list order
      procedure Delete(Index: Integer);
      // Delete Index, list order can be changed
      procedure FastDelete(Index: Integer);

      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream); override;

      property RecordSize: Integer read FRecordSize;
      property Count: Integer read FCount write SetCount;

      // Increase count by 1
      procedure IncCount;
      function Next: Integer;
      // Fill data with zeros
      procedure Zero;
      // Optimize memory use
      procedure Optimize; virtual;
    end;

  TListMember = class;

  TDynamicObjectList = class(TDynamicList)
    protected
      // If true, objects are freed on delete and destroy
      OwnsObjects : Boolean;
    public
      constructor Create(var MirrorPointer);

      procedure Clear; override;
      function Add(NewObject: TObject): Integer;
      function AddUnique(NewObject: TObject): Integer;
      procedure Insert(Index: Integer; NewObject: TObject);
      function IndexOf(Obj: TObject): Integer; virtual;
      procedure FreeAndNilAll;
      // Delete object and preserve list order
      procedure Delete(Index: Integer);
      procedure Remove(Obj: TObject);
      // Delete object but does preserve list order
      procedure FastDelete(Index: Integer);
      procedure FastRemove(Obj: TObject); overload;
      procedure FastRemove(Obj: TObject; FailIfNotExists: Boolean); overload;
      // Quick sort with compare callback
      procedure Sort(Compare: TListSortCompare);
    end;

  // Member of TDynamicObjectList. A TListMember can only be member of ONE list
  TListMember = class(TStreamClass)
    private
      FOwner : TDynamicObjectList;
      procedure SetOwner(List: TDynamicObjectList);
    public
      property Owner: TDynamicObjectList read FOwner write SetOwner;
      function AddTo(List: TDynamicObjectList): Integer;
      procedure InsertIn(List: TDynamicObjectList; Index: Integer);
      procedure Remove;
    end;

  // Sorted object list to enable binary search for fast IndexOf
  TSortedObjectList = class(TDynamicObjectList)
    protected
      GroupAdd : Boolean;
      procedure Sort;
      function Find(Obj: TObject; out Index: Integer): Boolean;
    public
      function Add(NewObject: TObject): Integer;
      function AddUnique(NewObject: TObject): Integer;
      function IndexOf(Obj: TObject): Integer; override;

      // Call before adding many objects to postpone the sorting
      procedure BeginGroupAdd;
    end;

  TDynamicPointerList = class(TDynamicList)
    protected
      // If true, objects are freed on clear and destroy
      OwnsObjects : Boolean;
    public
      constructor Create(var MirrorPointer);
      procedure Clear; override;
      function Add(NewData: Pointer): Integer;
    end;

  TDynamicIntegerList = class(TDynamicList)
    protected
      FValues : PIntegerArray;
    public
      constructor Create;
      function Add(Value: Integer): Integer;
      procedure Insert(Index: Integer; Value: Integer);
      function IndexOf(Value: Integer): Integer;
      property Values: PIntegerArray read FValues;
    end;

implementation

//=====================================================================================================
// TDynamicList
//=====================================================================================================
constructor TDynamicList.Create(RecordSize: Integer; var MirrorPointer);
begin
  inherited Create;
  FRecordSize:=RecordSize;
  FMirrorPointer:=@MirrorPointer;
end;

destructor TDynamicList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDynamicList.Initialize(RecordSize: Integer; var MirrorPointer);
begin
  Assert((FRecordSize=0) or (FRecordSize=RecordSize));
  Assert((FMirrorPointer=nil) or (FMirrorPointer=@MirrorPointer));
  FRecordSize:=RecordSize;
  FMirrorPointer:=@MirrorPointer;
end;

procedure TDynamicList.SetListSize(NewSize: Integer);
begin
  Assert(FCount<=FListSize); // Fail if not FCount<=FListSize
  if NewSize<>FListSize then
  begin
    FListSize:=NewSize;
    ReallocMem(FData,NewSize*FRecordSize);
    FMirrorPointer^:=FData;
    if NewSize<FCount then FCount:=NewSize;
  end;
end;

procedure TDynamicList.SetCount(NewCount: Integer);
begin
  if NewCount>ListSize then ListSize:=NewCount;
  FCount:=NewCount;
end;

procedure TDynamicList.SaveToStream(Stream: TBaseStream);
begin
  Stream.Write(FCount,4);
  Stream.Write(FData^,FCount*FRecordSize);
end;

procedure TDynamicList.LoadFromStream(Stream: TBaseStream);
var
  ACount : Integer;
begin
  ListSize:=0;
  if Stream.Read(ACount,4)=4 then
  begin
    Count:=ACount;
    Stream.Read(FData^,ACount*FRecordSize);
  end;
end;

procedure TDynamicList.Assign(Other: TObject);
begin
  if Other is TDynamicList then
  begin
    Assert(TDynamicList(Other).FRecordSize=FRecordSize);
    Clear;
    ListSize:=TDynamicList(Other).Count;
    FCount:=ListSize;
    System.Move(TDynamicList(Other).FData^,FData^,Count*FRecordSize);
  end
  else inherited;
end;

procedure TDynamicList.Append(Next: TDynamicList);
var
  NewStart : Integer;
begin
  NewStart:=Count;
  Count:=Count+Next.Count;
  System.Move(Next.FData^,FData^[NewStart*FRecordSize],Next.Count*FRecordSize);
end;

procedure TDynamicList.Clear;
begin
  ListSize:=0;
end;

procedure TDynamicList.IncCount;
begin
  if FCount>=ListSize then
  begin
    if (ListSize=0) and (GrowBy=0) then ListSize:=16
    else if GrowBy>0 then ListSize:=ListSize+GrowBy
    else ListSize:=ListSize*2;
  end;
  Inc(FCount);
end;

function TDynamicList.Next: Integer;
begin
  Result:=FCount;
  if FCount>=ListSize then
  begin
    if (ListSize=0) and (GrowBy=0) then ListSize:=16
    else if GrowBy>0 then ListSize:=ListSize+GrowBy
    else ListSize:=ListSize*2;
  end;
  Inc(FCount);
end;

procedure TDynamicList.Zero;
begin
  ZeroMem(FData^,Count*FRecordSize);
end;

procedure TDynamicList.Optimize;
begin
  ListSize:=Count;
end;

function TDynamicList.Add(const Item): Integer;
begin
  Result:=Next;
  System.Move(Item,FData^[Result*FRecordSize],FRecordSize);
end;

procedure TDynamicList.Move(Index,NewIndex: Integer);
var
  Temp : Pointer;
begin
  Assert((Index>=0) and (Index<Count));
  Assert((NewIndex>=0) and (NewIndex<Count));
  if Index<>NewIndex then
  begin
    if FRecordSize=4 then Temp:=Pointer(PIntegerArray(FData)^[Index])
    else
    begin
      GetMem(Temp,FRecordSize);
      System.Move(FData^[Index*FRecordSize],Temp^,FRecordSize);
    end;

    if NewIndex<Index then System.Move(FData^[NewIndex*FRecordSize],FData^[(NewIndex+1)*FRecordSize],(Index-NewIndex)*FRecordSize)
    else System.Move(FData^[(Index+1)*FRecordSize],FData^[Index*FRecordSize],(NewIndex-Index)*FRecordSize);

    if FRecordSize=4 then Pointer(PIntegerArray(FData)^[NewIndex]):=Temp
    else
    begin
      System.Move(Temp^,FData^[NewIndex*FRecordSize],FRecordSize);
      FreeMem(Temp);
    end;
  end;
end;

procedure TDynamicList.Delete(Index: Integer);
begin
  Assert((Index>=0) and (Index<Count));
  Dec(FCount);
  System.Move(FData^[(Index+1)*FRecordSize],FData^[Index*FRecordSize],(Count-Index)*FRecordSize);
end;

procedure TDynamicList.FastDelete(Index: Integer);
begin
  Assert((Index>=0) and (Index<Count));
  Dec(FCount);
  System.Move(FData^[FCount*FRecordSize],FData^[Index*FRecordSize],FRecordSize);
end;

//=====================================================================================================
// TDynamicObjectList
//=====================================================================================================
constructor TDynamicObjectList.Create(var MirrorPointer);
begin
  inherited Create(SizeOf(Pointer),MirrorPointer);
  OwnsObjects:=True;
end;

procedure TDynamicObjectList.Clear;
var I : Integer;
begin
  if OwnsObjects then for I:=0 to Count-1 do PObjectArray(FData)^[I].Free;
  inherited;
end;

function TDynamicObjectList.Add(NewObject: TObject): Integer;
begin
  Assert(not (NewObject is TListMember) or (TListMember(NewObject).Owner=Self),'A TListMember must use AddTo');
  Result:=Count;
  IncCount;
  PObjectArray(FData)^[Result]:=NewObject;
end;

function TDynamicObjectList.AddUnique(NewObject: TObject): Integer;
begin
  Result:=IndexOf(NewObject);
  if Result=-1 then Result:=Add(NewObject);
end;

function TDynamicObjectList.IndexOf(Obj: TObject): Integer;
begin
  for Result:=0 to Count-1 do if PObjectArray(FData)^[Result]=Obj then Exit;
  Result:=-1;
end;

procedure TDynamicObjectList.Insert(Index: Integer; NewObject: TObject);
begin
  IncCount;
  System.Move(PObjectArray(FData)^[Index],PObjectArray(FData)^[Index+1],(FCount-1-Index)*4);
  PObjectArray(FData)^[Index]:=NewObject;
end;

procedure TDynamicObjectList.FreeAndNilAll;
var
  I : Integer;
begin
  for I:=0 to Count-1 do FreeAndNil(PObjectArray(FData)^[I]);
end;

procedure TDynamicObjectList.Delete(Index: Integer);
begin
  Assert(Index<>-1);
  if OwnsObjects then PObjectArray(FData)^[Index].Free;
  Dec(FCount);
  System.Move(FData^[(Index+1)*FRecordSize],FData^[Index*FRecordSize],(Count-Index)*FRecordSize);
end;

procedure TDynamicObjectList.FastDelete(Index: Integer);
begin
  Assert(Index<>-1);
  if OwnsObjects then PObjectArray(FData)^[Index].Free;
  Dec(FCount);
  PObjectArray(FData)^[Index]:=PObjectArray(FData)^[FCount];
end;

procedure TDynamicObjectList.Remove(Obj: TObject);
begin
  Delete(IndexOf(Obj));
end;

procedure TDynamicObjectList.FastRemove(Obj: TObject);
begin
  FastDelete(IndexOf(Obj));
end;

procedure TDynamicObjectList.FastRemove(Obj: TObject; FailIfNotExists: Boolean);
var
  I : Integer;
begin
  I:=IndexOf(Obj);
  if I>=0 then FastDelete(I)
  else Assert(not FailIfNotExists,ClassName+': Object not in list');
end;

procedure TDynamicObjectList.Sort(Compare: TListSortCompare);

  procedure QuickSort(SortList: PPointerList; L, R: Integer);
  var
    I, J: Integer;
    P, T: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := SortList^[(L + R) shr 1];
      repeat
        while Compare(SortList^[I], P) < 0 do
          Inc(I);
        while Compare(SortList^[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(SortList, L, J);
      L := I;
    until I >= R;
  end;

begin
  if Count>0 then QuickSort(PPointerList(FData),0,Count-1);
end;

//=====================================================================================================
// TListMember
//=====================================================================================================
function TListMember.AddTo(List: TDynamicObjectList): Integer;
begin
  if Assigned(Owner) then Owner.Remove(Self);
  FOwner:=List;
  Result:=List.Add(Self)
end;

procedure TListMember.InsertIn(List: TDynamicObjectList; Index: Integer);
begin
  if Assigned(Owner) then Owner.Remove(Self);
  FOwner:=List;
  List.Insert(Index,Self);
end;

procedure TListMember.SetOwner(List: TDynamicObjectList);
begin
  AddTo(List);
end;

procedure TListMember.Remove;
var
  I : Integer;
begin
  with Owner do
  begin
    Assert(OwnsObjects);
    I:=IndexOf(Self);
    if I<>-1 then Delete(I)
    else Destroy;
  end;
end;

//=====================================================================================================
// TSortedObjectList
//=====================================================================================================

function TSortedObjectList.Find(Obj: TObject; out Index: Integer): Boolean;
var
  L, H, I : Integer;
begin
  if GroupAdd then Sort;
  Result:=False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if DWord(PObjectArray(FData)^[I])<DWord(Obj) then L := I + 1 else
    begin
      H := I - 1;
      if PObjectArray(FData)^[I]=Obj then
      begin
        Result := True;
        L := I; // Correct?
      end;
    end;
  end;
  Index := L;
end;

procedure TSortedObjectList.BeginGroupAdd;
begin
  GroupAdd:=True;
end;

procedure TSortedObjectList.Sort;
begin
  GroupAdd:=False;
  QuickSortDWord(PDWordArray(FData)^,0,Count-1);
end;

function TSortedObjectList.Add(NewObject: TObject): Integer;
begin
  if GroupAdd then Result:=inherited Add(NewObject)
  else
  begin
    Find(NewObject,Result);
    Insert(Result,NewObject);
  end;
end;

function TSortedObjectList.AddUnique(NewObject: TObject): Integer;
begin
  if not Find(NewObject,Result) then Insert(Result,NewObject);
end;

function TSortedObjectList.IndexOf(Obj: TObject): Integer;
begin
  if not Find(Obj,Result) then Result:=-1;
end;

//=====================================================================================================
// TDynamicPointerList
//=====================================================================================================
constructor TDynamicPointerList.Create(var MirrorPointer);
begin
  inherited Create(SizeOf(Pointer),MirrorPointer);
  OwnsObjects:=True;
end;

procedure TDynamicPointerList.Clear;
var I : Integer;
begin
  if OwnsObjects then for I:=0 to Count-1 do FreeMem(PPointerArray(FData)^[I]);
  inherited;
end;

function TDynamicPointerList.Add(NewData: Pointer): Integer;
begin
  Result:=Count;
  IncCount;
  PPointerArray(FData)^[Result]:=NewData;
end;

//=====================================================================================================
// TDynamicIntegerList
//=====================================================================================================

constructor TDynamicIntegerList.Create;
begin
  inherited Create(SizeOf(Integer),FValues);
end;

function TDynamicIntegerList.Add(Value: Integer): Integer;
begin
  Result:=Count;
  IncCount;
  Values^[Result]:=Value;
end;

function TDynamicIntegerList.IndexOf(Value: Integer): Integer;
begin
  for Result:=0 to Count-1 do if Values^[Result]=Value then Exit;
  Result:=-1;
end;

procedure TDynamicIntegerList.Insert(Index, Value: Integer);
begin
  IncCount;
  System.Move(Values^[Index],Values^[Index+1],(FCount-1-Index)*SizeOf(Integer));
  Values^[Index]:=Value;
end;

end.

