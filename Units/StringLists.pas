///////////////////////////////////////////////////////////////////////////////////////////////
//
// StringLists.pas
// ---------------
// Changed:   2003-12-24
// Maintain:  Michael Vinther: mv@logicnet·dk
//
// Contains:
//   (TStrings)
//     TOpenStringList - Like TStringList, but case sensitive
//       TStringObjectList - Same as TOpenStringList, but TStringObjectList
//                           free objects on Delele or Clear
//       TStringDataList - Same as TStringObjectList just with pointer in stead of TObject
//
// Last changes:
//   TStringIntegerList updated
//
unit StringLists;

interface

uses Classes, SysUtils, MemUtils;

resourcestring
  rsDuplicateString = 'String list does not allow duplicates';
  rsSortedListError = 'Operation not allowed on sorted string list';
  rsListIndexError  = 'List index out of bounds (%d)';

type
  TOpenStringList = class;

  TOpenStringListSortCompare = function(List: TOpenStringList; Index1, Index2: Integer): Integer;

  // Like TStringList, but case sensitive by default
  TOpenStringList = class(TStrings)
    protected
      FList: PStringItemList;
      FCount: Integer;
      FCapacity: Integer;
      FSorted, FCaseInsensitive: Boolean;
      FDuplicates: TDuplicates;
      procedure ExchangeItems(Index1, Index2: Integer);
      procedure Grow;
      procedure QuickSort(L, R: Integer; SCompare: TOpenStringListSortCompare);
      procedure InsertItem(Index: Integer; const S: string);
      procedure SetSorted(Value: Boolean);
      procedure SetCaseInsensitive(Value: Boolean);
      function Get(Index: Integer): string; override;
      function GetCapacity: Integer; override;
      function GetCount: Integer; override;
      procedure SetCount(Count: Integer); virtual;
      function GetObject(Index: Integer): TObject; override;
      procedure Put(Index: Integer; const S: string); override;
      procedure PutObject(Index: Integer; AObject: TObject); override;
      procedure SetCapacity(NewCapacity: Integer); override;
      function CompareStrings(const S1, S2: string): Integer; override;
    public
      destructor Destroy; override;
      function Add(const S: string): Integer; override;
      procedure Clear; override;
      procedure Delete(Index: Integer); override;
      // Delete without preserving list order
      procedure FastDelete(Index: Integer);
      procedure Exchange(Index1, Index2: Integer); override;
      function Find(const S: string; out Index: Integer): Boolean; virtual;
      function IndexOf(const S: string): Integer; override;
      procedure Insert(Index: Integer; const S: string); override;
      procedure Sort; virtual;
      procedure SubtractList(Other: TStrings);
      procedure CustomSort(Compare: TOpenStringListSortCompare); virtual;
      property Duplicates: TDuplicates read FDuplicates write FDuplicates;
      property Sorted: Boolean read FSorted write SetSorted;
      property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
      property Count: Integer read FCount write SetCount;
      property List: PStringItemList read FList;
    end;

  // Same as TOpenStringList just with integer in stead of TObject
  TStringIntegerList = class(TOpenStringList)
    protected
      function GetData(Index: Integer): Integer;
      procedure SetData(Index: Integer; Data: Integer);
    public
      // Add string or increase Value if dublicate
      function AddCount(const S: string): Integer;
      // Add string and set value
      function AddValue(const S: string; Value: Integer): Integer;
      // Same as Object property, just with Integer type
      property Value[Index: Integer]: Integer read GetData write SetData;
      procedure SortByValue;
      function IndexOfValue(Value: Integer): Integer;
      procedure SaveToFileWithValue(const FileName: string);
      procedure LoadFromFileWithValue(const FileName: string);
    end;

  // Same as TOpenStringList, but TStringObjectList free objects on Delete or Clear
  TStringObjectList = class(TOpenStringList)
    public
      procedure Clear; override;
      procedure Delete(Index: Integer); override;
      destructor Destroy; override;

      // Delete object, free if not nil
      procedure DeleteObject(Index: Integer);
    end;

  // Same as TStringObjectList just with pointer in stead of TObject
  TStringDataList = class(TOpenStringList)
    protected
      function GetData(Index: Integer): Pointer;
      procedure SetData(Index: Integer; Data: Pointer);
    public
      procedure Clear; override;
      procedure Delete(Index: Integer); override;
      destructor Destroy; override;
      function AddData(const S: string; Data: Pointer): Integer;

      // Delete data, free if not nil
      procedure DeleteData(Index: Integer);

      // Same as Object property, just with Pointer type
      property Data[Index: Integer]: Pointer read GetData write SetData;
    end;

// Case sensitive compare for CustomSort (default for Sort)
function StringListCompareCase(List: TOpenStringList; Index1, Index2: Integer): Integer;
// Case insensitive compare for CustomSort
function StringListCompare(List: TOpenStringList; Index1, Index2: Integer): Integer;
function StringListCompareInv(List: TOpenStringList; Index1, Index2: Integer): Integer;
// ANSI case insensitive compare for CustomSort
function StringListAnsiCompare(List: TOpenStringList; Index1, Index2: Integer): Integer;
// Signed value compare for CustomSort
function StringListCompareValue(List: TOpenStringList; Index1, Index2: Integer): Integer;
function StringListCompareValueInv(List: TOpenStringList; Index1, Index2: Integer): Integer;

implementation

uses Windows;

function StringListAnsiCompare(List: TOpenStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List.FList^[Index1].FString,List.FList^[Index2].FString);
end;

function StringListCompare(List: TOpenStringList; Index1, Index2: Integer): Integer;
begin
  Result:=CompareText(List.FList^[Index1].FString,List.FList^[Index2].FString);
end;

function StringListCompareInv(List: TOpenStringList; Index1, Index2: Integer): Integer;
begin
  Result:=-CompareText(List.FList^[Index1].FString,List.FList^[Index2].FString);
end;

function StringListCompareCase(List: TOpenStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List.FList^[Index1].FString,List.FList^[Index2].FString);
end;

function StringListCompareValue(List: TOpenStringList; Index1, Index2: Integer): Integer;
begin
  if Integer(List.FList^[Index1].FObject)=Integer(List.FList^[Index2].FObject) then Result:=0
  else if Integer(List.FList^[Index1].FObject)<Integer(List.FList^[Index2].FObject) then Result:=-1
  else Result:=1;
end;

function StringListCompareValueInv(List: TOpenStringList; Index1, Index2: Integer): Integer;
begin
  if Integer(List.FList^[Index1].FObject)=Integer(List.FList^[Index2].FObject) then Result:=0
  else if Integer(List.FList^[Index1].FObject)<Integer(List.FList^[Index2].FObject) then Result:=1
  else Result:=-1;
end;

//==================================================================================================
// TOpenStringList
//==================================================================================================

destructor TOpenStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TOpenStringList.Add(const S: string): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(rsDuplicateString,0);
      end;
  InsertItem(Result, S);
end;

procedure TOpenStringList.Clear;
begin
  if FCount<>0 then
  begin
    Finalize(FList^[0],FCount);
    FCount:=0;
    SetCapacity(0);
  end;
end;

procedure TOpenStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError,Index);
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
end;

procedure TOpenStringList.FastDelete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError,Index);
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index<FCount then
  begin
    System.Move(FList^[FCount],FList^[Index],SizeOf(TStringItem));
    FSorted:=False;
  end;
end;

procedure TOpenStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(rsListIndexError,Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(rsListIndexError,Index2);
  ExchangeItems(Index1, Index2);
end;

procedure TOpenStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TOpenStringList.Find(const S: string; out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOpenStringList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError,Index);
  Result := FList^[Index].FString;
end;

function TOpenStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TOpenStringList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TOpenStringList.SetCount(Count: Integer);
var
  I : Integer;
begin
  if Count<=FCount then for I:=FCount-1 downto Count do Delete(I)
  else
  begin
    if Count>FCapacity then SetCapacity(Count);
    ZeroMem(FList^[FCount],SizeOf(FList^[0])*(Count-FCount));
    FCount:=Count;
  end;
end;

function TOpenStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError,Index);
  Result := FList^[Index].FObject;
end;

procedure TOpenStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TOpenStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result:=inherited IndexOf(S)
  else if not Find(S,Result) then Result := -1;
end;

procedure TOpenStringList.Insert(Index: Integer; const S: string);
begin
  if Sorted then Error(rsSortedListError,0);
  if (Index < 0) or (Index > FCount) then Error(rsListIndexError,Index);
  InsertItem(Index, S);
end;

procedure TOpenStringList.InsertItem(Index: Integer; const S: string);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
end;

procedure TOpenStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then Error(rsSortedListError,0);
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError, Index);
  FList^[Index].FString := S;
end;

procedure TOpenStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError, Index);
  FList^[Index].FObject := AObject;
end;

procedure TOpenStringList.QuickSort(L, R: Integer; SCompare: TOpenStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TOpenStringList.SetCapacity(NewCapacity: Integer);
begin
  System.ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

procedure TOpenStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TOpenStringList.SetCaseInsensitive(Value: Boolean);
begin
  if FCaseInsensitive <> Value then
  begin
    FCaseInsensitive := Value;
    if Sorted then Sort;
  end;
end;

procedure TOpenStringList.Sort;
begin
  if CaseInsensitive then CustomSort(StringListAnsiCompare)
  else CustomSort(StringListCompareCase);
end;

procedure TOpenStringList.CustomSort(Compare: TOpenStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    QuickSort(0, FCount - 1, Compare);
  end;
end;

procedure TOpenStringList.SubtractList(Other: TStrings);
var
  I : Integer;
begin
  for I:=Count-1 downto 0 do if Other.IndexOf(Strings[I])<>-1 then Delete(I);
end;

function TOpenStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseInsensitive then Result:=AnsiCompareText(S1, S2)
  else Result:=CompareStr(S1,S2);
end;

//==================================================================================================
// TStringDataList
//==================================================================================================

procedure TStringDataList.Clear;
var
  I : Integer;
  Data : Pointer;
begin
  for I:=0 to Count-1 do
  begin
    Data:=FList^[I].FObject;
    if Assigned(Data) then FreeMem(Data);
  end;
  inherited;
end;

procedure TStringDataList.Delete(Index: Integer);
var
  Data : Pointer;
begin
  Data:=GetData(Index);
  if Assigned(Data) then FreeMem(Data);
  inherited;
end;

procedure TStringDataList.DeleteData(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError, Index);
  FreeAndNilData(FList^[Index].FObject);
end;

destructor TStringDataList.Destroy;
begin
  Clear;
  inherited;
end;

function TStringDataList.AddData(const S: string; Data: Pointer): Integer;
begin
  Result:=Add(S);
  FList^[Result].FObject:=Data;
end;

function TStringDataList.GetData(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TStringDataList.SetData(Index: Integer; Data: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError, Index);
  FList^[Index].FObject := Data;
end;

//==================================================================================================
// TStringObjectList
//==================================================================================================

procedure TStringObjectList.Clear;
var I : Integer;
begin
  for I:=0 to Count-1 do FList^[I].FObject.Free;
  inherited;
end;

procedure TStringObjectList.Delete(Index: Integer);
begin
  Objects[Index].Free;
  inherited;
end;

destructor TStringObjectList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TStringObjectList.DeleteObject(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError, Index);
  FreeAndNil(FList^[Index].FObject);
end;

//==================================================================================================
// TStringIntegerList
//==================================================================================================
function TStringIntegerList.GetData(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError,Index);
  Result:=Integer(FList^[Index].FObject);
end;

procedure TStringIntegerList.SetData(Index: Integer; Data: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(rsListIndexError,Index);
  FList^[Index].FObject:=Pointer(Data);
end;

function TStringIntegerList.AddValue(const S: string; Value: Integer): Integer;
begin
  Result:=Add(S);
  FList^[Result].FObject:=Pointer(Value);
end;

function TStringIntegerList.AddCount(const S: string): Integer;
begin
  if not Sorted then
  begin
    Result:=IndexOf(S);
    if Result=-1 then Result:=FCount
    else
    begin
      Inc(Integer(FList^[Result].FObject));
      Exit;
    end;
  end
  else if Find(S,Result) then
  begin
    Inc(Integer(FList^[Result].FObject));
    Exit;
  end;
  InsertItem(Result,S);
  Integer(FList^[Result].FObject):=1;
end;

procedure TStringIntegerList.SortByValue;
begin
  Sorted:=False;
  CustomSort(StringListCompareValue);
end;

function TStringIntegerList.IndexOfValue(Value: Integer): Integer;
begin
  Result:=IndexOfObject(TObject(Value));
end;

procedure TStringIntegerList.SaveToFileWithValue(const FileName: string);
var
  F : TextFile;
  I : Integer;
begin
  AssignFile(F,FileName);
  Rewrite(F);
  try
    for I:=0 to Count-1 do WriteLn(F,Integer(FList^[I].FObject),',',FList^[I].FString);
  finally
    CloseFile(F);
  end;
end;

procedure TStringIntegerList.LoadFromFileWithValue(const FileName: string);
var
  F : TextFile;
  P : Integer;
  Str : string;
begin
  Clear;
  AssignFile(F,FileName);
  Reset(F);
  try
    while not EOF(F) do
    begin
      ReadLn(F,Str);
      if Str<>'' then
      begin
        P:=Pos(',',Str);
        AddValue(Copy(Str,P+1,MaxInt),StrToInt(Copy(Str,1,P-1)));
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

end.

