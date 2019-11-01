////////////////////////////////////////////////////////////////////////////////
//
// SortUtils.pas - Optimized sorting routines
// ------------------------------------------
// Version:   2004-02-24
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
// Last changes:
//   QuickSelect bug fix
//
unit SortUtils;

interface

uses SysUtils, Windows, MathUtils;

type
  SortType1 = Byte;
  SortType2 = Float;
  SortType3 = DWord;

  DWordArray = array[0..0] of DWord;
  PDWordArray = ^DWordArray;
  TDataRecord4 = record
                   Value : Integer;
                   Data  : Integer;
                 end;


procedure QuickSortByte(var List: array of SortType1; Min, Max: Integer);
procedure QuickSortDWord(var List: array of SortType3; Min, Max: Integer);
procedure QuickSortDataRecord4(var List: array of TDataRecord4; Count: Integer);

procedure HeapSort(var List: array of SortType1; Count: DWord; FirstNeeded: DWord=0);

// Extract element with index Wanted in sorted List. Median=QuickSelect(List,0,Count-1,Count div 2)
function QuickSelectByte(var List: array of SortType1; Min, Max, Wanted: Integer): SortType1;
function QuickSelectFloat(var List: array of SortType2; Min, Max, Wanted: Integer): SortType2;
function QuickSelectDWord(var List: array of SortType3; Min, Max, Wanted: Integer): SortType3;

implementation

procedure QuickSortByte(var List: array of SortType1; Min, Max: Integer);

  procedure QSort(Min, Max: Integer);
  var
    A, B : Integer;
    T, Z : SortType1;
  begin
    A:=Min; B:=Max; Z:=List[(A+B) shr 1];
    repeat
      while List[A]<Z do Inc(A);
      while Z<List[B] do Dec(B);
      if A<=B then
      begin
        T:=List[A]; List[A]:=List[B]; List[B]:=T;
        Inc(A); Dec(B);
      end;
    until A>B;
    if Min<B then QSort(Min,B);
    if A<Max then QSort(A,Max);
  end;

  procedure QSort2(Min, Max: Integer);
  var
    A, B : Integer;
    T, Z : SortType1;
  begin
    A:=Min;
    while Min<Max do
    begin
      B:=Max; Z:=List[(A+B) shr 1];
      repeat
        while List[A]<Z do Inc(A);
        while Z<List[B] do Dec(B);
        if A<=B then
        begin
          T:=List[A]; List[A]:=List[B]; List[B]:=T;
          Inc(A); Dec(B);
        end;
      until A>B;
      if Min<B then QSort2(Min,B);
      Min:=A;
    end;
  end;

  procedure QSort3(Min, Max: PByte);
  var
    A, B : PByte;
    T, Z : Byte;
  begin
    A:=Min;
    while DWord(Min)<DWord(Max) do
    begin
      B:=Max; Z:=PByte((DWord(A)+DWord(B)) shr 1)^;
      repeat
        while A^<Z do Inc(A);
        while Z<B^ do Dec(B);
        if DWord(A)<=DWord(B) then
        begin
          T:=A^; A^:=B^; B^:=T;
          Inc(A); Dec(B);
        end;
      until DWord(A)>DWord(B);
      if DWord(Min)<DWord(B) then QSort3(Min,B);
      Min:=A;
    end;
  end;

begin
  //QSort(Min,Max);
  QSort3(@List[Min],@List[Max]);
end;

procedure QuickSortDWord(var List: array of SortType3; Min, Max: Integer);
  procedure QSort(Min, Max: Integer);
  var
    A, B : Integer;
    T, Z : SortType3;
  begin
    A:=Min;
    while Min<Max do
    begin
      B:=Max; Z:=List[(A+B) shr 1];
      repeat
        while List[A]<Z do Inc(A);
        while Z<List[B] do Dec(B);
        if A<=B then
        begin
          T:=List[A]; List[A]:=List[B]; List[B]:=T;
          Inc(A); Dec(B);
        end;
      until A>B;
      if Min<B then QSort(Min,B);
      Min:=A;
    end;
  end;
begin
  QSort(Min,Max);
end;

procedure QuickSortDataRecord4(var List: array of TDataRecord4; Count: Integer);

  procedure QSort(Min, Max: Integer);
  var
   A, B : Integer;
   T : TDataRecord4;
   Z : Integer;
  begin
    A:=Min; B:=Max; Z:=List[(A+B) shr 1].Value;
    repeat
      while List[A].Value>Z do Inc(A);
      while Z>List[B].Value do Dec(B);
      if A<=B then
      begin
        T:=List[A]; List[A]:=List[B]; List[B]:=T;
        Inc(A); Dec(B);
      end;
    until A>B;
    if Min<B then QSort(Min,B);
    if A<Max then QSort(A,Max);
  end;

begin
  QSort(0,Count-1);
end;

procedure HeapSort(var List: array of SortType1; Count, FirstNeeded: DWord);
var
  HeapSize : DWord;
  A : TByteArray absolute List;

  procedure Heapify(I: DWord);
  var
    Child, Best : DWord;
    T : SortType1;
  begin
    Child:=(I+1)*2; // Left
    if (Child<HeapSize) and (A[Child]<A[I]) then Best:=Child
    else Best:=I;
    Inc(Child); // Right
    if (Child<HeapSize) and (A[Child]<A[Best]) then Best:=Child;
    if Best<>I then
    begin
      T:=A[Best];
      A[Best]:=A[I];
      A[I]:=T;
      Heapify(Best);
    end;
  end;

var
  I : DWord;
  T : SortType1;
begin
  // Build heap
  HeapSize:=Count;
  for I:=Count div 2-1 downto 0 do Heapify(I);

  for I:=Count-1 downto FirstNeeded do
  begin
    T:=A[0];
    A[0]:=A[I];
    A[I]:=T;
    Dec(HeapSize);
    Heapify(0);
  end;
end;

function QuickSelectByte(var List: array of SortType1; Min, Max, Wanted: Integer): SortType1;
var
  q, i, j, k : Integer;
  T, x : SortType1;
begin
  while Min<Max do
  begin
    x:=List[Min];
    i:=Min-1;
    j:=Max+1;
    repeat
      repeat
        Inc(i);
      until List[i]>=x;
      repeat
        Dec(j);
      until List[j]<=x;
      if i>=j then Break;
      T:=List[i];
      List[i]:=List[j];
      List[j]:=T;
    until False;
    q:=j;
    k:=q-Min+1;
    if Wanted<k then Max:=q
    else
    begin
      Min:=q+1;
      Dec(Wanted,k);
    end;
  end;
  Result:=List[Min];
end;

function QuickSelectDWord(var List: array of SortType3; Min, Max, Wanted: Integer): SortType3;
var
  q, i, j, k : Integer;
  T, x : SortType3;
begin
  while Min<Max do
  begin
    x:=List[Min];
    i:=Min-1;
    j:=Max+1;
    repeat
      repeat
        Inc(i);
      until List[i]>=x;
      repeat
        Dec(j);
      until List[j]<=x;
      if i>=j then Break;
      T:=List[i];
      List[i]:=List[j];
      List[j]:=T;
    until False;
    q:=j;
    k:=q-Min+1;
    if Wanted<k then Max:=q
    else
    begin
      Min:=q+1;
      Dec(Wanted,k);
    end;
  end;
  Result:=List[Min];
end;

function QuickSelectFloat(var List: array of SortType2; Min, Max, Wanted: Integer): SortType2;
var
  q, i, j, k : Integer;
  T, x : SortType2;
begin
  while Min<Max do
  begin
    x:=List[Min];
    i:=Min-1;
    j:=Max+1;
    repeat
      repeat
        Inc(i);
      until List[i]>=x;
      repeat
        Dec(j);
      until List[j]<=x;
      if i>=j then Break;
      T:=List[i];
      List[i]:=List[j];
      List[j]:=T;
    until False;
    q:=j;
    k:=q-Min+1;
    if Wanted<k then Max:=q
    else
    begin
      Min:=q+1;
      Dec(Wanted,k);
    end;
  end;
  Result:=List[Min];
end;

end.
