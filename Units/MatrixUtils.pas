////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MatrixUtils.pas - Matrix math utilities
// ---------------------------------------
// Version:   2006-12-26
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//
unit MatrixUtils;

interface

uses Windows, Math, SysUtils, MathUtils, MatrixMath, ExpressionEval, MemUtils;

function LinSpace(Min,Max: Integer): TVector; overload;
function LinSpace(Min,Max: Float; Count: Integer): TVector; overload;
// Produce histogram
function Histogram(X: TMatrix; Bins: Integer; Min: Float=0; Max: Float=0; TakeOver: Boolean=False): TVector;
// Make vector where each element from X appears twice
function DoubleElements(X: TMatrix; TakeOver: Boolean=False): TVector;
// Evaluate expression in AVar for with each element value from VarValues
function EvaluateExpression(const Expression,AVar: string; VarValues: TMatrix): TVector; overload;
// Create filter matrix from expression in r,c,d
procedure CreateFilterFromExpression(A: TMatrix; const FilterExpression: string; Normalize: Boolean);
// Returns polynomial coefficients (starting with x^0) for least-squares fit
function PolyFit(X,Y: TVector; Degree: Integer): TVector;
function PolyVal(X: Double; Poly: TVector): Double; overload;
function PolyVal(X,Poly: TVector): TVector;  overload;

// Solve Ax=b with respect to x. Set L to small number for Tikhonov regularizarion
procedure LeastSquaresSolve(A,b: TMatrix; x: TVector; Lambda: Double=0; Cond: PDouble=nil; TempObjects: PObjectArray=nil);
procedure LeastSquaresSolveQR(A,b: TMatrix; x: TVector; Lambda: Double);

type
  TInterpolateFunction = class(TSymbolFunction)
    public
      class procedure AddToSymbolTable(Defines: TSymbolTable=nil);
      function Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat; override;
    end;

implementation

// Make vector where each element from X appears twice
function DoubleElements(X: TMatrix; TakeOver: Boolean): TVector;
var
  I : Integer;
begin
  Result:=TVector.Create(X.Size*2);
  for I:=0 to X.Size-1 do
  begin
    Result.Matrix^[I*2]:=X.Matrix^[I];
    Result.Matrix^[I*2+1]:=X.Matrix^[I];
  end;
  if TakeOver then X.Free;
end;

function LinSpace(Min,Max: Integer): TVector;
var
  I : Integer;
begin
  Result:=TVector.Create(Max-Min+1);
  for I:=0 to Result.Length-1 do Result.Vector^[I]:=Min+I;
end;

function LinSpace(Min,Max: Float; Count: Integer): TVector;
var
  I : Integer;
begin
  Result:=TVector.Create(Count);
  Dec(Count);
  for I:=0 to Count do Result.Vector^[I]:=Min+(Max-Min)*I/Count;
end;

// Produce histogram
function Histogram(X: TMatrix; Bins: Integer; Min,Max: Float; TakeOver: Boolean): TVector;
var
  I, Bin : Integer;
begin
  if Min>=Max then X.GetStat(Min,Max);
  Max:=Max-Min;
  Result:=TVector.Create(Bins);
  Result.Clear;
  for I:=0 to X.Size-1 do
  begin
    Bin:=Floor((X.Matrix^[I]-Min)/Max*Bins);
    if Bin<0 then Bin:=0
    else if Bin>=Bins then Bin:=Bins-1;
    Result.Matrix^[Bin]:=Result.Matrix^[Bin]+1;
  end;
  if TakeOver then X.Free;
end;

function EvaluateExpression(const Expression,AVar: string; VarValues: TMatrix): TVector;
var
  Symbol : TSymbolValue;
  I : Integer;
begin
  Symbol:=TSymbolValue.Create;
  try
    DefaultDefines.Define(AVar,Symbol);
    Result:=TVector.Create(VarValues.Size);
    try
      for I:=0 to Result.Size-1 do
      begin
        Symbol.Value:=VarValues.Matrix^[I];
        Result.Vector^[I]:=EvaluateExpression(Expression);
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    DefaultDefines.Remove(AVar);
  end;
end;

function PolyFit(X,Y: TVector; Degree: Integer): TVector;
var
  A : TMatrix;
  R, C : Integer;
begin
  Assert(X.Length=Y.Length);
  Assert(Degree>=1);
  A:=TMatrix.Create(X.Length,Degree+1);
  try
    for R:=1 to X.Length do
    begin
      A[R,1]:=1;
      for C:=2 to Degree+1 do A[R,C]:=A[R,C-1]*X[R];
    end;
    Result:=TVector.Create;
    try
      LeastSquaresSolve(A,y,Result);
    except
      Result.Free;
      raise;
    end;
  finally
    A.Free;
  end;
end;

function PolyVal(X: Double; Poly: TVector): Double;
var
  I : Integer;
  P : Double;
begin
  Result:=Poly[1];
  P:=1;
  for I:=2 to Poly.Length do
  begin
    P:=P*X;
    Result:=Result+P*Poly[I];
  end;
end;

function PolyVal(X,Poly: TVector): TVector;
var
  I : Integer;
  P : TVector;
begin
  Result:=TVector.Create(X.Length);
  Result.Clear(Poly[1]);
  P:=TVector.Create(X);
  try
    for I:=2 to Poly.Length do
    begin
      if I>2 then P.ElementMultiply(X);
      Result.AddScale(P,Poly[I]);
    end;
  finally
    P.Free;
  end;
end;

// Solve normal equation, x = (A'A)\(A'b)
procedure LeastSquaresSolve(A,b: TMatrix; x: TVector; Lambda: Double; Cond: PDouble; TempObjects: PObjectArray);

  procedure FormPseudoInv1(PseudoInv,A: TMatrix); // PseudoInv:=A*A'
  var
    R, C, I, ANCols : Integer;
    RowSum : Double;
    AE, BE : PDouble;
  begin
    with PseudoInv do
    begin
      New(A.NRows,A.NRows);
      ANCols:=A.NCols;
      for R:=0 to NRows-1 do
      begin
        for C:=0 to R do
        begin
          RowSum:=0;
          AE:=@A.Matrix^[R*ANCols];
          BE:=@A.Matrix^[C*ANCols];
          for I:=0 to ANCols-1 do
          begin
            RowSum:=RowSum+AE^*BE^;
            Inc(AE);
            Inc(BE);
          end;
          Matrix[C+R*NCols]:=RowSum;
          Matrix[R+C*NCols]:=RowSum;
        end;
      end;
    end;
  end;

  procedure FormPseudoInv2(PseudoInv,B: TMatrix); // PseudoInv:=B'*B
  var
    R, C, I, ANCols : Integer;
    RowSum : Double;
    AE, BE : PDouble;
  begin
    with PseudoInv do
    begin
      New(B.NCols,B.NCols);
      ANCols:=B.NRows;
      for R:=0 to NRows-1 do
      begin
        for C:=0 to R do
        begin
          RowSum:=0;
          AE:=@B.Matrix^[R];
          BE:=@B.Matrix^[C];
          for I:=0 to ANCols-1 do
          begin
            RowSum:=RowSum+AE^*BE^;
            Inc(AE,NCols);
            Inc(BE,NCols);
          end;
          Matrix[C+R*NCols]:=RowSum;
          Matrix[R+C*NCols]:=RowSum;
        end;

        {RowSum:=0;
        BE:=@B.Matrix^[R];
        for I:=0 to ANCols-1 do
        begin
          RowSum:=RowSum+Sqr(BE^);
          Inc(BE,NCols);
        end;
        Matrix[R+R*NCols]:=RowSum;{}
      end;
    end;
  end;

  procedure TransMultiply(Result,A,b: TMatrix); // Result:=A'*b
  var
    R, I : Integer;
    RowSum : Double;
    AE : PDouble;
  begin
    with Result do
    begin
      New(A.NCols,1);
      for R:=0 to NRows-1 do
      begin
        RowSum:=0;
        AE:=@A.Matrix^[R];
        for I:=0 to b.NRows-1 do 
        begin
          RowSum:=RowSum+AE^*b.Matrix^[I];
          Inc(AE,NRows);
        end;
        Matrix[R]:=RowSum;
      end;
    end;
  end;

var
  //ATrans : TMatrix;
  PseudoInv : TMatrix;
  y : TVector;
  I : Integer;
begin
  Assert(A.NRows=b.NRows,'Matrix dimensions mismatch');
  Assert(b.NCols=1,'b must be column vector');

  PseudoInv:=CreateOrGetObject(TempObjects^[0],TMatrix);
  y:=CreateOrGetObject(TempObjects^[1],TVector);
  //ATrans:=CreateOrGetObject(TempObjects^[2],TMatrix);
  try
    //ATrans.Transpose(A);
    //PseudoInv.Multiply(ATrans,A);
    //FormPseudoInv1(PseudoInv,ATrans);
    FormPseudoInv2(PseudoInv,A);
    if Lambda<>0 then
    begin
      Lambda:=Sqr(Lambda);
      with PseudoInv do
        for I:=0 to NCols-1 do
          Matrix^[I*NCols+I]:=Matrix^[I*NCols+I]+Lambda;
    end;
    //y.Multiply(ATrans,b);
    TransMultiply(y,A,b);
    if Assigned(Cond) then Cond^:=PseudoInv.Cond;
    PseudoInv.SolveLinear(x,y,False,True,@TempObjects^[3]);
  finally
    if Cardinal(TempObjects)<1024 then
    begin
      y.Destroy;
      PseudoInv.Destroy;
      //ATrans.Destroy;
    end;
  end;
end;{}

// Solve by QR factorization, Rx = Q'b
procedure LeastSquaresSolveQR(A,b: TMatrix; x: TVector; Lambda: Double);
var
  Q, R : TMatrix;
  I, J : Integer;
  Sum : Double;
  Row : PRow;
begin
  Assert(A.NRows=b.NRows,'Matrix dimensions mismatch');
  Assert(b.NCols=1,'b must be column vector');
  x.New(A.NCols);
  Q:=TMatrix.Create;
  R:=TMatrix.Create;
  try
    if Lambda=0 then Q.Assign(A)
    else
    begin // Tikhonov regularization, append diagonal matrix in the bottom
      Q.New(A.NRows+A.NCols,A.NCols);
      Move(A.Matrix^,Q.Matrix^,A.ByteSize);
      ZeroMem(Q.Matrix^[A.Size],Sqr(A.NCols)*SizeOf(Float));
      for I:=1 to A.NCols do Q[A.NRows+I,I]:=Lambda;
    end;

    Q.QRDecomp(Q,R);

    // Form Q'b and store the result in x
    for J:=1 to Q.NCols do
    begin
      Sum:=0;
      for I:=1 to b.NRows do Sum:=Sum+Q[I,J]*b.Matrix^[I-1];
      x[J]:=Sum;
    end;

    // Update X with the solution vector
    Row:=R.ScanLine[x.Length-1];
    x[x.Length]:=x[x.Length] / Row[x.Length];
    for I:=x.Length-1 downto 1 do
    begin
      Dec(Row,x.Length);
      Sum:=0;
      for J:=I+1 to x.Length do Sum:=Sum+Row[J]*x[J];
      x[I]:=(x[I]-Sum)/Row[I];
    end;
  finally
    Q.Free;
    R.Free;
  end;
end;{}

//==============================================================================================================================
// TInterpolateFunction
//==============================================================================================================================

class procedure TInterpolateFunction.AddToSymbolTable(Defines: TSymbolTable);
begin
  if Defines=nil then Defines:=DefaultDefines;
  Defines.Define('interp',TInterpolateFunction.Create);
end;

function TInterpolateFunction.Evaluate(Param: EvalFloat; Table: TSymbolTable): EvalFloat;
begin
  if Param<=0 then Result:=0
  else if Param>=1 then Result:=1
  else Result:=Param;
end;

// Create filter matrix from expression in r,c,d where (r,c) are row and column with (0,0) in the center
// of the matrix and d is the distance to the center.
procedure CreateFilterFromExpression(A: TMatrix; const FilterExpression: string; Normalize: Boolean);
var
  r, c, rMid, cMid : Integer;
  dSym, rSym, cSym : TSymbolValue;
  Element : ^Double;
  Sum : Double;
  SymbolTable : TSymbolTable;
begin
  if GetCurrentThreadId=MainThreadID then SymbolTable:=DefaultDefines
  else SymbolTable:=TSymbolTable.Create;
  try
    TInterpolateFunction.AddToSymbolTable(SymbolTable);
    dSym:=TSymbolValue.Create;
    rSym:=TSymbolValue.Create;
    cSym:=TSymbolValue.Create;
    SymbolTable.Define('d',dSym);
    SymbolTable.Define('r',rSym);
    SymbolTable.Define('c',cSym);
    try
      Element:=Pointer(A.Matrix);
      rMid:=A.NRows div 2;
      cMid:=A.NCols div 2;
      for r:=-rMid to A.NRows-rMid-1 do
      begin
        rSym.Value:=r;
        for c:=-cMid to A.NCols-cMid-1 do
        begin
          cSym.Value:=c;
          dSym.Value:=Sqrt(Sqr(r)+Sqr(c));
          Element^:=EvaluateExpression(FilterExpression,SymbolTable);
          Inc(Element);
        end;
      end;
    finally
      SymbolTable.Remove('r');
      SymbolTable.Remove('d');
      SymbolTable.Remove('c');
    end;
  finally
    if SymbolTable<>DefaultDefines then SymbolTable.Free;
  end;
  if Normalize then
  begin
    Sum:=A.Sum;
    if Sum<>0 then A.Multiply(1/Abs(Sum));
  end;
end;

end.

