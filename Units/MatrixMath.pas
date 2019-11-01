////////////////////////////////////////////////////////////////////////////////
//
// MatrixMath.pas - Matrix math
// ----------------------------
// Version:   2005-08-16
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//
unit MatrixMath;

interface

uses
  {$IFDEF FloatMapFileSupport} Forms, Dialogs, StyleForm, {$ENDIF}
  Windows, Math, MathUtils, MemUtils, SysUtils, FloatMap, DynamicLists;

{$IFDEF EnableInlining} {$INLINE AUTO} {$ENDIF}

// 2 x 2 matrices
type
  TMatrix2x2 = array[1..2,1..2] of Double;
  TVector2 = array[1..2] of Double;

// Solve 2x2 system Ax = b   UNTESTED
function Solve2x2(const A: TMatrix2x2; const b: TVector2): TVector2;

// 3 x 3 matrices
type
  TMatrix3x3 = array[1..3,1..3] of Double;
  TList9 = array[1..9] of Double;

procedure Eye3x3(var F: TMatrix3x3);
procedure MakeTransform3x3(var F: TMatrix3x3; const R,Sx,Sy,Tx,Ty: Double);
function InvertMatrix3x3(var A: TMatrix3x3): Boolean;
procedure TransposeMatrix3x3(const A: TMatrix3x3; var TA: TMatrix3x3);
procedure ComplementMatrix3x3(const A: TMatrix3x3; var CA: TMatrix3x3);
function Matrix3x3Determinant(const A: TMatrix3x3): Double;
// Transform point with F using homogeneous coordinates
function Transform2D(const X,Y: Double; const F: TMatrix3x3): TFloatPoint;

// 4 x 4 matrices
type
  TMatrix4x4 = array[1..4,1..4] of Double;
  PMatrix4x4 = ^TMatrix4x4;
  TMatrix4x4Obj = object
                    M : TMatrix4x4;
                    // M:=M*Other
                    procedure Multiply(const Other: TMatrix4x4);
                    // M:=Other*M, last row assumed to be [0 0 0 1]
                    procedure Transform(const Other: TMatrix4x4);
                    // Transformations are performed like the order of the arguments
                    procedure MakeTransform(const Rz,Ry,Rx,Sx,Sy,Sz,Tx,Ty,Tz: Double);
                    procedure Eye;
                    procedure Zero;
                    procedure RotateX(const R: Double);
                    procedure RotateY(const R: Double);
                    procedure RotateZ(const R: Double);
                    procedure DoRotateX(const R: Double);
                    procedure DoRotateY(const R: Double);
                    procedure DoRotateZ(const R: Double);
                    procedure Translate(const Tx,Ty,Tz: Double);
                    procedure DoTranslate(const Tx,Ty,Tz: Double);
                    procedure Scale(const Sx,Sy,Sz: Double); overload;
                    procedure Scale(const S: Double); overload;
                  end;

// Invert matrix assuming bottom row is [0 0 0 1]
function InvertTransform4x4(const M : TMatrix4x4): TMatrix4x4;

// Generel matrix
type
  TVector = class; // Forward
  TIntVector = class; // Forward

  ESingluarMatrix = class(Exception);

  TMatrix = class(TFloatMap)
    protected
      function Get(r,c: Integer): Float; {$IFDEF EnableInlining} inline; {$ENDIF}
      procedure Put(r,c: Integer; const Value: Float); {$IFDEF EnableInlining} inline; {$ENDIF}
    public
      property Matrix: PFloatArray read Map;
      property M[r: Integer; c: Integer]: Float read Get write Put; default;
      property NRows: Integer read FHeight;
      property NCols: Integer read FWidth;
      constructor Create(NRows: Integer; NCols: Integer=1); overload;
      {$WARNINGS OFF}
      procedure Assign(const Other: TMatrix4x4); overload;
      procedure Assign(const Other: TMatrix3x3); overload;
      procedure Assign(const Value: Float); overload;
      procedure AssignTo(var Other: TMatrix4x4); overload;
      procedure New(NRows: Integer; NCols: Integer=1);
      {$WARNINGS ON}
      function Column(c: Integer): TVector;
      function Row(r: Integer): TVector;
      procedure Zero;
      // Fill with random numbers in [Min;Max[
      procedure Random(Min: Float=0; Max: Float=1);
      procedure Crop(rFrom,rTo: Integer; cFrom: Integer=1; cTo: Integer=MaxInt);
      function Block(rFrom,rTo: Integer; cFrom: Integer=1; cTo: Integer=1): TMatrix;

      procedure Transpose; overload;
      procedure Transpose(Source: TMatrix); overload;
      // Sum(elements²)
      function FrobeniusNormSqr: Double;
      function Sum: Double;
      function SqrSum: Double;
      function Mean: Double;
      function Variance: Double;
      // Convert vector to diagonal matrix
      procedure Diag;
      procedure AbsMatrix;
      // Self:=Other*Self
      procedure Transform(Other: TMatrix);
      // Self:=Self*Other
      procedure Multiply(Other: TMatrix); overload;
      procedure Multiply(Other: Double); overload;
      // Self:=A*B
      procedure Multiply(A,B: TMatrix); overload;
      // Self:=Self/Other element wise
      procedure ElementDivide(Other: TMatrix); overload;
      // Self:=Self*Other element wise
      procedure ElementMultiply(Other: TMatrix); overload;
      // Self:=Self^2 element wise
      procedure ElementSquare;
      // Self:=Self+Other
      procedure Add(Other: TFloatMap); overload;
      procedure Add(Value: Double); overload;
      // Self:=Self+Other*Scale
      procedure AddScale(Other: TMatrix; const Scale: Double); overload;
      // Self:=Self-Other
      procedure Subtract(Other: TMatrix); overload;
      // Self:=Self-Other*Scale
      procedure SubtractScale(Other: TMatrix; Scale: Double);
      // Self:=Other-Self
      procedure SubtractFrom(Other: TMatrix); overload;
      procedure SubtractFrom(const Value: Double); overload;
      // Calculate eigenvectors (columns in V) and corrosponding eigenvalues (d) and sort by eigenvalue
      function Eigen(V,d: TMatrix): Integer;
      // Solve the linear system represented by m and right-hand side b
      procedure LUSolve(index : TIntVector; b : TVector);
      // Form LU decomposition of Self matrix
      procedure LUDecomp(M : TMatrix; index : TIntVector; TempObjects: PObjectArray=nil);
      // Economy size QR decomposition. Q can be set to Self for optimization
      procedure QRDecomp(Q,R: TMatrix);
      // Calculate condition number
      function Cond: Double;
      // Solve Self*v = b with respect to v
      procedure SolveLinear(v,b: TVector; SelfToInv: Boolean=False; CanTrashSelf: Boolean=False; TempObjects: PObjectArray=nil);
      // Make unit matrix
      procedure Eye;
      // Self:=Other rotated 180°
      procedure Rotate180(Other: TMatrix=nil);
      // Self:=Other mirrored
      procedure Mirror(Other: TMatrix=nil);
      // Normalize so that sum of elements is 1
      procedure Normalize;
      procedure Log10;
      procedure ForceInRange(Min,Max: Float);
      function Min: Float;
      function Max: Float;
      // Write to standard out
      procedure Write(Precision: Integer);
      {$IFDEF FloatMapFileSupport}
      procedure Show(const ATitle: string='');
      {$ENDIF}
    end;

  TRow = array[1..1] of Float;
  PRow = ^TRow;

  TVector = class(TMatrix)
    protected
      function Get(i: Integer): Float; {$IFDEF EnableInlining} inline; {$ENDIF}
      procedure Put(i: Integer; const Value: Float); {$IFDEF EnableInlining} inline; {$ENDIF}
      procedure SetLength(const NewLength: Integer);
    public
      constructor Create(const Values: array of Float); overload;
      procedure New(const Values: array of Float); overload;
      procedure Assign(Source: TObject); override;
      procedure TakeOver(Other: TFloatMap);
      property Vector: PFloatArray read Map;
      property V[i: Integer]: Float read Get write Put; default;
      property Length: Integer read FSize write SetLength;
      function IndexOfMax: Integer;
      function IndexOfMin: Integer;
    end;

  TIntVector = class(TDynamicList)
    protected
      FVector : PIntegerArray;
      function Get(i: Integer): Integer; {$IFDEF EnableInlining} inline; {$ENDIF}
      procedure Put(i: Integer; const Value: Integer); {$IFDEF EnableInlining} inline; {$ENDIF}
    public
      constructor Create; overload;
      constructor Create(Length: Integer); overload;
      procedure New(Length: Integer);
      procedure AssignTo(Other: TObject); override;

      property Vector: PIntegerArray read FVector;
      property V[i: Integer]: Integer read Get write Put; default;
      property Length: Integer read FCount;
    end;

function Vector(const Values: array of Float): TVector;
function VectorDot(a,b: TMatrix): Double;

implementation

uses
  StringUtils, MultiCoreProcessing;

//==============================================================================================================================
// TMatrix2x2
//==============================================================================================================================

// Solve 2x2 system Ax = b
function Solve2x2(const A: TMatrix2x2; const b: TVector2): TVector2;
var
  Det : Double;
begin
  Det:=A[1,1]*A[2,2]-A[1,2]*A[2,1];
  Result[1]:=(b[1]*A[2,2]-b[2]*A[1,2])/Det;
  Result[2]:=(b[2]*A[1,1]-b[1]*A[2,1])/Det;
end;

//==============================================================================================================================
// TMatrix3x3
//==============================================================================================================================
procedure Eye3x3(var F: TMatrix3x3);
begin
  ZeroMem(F,SizeOf(F));
  F[1,1]:=1;
  F[2,2]:=1;
  F[3,3]:=1;
end;

procedure MakeTransform3x3(var F: TMatrix3x3; const R,Sx,Sy,Tx,Ty: Double);
begin
  F[1,1]:=Cos(R)*Sx;  F[1,2]:=-Sin(R)*Sx;  F[1,3]:=Tx;
  F[2,1]:=Sin(R)*Sy;  F[2,2]:= Cos(R)*Sy;  F[2,3]:=Ty;
  F[3,1]:=0;          F[3,2]:=0;           F[3,3]:=1;
end;

function Transform2D(const X,Y: Double; const F: TMatrix3x3): TFloatPoint;
var
  S : Double;
begin
  S:=1/(X*F[3,1]+Y*F[3,2]+F[3,3]);
  Result.X:=(X*F[1,1]+Y*F[1,2]+F[1,3])*S;
  Result.Y:=(X*F[2,1]+Y*F[2,2]+F[2,3])*S;
end;

function InvertMatrix3x3(var A: TMatrix3x3): Boolean;
var
  B : TMatrix3x3;
  I : Integer;
  InvDet : Double;
begin
  ComplementMatrix3x3(A,B);
  InvDet:=TList9(A)[1]*TList9(B)[1]+TList9(A)[2]*TList9(B)[2]+TList9(A)[3]*TList9(B)[3];
  Result:=InvDet<>0;
  if Result then
  begin
    InvDet:=1/InvDet;
    TransposeMatrix3x3(B,A);
    for I:=1 to 9 do TList9(A)[I]:=TList9(A)[I]*InvDet;
  end;
end;

function Matrix3x3Determinant(const A: TMatrix3x3): Double;
begin
  Result:=+TList9(A)[1]*(TList9(A)[5]*TList9(A)[9]-TList9(A)[6]*TList9(A)[8])
          -TList9(A)[2]*(TList9(A)[4]*TList9(A)[9]-TList9(A)[7]*TList9(A)[6])
          +TList9(A)[3]*(TList9(A)[4]*TList9(A)[8]-TList9(A)[7]*TList9(A)[5]);
end;

procedure ComplementMatrix3x3(const A: TMatrix3x3; var CA: TMatrix3x3);
begin
  TList9(CA)[1]:=TList9(A)[5]*TList9(A)[9]-TList9(A)[6]*TList9(A)[8];
  TList9(CA)[2]:=TList9(A)[7]*TList9(A)[6]-TList9(A)[4]*TList9(A)[9];
  TList9(CA)[3]:=TList9(A)[4]*TList9(A)[8]-TList9(A)[7]*TList9(A)[5];
  TList9(CA)[4]:=TList9(A)[8]*TList9(A)[3]-TList9(A)[2]*TList9(A)[9];
  TList9(CA)[5]:=TList9(A)[1]*TList9(A)[9]-TList9(A)[7]*TList9(A)[3];
  TList9(CA)[6]:=TList9(A)[7]*TList9(A)[2]-TList9(A)[1]*TList9(A)[8];
  TList9(CA)[7]:=TList9(A)[2]*TList9(A)[6]-TList9(A)[5]*TList9(A)[3];
  TList9(CA)[8]:=TList9(A)[4]*TList9(A)[3]-TList9(A)[1]*TList9(A)[6];
  TList9(CA)[9]:=TList9(A)[1]*TList9(A)[5]-TList9(A)[4]*TList9(A)[2];
end;

procedure TransposeMatrix3x3(const A: TMatrix3x3; var TA: TMatrix3x3);
var
  R, C : Integer;
begin
  for R:=1 to 3 do for C:=1 to 3 do TA[R,C]:=A[C,R];
end;

//==============================================================================================================================
// TMatrix4x4
//==============================================================================================================================
procedure TMatrix4x4Obj.MakeTransform(const Rz,Ry,Rx,Sx,Sy,Sz,Tx,Ty,Tz: Double);
//var Temp : TMatrix4x4Obj;
begin
  M[1,1]:=Cos(Ry)*Cos(Rz)*Sx;  M[2,1]:=(Sin(Rx)*Sin(Ry)*Cos(Rz)+Cos(Rx)*Sin(Rz))*Sy;  M[3,1]:=(Sin(Rx)*Sin(Rz)-Cos(Rx)*Sin(Ry)*Cos(Rz))*Sz; M[4,1]:=0;
  M[1,2]:=-Cos(Ry)*Sin(Rz)*Sx; M[2,2]:=(Cos(Rx)*Cos(Rz)-Sin(Rx)*Sin(Ry)*Sin(Rz))*Sy;  M[3,2]:=(Sin(Rx)*Cos(Rz)+Cos(Rx)*Sin(Ry)*Sin(Rz))*Sz; M[4,2]:=0;
  M[1,3]:=Sin(Ry)*Sx;          M[2,3]:=-Sin(Rx)*Cos(Ry)*Sy;                           M[3,3]:=Cos(Rx)*Cos(Ry)*Sz;                           M[4,3]:=0;
  M[1,4]:=Tx;                  M[2,4]:=Ty;                                            M[3,4]:=Tz;                                           M[4,4]:=1;{}

  {Scale(Sx,Sy,Sz);
  Temp.RotateZ(Rz); Transform(Temp.M);
  Temp.RotateX(Rx); Transform(Temp.M);
  Temp.RotateY(Ry); Transform(Temp.M);
  Temp.Translate(Tx,Ty,Tz); Transform(Temp.M);{}
  //M[1,4]:=Tx; M[2,4]:=Ty; M[3,4]:=Tz;
end;

procedure TMatrix4x4Obj.Eye;
{[1 0 0 0]
 [0 1 0 0]
 [0 0 1 0]
 [0 0 0 1]}
var
  A : Integer;
begin
  ZeroMem(M,SizeOf(M)-SizeOf(Double));
  for A:=1 to 4 do M[A,A]:=1;
end;

procedure TMatrix4x4Obj.Zero;
begin
  ZeroMem(M,SizeOf(M));
end;

procedure TMatrix4x4Obj.Translate(const Tx,Ty,Tz: Double);
{[1 0 0 Tx]
 [0 1 0 Ty]
 [0 0 1 Tz]
 [0 0 0 1 ]}
begin
  Eye;
  M[1,4]:=Tx;
  M[2,4]:=Ty;
  M[3,4]:=Tz;
end;

procedure TMatrix4x4Obj.DoTranslate(const Tx,Ty,Tz: Double);
begin
  M[1,4]:=M[1,4]+Tx;
  M[2,4]:=M[2,4]+Ty;
  M[3,4]:=M[3,4]+Tz;
end;

procedure TMatrix4x4Obj.RotateX(const R: Double);
{[1 0    0     0]
 [0 CosR -SinR 0]
 [0 SinR CosR  0]
 [0 0    0     1]}
var
  CosR, SinR : Double;
begin
  ZeroMem(M,SizeOf(M)-SizeOf(Double));
  CosR:=Cos(R); SinR:=Sin(R);
  M[2,2]:=CosR; M[2,3]:=-SinR;
  M[3,2]:=SinR; M[3,3]:=CosR;
  M[1,1]:=1; M[4,4]:=1;
end;

procedure TMatrix4x4Obj.RotateY(const R: Double);
{[CosR  0 SinR 0]
 [0     1 0    0]
 [-SinR 0 CosR 0]
 [0     0 0    1]}
var
  CosR, SinR : Double;
begin
  ZeroMem(M,SizeOf(M)-SizeOf(Double));
  CosR:=Cos(R); SinR:=Sin(R);
  M[1,1]:=CosR;  M[1,3]:=SinR;
  M[3,1]:=-SinR; M[3,3]:=CosR;
  M[2,2]:=1; M[4,4]:=1;
end;

procedure TMatrix4x4Obj.RotateZ(const R: Double);
{[CosR -SinR 0 0]
 [SinR CosR  0 0]
 [0    0     1 0]
 [0    0     0 1]}
var
  CosR, SinR : Double;
begin
  ZeroMem(M,SizeOf(M)-SizeOf(Double));
  CosR:=Cos(R); SinR:=Sin(R);
  M[1,1]:=CosR; M[1,2]:=-SinR;
  M[2,1]:=SinR; M[2,2]:=CosR;
  M[3,3]:=1; M[4,4]:=1;
end;

procedure TMatrix4x4Obj.DoRotateZ(const R: Double);
var T : TMatrix4x4Obj;
begin
  T.RotateZ(R);
  Transform(T.M);
end;

procedure TMatrix4x4Obj.DoRotateY(const R: Double);
var T : TMatrix4x4Obj;
begin
  T.RotateY(R);
  Transform(T.M);
end;

procedure TMatrix4x4Obj.DoRotateX(const R: Double);
var T : TMatrix4x4Obj;
begin
  T.RotateX(R);
  Transform(T.M);
end;

procedure TMatrix4x4Obj.Scale(const Sx,Sy,Sz: Double);
{[Sx 0  0  0]
 [0  Sy 0  0]
 [0  0  Sz 0]
 [0  0  0  1]}
begin
  ZeroMem(M,SizeOf(M)-SizeOf(Double));
  M[1,1]:=Sx; M[2,2]:=Sy; M[3,3]:=Sz; M[4,4]:=1;
end;

procedure TMatrix4x4Obj.Scale(const S: Double);
{[S 0 0 0]
 [0 S 0 0]
 [0 0 S 0]
 [0 0 0 1]}
begin
  ZeroMem(M,SizeOf(M)-SizeOf(Double));
  M[1,1]:=S; M[2,2]:=S; M[3,3]:=S; M[4,4]:=1;
end;

procedure TMatrix4x4Obj.Transform(const Other: TMatrix4x4);
var
  Old : TMatrix4x4;
  R, C, I : Integer;
  Sum : Double;
begin
  Old:=M;
  for R:=1 to 3 do
    for C:=1 to 4 do
    begin
      Sum:=0;
      for I:=1 to 4 do Sum:=Sum+Old[I,C]*Other[R,I];
      M[R,C]:=Sum;
    end;
end;

procedure TMatrix4x4Obj.Multiply(const Other: TMatrix4x4);
var
  Old : TMatrix4x4;
  R, C, I : Integer;
  Sum : Double;
begin
  Old:=M;
  for R:=1 to 4 do
    for C:=1 to 4 do
    begin
      Sum:=0;
      for I:=1 to 4 do Sum:=Sum+Old[R,I]*Other[I,C];
      M[R,C]:=Sum;
    end;
end;

function InvertTransform4x4(const M : TMatrix4x4): TMatrix4x4;
var
  T : TMatrix3x3;
  R : Integer;
begin
  for R:=1 to 3 do Move(M[R],T[R],3*SizeOf(M[1,1]));
  InvertMatrix3x3(T);
  for R:=1 to 3 do Move(T[R],Result[R],3*SizeOf(M[1,1]));
  Result[1,4]:=-(M[1,4]*T[1,1]+M[2,4]*T[1,2]+M[3,4]*T[1,3]);
  Result[2,4]:=-(M[1,4]*T[2,1]+M[2,4]*T[2,2]+M[3,4]*T[2,3]);
  Result[3,4]:=-(M[1,4]*T[3,1]+M[2,4]*T[3,2]+M[3,4]*T[3,3]);
  Result[4,1]:=0; Result[4,2]:=0; Result[4,3]:=0; Result[4,4]:=1;
end;

//==============================================================================================================================
// TVector
//==============================================================================================================================
function Vector(const Values: array of Float): TVector;
begin
  result:=TVector.Create(Values);
end;

function VectorDot(a,b: TMatrix): Double;
var
  I : Integer;
begin
  with a do
  begin
    Assert(Size=b.Size);
    Result:=0;
    for I:=0 to Size-1 do Result:=Result+Matrix^[I]*b.Matrix^[I];
  end;
end;

constructor TVector.Create(const Values: array of Float);
begin
  Create;
  New(Values);
end;

procedure TVector.New(const Values: array of Float);
begin
  New(System.Length(Values));
  Move(Values[0],Vector^,ByteSize);
end;

procedure TVector.Assign(Source: TObject);
begin
  inherited;
  if NCols>1 then
  begin
    Assert(NRows=1,'Can only assign simple vector');
    Transpose;
  end;
end;

procedure TVector.TakeOver(Other: TFloatMap);
begin
  inherited;
  if NCols>1 then
  begin
    Assert(NRows=1,'Can only assign simple vector');
    Transpose;
  end;
end;

function TVector.Get(i: Integer): Float;
begin
  Assert((i>=1) and (i<=Length));
  Result:=Vector^[i-1];
end;

procedure TVector.Put(i: Integer; const Value: Float);
begin
  Assert((i>=1) and (i<=Length));
  Vector^[i-1]:=Value;
end;

procedure TVector.SetLength(const NewLength: Integer);
begin
  ResizeCanvas(1,NewLength);
end;

//==============================================================================================================================
// TIntVector
//==============================================================================================================================
constructor TIntVector.Create;
begin
  inherited Create(SizeOf(Integer),FVector);
end;

constructor TIntVector.Create(Length: Integer);
begin
  Create;
  New(Length);
end;

procedure TIntVector.New(Length: Integer);
begin
  if RecordSize=0 then Initialize(SizeOf(Integer),FVector);
  Count:=Length;
end;

procedure TIntVector.AssignTo(Other: TObject);
var
  I : Integer;
begin
  if Other is TMatrix then
  begin
    TMatrix(Other).New(Length);
    for I:=0 to Length-1 do TMatrix(Other).Matrix^[I]:=Vector^[I];
  end
  else inherited;
end;

function TIntVector.Get(i: Integer): Integer;
begin
  Assert((i>=1) and (i<=Length));
  Result:=Vector^[i-1];
end;

procedure TIntVector.Put(i: Integer; const Value: Integer);
begin
  Assert((i>=1) and (i<=Length));
  Vector^[i-1]:=Value;
end;

//==============================================================================================================================
// TMatrix
//==============================================================================================================================

constructor TMatrix.Create(NRows,NCols: Integer);
begin
  Create;
  inherited New(NCols,NRows);
end;

procedure TMatrix.New(NRows,NCols: Integer);
begin
  inherited New(NCols,NRows);
end;

function TMatrix.Column(c: Integer): TVector;
var
  I : Integer;
begin
  Dec(c);
  Result:=TVector.Create(NRows);
  for I:=0 to NRows-1 do Result.Vector^[I]:=Matrix^[I*NCols+c];
end;

// Calculate condition number (the ratio of the largest singular value of Self to the smallest).
// This probably only works for symmetrical matrices
function TMatrix.Cond: Double;
var
  V : TMatrix;
  d : TVector;
begin
  V := TMatrix.Create;
  d := TVector.Create;
  try
    Eigen(V,d);
    Result:=d[1]/d[d.Length];
  finally
    V.Free;
    d.Free;
  end;
end;

function TMatrix.Row(r: Integer): TVector;
begin
  Result:=TVector.Create(NCols);
  Move(Matrix^[(r-1)*NCols],Result.Matrix^,BytesPerLine);
end;

procedure TMatrix.Zero;
begin
  ZeroMem(Matrix^,ByteSize);
end;

procedure TMatrix.Crop(rFrom,rTo,cFrom,cTo: Integer);
begin
  if rTo>NRows then rTo:=NRows;
  if cTo>NCols then cTo:=NCols;
  ResizeCanvas(cTo-cFrom+1,rTo-rFrom+1,-(cFrom-1),-(rFrom-1));   
end;

function TMatrix.Block(rFrom,rTo,cFrom,cTo: Integer): TMatrix;
begin
  if rTo>NRows then rTo:=NRows;
  if cTo>NCols then cTo:=NCols;
  Result:=TMatrix.Create(rTo-rFrom+1,cTo-cFrom+1);
  Result.Paste(Self,-(cFrom-1),-(rFrom-1));
end;

procedure TMatrix.Eye;
var i : Integer;
begin
  Assert(NRows=NCols);
  ZeroMem(Matrix^,ByteSize);
  for i:=0 to NRows-1 do Matrix^[i*(NRows+1)]:=1;
end;

procedure TMatrix.Assign(const Other: TMatrix3x3);
begin
  New(3,3);
  Move(Other,Matrix^,ByteSize);
end;

procedure TMatrix.Assign(const Other: TMatrix4x4);
begin
  New(4,4);
  Move(Other,Matrix^,ByteSize);
end;

procedure TMatrix.Assign(const Value: Float);
begin
  Clear(Value);
end;

procedure TMatrix.AssignTo(var Other: TMatrix4x4);
begin
  Assert((NRows=4) and (NCols=4));
  Move(Matrix^,Other,ByteSize);
end;

function TMatrix.Get(r,c: Integer): Float;
begin
  Assert((r>=1) and (c>=1));
  Assert((r<=NRows) and (c<=NCols));
  Result:=Matrix^[(r-1)*NCols+c-1];
end;

procedure TMatrix.Put(r,c: Integer; const Value: Float);
begin
  Assert((r>=1) and (c>=1));
  Assert((r<=NRows) and (c<=NCols));
  Matrix^[(r-1)*NCols+c-1]:=Value;
end;

procedure TMatrix.Write(Precision: Integer);
var
  r, c : Integer;
begin
  for r:=1 to NRows do
  begin
    for c:=1 to NCols do System.Write(M[r,c]:Precision+5:Precision,' ');
    WriteLn;
  end;
end;

{$IFDEF FloatMapFileSupport}
resourcestring
  rsOK = 'OK';
  rsSave = '&Save';

procedure TMatrix.Show(const ATitle: string);
const
  MaxCols = 7;
  MaxRows = 21;
var
  V, FileName : string;
  r, c, Result : Integer;
begin
  if ATitle='' then V:=''
  else V:=ATitle+' ='#13;
  for r:=1 to Math.Min(MaxRows,NRows) do
  begin
    for c:=1 to Math.Min(MaxCols,NCols) do V:=V+GetStr(M[r,c],10,4)+#9;
    if NCols>MaxCols then V:=V+'··';
    V:=V+#13;
  end;
  if NRows>MaxRows then V:=V+#9':';
  repeat
    Result:=MessageDlgStr(V,mtInformation,[rsOK,rsSave],1);
    if Result<>2 then Break
    else
    begin
      FileName:=ATitle+'.txt';
      if SaveFileDialog(FileName,rsMapSaveFilter) then
      try
        SaveToFile(FileName);
      except
        Application.HandleException(Self);
      end;
    end;
  until False;
end;
{$ENDIF}

{ Based on JACOBI.PAS and EIGSRT.PAS from

  NUMERICAL RECIPES IN PASCAL: THE ART OF SCIENTIFIC COMPUTING
  by William H. Press, Saul A. Teukolsky, Brian P. Flannery,
  and William T. Vetterling
  Copyright (C) 1986, 1989 by Cambridge University Press and
  Numerical Recipes Software.                                  }

function TMatrix.Eigen(V,d: TMatrix): Integer;
VAR
   j,iq,ip,i,k,n : integer;
   tresh,theta,tau,t,sm,s,h,g,c,p: Double;
   b,z : array of Double;
   a : TMatrix;
   Done : Boolean;
BEGIN
   // Suggested optimization: Keep d in var array during calculations
   n:=NRows;
   Assert(n=NCols);
   V.New(n,n);
   d.New(n);
   SetLength(b,n+1); // +1 because the vector is 1-indexed
   SetLength(z,n+1);
   a:=TMatrix.Create(Self);
   V.Eye;
   FOR ip := 1 TO n DO
   BEGIN
      b[ip] := a[ip,ip];
      d.Matrix^[-1+ip] := b[ip];
      z[ip] := 0.0
   END;
   Result := 0;
   Done:=False;
   FOR i := 1 TO 50 DO BEGIN
      sm := 0.0;
      FOR ip := 1 TO n-1 DO
         FOR iq := ip+1 TO n DO
            sm := sm+abs(a[ip,iq]);
      IF sm = 0.0 THEN
      begin
        Done:=True;
        Break;
      end;
      IF (i < 4) THEN tresh := 0.2*sm/sqr(n)
      ELSE tresh := 0.0;
      FOR ip := 1 TO n-1 DO
      BEGIN
         FOR iq := ip+1 TO n DO
         BEGIN
            g := 100.0*abs(a[ip,iq]);
            IF ((i > 4) AND ((abs(d.Matrix^[-1+ip])+g) = abs(d.Matrix^[-1+ip]))
               AND ((abs(d.Matrix^[-1+iq])+g) = abs(d.Matrix^[-1+iq]))) THEN
               a[ip,iq] := 0.0
            ELSE IF (abs(a[ip,iq]) > tresh) THEN BEGIN
               h := d.Matrix^[-1+iq]-d.Matrix^[-1+ip];
               IF ((abs(h)+g) = abs(h)) THEN BEGIN
                  t := a[ip,iq]/h
               END ELSE BEGIN
                  theta := 0.5*h/a[ip,iq];
                  t := 1.0/(abs(theta)+sqrt(1.0+sqr(theta)));
                  IF (theta < 0.0) THEN t := -t
               END;
               c := 1.0/sqrt(1+sqr(t));
               s := t*c;
               tau := s/(1.0+c);
               h := t*a[ip,iq];
               z[ip] := z[ip]-h;
               z[iq] := z[iq]+h;
               d.Matrix^[-1+ip] := d.Matrix^[-1+ip]-h;
               d.Matrix^[-1+iq] := d.Matrix^[-1+iq]+h;
               a[ip,iq] := 0.0;
               FOR j := 1 TO ip-1 DO BEGIN
                  g := a[j,ip];
                  h := a[j,iq];
                  a[j,ip] := g-s*(h+g*tau);
                  a[j,iq] := h+s*(g-h*tau)
               END;
               FOR j := ip+1 TO iq-1 DO BEGIN
                  g := a[ip,j];
                  h := a[j,iq];
                  a[ip,j] := g-s*(h+g*tau);
                  a[j,iq] := h+s*(g-h*tau)
               END;
               FOR j := iq+1 TO n DO BEGIN
                  g := a[ip,j];
                  h := a[iq,j];
                  a[ip,j] := g-s*(h+g*tau);
                  a[iq,j] := h+s*(g-h*tau)
               END;
               FOR j := 1 TO n DO BEGIN
                  g := v[j,ip];
                  h := v[j,iq];
                  v[j,ip] := g-s*(h+g*tau);
                  v[j,iq] := h+s*(g-h*tau)
               END;
               Inc(Result);
            END
         END
      END
   END;
   a.Free;
   if not Done then
   begin
     FOR ip := 1 TO n DO BEGIN
        b[ip] := b[ip]+z[ip];
        d.Matrix^[-1+ip] := b[ip];
        z[ip] := 0.0
     END;
     Assert(False,'50 iterations should not happen');
   end;

  // Sort by eigenvalue
  FOR i := 1 TO n-1 DO BEGIN
     k := i;
     p := d.Matrix^[-1+i];
     FOR j := i+1 TO n DO BEGIN
        IF (d.Matrix^[-1+j] >= p) THEN BEGIN
           k := j;
           p := d.Matrix^[-1+j]
        END
     END;
     IF (k <> i) THEN BEGIN
        d.Matrix^[-1+k] := d.Matrix^[-1+i];
        d.Matrix^[-1+i] := p;
        FOR j := 1 TO n DO BEGIN
           p := v[j,i];
           v[j,i] := v[j,k];
           v[j,k] := p
        END
     END
  END
END;

procedure TMatrix.Transform(Other: TMatrix); // Self:=Other*Self
var
  Old : PFloatArray;
  R, C, I, OldNRows : Integer;
  Sum : Double;
  E : PFloat;
begin
  Assert(NRows=Other.NCols);
  Old:=Matrix;
  OldNRows:=NRows;
  FHeight:=Other.NRows;
  fSize:=NRows*NCols; FByteSize:=Size*SizeOf(Float);
  GetMem(Map,ByteSize);
  try
    for R:=0 to NRows-1 do
      for C:=0 to NCols-1 do
      begin
        Sum:=0;
        E:=@Other.Matrix^[R*OldNRows];;
        for I:=0 to OldNRows-1 do
        begin
          Sum:=Sum+Old[C+I*NCols]*E^;
          Inc(E);
        end;{}
        Matrix[C+R*NCols]:=Sum;
      end;
  finally
    FreeMem(Old);
  end;
end;

procedure TMatrix.Multiply(Other: TMatrix); // Self:=Self*Other
var
  Old : PFloatArray;
  R, C, I, OldNCols, RC : Integer;
  Sum : Double;
  OtherE : PFloat;
begin
  Assert(Other.NRows=NCols);
  Old:=Map;
  OldNCols:=NCols;
  FWidth:=Other.NCols;
  fSize:=NRows*NCols; FByteSize:=Size*SizeOf(Float); FBytesPerLine:=Width*SizeOf(Float);
  GetMem(Map,ByteSize);
  try
    for R:=0 to NRows-1 do
    begin
      RC:=R*OldNCols;
      for C:=0 to NCols-1 do
      begin
        Sum:=0;
        OtherE:=@Other.Matrix[C];;
        for I:=0 to OldNCols-1 do
        begin
          Sum:=Sum+Old^[RC+I]*OtherE^;
          Inc(OtherE,NCols);
        end;
        Matrix[C+R*NCols]:=Sum;
      end;
    end;
  finally
    FreeMem(Old);
  end;
end;

procedure TMatrix.Multiply(A,B: TMatrix); // Self:=A*B
var
  R, C, I, ANCols : Integer;
  Sum : Double;
  AE, BE : PFloat;
begin
  Assert(A.NCols=B.NRows);
  New(A.NRows,B.NCols);
  ANCols:=A.NCols;
  for R:=0 to NRows-1 do
  begin
    for C:=0 to NCols-1 do
    begin
      Sum:=0;
      BE:=@B.Matrix^[C];
      AE:=@A.Matrix^[R*ANCols];
      for I:=0 to ANCols-1 do
      begin
        Sum:=Sum+AE^*BE^;
        Inc(BE,NCols);
        Inc(AE);
      end;
      Matrix[C+R*NCols]:=Sum;
    end;
  end;{}
  //Assign(A); Multiply(B);
  //Assign(B); Transform(A);
end;

procedure TMatrix.Multiply(Other: Double);
var I : Integer;
begin
  if Other=0 then Clear
  else for I:=0 to Size-1 do Matrix^[I]:=Matrix^[I]*Other;
end;

procedure TMatrix.ElementDivide(Other: TMatrix);
var
  I : Integer;
  E, OtherE : PFloat;
begin
  Assert((Other.Width=Width) and (Other.Height=Height));
  OtherE:=Pointer(Other.Map);
  E:=Pointer(Matrix);
  for I:=1 to Size do
  begin
    E^:=E^/OtherE^;
    Inc(OtherE);
    Inc(E);
  end;
end;

// Self:=Self*Other element wise
procedure TMatrix.ElementMultiply(Other: TMatrix);
var
  I : Integer;
  E, OtherE : PFloat;
begin
  Assert((Other.Width=Width) and (Other.Height=Height));
  OtherE:=Pointer(Other.Map);
  E:=Pointer(Matrix);
  for I:=1 to Size do
  begin
    E^:=E^*OtherE^;
    Inc(OtherE);
    Inc(E);
  end;
end;

procedure TMatrix.ElementSquare;
var
  I : Integer;
begin
  for I:=0 to Size-1 do Matrix^[I]:=Sqr(Matrix^[I]);
end;

procedure TMatrix.Add(Value: Double);
var
  I : Integer;
begin
  for I:=0 to Size-1 do Matrix^[I]:=Matrix^[I]+Value;
end;

procedure TMatrix.Add(Other: TFloatMap);
var
  I : Integer;
  E, OtherE : PFloat;
begin
  Assert((Other.Width=Width) and (Other.Height=Height));
  OtherE:=Pointer(Other.Map);
  E:=Pointer(Matrix);
  for I:=1 to Size do
  begin
    E^:=E^+OtherE^;
    Inc(OtherE);
    Inc(E);
  end;
end;

procedure TMatrix.AddScale(Other: TMatrix; const Scale: Double);
var
  I : Integer;
begin
  Assert((Other.NRows=NRows) and (Other.NCols=NCols));
  if Scale<>0 then
    for I:=0 to Size-1 do Matrix^[I]:=Matrix^[I]+Other.Matrix^[I]*Scale;
end;

procedure TMatrix.Subtract(Other: TMatrix);
var
  I : Integer;
begin
  Assert((Other.NRows=NRows) and (Other.NCols=NCols));
  for I:=0 to Size-1 do Matrix^[I]:=Matrix^[I]-Other.Matrix^[I];
end;

procedure TMatrix.SubtractScale(Other: TMatrix; Scale: Double);
var
  I : Integer;
begin
  Assert((Other.NRows=NRows) and (Other.NCols=NCols));
  for I:=0 to Size-1 do Matrix^[I]:=Matrix^[I]-Other.Matrix^[I]*Scale;
end;

procedure TMatrix.SubtractFrom(Other: TMatrix);
var
  I : Integer;
begin
  Assert((Other.NRows=NRows) and (Other.NCols=NCols));
  for I:=0 to Size-1 do Matrix^[I]:=Other.Matrix^[I]-Matrix^[I];
end;

procedure TMatrix.SubtractFrom(const Value: Double);
var
  I : Integer;
begin
  for I:=0 to Size-1 do Matrix^[I]:=Value-Matrix^[I];
end;

procedure TMatrix.Diag;
var
  Old : PFloatArray;
  I : Integer;
begin
  Assert((NRows=1) or (NCols=1));
  if (NRows>1) or (NCols>1) then
  begin
    Old:=Map;
    Map:=nil;
    New(Size,Size);
    Zero;
    for I:=0 to NRows-1 do Matrix[I+I*NCols]:=Old[I];
    FreeMem(Old);
  end;
end;

procedure TMatrix.Transpose;
var
  Old : PFloatArray;
  R, C : Integer;
begin
  SwapDWords(FWidth,FHeight);
  FBytesPerLine:=Width*SizeOf(Float);
  if (FWidth<>1) and (FHeight<>1) then
  begin
    Old:=Matrix;
    GetMem(Map,ByteSize);
    for R:=0 to NRows-1 do for C:=0 to NCols-1 do Matrix[C+R*NCols]:=Old[R+C*NRows];
    FreeMem(Old);
  end;
end;

procedure TMatrix.Transpose(Source: TMatrix);
var
  Old : PFloatArray;
  R, C : Integer;
begin
  Assert(Source<>Self);
  New(Source.NCols,Source.NRows);
  Old:=Source.Matrix;
  for R:=0 to NRows-1 do for C:=0 to NCols-1 do Matrix[C+R*NCols]:=Old[R+C*NRows];
end;

function TMatrix.Sum: Double;
var I : Integer;
begin
  Result:=0;
  for I:=0 to Size-1 do Result:=Result+Matrix^[I];
end;

function TMatrix.SqrSum: Double;
var I : Integer;
begin
  Result:=0;
  for I:=0 to Size-1 do Result:=Result+Sqr(Matrix^[I]);
end;

function TMatrix.Mean: Double;
begin
  Result:=Sum/Size;
end;

function TMatrix.Variance: Double;
begin
  Result:=SqrSum/Size-Sqr(Mean);
end;

procedure TMatrix.AbsMatrix;
var I : Integer;
begin
  for I:=0 to Size-1 do Matrix^[I]:=Abs(Matrix^[I]);
end;

procedure TMatrix.Log10;
var I : Integer;
begin
  for I:=0 to Size-1 do Matrix^[I]:=Math.Log10(Matrix^[I]);
end;

// Fill with random numbers in [Min;Max[
procedure TMatrix.Random(Min,Max: Float);
var I : Integer;
begin
  for I:=0 to Size-1 do Matrix^[I]:=Min+System.Random*Max;
end;

procedure TMatrix.ForceInRange(Min,Max: Float);
var I : Integer;
begin
  for I:=0 to Size-1 do
    if Matrix^[I]>Max then Matrix^[I]:=Max
    else if Matrix^[I]<Min then Matrix^[I]:=Min;
end;

function TMatrix.Min: Float;
var I : Integer;
begin
  Result:=MaxFloat;
  for I:=0 to Size-1 do if Matrix^[I]<Result then Result:=Matrix^[I];
end;

function TMatrix.Max: Float;
var I : Integer;
begin
  Result:=-MaxFloat;
  for I:=0 to Size-1 do if Matrix^[I]>Result then Result:=Matrix^[I];
end;

function TMatrix.FrobeniusNormSqr: Double; // Sum(elements²)
var I : Integer;
begin
  Result:=0;
  for I:=0 to Size-1 do Result:=Result+Sqr(Matrix^[I]);
end;

procedure TMatrix.Rotate180(Other: TMatrix);
var
  E, OtherE : PFloat;
  T : Float;
  I : Integer;
begin
  if (Other=nil) or (Other=Self) then for I:=0 to Size div 2-1 do
  begin
    T:=Matrix^[I];
    Matrix^[I]:=Matrix^[Size-1-I];
    Matrix^[Size-1-I]:=T;
  end
  else
  begin
    New(Other.NRows,Other.NCols);
    E:=Pointer(Matrix);
    OtherE:=@Other.Matrix^[Size-1];
    for I:=1 to Size do
    begin
      E^:=OtherE^;
      Inc(E);
      Dec(OtherE);
    end;
  end;
end;

procedure TMatrix.Mirror(Other: TMatrix);
var
  E, OtherE : PFloat;
  T : Float;
  R, C : Integer;
begin
  if (Other=nil) or (Other=Self) then
  begin
    for R:=0 to NRows-1 do
    begin
      E:=ScanLine[R];
      OtherE:=Pixel[NCols-1,R];
      for C:=0 to NCols div 2 do
      begin
        T:=E^;
        E^:=OtherE^;
        OtherE^:=T;
        Inc(E);
        Dec(OtherE);
      end;
    end;
  end
  else
  begin
    Assign(Other);
    Mirror;
  end;
end;

procedure TMatrix.Normalize;
begin
  Multiply(1/Sum);
end;

type
  // G = F*H
  TFloatFilterProcess = class(TMultiCoreProcess)
  protected
    G, H : TMatrix;
    FMatrix : PFloatArray;
    procedure FilterBorders;
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

procedure TFloatFilterProcess.FilterBorders;
var
  r, c, Hr, Hc, rMid, cMid, rMin, rMax, cMin, cMax, RR, CC : Integer;
  ElementSum : Double;
  HElement : PFloat;
begin
  with G do
  begin
    rMid:=H.NRows div 2;
    cMid:=H.NCols div 2;
    rMin:=-rMid; rMax:=rMin+H.NRows-1;
    cMin:=-cMid; cMax:=cMin+H.NCols-1;
    // Top center part
    for r:=0 to rMid-1 do
      for c:=cMid to NCols-(H.NCols-cMid) do
      begin
        ElementSum:=0;
        HElement:=Pointer(H.Matrix);
        for Hr:=rMin to rMax do
          for Hc:=cMin to cMax do
          begin
            ElementSum:=ElementSum+HElement^*FMatrix^[Abs(r+Hr)*NCols+c+Hc];
            Inc(HElement);
          end;
        Matrix^[r*NCols+c]:=ElementSum;
      end;
    // Bottom center part
    for r:=NRows-(H.NRows-rMid)+1 to NRows-1 do
      for c:=cMid to NCols-(H.NCols-cMid) do
      begin
        ElementSum:=0;
        HElement:=Pointer(H.Matrix);
        for Hr:=rMin to rMax do
          for Hc:=cMin to cMax do
          begin
            RR:=r+Hr;
            if RR>=NRows then RR:=2*NRows-RR-1;
            ElementSum:=ElementSum+HElement^*FMatrix^[RR*NCols+c+Hc];
            Inc(HElement);
          end;
        Matrix^[r*NCols+c]:=ElementSum;
      end;
    // Left border
    for r:=0 to NRows-1 do
      for c:=0 to cMid-1 do
      begin
        ElementSum:=0;
        HElement:=Pointer(H.Matrix);
        for Hr:=rMin to rMax do
          for Hc:=cMin to cMax do
          begin
            RR:=Abs(r+Hr);
            if RR>=NRows then RR:=2*NRows-RR-1;
            ElementSum:=ElementSum+HElement^*FMatrix^[RR*NCols+Abs(c+Hc)];
            Inc(HElement);
          end;
        Matrix^[r*NCols+c]:=ElementSum;
      end;
    // Right border
    for r:=0 to NRows-1 do
      for c:=NCols-(H.NCols-cMid)+1 to NCols-1 do
      begin
        ElementSum:=0;
        HElement:=Pointer(H.Matrix);
        for Hr:=rMin to rMax do
          for Hc:=cMin to cMax do
          begin
            RR:=Abs(r+Hr);
            if RR>=NRows then RR:=2*NRows-RR-1;
            CC:=c+Hc;
            if CC>=NCols then CC:=2*NCols-CC-1;
            ElementSum:=ElementSum+HElement^*FMatrix^[RR*NCols+CC];
            Inc(HElement);
          end;
        Matrix^[r*NCols+c]:=ElementSum;
      end;
  end;
end;

procedure TFloatFilterProcess.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  r, c, Hr, Hc, rMid, cMid : Integer;
  ElementSum : Double;
  HElement, FElement : PFloat;
  ThreadData : PThreadDataStartStop;
begin
  rMid:=H.NRows div 2;
  cMid:=H.NCols div 2;
  ThreadData:=Data;

  // Borders
  if ThreadData.Start<rMid then
  begin
    FilterBorders;
    Inc(ThreadData.Start);
  end;

  // Central part
  with G do
  //for r:=rMid to NRows-(H.NRows-rMid) do
  for r:=ThreadData.Start to ThreadData.Stop do
    for c:=cMid to NCols-(H.NCols-cMid) do
    begin
      ElementSum:=0;
      HElement:=Pointer(H.Matrix);
      FElement:=@FMatrix^[(r-rMid)*NCols+c-cMid];
      for Hr:=1 to H.NRows do
      begin
        for Hc:=1 to H.NCols do
        begin
          ElementSum:=ElementSum+HElement^*FElement^;
          Inc(HElement);
          Inc(FElement);
        end;
        Inc(FElement,NCols-H.NCols);
      end;
      Matrix^[r*NCols+c]:=ElementSum;
    end;
end;


{ ******************************************************************** }
{ Solve a linear system of equations: Self*v = b, i.e solve for v      }
{                                                                      }
{ Usage: A.SolveLinear (v, b, t);                                      }
{        Solution in v                                                 }
{ If the boolean t is true then self is replaced by the inverse        }
{ ******************************************************************** }
procedure TMatrix.SolveLinear(v, b: TVector; SelfToInv, CanTrashSelf: Boolean; TempObjects: PObjectArray);
var
  i, j : integer;
  indx : TIntVector;
  col : TVector;
  dest, src : TMatrix;
begin
  Assert(Self.NRows=Self.NCols,'SolveLinear: Matrix must be square');
  Assert(Self.NRows=b.Length);
  v.Assign(b);
  { Make a copy and work on the copy }
  dest:=CreateOrGetObject(TempObjects^[0],TMatrix);
  indx:=CreateOrGetObject(TempObjects^[1],TIntVector);
  if CanTrashSelf or SelfToInv then src:=Self
  else src:=TMatrix.Create(Self);
  try
    indx.New(NCols);
    src.LUDecomp(dest,indx,@TempObjects^[2]);
    dest.LUSolve(indx,v);
    if SelfToInv then
    begin
       col:=TVector.Create(NCols);
       try
         for j:=1 to NRows do
         begin
           col.Clear;
           col[j]:=1;
           dest.LUSolve(indx,col);
           for i:=1 to NCols do Self[i,j]:=col[i]; // TODO: Use Move?
         end;
       finally
         col.free;
       end;
     end;
  finally
    if src<>Self then src.destroy;
    if Cardinal(TempObjects)<1024 then
    begin
      indx.destroy;
      dest.destroy;
    end;
  end;
end;

{ ******************************************************************** }
{ LU Solve. Solve the linear system represented by m and right-hand    }
{ side b. m is assumed have have been decomposed by LUDecomp.          }
{                                                                      }
{ Usage: m.LUSolve (index, b)                                          }
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.LUSolve(index: TIntVector; b: TVector);
var
  i, j, ii, ip : integer;
  sum : Double;
begin
   ii := 0;
   for i := 1 to nRows do
   begin
     ip := index[i];
     sum := b[ip];
     b[ip] := b[i];
     if ii <> 0 then
       for j := ii TO i-1 do sum := sum - Self[i,j]*b[j]
     else if sum <> 0 then ii := i;
     b[i] := sum;
   end;
   for i := nRows downto 1 do
   begin
     sum := b[i];
     if i < nRows then
       for j := i+1 to nRows do sum := sum - Self[i,j]*b[j];
     b[i] := sum/Self[i,i];
   end
end;

{ ******************************************************************** }
{ Form LU decomposition of Self matrix. Result goes into M             }
{                                                                      }
{ Usage: m.LUDecomp(result, index);                                    }
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.LUDecomp(M: TMatrix; index : TIntVector; TempObjects: PObjectArray);
var
  v : TVector;
  i, k, j, iMax : integer;
  sum, big, tmp : Double;
  Row : PRow;
begin
  Assert(NRows=NCols,'LUDecomp: Matrix must be square');
  iMax:=0;
  M.Assign(Self);
  v:=CreateOrGetObject(TempObjects^[1],TVector);
  try
    v.New(NCols);
    { Find the largest element in every row, and store its reciprocal in v[i] }
    Row:=Pointer(Matrix);
    for i := 1 to NRows do
    begin
      big := 0; { Needed to test for singularity }
      for j := 1 to NCols do if (abs(Row[j]) > big) then big := abs(Row[j]);
      if big = 0 then raise ESingluarMatrix.Create('LUDecomp: Singular matrix in LUDecomp, found row of zeros');
      v[i] := 1/big;
      Inc(Row,NCols);
    end;

    for j := 1 TO NCols do
    begin
      { Form beta = aij - sum_k=1^i-1 aik * bkj }
      for i := 1 TO j-1 do
      begin
        Row:=M.ScanLine[i-1];
        sum:=Row[j];
        for k:=1 to i-1 do sum:=sum-Row[k]*M[k,j];
        Row[j]:=sum;
      end;
      big := 0;
      for i := j to nRows do
      begin
        Row:=M.ScanLine[i-1];
        sum := Row[j];
        for k := 1 to j-1 do sum:=sum-Row[k]*M[k,j];
        Row[j] := sum;
        tmp:=v[i]*abs(sum);
        if tmp >= big then
        begin
          big := tmp;
          imax := i;
        end;
      end;

      { Interchange rows if necessary }
      if j<>iMax then
      begin
        Row:=M.ScanLine[iMax-1];
        for k:=1 to NCols do
        begin
          tmp:=Row[k];
          Row[k]:=M[j,k];
          M[j,k]:=tmp
        end;
        v[imax]:=v[j]
      end;
      index[j] := imax;
      { Get ready to divide by pivot element }
      if M[j,j] = 0 then raise ESingluarMatrix.Create ('LUDecomp: Singular Matrix, pivot value is zero');
      if j <> NCols then
      begin
        tmp:=1/m[j,j];
        for i:=j+1 to nRows do
          m[i,j] := m[i,j]*tmp
      end
    end;
  finally
    if Cardinal(TempObjects)<1024 then
      v.Destroy;
  end;
end;

{ ----------------------------------------------------------------------
  QR decomposition. Factors the matrix Self (n x m, with n >= m) as a
  product Q * R where Q is a (n x m) column-orthogonal matrix, and R
  a (m x m) upper triangular matrix.
  ---------------------------------------------------------------------- }
procedure TMatrix.QRDecomp(Q,R: TMatrix);
var
  I, J, K : Integer;
  Sum : Double;
  Row : PRow;
begin
  Assert(NRows>=NCols);
  if Q<>Self then Q.Assign(Self);
  R.New(NCols,NCols);

  for K:=1 to NCols do
  begin
    // Compute the "k"th diagonal entry in R
    Sum:=0;
    for I:=1 to NRows do Sum:=Sum+Sqr(Q[I,K]);
    if Sum=0 then raise ESingluarMatrix.Create('QRDecomp: Singular Matrix');
    Sum:=Sqrt(Sum);
    R[K,K]:=Sum;

    // Divide the entries in the "k"th column of Self by the computed "k"th
    // diagonal element of R.  this begins the process of overwriting Self
    // with Self . . .
    Sum:=1/Sum;
    for I:=1 to NRows do Q[I,K]:=Q[I,K]*Sum;
    for J:=K+1 to NCols do
    begin
      // Complete the remainder of the row entries in R
      Sum:=0;
      Row:=Pointer(Matrix);
      for I:=1 to NRows do
      begin
        Sum:=Sum+Row[K]*Row[J];
        Inc(Row,NCols);
      end;
      R[K,J]:=Sum;
      // Update the column entries of the Q/A matrix
      Row:=Pointer(Matrix);
      for I:=1 to NRows do
      begin
        Row[J]:=Row[J]-Row[K]*Sum;
        Inc(Row,NCols);
      end;
    end;
  end;
end;

function TVector.IndexOfMax: Integer;
var
  Max : Float;
  I : Integer;
begin
  Result:=-1;
  Max:=-Infinity;
  for I:=0 to Size-1 do
    if Matrix^[I]>Max then
    begin
      Max:=Matrix^[I];
      Result:=I;
    end;
  Inc(Result);
end;

function TVector.IndexOfMin: Integer;
var
  Min : Float;
  I : Integer;
begin
  Result:=-1;
  Min:=Infinity;
  for I:=0 to Size-1 do
    if Matrix^[I]<Min then
    begin
      Min:=Matrix^[I];
      Result:=I;
    end;
  Inc(Result);
end;

end.

