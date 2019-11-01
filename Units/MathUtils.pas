////////////////////////////////////////////////////////////////////////////////
//
// MathUtils.pas - Math functions
// ------------------------------
// Version:   2006-01-15
// Maintain:  Michael Vinther   |    mv@logicnet·dk
//
unit MathUtils;

interface

uses Windows, SysUtils, Math;

{$IFDEF SingleFloat}
type Float = Single; 
const MaxFloat = MaxSingle;
{$ELSE}
type Float = Double;
const MaxFloat = MaxDouble;
{$ENDIF}

var
  DefaultFPUControlWord : Word = $1232; // Note: Updated in initialization section

type
  PFloat = ^Float;
  TFloatArray = array[0..32767] of Float;
  PFloatArray = ^TFloatArray;
  TFloatDynArray = array of Float;


  TSingleArray = array[0..32767] of Single;
  PSingleArray = ^TSingleArray;

  TDoubleArray = array[0..32767] of Double;
  PDoubleArray = ^TDoubleArray;

  TFloatRect = record
                  Left, Top, Right, Bottom : Float;
                end;
  TFloatPoint = record
                  X, Y : Float;
                end;
  PFloatPoint = ^TFloatPoint;
  TFloatPointArray = array of TFloatPoint;

  TDoublePoint = record
                   X, Y : Double;
                 end;

const
  Origo      : TPoint = (x:0;y:0);
  FloatOrigo : TFloatPoint = (x:0;y:0);

type
  Fix = packed record // Used for typecasting in fix-point math
          Dec : Word; Int : SmallInt;
        end;

  TVarianceUpdater = object
                       Count : Integer;
                       Sum, SqrSum : Double;
                       procedure Init;
                       procedure Add(const Value: Double);
                       function Variance: Double;
                       function StdDev: Double;
                       function Average: Double;
                     end;

// Solve system of liniar equtions, Tx = y when T is tridiagonal, by simple
// Gaussian elimination.
// T(i,1)x(i-1) + T(i,2)x(i) + T(i,3)x(i+1) = y(i)
// The contents of y is replaced by the solution, x.
// Note that T is modified
type
  TTriDiagRec = record case Integer of
                   0:(Upper, Diag, Lower : Double);
                   1:(T1,T2,T3 : Double);
                end;
procedure TridiagonalSolve(var T: array of TTriDiagRec; var y: array of Float; N: Integer=0);

// Shuffle list
procedure ShuffleList(var List: array of Integer; Count: Integer);

// Integer "rounding"
function Ceil4(X: Integer): Integer;
function Ceil8(X: Integer): Integer;
function Ceil16(X: Integer): Integer;
function CeilInt(Value: Integer; StepSize: Integer): Integer;
function FloorInt(Value: Integer; StepSize: Integer): Integer; // Round towards zero
function RoundInt(Value: Integer; StepSize: Integer): Integer; // OPTIMIZE!
function FloatMod(const Value,Divisor: Double): Double;

// "Differentiate" X, like diff in Matlab
procedure Diff(var X: array of Double);

function InRangeR(const A,Min,Max: Double): Boolean;
function ForceInRange(A,Min,Max: Integer): Integer;
function ForceInRangeR(const A,Min,Max: Double): Double;
function ForceInBox(const Point: TPoint; const Box: TRect): TPoint;

function RoundPoint(const X,Y: Double): TPoint; overload;
function RoundPoint(const P: TFloatPoint): TPoint; overload;
function FloatPoint(const X,Y: Float): TFloatPoint; overload;
function FloatPoint(const P: TPoint): TFloatPoint; overload;
function DoublePoint(const X,Y: Double): TDoublePoint; overload;
function OffsetPoint(const P,Offset: TPoint): TPoint;
// True if at least part of the line is in the rectangle
function LineInRect(const P1,P2: TPoint; const Rect: TRect): Boolean; overload; // - MIGHT NOT WORK
function LineInRect(const P1,P2: TFloatPoint; const Rect: TFloatRect): Boolean; overload; // - MIGHT NOT WORK
function ClipLineToRect(var P1,P2: TFloatPoint; const Rect: TFloatRect): Boolean;
// Return intersection of two line sections or false if no intersection. UNTESTED
function LineSegmentIntersection(const L1P1: TFloatPoint; L1P2: TFloatPoint; const L2P1: TFloatPoint; L2P2: TFloatPoint; var P: TFloatPoint): Boolean;
function PointToLineSegmentDist(const Point,LineP1,LineP2: TFloatPoint): Double;
function PointToLineDist(const Point,LinePt,LineDir: TFloatPoint): Double;
function PointDist(const P1,P2: TPoint): Double; overload;
function PointDist(const P1,P2: TFloatPoint): Double; overload;
function AreaOfTriangle(const P1,P2,P3: TFloatPoint): Double;
// Ensure that Left<=Right and Top<=Bottom
function NormalizeRect(const Rect: TRect): TRect;
// Same as Rect(), but with floating point arguments
function RoundRect(const ALeft, ATop, ARight, ABottom: Double): TRect; overload;
function RoundRect(const Rect: TFloatRect): TRect; overload;
function RoundBounds(const ALeft, ATop, AWidth, AHeight: Double): TRect;
function IsZeroRect(const Rect: TRect): Boolean;
function FloatRect(const ALeft, ATop, ARight, ABottom: Double): TFloatRect; overload;
function FloatRect(const Rect: TRect): TFloatRect; overload;
function FloatBounds(const ALeft, ATop, AWidth, AHeight: Double): TFloatRect;
function FloatPtInRect(const Rect: TFloatRect; const P: TFloatPoint): Boolean;

function sinc(x: Single): Single;
function Lanczos2(x: Single): Single;
function Lanczos3(x: Single): Single;
function Blackman2(x: Single): Single;
function Gauss(const x,Spread: Double): Double;
// Smooth transition from 0 to 1 for input 0 to 1
function SmoothStep(const y: Double): Double;

// Result = V1+V2
function VectorAdd(const V1,V2: TFloatPoint): TFloatPoint; overload;
function VectorAdd(const V1,V2: TPoint): TPoint; overload;
// Result = V1-V2
function VectorSubtract(const V1,V2: TFloatPoint): TFloatPoint; overload;
function VectorSubtract(const V1,V2: TPoint): TPoint; overload;
// Result = (V1+V2)/2
function VectorAverage(const V1,V2: TFloatPoint): TFloatPoint;
// Result = V1·V2
function VectorDot(const V1,V2: TFloatPoint): Double;
// Result = |V|²
function VectorLengthSqr(const V: TFloatPoint): Double; overload;
// Result = |V|
function VectorLength(const V: TFloatPoint): Double;
// Result = V*s
function VectorMult(const V: TFloatPoint; const s: Double): TFloatPoint;
function VectorHat(const V: TFloatPoint): TFloatPoint;

// Make sure that angle is in 0-2pi
function NormalizeAngle(Angle: Double): Double;

function RotatePoint(Point: TFloatPoint; const Center: TFloatPoint; const Angle: Float): TFloatPoint;
procedure SinCosDouble(const Theta: Extended; out Sin, Cos: Double);
procedure SinCosSingle(const Theta: Extended; out Sin, Cos: Single);

// Return spline value for t in [0,1] between P1 and P2
function CatmullRomPoly(P0,P1,P2,P3: Integer; const t: Double): Double; overload;
function CatmullRomPoly(P0,P1,P2,P3: Single; t: Single): Single; overload;
// Return spline derivative for t in [0,1] between P1 and P2
function CatmullRomPolyDeriv(P0,P1,P2,P3: Integer; const t: Double): Double;

implementation

{$IFDEF VER140}
uses Types;
{$ENDIF}

function sinc(x: Single): Single;
var
  xPi : Single;
begin     
  if abs(x)<1e-8 then Result:=1
  else
  begin
    xPi:=x*Pi;
    Result:=sin(xPi)/xPi;
  end;
end;

// http://avisynth.nl/index.php/Resampling#Blackman_resampler
function Blackman2(x: Single): Single;
const taps = 2;
const a = 0.16;

  function w(x: Single; taps: Integer): Single;
  begin
    if Abs(X)>taps then Result:=0
    else Result:=(1-a)/2 + 1/2*cos(x/taps) + a/2*cos(2*x/taps)
  end;

begin
  Result := sinc(x) * w(x, taps);
end;

// http://en.wikipedia.org/wiki/Lanczos_resampling
function Lanczos2(x: Single): Single;
const a = 2;
begin
  x:=Abs(x);
  if x<1e-8 then Result:=1
  else if x>=a then Result:=0
  else Result:=a*sin(pi*x)*sin((pi/a)*x)/Sqr(pi*x);
end;

function Lanczos3(x: Single): Single;
const a = 3;
begin
  x:=Abs(x);
  if x<1e-8 then Result:=1
  else if x>=a then Result:=0
  else Result:=a*sin(pi*x)*sin((pi/a)*x)/Sqr(pi*x);
end;

function Gauss(const x,Spread: Double): Double;
begin
  Result:=exp(-sqr(x/spread));
end;

function SmoothStep(const y: Double): Double;
begin
  Result := y * y * (y - 2) * (y - 2);
end;

function FloatPoint(const X,Y: Float): TFloatPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

function RoundRect(const ALeft, ATop, ARight, ABottom: Double): TRect;
begin
  with Result do
  begin
    Left:=Round(ALeft);
    Right:=Round(ARight);
    Top:=Round(ATop);
    Bottom:=Round(ABottom);
  end;
end;

function RoundRect(const Rect: TFloatRect): TRect; 
begin
  with Result do
  begin
    Left:=Round(Rect.Left);
    Right:=Round(Rect.Right);
    Top:=Round(Rect.Top);
    Bottom:=Round(Rect.Bottom);
  end;
end;

function RoundBounds(const ALeft, ATop, AWidth, AHeight: Double): TRect;
begin
  with Result do
  begin
    Left := Round(ALeft);
    Top := Round(ATop);
    Right := Round(ALeft + AWidth);
    Bottom :=  Round(ATop + AHeight);
  end;
end;

function RoundPoint(const X,Y: Double): TPoint;
begin
  Result.X:=Round(X);
  Result.Y:=Round(Y);
end;

function RoundPoint(const P: TFloatPoint): TPoint; 
begin
  Result.X:=Round(P.X);
  Result.Y:=Round(P.Y);
end;

function IsZeroRect(const Rect: TRect): Boolean;
begin
  Result:=(Rect.Left=0) and (Rect.Top=0) and (Rect.Right=0) and (Rect.Bottom=0);
end;

function FloatRect(const ALeft, ATop, ARight, ABottom: Double): TFloatRect;
begin
  with Result do
  begin
    Left:=ALeft;
    Right:=ARight;
    Top:=ATop;
    Bottom:=ABottom;
  end;
end;

function FloatBounds(const ALeft, ATop, AWidth, AHeight: Double): TFloatRect;
begin
  with Result do
  begin
    Left:=ALeft;
    Right:=ALeft+AWidth;
    Top:=ATop;
    Bottom:=ATop+AHeight;
  end;
end;

function FloatRect(const Rect: TRect): TFloatRect;
begin
  with Result do
  begin
    Left:=Rect.Left;
    Right:=Rect.Right;
    Top:=Rect.Top;
    Bottom:=Rect.Bottom;
  end;
end;

function FloatPoint(const P: TPoint): TFloatPoint;
begin
  Result.X:=P.X;
  Result.Y:=P.Y;
end;

function DoublePoint(const X,Y: Double): TDoublePoint; overload;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

procedure SinCosDouble(const Theta: Extended; out Sin, Cos: Double);
asm
  FLD     Theta
  FSINCOS
  FSTP    qword ptr [edx]    // Cos
  FSTP    qword ptr [eax]    // Sin
  FWAIT
end;

procedure SinCosSingle(const Theta: Extended; out Sin, Cos: Single);
asm
  FLD     Theta
  FSINCOS
  FSTP    dword ptr [edx]    // Cos
  FSTP    dword ptr [eax]    // Sin
  FWAIT
end;

// Make sure that angle is in 0-2pi
function NormalizeAngle(Angle: Double): Double;
var I : Integer;
begin
  while Angle<0 do Angle:=Angle+2*Pi;
  I:=Trunc(Angle/(2*Pi));
  Result:=Angle-I*2*Pi;
end;

function RotatePoint(Point: TFloatPoint; const Center: TFloatPoint; const Angle: Float): TFloatPoint;
var
  Sin, Cos : Double;
begin
  SinCosDouble(Angle,Sin,Cos);
  Point.X:=Point.X-Center.X;
  Point.Y:=Point.Y-Center.Y;
  Result.X:=Cos*Point.X-Sin*Point.Y+Center.X;
  Result.Y:=Sin*Point.X+Cos*Point.Y+Center.Y;
end;

function OffsetPoint(const P,Offset: TPoint): TPoint;
begin
  Result.X:=P.X+Offset.X;
  Result.Y:=P.Y+Offset.Y;
end;

function LineInRect(const P1,P2: TPoint; const Rect: TRect): Boolean;
begin
  Result:=(Min(P1.Y,P2.Y)<=Rect.Bottom) and (Max(P1.Y,P2.Y)>=Rect.Top) and
          (Min(P1.X,P2.X)<=Rect.Right) and (Max(P1.X,P2.X)>=Rect.Left) and (
          ((P2.Y=P1.Y) or
           InRange(P1.X+Round((P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Top-P1.Y)),Rect.Left,Rect.Right) or
           InRange(P1.X+Round((P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Bottom-P1.Y)),Rect.Left,Rect.Right)) or
          ((P2.X=P1.X) or
           InRange(P1.Y+Round((P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Left-P1.X)),Rect.Top,Rect.Bottom) or
           InRange(P1.Y+Round((P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Right-P1.X)),Rect.Top,Rect.Bottom)));
end;

function LineInRect(const P1,P2: TFloatPoint; const Rect: TFloatRect): Boolean;
begin
  Result:=(Min(P1.Y,P2.Y)<=Rect.Bottom) and (Max(P1.Y,P2.Y)>=Rect.Top) and
          (Min(P1.X,P2.X)<=Rect.Right) and (Max(P1.X,P2.X)>=Rect.Left) and (
          ((P2.Y=P1.Y) or
           InRange(P1.X+(P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Top-P1.Y),Rect.Left,Rect.Right) or      // Top edge
           InRange(P1.X+(P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Bottom-P1.Y),Rect.Left,Rect.Right)) or  // Bottom edge
          ((P2.X=P1.X) or
           InRange(P1.Y+(P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Left-P1.X),Rect.Top,Rect.Bottom) or     // Left edge
           InRange(P1.Y+(P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Right-P1.X),Rect.Top,Rect.Bottom)));    // Right edge
end;

// 2 x 2 matrices
type
  TMatrix2x2 = array[1..2,1..2] of Double;
  TVector2 = array[1..2] of Double;

// Solve 2x2 system Ax = b
function Solve2x2(const A: TMatrix2x2; const b: TVector2): TVector2;
var
  Det : Double;
begin
  Det:=A[1,1]*A[2,2]-A[1,2]*A[2,1];
  Result[1]:=(b[1]*A[2,2]-b[2]*A[1,2])/Det;
  Result[2]:=(b[2]*A[1,1]-b[1]*A[2,1])/Det;
end;

function LineSegmentIntersection(const L1P1: TFloatPoint; L1P2: TFloatPoint; const L2P1: TFloatPoint; L2P2: TFloatPoint; var P: TFloatPoint): Boolean;
var
 A : TMatrix2x2;
 x, b : TVector2;
begin
  with L1P2 do
  begin
    X:=X-L1P1.X;
    Y:=Y-L1P1.Y;
  end;
  with L2P2 do
  begin
    X:=X-L2P1.X;
    Y:=Y-L2P1.Y;
  end;
  b[1]:=L1P1.X-L2P1.X;
  b[2]:=L1P1.Y-L2P1.Y;
  A[1,1]:=L2P2.X; A[1,2]:=-L1P2.X;
  A[2,1]:=L2P2.Y; A[2,2]:=-L1P2.Y;
  try
    x:=Solve2x2(A,b);
  except
    Result:=False;
    Exit;
  end;
  if not InRange(x[1],0,1) or not InRange(x[2],0,1) then
  begin
    Result:=False;
    Exit;
  end;
  P.X:=L1P1.X+x[1]*L1P2.X;
  P.Y:=L1P1.Y+x[1]*L1P2.Y;
  Result:=True;
end;

// Is this working? Looks strange
function PointToLineSegmentDist(const Point,LineP1,LineP2: TFloatPoint): Double;
var
  V : TFloatPoint;
  d : Double;
begin
  V:=VectorSubtract(LineP2,LineP1);
  d:=VectorLengthSqr(V);
  if d>0 then d:=VectorDot(VectorSubtract(Point,LineP1),V)/d;
  if d<=0 then Result:=PointDist(Point,LineP1)
  else if d>=1 then Result:=PointDist(Point,LineP2)
  else Result:=PointDist(Point,VectorAdd(LineP1,VectorMult(V,d)));
end;

function PointToLineDist(const Point,LinePt,LineDir: TFloatPoint): Double;
begin
  Result:=Abs(LineDir.X*(LinePt.Y-Point.Y)-LineDir.Y*(LinePt.X-Point.X))/VectorLength(LineDir);
end;

function FloatPtInRect(const Rect: TFloatRect; const P: TFloatPoint): Boolean;
begin
  with P do Result:=(X>=Rect.Left) and (Y>=Rect.Top) and (X<=Rect.Right) and (Y<=Rect.Bottom);
end;

function ClipLineToRect(var P1,P2: TFloatPoint; const Rect: TFloatRect): Boolean;

  function InRangeX(const X: Double): Boolean;
  begin
    if P1.X<=P2.X then Result:=(X>=P1.X) and (X<=P2.X)
    else Result:=(X>=P2.X) and (X<=P1.X);
  end;

  function InRangeY(const Y: Double): Boolean;
  begin
    if P1.Y<=P2.Y then Result:=(Y>=P1.Y) and (Y<=P2.Y)
    else Result:=(Y>=P2.Y) and (Y<=P1.Y);
  end;

var
  X, Y, S : Double;
  FoundOne : Boolean;
  P1Temp : TFloatPoint;
begin
  Result:=True;
  if FloatPtInRect(Rect,P1) then
  begin
    if not FloatPtInRect(Rect,P2) then // P1 inside, P2 not
    begin
      if P2.Y<>P1.Y then
      begin
        if P2.Y<Rect.Top then
        begin
          X:=P1.X+(P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Top-P1.Y); // Top edge
          if (X>=Rect.Left) and (X<=Rect.Right) then
          begin
            P2.X:=X;
            P2.Y:=Rect.Top;
            Exit;
          end;
        end
        else if P2.Y>Rect.Bottom then
        begin
          X:=P1.X+(P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Bottom-P1.Y); // Bottom edge
          if (X>=Rect.Left) and (X<=Rect.Right) then
          begin
            P2.X:=X;
            P2.Y:=Rect.Bottom;
            Exit;
          end;
        end;
      end;
      if P2.X<>P1.X then
      begin
        if P2.X<Rect.Left then
        begin
          Y:=P1.Y+(P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Left-P1.X); // Left edge
          if (Y>=Rect.Top) and (Y<=Rect.Bottom) then
          begin
            P2.X:=Rect.Left;
            P2.Y:=Y;
            Exit;
          end;
        end;
        Y:=P1.Y+(P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Right-P1.X); // Right edge
        {if (Y>=Rect.Top) and (Y<=Rect.Bottom) then}
        begin
          P2.X:=Rect.Right;
          P2.Y:=Y;
          Exit;
        end;
      end;
    end;
  end
  else if FloatPtInRect(Rect,P2) then // P2 inside, P1 not
  begin
    if P2.Y<>P1.Y then
    begin
      if P1.Y<Rect.Top then
      begin
        X:=P1.X+(P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Top-P1.Y); // Top edge
        if (X>=Rect.Left) and (X<=Rect.Right) then
        begin
          P1.X:=X;
          P1.Y:=Rect.Top;
          Exit;
        end;
      end
      else if P1.Y>Rect.Bottom then
      begin
        X:=P1.X+(P2.X-P1.X)/(P2.Y-P1.Y)*(Rect.Bottom-P1.Y); // Bottom edge
        if (X>=Rect.Left) and (X<=Rect.Right) then
        begin
          P1.X:=X;
          P1.Y:=Rect.Bottom;
          Exit;
        end;
      end;
    end;
    if P2.X<>P1.X then
    begin
      if P1.X<Rect.Left then
      begin
        Y:=P1.Y+(P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Left-P1.X); // Left edge
        if (Y>=Rect.Top) and (Y<=Rect.Bottom) then
        begin
          P1.X:=Rect.Left;
          P1.Y:=Y;
          Exit;
        end;
      end;
      Y:=P1.Y+(P2.Y-P1.Y)/(P2.X-P1.X)*(Rect.Right-P1.X); // Right edge
      {if (Y>=Rect.Top) and (Y<=Rect.Bottom) then}
      begin
        P1.X:=Rect.Right;
        P1.Y:=Y;
        Exit;
      end;
    end;
  end
  else if (Min(P1.Y,P2.Y)<=Rect.Bottom) and (Max(P1.Y,P2.Y)>=Rect.Top) and
          (Min(P1.X,P2.X)<=Rect.Right) and (Max(P1.X,P2.X)>=Rect.Left) then // Crossing from two points outside
  begin
    FoundOne:=False;
    if P2.Y<>P1.Y then
    begin
      S:=(P2.X-P1.X)/(P2.Y-P1.Y);
      if InRangeY(Rect.Top) then // Top edge
      begin
        X:=P1.X+S*(Rect.Top-P1.Y);
        if (X>=Rect.Left) and (X<=Rect.Right) then
        begin
          P1Temp.X:=X;
          P1Temp.Y:=Rect.Top;
          FoundOne:=True;
        end;
      end;
      if InRangeY(Rect.Bottom) then // Bottom edge
      begin
        X:=P1.X+S*(Rect.Bottom-P1.Y);
        if (X>=Rect.Left) and (X<=Rect.Right) then
        begin
          if FoundOne then
          begin
            P2.X:=X;
            P2.Y:=Rect.Bottom;
            P1:=P1Temp;
            Exit;
          end
          else
          begin
            P1Temp.X:=X;
            P1Temp.Y:=Rect.Bottom;
            FoundOne:=True;
          end;
        end;
      end;
    end;
    if P2.X<>P1.X then
    begin
      S:=(P2.Y-P1.Y)/(P2.X-P1.X);
      if InRangeX(Rect.Left) then // Left edge
      begin
        Y:=P1.Y+S*(Rect.Left-P1.X);
        if (Y>=Rect.Top) and (Y<=Rect.Bottom) then
        begin
          if FoundOne then
          begin
            P2.X:=Rect.Left;
            P2.Y:=Y;
            P1:=P1Temp;
            Exit;
          end
          else
          begin
            P1Temp.X:=Rect.Left;
            P1Temp.Y:=Y;
            FoundOne:=True;
          end
        end;
      end;
      if InRangeX(Rect.Right) then // Right edge
      begin
        Y:=P1.Y+S*(Rect.Right-P1.X);
        if FoundOne and (Y>=Rect.Top) and (Y<=Rect.Bottom) then
        begin
          P2.X:=Rect.Right;
          P2.Y:=Y;
          P1:=P1Temp;
          Exit;
        end;
      end;
    end;
    Result:=False;
  end
  else Result:=False;
end;

function NormalizeRect(const Rect: TRect): TRect;
begin
  Result.Left:=Min(Rect.Left,Rect.Right);
  Result.Right:=Max(Rect.Left,Rect.Right);
  Result.Top:=Min(Rect.Top,Rect.Bottom);
  Result.Bottom:=Max(Rect.Top,Rect.Bottom);
end;

function InRangeR(const A,Min,Max: Double): Boolean;
begin
  Result:=(A>=Min) and (A<=Max);
end;

function ForceInRange(A,Min,Max: Integer): Integer;
begin
  Result:=A;
  if Result<Min then Result:=Min
  else if Result>Max then Result:=Max;
end;

function ForceInRangeR(const A,Min,Max: Double): Double;
begin
  Result:=A;
  if Result<Min then Result:=Min
  else if Result>Max then Result:=Max;
end;

function ForceInBox(const Point: TPoint; const Box: TRect): TPoint;
begin
  with Box do
  begin
    Result.X:=ForceInRange(Point.X,Left,Right);
    Result.Y:=ForceInRange(Point.Y,Top,Bottom);
  end;
end;

procedure ShuffleList(var List: array of Integer; Count: Integer);
var
  I, R, T : Integer;
begin
  for I:=0 to Count-1 do
  begin
    R:=Random(Count);
    T:=List[I];
    List[I]:=List[R];
    List[R]:=T;
  end;
end;

function Ceil4(X: Integer): Integer;
begin
  Result:=(X+3) and $fffffffc;
end;

function Ceil8(X: Integer): Integer;
begin
  Result:=(X+7) and $fffffff8;
end;

function Ceil16(X: Integer): Integer;
begin
  Result:=(X+15) and $fffffff0;
end;

function CeilInt(Value: Integer; StepSize: Integer): Integer;
var
  I : Integer;
begin
  Result:=Value;
  I:=Value mod StepSize;
  if I<>0 then Inc(Result,StepSize-I);
end;

// Round towards zero
function FloorInt(Value: Integer; StepSize: Integer): Integer;
begin
  Result:=Value-Value mod StepSize;
end;

function RoundInt(Value: Integer; StepSize: Integer): Integer;
begin
  //Result:=FloorInt(Value+StepSize div 2,StepSize);
  Result:=Round(Value/StepSize)*StepSize;
end;

procedure Diff(var X: array of Double);
var
  I : Integer;
begin
  for I:=Low(X) to High(X)-1 do X[I]:=X[I+1]-X[I];
  X[High(X)]:=0;
end;

procedure TridiagonalSolve(var T: array of TTriDiagRec; var y: array of Float; N: Integer);
// Solve system of liniar equtions, Tx = y when T is tridiagonal by simple
// Gaussian elimination.
//
// T(i,1)x(i-1) + T(i,2)x(i) + T(i,3)x(i+1) = y(i)
//
// The contents of y is replaced by the solution, x.
//
// Original Matlab version by Hans Bruun Nielsen, IMM, DTU. 01.10.09 / 10.30
// (tridsolv.m)
var
  i : Integer;
  e : Double;
begin
  if n=0 then n:=Length(T);

  // Forward
  for i:=1 to n-1 do
  begin
    e:=T[i].T1/T[i-1].T2;  // Elimination factor
    T[i].T2:=T[i].T2-e*T[i-1].T3;
    y[i]:=y[i]-e*y[i-1];
  end;
  // Back
  y[n-1]:=y[n-1]/T[n-1].T2;
  for i:=n-2 downto 0 do y[i]:=(y[i]-T[i].T3*y[i+1])/T[i].T2;
end;

function PointDist(const P1,P2: TFloatPoint): Double;
begin
  Result:=Sqrt(Sqr(P1.X-P2.X)+Sqr(P1.Y-P2.Y));
end;

function PointDist(const P1,P2: TPoint): Double;
begin
  Result:=Sqrt(Sqr(P1.X-P2.X+0.0)+Sqr(P1.Y-P2.Y+0.0));
end;

// Result = V1+V2
function VectorAdd(const V1,V2: TFloatPoint): TFloatPoint;
begin
  Result.X:=V1.X+V2.X;
  Result.Y:=V1.Y+V2.Y;
end;

// Result = V1+V2
function VectorAdd(const V1,V2: TPoint): TPoint;
begin
  Result.X:=V1.X+V2.X;
  Result.Y:=V1.Y+V2.Y;
end;

// Result = V1-V2
function VectorSubtract(const V1,V2: TFloatPoint): TFloatPoint;
begin
  Result.X:=V1.X-V2.X;
  Result.Y:=V1.Y-V2.Y;
end;

// Result = V1-V2
function VectorSubtract(const V1,V2: TPoint): TPoint; overload;
begin
  Result.X:=V1.X-V2.X;
  Result.Y:=V1.Y-V2.Y;
end;

// Result = (V1+V2)/2
function VectorAverage(const V1,V2: TFloatPoint): TFloatPoint;
begin
  Result.X:=(V1.X+V2.X)/2;
  Result.Y:=(V1.Y+V2.Y)/2;
end;

// Result = V1·V2
function VectorDot(const V1,V2: TFloatPoint): Double;
begin
  Result:=V1.X*V2.X+V1.Y*V2.Y;
end;

// Result = |V|²
function VectorLengthSqr(const V: TFloatPoint): Double;
begin
  Result:=Sqr(V.X)+Sqr(V.Y);
end;

// Result = |V|
function VectorLength(const V: TFloatPoint): Double;
begin
  Result:=Sqrt(Sqr(V.X)+Sqr(V.Y));
end;

// Result = V*s
function VectorMult(const V: TFloatPoint; const s: Double): TFloatPoint;
begin
  Result.X:=V.X*s;
  Result.Y:=V.Y*s;
end;

function VectorHat(const V: TFloatPoint): TFloatPoint;
begin
  Result.X:=-V.Y; // Is this right?
  Result.Y:=V.X;
end;

{ TVarianceUpdater }

procedure TVarianceUpdater.Add(const Value: Double);
begin
  Sum:=Sum+Value;
  SqrSum:=SqrSum+Sqr(Value);
  Inc(Count);
end;

function TVarianceUpdater.Average: Double;
begin
  Result:=Sum/Count;
end;

procedure TVarianceUpdater.Init;
begin
  Sum:=0;
  SqrSum:=0;
  Count:=0;
end;

function TVarianceUpdater.StdDev: Double;
begin
  Result:=Sqrt(Variance);
end;

function TVarianceUpdater.Variance: Double;
begin
  Result:=SqrSum/Count-Sqr(Average);
end;

function AreaOfTriangle(const P1,P2,P3: TFloatPoint): Double;
begin
  Result:=Abs(0.5*(P1.X*P2.Y-P2.X*P1.Y + P2.X*P3.Y-P3.X*P2.Y + P3.X*P1.Y-P1.X*P3.Y));
end;

// Return spline value for t in [0,1] between P1 and P2
function CatmullRomPoly(P0,P1,P2,P3: Integer; const t: Double): Double;
begin
  // http://www.mvps.org/directx/articles/catmull/
  Result:=0.5*((2 * P1) +
               (-P0 + P2) * t +
               (2*P0 - 5*P1 + 4*P2 - P3) * Sqr(t) +
               (-P0 + 3*P1- 3*P2 + P3) * Sqr(t)*t );
end;

// Return spline value for t in [0,1] between P1 and P2
function CatmullRomPoly(P0,P1,P2,P3: Single; t: Single): Single;
begin
  // http://www.mvps.org/directx/articles/catmull/
  Result:=0.5*((2 * P1) +
               (-P0 + P2) * t +
               (2*P0 - 5*P1 + 4*P2 - P3) * Sqr(t) +
               (-P0 + 3*P1- 3*P2 + P3) * Sqr(t)*t );
end;

// Return spline derivative for t in [0,1] between P1 and P2
function CatmullRomPolyDeriv(P0,P1,P2,P3: Integer; const t: Double): Double;
begin
  Result:=0.5*((-P0 + P2) +
               (2*P0 - 5*P1 + 4*P2 - P3) * 2 * t +
               (-P0 + 3*P1- 3*P2 + P3) * 3 * Sqr(t) );
end;

function FloatMod(const Value,Divisor: Double): Double;
begin
  Result:=Frac(Value/Divisor)*Divisor;
end;

initialization
  Set8087CW(DefaultFPUControlWord); // Set known start value
  SetExceptionMask([exDenormalized,exUnderflow,exPrecision]);
  SetPrecisionMode(pmDouble);
  SetRoundMode(rmNearest);
  DefaultFPUControlWord:=Get8087CW and not $40; // Bit 6 is reserved so always set to 0 (may be STACK FAULT which we never want to mask)
  Set8087CW(DefaultFPUControlWord);
end.

