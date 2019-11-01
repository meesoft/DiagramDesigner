////////////////////////////////////////////////////////////////////////////////
//
// Interpolation.pas - Interpolation routines
// ------------------------------------------
// Version:   2004-08-15
// Maintain:  Michael Vinther         mv@logicnet·dk
//
// Last changes:
//
unit Interpolation;

interface

uses Windows, SysUtils, MathUtils, MemUtils, Math;

function LinearInterpolateSample(const List: TFloatPointArray; X: Float): Float; overload;

// Calculate polunomial coefficients for natural cubic spline
// Tne Spline and list parameters must have same length
type
  T4PolyRec = record
                // When z in [x(i),x(i+1)],
                //   S(z) =  a(i) + b(i)(z-x(i)) + c(i)(z-x(i))^2 + d(i)(z-x(i))^2(z-x(i)).
                x : Float;
                a, b, c, d : Float;
              end;
  T4PolyRecArray = array of T4PolyRec;
procedure NaturalCubicSpline(const List: TFloatPointArray; var Spline: T4PolyRecArray; N: Integer=0);

// Get spline value at X
function EvaluateSpline(const Spline: array of T4PolyRec; Z: Float): Float;
function LinearInterpolateSample(const Spline: array of T4PolyRec; X: Float): Float; overload;

// Apply 1. order IIR lowpass filter in both directions
// Matlab analysis: a=(LookBack+1); freqz(conv([1/a],[0 1/a]),conv([1 -(a-1)/a],[-(a-1)/a 1]),1024,Fs)
procedure LowPassFilterIIR(var List: array of Double; Count: Integer; LookBack: Integer=1);

// 3. or 5. order FIR mean filter.
procedure LowPassFilterFIR(var List: array of Double; Count: Integer; Order: Integer);

procedure LinearResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
procedure CubicSplineResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
procedure CatmullRomSplineResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
procedure SincResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
procedure Lanczos2ResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
procedure Lanczos3ResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
procedure Lanczos3ResampleLine8(Line,NewLine: PByteArray; Width, NewWidth: Integer; Skip,NewSkip: Integer; var OptimizationData: TObject);
procedure Blackman2ResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);

// Interpolate between Value0 and Value1 at Pos in [0;1]
function LinearInterpolate(const Value0,Value1,Pos: Double): Double; overload;
// Interpolate between Value0 and Value1 at Pos in [IntStart;IntEnd]
function LinearInterpolate(const Value0,Value1,Pos,IntStart,IntEnd: Double): Double; overload;

implementation

function LinearInterpolateSample(const List: TFloatPointArray; X: Float): Float;
var
  L, H, I : Integer;
begin
  if X<=List[0].X then Result:=List[0].Y
  else if X>=List[High(List)].X then Result:=List[High(List)].Y
  else
  begin
    L:=0;
    H:=High(List);
    while L<=H do
    begin
      I:=(L+H) shr 1;
      if X>List[I].X then L:=I+1 else H:=I-1;
    end;
    if List[L].X=X then Result:=List[L].Y
    else Result:=List[L].Y+(List[H].Y-List[L].Y)*(X-List[L].X)/(List[H].X-List[L].X);
  end;
end;

function EvaluateSpline(const Spline: array of T4PolyRec; Z: Float): Float;
var
  L, H, I : Integer;
  Z2 : Float;
begin
  if Z<=Spline[0].X then H:=0
  else if Z>=Spline[High(Spline)].X then H:=High(Spline)-1
  else
  begin
    L:=0;
    H:=High(Spline);
    while L<=H do
    begin
      I:=(L+H) shr 1;
      if Z>Spline[I].X then L:=I+1 else H:=I-1;
    end;
  end;
  with Spline[H] do
  begin
    Z:=Z-x;
    Z2:=Sqr(Z);
    Result:=a+b*Z+c*Z2+d*Z*Z2;
  end;
end;

function LinearInterpolateSample(const Spline: array of T4PolyRec; X: Float): Float; overload;
var
  L, H, I : Integer;
begin
  if X<=Spline[0].X then Result:=Spline[0].a
  else if X>=Spline[High(Spline)].X then Result:=Spline[High(Spline)].a
  else
  begin
    L:=0;
    H:=High(Spline);
    while L<=H do
    begin
      I:=(L+H) shr 1;
      if X>Spline[I].X then L:=I+1 else H:=I-1;
    end;
    if Spline[L].X=X then Result:=Spline[L].a
    else Result:=Spline[L].a+(Spline[H].a-Spline[L].a)*(X-Spline[L].X)/(Spline[H].X-Spline[L].X);
  end;
end;

procedure NaturalCubicSpline(const List: TFloatPointArray; var Spline: T4PolyRecArray; N: Integer);

// Natural cubic spline interpolation.  pp  = natcubspl(x,y)
// x,y are column n-vectors. It is assumed that n >= 4 and x(1) < ... < x(n).
//
// pp is a piecewise polynomial representation of the spline.  Values of S
// can be computed by PPVAL.
//
// First, set up all but the first and last equations that
// define the vector of interior knot slopes s(2:n-1).

var
  I : Integer;
  Dx, yp, r : array of Float;
  T3 : array of TTriDiagRec;
begin
  if N=0 then N:=Length(List);
  Assert(N=Length(Spline));

  SetLength(Dx,N-1);
  SetLength(yp,N-1);
  for I:=0 to N-2 do
  begin
    Dx[I]:=List[I+1].X-List[I].X;
    yp[I]:=(List[I+1].Y-List[I].Y)/Dx[I];
  end;

  SetLength(T3,N-2); ZeroMem(T3[0],(N-2)*SizeOf(TTriDiagRec));
  SetLength(r,N-2); ZeroMem(r[0],(N-2)*SizeOf(Float));
  for i:=1 to N-4 do
  begin
    T3[i].T2:=2*(Dx[i]+Dx[i+1]);
    T3[i].T1:=Dx[i+1];
    T3[i].T3:=Dx[i];
    r[i]:=3*(Dx[i+1]*yp[i]+Dx[i]*yp[i+1]);
  end;

  // Finish setting up the linear system
  T3[0].T2:=2*Dx[0]+1.5*Dx[1]; T3[0].T3:=Dx[0];
  r[0]:=1.5*Dx[1]*yp[0]+3*Dx[0]*yp[1];

  T3[n-3].T2 := 1.5*Dx[n-3]+2*Dx[n-2];  T3[n-3].T1:=Dx[n-2];
  r[n-3]:=3*Dx[n-2]*yp[n-3] + 1.5*Dx[n-3]*yp[n-2];

  // Solve the system, and set s(1:n) to be the vector of slopes.
  TridiagonalSolve(T3,r);

  // Compute the a,b,c,d vectors.
  // a, b, c, d are (n-1)-vectors that define the spline S(z).
  // On [x(i),x(i+1)],
  //   S(z) =  a(i) + b(i)(z-x(i)) + c(i)(z-x(i))^2 + d(i)(z-x(i))^2(z-x(i)).

  Spline[0].b:=(3*yp[0] - r[0])/2;
  Spline[N-1].b:=(3*yp[n-2]-r[n-3])/2;
  for i:=1 to N-2 do Spline[i].b:=r[i-1];
  for i:=0 to N-2 do with Spline[i] do
  begin
    x:=List[i].X;
    a:=List[i].Y;
    d:=(Spline[i+1].b+b-2*yp[i])/Sqr(Dx[i]);
    c:=(yp[i]-b)/Dx[i]-d*Dx[i];
  end;
  Spline[N-1].x:=List[N-1].X;
  Spline[N-1].a:=List[N-1].Y;
end;

procedure LowPassFilterIIR(var List: array of Double; Count: Integer; LookBack: Integer);
var
  I : Integer;
  LookBack1, Mean : Double;
begin
  LookBack1:=1/(LookBack+1);
  Mean:=List[0];
  for I:=0 to Count-1 do
  begin
    Mean:=(Mean*LookBack+List[I])*LookBack1;
    List[I]:=Mean;
  end;
  Mean:=List[Count-1];
  for I:=Count-1 downto 0 do
  begin
    Mean:=(Mean*LookBack+List[I])*LookBack1;
    List[I]:=Mean;
  end;
end;

procedure LowPassFilterFIR(var List: array of Double; Count: Integer; Order: Integer);
var
  I : Integer;
  Last, Last2, Cur : Double;
begin
  if (Count<1) then Exit
  else if Order=3 then
  begin
    Last:=List[0];
    for I:=0 to Count-2 do
    begin
      Cur:=List[I];
      List[I]:=(Last+Cur+List[I+1])/3;
      Last:=Cur;
    end;
  end
  else if Order=5 then
  begin
    Last:=List[0];
    Last2:=List[0];
    for I:=0 to Count-3 do
    begin
      Cur:=List[I];
      List[I]:=(Last2+Last+Cur+List[I+1]+List[I+2])/5;
      Last2:=Last;
      Last:=Cur;
    end;
  end
  else raise Exception.Create('Unsupported order');
end;

procedure LinearResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
var
  NewI, X1, X2 : Integer;
  Scale, I : Double;
begin
  if Line=nil then Exit;
  Scale:=(Width-1)/(NewWidth-1);
  for NewI:=0 to NewWidth-1 do
  begin
    I:=NewI*Scale;
    X1:=Floor(I);
    X2:=Ceil(I);
    if X2=Width then NewLine^[NewI]:=Line^[X1]
    else
    begin
      I:=Frac(I);
      NewLine^[NewI]:=Line^[X1]*(1-I)+Line^[X2]*I;
    end;
  end;
end;

procedure CubicSplineResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
var
  I : Integer;
  Scale : Double;
  Spline : T4PolyRecArray;
  Knots : TFloatPointArray;
begin
  if Line=nil then Exit;
  SetLength(Knots,Width);
  SetLength(Spline,Width);
  Scale:=(NewWidth-1)/(Width-1);
  for I:=0 to Width-1 do with Knots[I] do
  begin
    X:=I*Scale;
    Y:=Line^[I];
  end;
  NaturalCubicSpline(Knots,Spline);
  for I:=0 to NewWidth-1 do NewLine^[I]:=EvaluateSpline(Spline,I);
end;

procedure CatmullRomSplineResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
var
  I, FloorOldI : Integer;
  OldI, Step : Double;
  P : array[0..3] of Single;
begin
  if Line=nil then Exit;
  P[0]:=Line^[0];
  P[1]:=Line^[0];
  P[2]:=Line^[1];
  P[3]:=Line^[1];
  Step:=(Width-1)/(NewWidth-1);
  OldI:=0;
  for I:=0 to NewWidth-1 do
  begin
    FloorOldI:=Floor(OldI);
    if FloorOldI>0 then P[0]:=Line[FloorOldI-1];
    P[1]:=Line[FloorOldI];
    if FloorOldI+1<Width then P[2]:=Line[FloorOldI+1];
    if FloorOldI+2<Width then P[3]:=Line[FloorOldI+2];
    NewLine^[I]:=CatmullRomPoly(P[0],P[1],P[2],P[3],Frac(OldI));
    OldI:=OldI+Step;
  end;
end;

type
  TResamplingFilter = function(x: Single): Single;

  TLineFilterResampleData = class
  public
    Weights : array of record
                         SourcePixelWeights : array of record
                                                         SourceIndex : Integer;
                                                         SourceWeight : Single;
                                                       end;
                         Scale : Single;
                       end;
    constructor Create(Filter: TResamplingFilter; FilterWindow: Single; SrcWidth,DstWidth: Integer);
    procedure Apply(Source,Dest: PSingleArray); overload;
  end;

  TLineFilterResampleDataInt = class
  public
    Weights : array of record
                         SourcePixelWeights : array of record
                                                         SourceIndex : Integer;
                                                         SourceWeight : Single;
                                                         SourceWeightInt : Integer;
                                                       end;
                         Scale : Single;
                       end;
    constructor Create(Filter: TResamplingFilter; FilterWindow: Single; SrcWidth,DstWidth: Integer);
    procedure Apply(Source,Dest: PByteArray; SrcSkip,DstSkip: Integer); overload;
  end;

constructor TLineFilterResampleData.Create(Filter: TResamplingFilter; FilterWindow: Single; SrcWidth, DstWidth: Integer);
var
  I, P, PMin, PMax : Integer;
  ScaleWN, ScaleNW, Center, Window, Sum : Single;
begin
  // Compute filter weights for each position along the line
  SetLength(Weights,DstWidth);
  ScaleWN:=(SrcWidth-1)/Max(DstWidth-1,1);
  ScaleNW:=(DstWidth-1)/Max(SrcWidth-1,1);
  if DstWidth<SrcWidth then
  begin
    Window:=FilterWindow*ScaleWN;
    for I:=0 to DstWidth-1 do
    begin
      Center:=I*ScaleWN;
      PMin:=Floor(Center-Window);
      PMax:=Ceil(Center+Window);
      SetLength(Weights[I].SourcePixelWeights,PMax-PMin+1);
      Sum:=0;
      for P:=PMin to PMax do
        with Weights[I].SourcePixelWeights[P-PMin] do
        begin
          if P<0 then SourceIndex:=0
          else if P>=SrcWidth then SourceIndex:=SrcWidth-1
          else SourceIndex:=P;
          SourceWeight:=Filter((P-Center)*ScaleNW)*ScaleNW;
          Sum:=Sum+SourceWeight;
        end;
      if Sum>0 then Weights[I].Scale:=1/Sum;
    end;
  end
  else
  begin
    for I:=0 to DstWidth-1 do
    begin
      Center:=I*ScaleWN;
      PMin:=Floor(Center-FilterWindow);
      PMax:=Ceil(Center+FilterWindow);
      SetLength(Weights[I].SourcePixelWeights,PMax-PMin+1);
      Sum:=0;
      for P:=PMin to PMax do
        with Weights[I].SourcePixelWeights[P-PMin] do
        begin
          if P<0 then SourceIndex:=0
          else if P>=SrcWidth then SourceIndex:=SrcWidth-1
          else SourceIndex:=P;
          SourceWeight:=Filter(P-Center);
          Sum:=Sum+SourceWeight;
        end;
      if Sum>0 then Weights[I].Scale:=1/Sum;
    end;
  end;
  // Normalize filter weights
  for I:=0 to DstWidth-1 do
    with Weights[I] do
      for P:=0 to High(SourcePixelWeights) do
        with SourcePixelWeights[P] do
          SourceWeight:=SourceWeight*Scale;
end;

procedure TLineFilterResampleData.Apply(Source,Dest: PSingleArray);
var
  I, J : Integer;
  S, Min, Max, Sum : Single;
begin
  for I:=0 to High(Weights) do
  begin
    Sum:=0;
    Min:=Infinity;
    Max:=NegInfinity;
    with Weights[I] do
    begin
      for J:=0 to High(SourcePixelWeights) do
      begin
        S:=Source[SourcePixelWeights[J].SourceIndex];
        if S>Max then Max:=S;
        if S<Min then Min:=S;
        Sum:=Sum+S*SourcePixelWeights[J].SourceWeight;
      end;
      if Sum<Min then Sum:=Min;
      if Sum>Max then Sum:=Max;
      Dest[I]:=Sum;
    end;
  end;
end;

constructor TLineFilterResampleDataInt.Create(Filter: TResamplingFilter; FilterWindow: Single; SrcWidth, DstWidth: Integer);
var
  I, P, PMin, PMax : Integer;
  ScaleWN, ScaleNW, Center, Window, Sum : Single;
begin
  // Compute filter weights for each position along the line
  SetLength(Weights,DstWidth);
  ScaleWN:=(SrcWidth-1)/Max(DstWidth-1,1);
  ScaleNW:=(DstWidth-1)/Max(SrcWidth-1,1);
  if DstWidth<SrcWidth then
  begin
    Window:=FilterWindow*ScaleWN;
    for I:=0 to DstWidth-1 do
    begin
      Center:=I*ScaleWN;
      PMin:=Floor(Center-Window);
      PMax:=Ceil(Center+Window);
      SetLength(Weights[I].SourcePixelWeights,PMax-PMin+1);
      Sum:=0;
      for P:=PMin to PMax do
        with Weights[I].SourcePixelWeights[P-PMin] do
        begin
          if P<0 then SourceIndex:=0
          else if P>=SrcWidth then SourceIndex:=SrcWidth-1
          else SourceIndex:=P;
          SourceWeight:=Filter((P-Center)*ScaleNW)*ScaleNW;
          Sum:=Sum+SourceWeight;
        end;
      if Sum>0 then Weights[I].Scale:=1/Sum;
    end;
  end
  else
  begin
    for I:=0 to DstWidth-1 do
    begin
      Center:=I*ScaleWN;
      PMin:=Floor(Center-FilterWindow);
      PMax:=Ceil(Center+FilterWindow);
      SetLength(Weights[I].SourcePixelWeights,PMax-PMin+1);
      Sum:=0;
      for P:=PMin to PMax do
        with Weights[I].SourcePixelWeights[P-PMin] do
        begin
          if P<0 then SourceIndex:=0
          else if P>=SrcWidth then SourceIndex:=SrcWidth-1
          else SourceIndex:=P;
          SourceWeight:=Filter(P-Center);
          Sum:=Sum+SourceWeight;
        end;
      if Sum>0 then Weights[I].Scale:=1/Sum;
    end;
  end;
  // Normalize filter weights
  for I:=0 to DstWidth-1 do
    with Weights[I] do
      for P:=0 to High(SourcePixelWeights) do
        with SourcePixelWeights[P] do
          SourceWeightInt:=Round(SourceWeight*Scale*65536);
end;

procedure TLineFilterResampleDataInt.Apply(Source, Dest: PByteArray; SrcSkip, DstSkip: Integer);
var
  I, J, Sum : Integer;
begin
  for I:=0 to High(Weights) do
  begin
    Sum:=0;
    with Weights[I] do
    begin
      for J:=0 to High(SourcePixelWeights) do
        Sum:=Sum+Source[SourcePixelWeights[J].SourceIndex*SrcSkip]*SourcePixelWeights[J].SourceWeightInt;
      Dest[I*DstSkip]:=EnsureRange((Sum+32768) div 65536,0,255);
    end;
  end;
end;

procedure Blackman2ResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
begin
  if Line=nil then OptimizationData:=TLineFilterResampleData.Create(Blackman2,2,Width,NewWidth)
  else if OptimizationData=nil then
  with TLineFilterResampleData.Create(Blackman2,2,Width,NewWidth) do
  begin
    Apply(Line,NewLine);
    Free;
  end
  else TLineFilterResampleData(OptimizationData).Apply(Line,NewLine);
end;

procedure Lanczos2ResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
begin
  if Line=nil then OptimizationData:=TLineFilterResampleData.Create(Lanczos2,2,Width,NewWidth)
  else if OptimizationData=nil then
  with TLineFilterResampleData.Create(Lanczos2,2,Width,NewWidth) do
  begin
    Apply(Line,NewLine);
    Free;
  end
  else TLineFilterResampleData(OptimizationData).Apply(Line,NewLine);
end;

procedure Lanczos3ResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
begin
  if Line=nil then OptimizationData:=TLineFilterResampleData.Create(Lanczos3,3,Width,NewWidth)
  else if OptimizationData=nil then
  with TLineFilterResampleData.Create(Lanczos3,3,Width,NewWidth) do
  begin
    Apply(Line,NewLine);
    Free;
  end
  else TLineFilterResampleData(OptimizationData).Apply(Line,NewLine);
end;

procedure Lanczos3ResampleLine8(Line,NewLine: PByteArray; Width, NewWidth: Integer; Skip,NewSkip: Integer; var OptimizationData: TObject);
begin
  if Line=nil then OptimizationData:=TLineFilterResampleDataInt.Create(Lanczos3,3,Width,NewWidth)
  else if OptimizationData=nil then
  with TLineFilterResampleDataInt.Create(Lanczos3,3,Width,NewWidth) do
  begin
    Apply(Line,NewLine,Skip,NewSkip);
    Free;
  end
  else TLineFilterResampleDataInt(OptimizationData).Apply(Line,NewLine,Skip,NewSkip);
end;

procedure SincResampleLine(Line,NewLine: PSingleArray; Width, NewWidth: Integer; var OptimizationData: TObject);
begin
  if Line=nil then OptimizationData:=TLineFilterResampleData.Create(Sinc,8,Width,NewWidth)
  else if OptimizationData=nil then
  with TLineFilterResampleData.Create(Sinc,8,Width,NewWidth) do
  begin
    Apply(Line,NewLine);
    Free;
  end
  else TLineFilterResampleData(OptimizationData).Apply(Line,NewLine);
end;

// Interpolate between Value0 and Value1 at Pos in [0;1]
function LinearInterpolate(const Value0,Value1,Pos: Double): Double;
begin
  Result:=Value0*(1-Pos)+Value1*Pos;
end;

// Interpolate between Value0 and Value1 at Pos in [IntStart;IntEnd]
function LinearInterpolate(const Value0,Value1,Pos,IntStart,IntEnd: Double): Double; overload;
begin
  Result:=LinearInterpolate(Value0,Value1,(Pos-IntStart)/(IntEnd-IntStart));
end;

end.

