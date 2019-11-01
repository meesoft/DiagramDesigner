unit BitmapGammaInterpolation;

interface

uses
  Windows, LinarBitmap, FloatMap, MultiCoreProcessing;

const
  DefaultMonitorGamma = 2.2;

type
  T8bitToSingleGammaLUT = array[0..255] of Single;
  T8bitToDoubleGammaLUT = array[0..255] of Double;
  T16bitTo8bitGammaLUT = array[0..255*256] of Byte;

  TAverage2GammaLUT = array[0..255,0..255] of Byte;

  TGammaConverter = object
                      InvGammaLUT : T8bitToSingleGammaLUT;
                      GammaLUT : T16bitTo8bitGammaLUT;
                      procedure Prepare(Gamma: Single);
                      function ApplyGamma(Value: Single): Byte;
                      function ConvertToGrayscale(const Color: RGBRec): Byte;
                      function Average4(P1,P2,P3,P4: Byte): Byte;
                    end;

procedure CreateGammaLUT(Gamma: Single; var LUT: T8bitToDoubleGammaLUT); overload;
procedure CreateGammaLUT(Gamma: Single; var LUT: T8bitToSingleGammaLUT); overload;
procedure CreateGammaLUT(Gamma: Single; var LUT: T16bitTo8bitGammaLUT); overload;
procedure CreateGammaLUT(Gamma: Single; var LUT: TAverage2GammaLUT); overload;

procedure ConvertBitmapToFloatMap(Image: TLinearBitmap; FloatMap: TFloatMap; const Gamma: Double); overload;
procedure ConvertBitmapToFloatMap(Image: TLinearBitmap; FloatMap: TFloatMap32; const Gamma: Double); overload;
procedure ConvertFloatMapToBitmap(FloatMap: TFloatMap; Image: TLinearBitmap; const Gamma: Double); overload;
procedure ConvertFloatMapToBitmap(FloatMap: TFloatMap32; Image: TLinearBitmap; const Gamma: Double); overload;

function ApplyGamma(const X,Gamma: Double): Double;

implementation

uses
  Types, SysUtils, Graphics, Math, MathUtils;

function ApplyGamma(const X,Gamma: Double): Double;
begin
  if X>0 then Result:=Power(X/255,1/Gamma)*255
  else Result:=0;
end;

procedure TGammaConverter.Prepare(Gamma: Single);
begin
  CreateGammaLUT(1/Gamma,InvGammaLUT);
  CreateGammaLUT(Gamma,GammaLUT);
end;

function TGammaConverter.ApplyGamma(Value: Single): Byte;
begin
  if Value<=0 then Result:=0
  else if Value>=255 then Result:=255
  else Result:=GammaLUT[Round(Value*256)];
end;

function TGammaConverter.ConvertToGrayscale(const Color: RGBRec): Byte;
begin
  Result:=GammaLUT[Round(InvGammaLUT[Color.B]*(256*0.1145)+
                         InvGammaLUT[Color.G]*(256*0.5866)+
                         InvGammaLUT[Color.R]*(256*0.2989))];
end;

function TGammaConverter.Average4(P1,P2,P3,P4: Byte): Byte;
begin
  Result:=GammaLUT[Round((InvGammaLUT[P1]+InvGammaLUT[P2]+InvGammaLUT[P3]+InvGammaLUT[P4])*64)];
end;

procedure CreateGammaLUT(Gamma: Single; var LUT: T8bitToSingleGammaLUT); overload;
var
  I : Integer;
begin
  Gamma:=1/Gamma;
  LUT[0]:=0;
  for I:=1 to High(LUT) do
    LUT[I]:=Power(I/High(LUT),Gamma)*255;
end;

procedure CreateGammaLUT(Gamma: Single; var LUT: T8bitToDoubleGammaLUT); overload;
var
  I : Integer;
begin
  Gamma:=1/Gamma;
  LUT[0]:=0;
  for I:=1 to High(LUT) do
    LUT[I]:=Power(I/High(LUT),Gamma)*255;
end;

procedure CreateGammaLUT(Gamma: Single; var LUT: T16bitTo8bitGammaLUT);
var
  I : Integer;
begin
  Gamma:=1/Gamma;
  LUT[0]:=0;
  for I:=1 to High(LUT) do
    LUT[I]:=Round(Power(I/High(LUT),Gamma)*255);
end;

procedure CreateGammaLUT(Gamma: Single; var LUT: TAverage2GammaLUT); overload;
var
  InvGammaLUT : T8bitToSingleGammaLUT;
  A, B : Integer;
begin
  CreateGammaLUT(1/Gamma,InvGammaLUT);
  Gamma:=1/Gamma;
  for A:=0 to 255 do
    for B:=0 to A do
    begin
      LUT[A,B]:=Round(Power((InvGammaLUT[A]+InvGammaLUT[B])/(255+255),Gamma)*255);
      LUT[B,A]:=LUT[A,B];
    end;
end;

procedure ConvertBitmapToFloatMap(Image: TLinearBitmap; FloatMap: TFloatMap; const Gamma: Double);
var
  I : Integer;
  LUT : T8bitToDoubleGammaLUT;
  Pix : PByte;
begin
  Assert(Image.PixelFormat=pf8bit);
  Assert(Image.BytesPerLine=Image.Width);

  if Gamma=1 then FloatMap.Assign(Image)
  else with FloatMap do
  begin
    CreateGammaLUT(Gamma,LUT);
    New(Image.Width,Image.Height);
    Pix:=@Image.Map^;
    for I:=0 to Size-1 do
    begin
      Map^[I]:=LUT[Pix^];
      Inc(Pix);
    end;
  end;
end;

procedure ConvertBitmapToFloatMap(Image: TLinearBitmap; FloatMap: TFloatMap32; const Gamma: Double);
var
  LUT : T8bitToSingleGammaLUT;

  procedure ProcessLine(Y: Integer);
  var
    X : Integer;
    Src : PByteArray;
    Dst : PSingleArray;
  begin
    Src:=Image.ScanLine[Y];
    Dst:=FloatMap.ScanLine[Y];
    for X:=0 to Image.Width-1 do Dst^[X]:=LUT[Src^[X]];
  end;

begin
  Assert(Image.PixelFormat=pf8bit);
  Assert(Image.BytesPerLine=Image.Width);
  if Gamma=1 then FloatMap.Assign(Image)
  else
  begin
    CreateGammaLUT(Gamma,LUT);
    FloatMap.New(Image.Width,Image.Height);
    ParallelFor(0,Image.Height-1,@ProcessLine);
  end;
end;

procedure ConvertFloatMapToBitmap(FloatMap: TFloatMap; Image: TLinearBitmap; const Gamma: Double);
var
  LUT : T16bitTo8bitGammaLUT;

  procedure ProcessLine(Y: Integer);
  var
    X : Integer;
    Src : PFloatArray;
    Dst : PByteArray;
  begin
    Src:=FloatMap.ScanLine[Y];
    Dst:=Image.ScanLine[Y];
    for X:=0 to Image.Width-1 do
    begin
      if Src^[X]<=0 then Dst^[X]:=0
      else if Src^[X]>=255 then Dst^[X]:=255
      else Dst^[X]:=LUT[Round(Src^[X]*256)];
    end;
  end;

begin
  if Gamma=1 then FloatMap.AssignTo(Image)
  else
  begin
    CreateGammaLUT(Gamma,LUT);
    Image.New(FloatMap.Width,FloatMap.Height,pf8bit);
    ParallelFor(0,Image.Height-1,@ProcessLine);
  end;
end;

procedure ConvertFloatMapToBitmap(FloatMap: TFloatMap32; Image: TLinearBitmap; const Gamma: Double);
var
  LUT : T16bitTo8bitGammaLUT;

  procedure ProcessLine(Y: Integer);
  var
    X : Integer;
    Src : PSingleArray;
    Dst : PByteArray;
  begin
    Src:=FloatMap.ScanLine[Y];
    Dst:=Image.ScanLine[Y];
    for X:=0 to Image.Width-1 do
    begin
      if Src^[X]<=0 then Dst^[X]:=0
      else if Src^[X]>=255 then Dst^[X]:=255
      else Dst^[X]:=LUT[Round(Src^[X]*256)];
    end;
  end;

begin
  if Gamma=1 then FloatMap.AssignTo(Image)
  else with Image do
  begin
    CreateGammaLUT(Gamma,LUT);
    Image.New(FloatMap.Width,FloatMap.Height,pf8bit);
    ParallelFor(0,Image.Height-1,@ProcessLine);
  end;
end;

end.

