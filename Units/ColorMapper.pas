////////////////////////////////////////////////////////////////////////////////
//
// ColorMapper.pas - Color mapping and histogram calculations for TLinearBitmap
// ----------------------------------------------------------------------------
// Version:   2006-02-20
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//
unit ColorMapper;

interface

uses Windows, Classes, SysUtils, Graphics, Streams, LinarBitmap, 
  MathUtils, BitmapConversion;

resourcestring
  rsFileMustContain256Values = 'File must contain 256 values';
  
const
  BluePlane  = 0;
  GreenPlane = 1;
  RedPlane   = 2;
  AllPlanes  = 3;

type
  TColorMapList = packed array[0..255] of Byte;
  TColorMap = object
                Map : TColorMapList;

                procedure SetDirect; // Output = Input
                function IsDirect: Boolean;
                procedure SetNegative(ImageGamma: Single=1); // Output = 255-Input
                procedure SetBrightnessContrast(Brightness,Contrast: Integer); // [-127 : 127]
                procedure SetBrightnessContrastCurve(Brightness,Contrast: Integer; var Curve: array of TPoint); // [-127 : 127]
                procedure SetBlackWhitePoint(Black,White: Integer); // [-255 : 255]
                procedure SetMidtone(Value: Single; Point: Integer=128);
                procedure SetGamma(Gamma: Single);
                procedure SetScale(Scale: Single);
                procedure Threshold(Value: Integer); // [-255;255]
                procedure SetZeroPoint(Point: Integer); // [0;255]
                procedure Quantize(Levels: Integer); // [2;256]
                procedure SmoothCurve(const Points: array of TPoint); // Minimum 5 points
                procedure CatmullRomCurve(const Points: array of TPoint); // Minimum 2 points
                procedure Add(A: Integer);

                // Produce color mapping to first apply Self and then ColorMap2
                procedure Combine(var ColorMap2: TColorMap);

                procedure Apply(Image: TLinearBitmap; Plane: Integer=AllPlanes); overload;
                procedure Apply(Image: TLinearBitmap; Rect: TRect; Plane: Integer=AllPlanes); overload;
                procedure Apply(Palette: PPalette; Plane: Integer=AllPlanes); overload;
                procedure Apply(OrgMap: PByteArray; NewImage: TLinearBitmap; Plane: Integer=AllPlanes); overload;

                procedure SaveToStream(Stream: TBaseStream);
                procedure LoadFromStream(Stream: TBaseStream);
                procedure SaveToFile(const FileName: string);
                procedure LoadFromFile(const FileName: string);
              end;
  PColorMap = ^TColorMap;

  THistogramStat = array[0..255] of DWord;
  THistogram = object
                 Stat : THistogramStat;
                 Count : DWord;

                 procedure Reset;

                 // Find maximum height
                 function Max: DWord;

                 // Calculate histogram
                 procedure Calc(Image: TLinearBitmap; Plane: Integer=AllPlanes); overload;
                 procedure Calc(Image: TLinearBitmap; const Rect: TRect; Plane: Integer=AllPlanes); overload;
                 procedure Append(Image: TLinearBitmap; Plane: Integer=AllPlanes); overload;
                 procedure Append(const Other: THistogram); overload;

                 // Update histogram to follow an image where ColorMap is applied to
                 procedure Update(const ColorMap: TColorMap);

                 // Color statistics
                 procedure GetStat(var MinCol,MaxCol: Integer; var AMean,StdDev: Double);
                 function IntervalSum(A,B: Integer): Integer;
                 function Variance(First: Byte=0; Last: Byte=255): Double;
                 function Mean: Double;
                 function MostUsedColor: Byte;
                 function CountColorsUsed: Integer;


                 // Limit histogram peak height, for use with Contrast Limited Adaptive Histogram Equalization (CLAHE)
                 procedure ClipHistogram(MaxBinCount: DWord);
                 // Equalize histogram, ResultLevels must be 256 so far
                 procedure Equalize(var Map: TColorMap; Levels: Integer=256);
                 // Get equlized value for single color
                 function EqualizeColor(Color: Byte): Byte;

                 // Find the gamma setting that gvies the closest match to an equalized histogram
                 function BestGamma(const MinGamma: Double=0.6; const MaxGamma: Double=2.5): Double;

                 // Auto adjust black and white points
                 procedure GetBlackWhitePoint(PixelsOut: Integer; out Black, White: Integer);

                 function SplitBimodalHistogram(MinVal: Integer=0; MaxVal: Integer=255): Byte;
               end;

function CountColorsUsed(Image: TLinearBitmap): Integer;
function MostUsedColor(Image: TLinearBitmap): TColor;
function ImageMeanColor(Image: TLinearBitmap): TColor;

function ColorDifference(Color1,Color2: TColor): Integer;

// Find the gamma setting that gvies the closest match to an equalized histogram
var AllRect : TRect = (Left:0);
type TColorCorrectionMode = (ccAll,ccRGBSeparated,ccYSeparated);
procedure AutoColorCorrection(Image: TLinearBitmap; AnalyzeRect: TRect; Mode: TColorCorrectionMode=ccAll);
procedure AutoBlackWhitePoint(Image: TLinearBitmap);
procedure ApplyThreshold(Image: TLinearBitmap; Threshold: Integer; Result: TLinearBitmap=nil);
procedure AdjustSaturation(Image: TLinearBitmap; const Saturation: Double; Result: TLinearBitmap=nil);
procedure AdjustSaturationGamma(Image: TLinearBitmap; const Saturation: Double; GammaR: Double=1; GammaG: Double=0; GammaB: Double=0);
procedure AdjustSaturationAndVibrance(Image: TLinearBitmap; const Saturation,Vibrance: Double; Result: TLinearBitmap=nil);
procedure AdjustWhiteBalance(Image: TLinearBitmap; const SelectRect: TRect);

implementation

uses
  {$IFNDEF VER130} Types, {$ENDIF}
  {$IFDEF FloatMapFileSupport} MatrixMath, FloatMap, {$ENDIF}
  MemUtils, Math, Interpolation, MultiCoreProcessing;

function ColorDifference(Color1,Color2: TColor): Integer;
begin
  Result:=Abs(TRGBQuad(Color1).rgbBlue-TRGBQuad(Color2).rgbBlue)+
          Abs(TRGBQuad(Color1).rgbGreen-TRGBQuad(Color2).rgbGreen)+
          Abs(TRGBQuad(Color1).rgbRed-TRGBQuad(Color2).rgbRed);
end;

function CountColorsUsed(Image: TLinearBitmap): Integer;
type
  Pix24Rec = packed record
               A : Word;
               B : Byte;
             end;
var
  I : Integer;
  Pix : ^Pix24Rec;
  P : ^Byte;
  Histogram : THistogram;
  ColorsFound : PByteArray;
begin
  if Image.PixelFormat=pf24bit then
  begin
    GetMem(ColorsFound,256*256*256 div 8);
    try
      ZeroMem(ColorsFound^,256*256*256 div 8);
      Result:=0;
      Pix:=@Image.Map^;
      for I:=1 to Image.Width*Image.Height do
      begin
        P:=@ColorsFound^[(Pix^.A shl 5) or (Pix^.B shr 3)];
        if not Boolean((P^ shr (Pix^.B and 7)) and 1) then
        begin
          P^:=P^ or (1 shl (Pix^.B and 7));
          Inc(Result);
        end;
        Inc(Pix);
      end;
    finally
      FreeMem(ColorsFound);
    end;
  end
  else
  begin
    Histogram.Calc(Image);
    Result:=Histogram.CountColorsUsed;
  end;
end;

function MostUsedColor(Image: TLinearBitmap): TColor;
var
  Histogram : THistogram;
begin
  if (Image.PixelFormat=pf8bit) and Image.IsGrayScale then // 8-bit grayscale
  begin
    Histogram.Calc(Image);
    Result:=Histogram.MostUsedColor;
    Result:=Result or (Result shl 8) or (Result shl 16);
  end
  else if Image.PixelFormat=pf8bit then // 8-bit palette
  begin
    Histogram.Calc(Image);
    with Image.Palette^[Histogram.MostUsedColor] do Result:=RGB2TColor(R,G,B);
  end
  else if Image.PixelFormat=pf24bit then // 24-bit color
  begin
    Histogram.Calc(Image,BluePlane);
    Result:=Histogram.MostUsedColor;
    Histogram.Calc(Image,GreenPlane);
    Result:=Histogram.MostUsedColor or (Result shl 8);
    Histogram.Calc(Image,RedPlane);
    Result:=Histogram.MostUsedColor or (Result shl 8);
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

function ImageMeanColor(Image: TLinearBitmap): TColor;
var
  Histogram : THistogram;
begin
  if (Image.PixelFormat=pf8bit) and Image.IsGrayScale then // 8-bit grayscale
  begin
    Histogram.Calc(Image);
    Result:=Round(Histogram.Mean);
    Result:=Result or (Result shl 8) or (Result shl 16);
  end
  else if Image.PixelFormat=pf8bit then // 8-bit palette
  begin
    Histogram.Calc(Image);
    with Image.Palette^[Histogram.MostUsedColor] do Result:=RGB2TColor(R,G,B);
  end
  else if Image.PixelFormat=pf24bit then // 24-bit color
  begin
    Histogram.Calc(Image,BluePlane);
    Result:=Round(Histogram.Mean);
    Histogram.Calc(Image,GreenPlane);
    Result:=Round(Histogram.Mean) or (Result shl 8);
    Histogram.Calc(Image,RedPlane);
    Result:=Round(Histogram.Mean) or (Result shl 8);
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

procedure AutoColorCorrection(Image: TLinearBitmap; AnalyzeRect: TRect; Mode: TColorCorrectionMode);
var
  Histogram : THistogram;
  ColorMap, GammaMap : TColorMap;
  P, Black, White : Integer;
  Gamma, GammaR, GammaG, GammaB, Saturation : Double;
  Planes : T3PlanesArray;
begin
  if (AnalyzeRect.Right>=Image.Width) then AnalyzeRect.Right:=Image.Width-1;
  if (AnalyzeRect.Bottom>=Image.Height) then AnalyzeRect.Bottom:=Image.Height-1;
  if (AnalyzeRect.Left<0) or (AnalyzeRect.Top<0) or (AnalyzeRect.Right-AnalyzeRect.Left<2) or (AnalyzeRect.Bottom-AnalyzeRect.Top<2) then
    AnalyzeRect:=Rect(0,0,Image.Width-1,Image.Height-1);

  if Image.PixelFormat=pf8bit then // 8 bit grayscale
  begin
    Histogram.Calc(Image,AnalyzeRect);
    Histogram.GetBlackWhitePoint(Histogram.Count div 10000,Black,White);
    ColorMap.SetBlackWhitePoint(Black,White);
    Histogram.Update(ColorMap);
    GammaMap.SetGamma(Histogram.BestGamma);
    ColorMap.Combine(GammaMap);
    ColorMap.Apply(Image);
  end
  else if Mode=ccRGBSeparated then // 24 bit, split planes
  begin  
    for P:=0 to 2 do
    begin
      Histogram.Calc(Image,AnalyzeRect,P);
      Histogram.GetBlackWhitePoint(Histogram.Count div 10000,Black,White);
      ColorMap.SetBlackWhitePoint(Black,White);
      ColorMap.Apply(Image,P);
    end;

    Histogram.Calc(Image,AnalyzeRect,RedPlane);   GammaR:=Histogram.BestGamma;
    Histogram.Calc(Image,AnalyzeRect,GreenPlane); GammaG:=Histogram.BestGamma;
    Histogram.Calc(Image,AnalyzeRect,BluePlane);  GammaB:=Histogram.BestGamma;
    Gamma:=(GammaR+GammaG+GammaB)/3;
    if Gamma<1 then Saturation:=1 else Saturation:=Power(Gamma,1/Gamma);
    AdjustSaturationGamma(Image,Saturation,GammaR,GammaG,GammaB);
  end
  else if Mode=ccYSeparated then // 24 bit, Y plane
  begin
    SplitColorPlanesCreate(Image,Planes,YCbCrTransform);
    try
      Histogram.Calc(Planes[1],AnalyzeRect);
      Histogram.GetBlackWhitePoint(Histogram.Count div 10000,Black,White);
      ColorMap.SetBlackWhitePoint(Black,White);
      Histogram.Update(ColorMap);
      GammaMap.SetGamma(Histogram.BestGamma);
      ColorMap.Combine(GammaMap);
      ColorMap.Apply(Planes[1]);
      CombineColorPlanes(Planes[1],Planes[2],Planes[3],Image,InvYCbCrTransform);
    finally
      FreeColorPlanes(Planes);
    end;
  end
  else // 24 bit, saturation compensation
  begin
    Histogram.Calc(Image,AnalyzeRect);
    Histogram.GetBlackWhitePoint(Histogram.Count div 10000,Black,White);
    if (Black<>0) or (White<>255) then
    begin
      ColorMap.SetBlackWhitePoint(Black,White);
      ColorMap.Apply(Image);
      Histogram.Update(ColorMap);
    end;
    Gamma:=Histogram.BestGamma;
    if Gamma<1 then Saturation:=1
    else Saturation:=Power(Gamma,0.5/Gamma);
    AdjustSaturationGamma(Image,Saturation,Gamma);
  end;
end;

procedure AutoBlackWhitePoint(Image: TLinearBitmap);
var
  Histogram : THistogram;
  ColorMap : TColorMap;
  Black, White : Integer;
begin
  Histogram.Calc(Image);
  Histogram.GetBlackWhitePoint(Histogram.Count div 10000,Black,White);
  ColorMap.SetBlackWhitePoint(Black,White);
  ColorMap.Apply(Image);
end;

procedure AdjustSaturationGamma(Image: TLinearBitmap; const Saturation: Double; GammaR,GammaG,GammaB: Double);
var
  P, Res : Integer;
  Pix : ^RGBRec;
  I, R, G, B : Double;
  GammaMapR, GammaMapG, GammaMapB : array[0..255] of Double;
  ColorMap : TColorMap;
begin
  if Image.PixelFormat=pf24bit then
  begin
    GammaR:=1/GammaR;
    if GammaG=0 then GammaG:=GammaR else GammaG:=1/GammaG;
    if GammaB=0 then GammaB:=GammaR else GammaB:=1/GammaB;
    GammaMapR[0]:=0; GammaMapG[0]:=0; GammaMapB[0]:=0;
    for P:=1 to 255 do
    begin
      GammaMapR[P]:=Power(P/255,GammaR)*255;
      GammaMapG[P]:=Power(P/255,GammaG)*255;
      GammaMapB[P]:=Power(P/255,GammaB)*255;
    end;

    Pix:=@Image.Map^;
    for P:=1 to Image.Size div 3 do
    begin
      R:=GammaMapR[Pix^.R]; G:=GammaMapG[Pix^.G]; B:=GammaMapB[Pix^.B];
      I:=(R*2+G*3+B)/6;
      //I:=R*0.299+G*0.587+B*0.114;
      //I:=(R+B+G)/3;

      Res:=Round(I+(R-I)*Saturation);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.R:=Res;
      Res:=Round(I+(G-I)*Saturation);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.G:=Res;
      Res:=Round(I+(B-I)*Saturation);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.B:=Res;

      Inc(Pix);
    end
  end
  else if (GammaR<>0) and (GammaR<>1) then
  begin
    ColorMap.SetGamma(GammaR);
    ColorMap.Apply(Image);
  end;
end;

procedure AdjustSaturation(Image: TLinearBitmap; const Saturation: Double; Result: TLinearBitmap);
var
  P, Res : Integer;
  Pix : PRGBRec;
  I : Double;
begin
  if Image.PixelFormat=pf24bit then
  begin
    if Result=nil then Result:=Image
    else Result.Assign(Image);
    Pix:=@Result.Map^;
    for P:=1 to Result.Size div 3 do
    begin
      I:=(Pix^.R*2+Pix^.G*3+Pix^.B)/6;
      Res:=Round(I+(Pix^.R-I)*Saturation);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.R:=Res;
      Res:=Round(I+(Pix^.G-I)*Saturation);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.G:=Res;
      Res:=Round(I+(Pix^.B-I)*Saturation);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.B:=Res;
      Inc(Pix);
    end
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

procedure AdjustSaturationAndVibrance(Image: TLinearBitmap; const Saturation,Vibrance: Double; Result: TLinearBitmap);
var
  P, Res : Integer;
  Pix : PRGBRec;
  I, V, MaxR, MaxG, MaxB : Double;
begin
  if Vibrance<=1 then AdjustSaturation(Image,Saturation*Vibrance,Result)
  else if Image.PixelFormat=pf24bit then
  begin
    if Result=nil then Result:=Image
    else Result.Assign(Image);
    Pix:=@Result.Map^;
    for P:=1 to Result.Size div 3 do
    begin
      I:=(Pix^.R*2+Pix^.G*3+Pix^.B)/6;

      if Pix^.R<I then MaxR:=(0-I)/(Pix^.R-I)
      else if Pix^.R>I then MaxR:=(255-I)/(Pix^.R-I)
      else MaxR:=2;
      Assert(MaxR>=1);

      if Pix^.G<I then MaxG:=(0-I)/(Pix^.G-I)
      else if Pix^.G>I then MaxG:=(255-I)/(Pix^.G-I)
      else MaxG:=2;
      Assert(MaxG>=1);

      if Pix^.B<I then MaxB:=(0-I)/(Pix^.B-I)
      else if Pix^.B>I then MaxB:=(255-I)/(Pix^.B-I)
      else MaxB:=2;
      Assert(MaxB>=1);

      V:=MaxR;
      if MaxG<V then V:=MaxG;
      if MaxB<V then V:=MaxB;

      if V<2 then V:=(Vibrance-1)*(V-1)+1
      else V:=Vibrance;

      V:=Saturation*V;

      Res:=Round(I+(Pix^.R-I)*V);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.R:=Res;
      Res:=Round(I+(Pix^.G-I)*V);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.G:=Res;
      Res:=Round(I+(Pix^.B-I)*V);
      if Res<0 then Res:=0 else if Res>255 then Res:=255;
      Pix^.B:=Res;

      Inc(Pix);
    end
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

// Set all values above threshold to 255 and values below or equal to 0
procedure ApplyThreshold(Image: TLinearBitmap; Threshold: Integer; Result: TLinearBitmap);
var
  Pix : PByte;
  X, Y : Integer;
begin
  if (Result=nil) or (Result=Image) then Result:=Image
  else Result.Assign(Image);
  if Image.MustUpdateScanLine then
  begin
    for Y:=0 to Result.Height-1 do
    begin
      Pix:=Result.ScanLineSafe[Y];
      for X:=1 to Result.BytesPerLine do
      begin
        if Pix^>Threshold then Pix^:=255
        else Pix^:=0;
        Inc(Pix);
      end;
    end;
  end
  else
  begin
    Pix:=@Result.Map^;
    for X:=1 to Result.Size do
    begin
      if Pix^>Threshold then Pix^:=255
      else Pix^:=0;
      Inc(Pix);
    end;
  end;
end;

procedure AdjustWhiteBalance(Image: TLinearBitmap; const SelectRect: TRect);
var
  Histogram : THistogram;
  ColorMap : TColorMap;
  Tone : array[BluePlane..RedPlane] of Double;
  MaxTone : Double;
  Plane : Integer;
begin
  if Image.PixelFormat<>pf24bit then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  for Plane:=BluePlane to RedPlane do
  begin
    Histogram.Calc(Image,SelectRect,Plane);
    Tone[Plane]:=Histogram.Mean;
  end;
  MaxTone:=Max(Max(Tone[BluePlane],Tone[GreenPlane]),Tone[RedPlane]);
  for Plane:=BluePlane to RedPlane do
  begin
    ColorMap.SetBlackWhitePoint(0,Round(Tone[Plane]/MaxTone*255));
    ColorMap.Apply(Image,Plane);
  end;
end;

//==============================================================================================================================
// THistogram
//==============================================================================================================================

procedure THistogram.Reset;
begin
  Count:=0;
  ZeroMem(Stat,SizeOf(Stat));
end;

procedure THistogram.Append(Image: TLinearBitmap; Plane: Integer);
var
  Size, Planes, P, Y : Integer;
  Pix : ^Byte;
begin
  if Plane=AllPlanes then
  begin
    Plane:=0;
    Planes:=1;
  end
  else if Image.PixelFormat=pf24bit then Planes:=3
  else Planes:=1;
  Size:=Image.Size div Planes;
  Inc(Count,Size);
  if Image.MustUpdateScanLine then
  begin
    Size:=Image.BytesPerLine div Planes;
    for Y:=0 to Image.Height-1 do
    begin
      Pix:=@PByteArray(Image.ScanLineSafe[Y])^[Plane];
      for P:=1 to Size do
      begin
        Inc(Stat[Pix^]);
        Inc(Pix,Planes);
      end;
    end;
  end
  else
  begin
    Pix:=@Image.Map^[Plane];
    for P:=1 to Size do
    begin
      Inc(Stat[Pix^]);
      Inc(Pix,Planes);
    end;
  end;
end;

procedure THistogram.Append(const Other: THistogram);
var
  I : Integer;
begin
  Inc(Count,Other.Count);
  for I:=0 to 255 do Inc(Stat[I],Other.Stat[I]);
end;

procedure THistogram.Calc(Image: TLinearBitmap; Plane: Integer);
begin
  Reset;
  Append(Image,Plane);
end;

procedure THistogram.Calc(Image: TLinearBitmap; const Rect: TRect; Plane: Integer=AllPlanes);
var
  Planes, X, Y, LineStart, LineCount : Integer;
  Pix : ^Byte;
begin
  if (Plane=AllPlanes) or (Image.PixelFormat=pf8bit) then
  begin
    Planes:=1;
    if Image.PixelFormat=pf24bit then
    begin
      LineStart:=Rect.Left*3;
      LineCount:=(Rect.Right-Rect.Left+1)*3;
    end
    else // 8 bit
    begin
      LineStart:=Rect.Left;
      LineCount:=Rect.Right-Rect.Left+1;
    end;
  end
  else // 24 bit, one plane
  begin
    Planes:=3;
    LineStart:=Plane+Rect.Left*3;
    LineCount:=Rect.Right-Rect.Left+1;
  end;
  ZeroMem(Stat,SizeOf(Stat));
  for Y:=Rect.Top to Rect.Bottom do
  begin
    Pix:=@Image.Map^[Y*Image.BytesPerLine+LineStart];
    for X:=1 to LineCount do
    begin
      Inc(Stat[Pix^]);
      Inc(Pix,Planes);
    end;
  end;
  Count:=LineCount*(Rect.Bottom-Rect.Top+1);
end;

procedure THistogram.Update(const ColorMap: TColorMap);
var
  OldStat : THistogramStat;
  Col : Integer;
begin
  OldStat:=Stat;
  ZeroMem(Stat,SizeOf(Stat));
  for Col:=0 to 255 do Inc(Stat[ColorMap.Map[Col]],OldStat[Col]);
end;

// Find maximum height
function THistogram.Max: DWord;
var Col : Integer;
begin
  Result:=Stat[0];
  for Col:=1 to 255 do if Stat[Col]>Result then Result:=Stat[Col];
end;

procedure THistogram.GetStat(var MinCol,MaxCol: Integer; var AMean,StdDev: Double);
var
  Col : Integer;
  Sum, SqrSum : Int64;
begin
  MinCol:=255;
  MaxCol:=0;
  Sum:=0;
  SqrSum:=0;
  for Col:=0 to 255 do
  begin
    if (Col>MaxCol) and (Stat[Col]>0) then MaxCol:=Col;
    if (Col<MinCol) and (Stat[Col]>0) then MinCol:=Col;
    Inc(Sum,Int64(Stat[Col])*Col);
    Inc(SqrSum,Int64(Stat[Col])*Sqr(Col));
  end;
  if Count>0 then
  begin
    AMean:=Sum/Count;
    StdDev:=Sqrt(SqrSum/Count-Sqr(AMean));
  end
  else
  begin
    AMean:=0;
    StdDev:=0;
  end;
end;

function THistogram.IntervalSum(A,B: Integer): Integer;
begin
  Result:=0;
  for A:=A to B do Inc(Result,Stat[A]);
end;

function THistogram.Mean: Double;
var
  C : Integer;
  StdDev : Double;
begin
  GetStat(C,C,Result,StdDev);
end;

function THistogram.Variance(First,Last: Byte): Double;
var
  Col, PixCount : DWord;
  Sum, SqrSum : Int64;
begin
  Sum:=0; SqrSum:=0; PixCount:=0;
  for Col:=First to Last do
  begin
    Inc(PixCount,Stat[Col]);
    Inc(Sum,Stat[Col]*Col);
    Inc(SqrSum,Int64(Stat[Col])*Sqr(Col));
  end;
  if PixCount>0 then Variance:=SqrSum/PixCount-Sqr(Sum/PixCount)
  else Variance:=1e50;
end;

function THistogram.SplitBimodalHistogram(MinVal,MaxVal: Integer): Byte;
var
  Col : Integer;
  CurVar, BestVar : Double;
begin
  BestVar:=Infinity; Result:=127;
  for Col:=MinVal+1 to MaxVal-1 do
  begin
    CurVar:=Variance(MinVal,Col)+Variance(Col,MaxVal);
    if CurVar<BestVar then
    begin
      BestVar:=CurVar;
      Result:=Col;
    end;
  end;
end;

procedure THistogram.Equalize(var Map: TColorMap; Levels: Integer);
var
  Col, Sum, Offset, L : Integer;
  H : Double;
begin
  if Count=0 then Map.SetDirect
  else
  begin
    Offset:=Round(Stat[0]/Count*255);
    if Offset=255 then Offset:=127;
    Sum:=Stat[0];
    for Col:=1 to 254 do
    begin
      Inc(Sum,Stat[Col]);
      H:=(Sum*255.0/Count-Offset)/(255-Offset)*255; // Equalize
      L:=Trunc(H*Levels/256)*256 div (Levels-1); // Quantize
      if L>255 then L:=255;
      Map.Map[Col]:=L;
    end;
    Map.Map[0]:=0;
    Map.Map[255]:=255;
  end;
end;

function THistogram.BestGamma(const MinGamma,MaxGamma: Double): Double;
const
  GammaSteps = 512;
var
  EqualMap, GammaMap : TColorMap;
  I, C, Diff, BestDiff : Integer;
  Step, Gamma : Double;
begin
  BestDiff:=High(BestDiff); Result:=1;
  Gamma:=MinGamma;
  Step:=(MaxGamma-MinGamma)/(GammaSteps-1);

  Equalize(EqualMap);
  for I:=1 to GammaSteps do
  begin
    GammaMap.SetGamma(Gamma);
    Diff:=0;
    for C:=0 to 255 do Inc(Diff,Sqr(Integer(EqualMap.Map[C])-GammaMap.Map[C]));
    if Diff<BestDiff then
    begin
      BestDiff:=Diff;
      Result:=Gamma;
    end;
    Gamma:=Gamma+Step;
  end;
end;

// https://github.com/erich666/GraphicsGems/blob/master/gemsiv/clahe.c
procedure THistogram.ClipHistogram(MaxBinCount: DWord);
var
  BinIndex, Start : Integer;
  Excess, Upper, BinIncr, StepSize : DWord;
  I, BinExcess : Integer;
begin
  if MaxBinCount>=Count then Exit;
  if MaxBinCount*256<=Count then
  begin
    for I:=0 to 255 do Stat[I]:=MaxBinCount;
    Count:=MaxBinCount*256;
    Exit;
  end;

  Excess:=0;
  for I:=0 to 255 do  // Calculate total number of excess pixels
  begin
    BinExcess:=Stat[I]-MaxBinCount;
    if BinExcess>0 then Inc(Excess,BinExcess); // Excess in current bin
  end;

  // Second part: clip histogram and redistribute excess pixels in each bin
  BinIncr:=Excess div 256; // Average binincrement
  Upper:=MaxBinCount-BinIncr;

  for I:=0 to 255 do
    if Stat[I]>MaxBinCount then Stat[I]:=MaxBinCount // Clip bin
    else if Stat[I]>Upper then // High bin count
    begin
      Dec(Excess,MaxBinCount-Stat[I]);
      Stat[I]:=MaxBinCount;
    end
    else // Low bin count
    begin
      Dec(Excess,BinIncr);
      Inc(Stat[I],BinIncr);
    end;

  while Excess>0 do // Redistribute remaining excess
  begin
    Start:=0;
    while (Excess>0) and (Start<256) do
    begin
	    StepSize:=MaxBinCount div Excess;
	    if StepSize=0 then StepSize:=1;	// StepSize at least 1
	    BinIndex:=Start;
      while (BinIndex<256) and (Excess>0) do
      begin
  		  if Stat[BinIndex]<MaxBinCount then
        begin
  		    Inc(Stat[BinIndex]);
          Dec(Excess);// Reduce excess
        end;
  		  Inc(BinIndex,StepSize);
      end;
	    Inc(Start);	// Restart redistributing on other bin location
  	end;
  end;
end;

function THistogram.EqualizeColor(Color: Byte): Byte;
var
  Col, Sum : DWord;
  InvCount, Offset : Double;
begin
  if Stat[Color]=Count then Result:=Color // All pixels have same color
  else
  begin
    Sum:=0; for Col:=0 to Color do Inc(Sum,Stat[Col]);
    //Result:=Round((Sum-Stat[0])/(Count-Stat[0])*255); // Slower for some reason
    InvCount:=1/Count;
    Offset:=Stat[0]*InvCount;
    Result:=Round((Sum*InvCount-Offset)/(1-Offset)*255);{}
    //Result:=255*(Sum-Stat[0]) div (Count-Stat[0]); // Faster, but less accurate
    //Result:=Sum*255 div Count; // Faster, but less accurate
  end;
end;

function THistogram.CountColorsUsed: Integer;
var
  Col : Integer;
begin
  Result:=0;
  for Col:=0 to 255 do if Stat[Col]<>0 then Inc(Result);
end;

function THistogram.MostUsedColor: Byte;
var
  Col, Best : DWord;
begin
  Best:=Stat[0];
  Result:=0;
  for Col:=1 to 255 do if Stat[Col]>Best then
  begin
    Best:=Stat[Col];
    Result:=Col;
  end;
end;

procedure THistogram.GetBlackWhitePoint(PixelsOut: Integer; out Black, White: Integer);
var
  C, Sum : Integer;
begin
  Black:=0;
  Sum:=0;
  for C:=0 to 254 do
  begin
    Inc(Sum,Stat[C]);
    if Sum>PixelsOut then
    begin
      Black:=C;
      Break;
    end;
  end;
  White:=255;
  Sum:=0;
  for C:=255 downto Black+1 do
  begin
    Inc(Sum,Stat[C]);
    if Sum>PixelsOut then
    begin
      White:=C;
      Break;
    end;
  end;
end;

//==============================================================================================================================
// TColorMap
//==============================================================================================================================

procedure TColorMap.SetDirect;
var Col : Byte;
begin
  for Col:=0 to 255 do Map[Col]:=Col;
end;

function TColorMap.IsDirect: Boolean;
var I : Integer;
begin
  Result:=True;
  for I:=0 to 255 do if Map[I]<>I then
  begin
    Result:=False;
    Exit;
  end;
end;

procedure TColorMap.Add(A: Integer);
var Col : Byte;
begin
  for Col:=0 to 255 do Map[Col]:=ForceInRange(Col+A,0,255);
end;

procedure TColorMap.SetNegative(ImageGamma: Single);
var
  Col : Byte;
  InvImageGamma : Single;
begin
  if ImageGamma=1 then for Col:=0 to 255 do Map[Col]:=255-Col
  else
  begin
    InvImageGamma:=1/ImageGamma;
    for Col:=0 to 255 do Map[Col]:=Round(Power(1-Power(Col/255,ImageGamma),InvImageGamma)*255);
  end;
end;

procedure TColorMap.SetBrightnessContrast(Brightness,Contrast: Integer);
var
  Col : Integer;
  B, C, P : Double;
begin
  B:=1+Brightness/100;
  C:=1+Contrast/100;
  Map[0]:=0;
  for Col:=1 to 254 do
  begin
    P:=Col/255;
    // Brightness
    P:=1-Power((1-P),B);
    // Contrast
    if P<=0.5 then P:=Power(P*2,C)*0.5
    else P:=1-Power((1-P)*2,C)*0.5;
    Map[Col]:=Round(P*255);
  end;
  Map[255]:=255;
end;

procedure TColorMap.SetBrightnessContrastCurve(Brightness,Contrast: Integer; var Curve: array of TPoint);
const
  ContrastHeight = 40;
  ExtraContrastStart = 74;
var
  ExtraContrast : Integer;
begin
  Assert(Length(Curve)=5);
  if Contrast>ExtraContrastStart then
  begin
    Curve[0].X:=0;
    Curve[1].X:=EnsureRange(ContrastHeight+ExtraContrastStart-Brightness div 2,0,127-Brightness);
    Curve[2].X:=127-Brightness;
    Curve[3].X:=EnsureRange(255-ContrastHeight-ExtraContrastStart-Brightness div 2,127-Brightness,255);
    Curve[4].X:=255;

    ExtraContrast:=(Contrast-ExtraContrastStart)*2;
    Curve[0].Y:=0;
    Curve[1].Y:=ContrastHeight-ExtraContrast;
    Curve[2].Y:=127;
    Curve[3].Y:=255-ContrastHeight+ExtraContrast;
    Curve[4].Y:=255;
  end
  else if Contrast>=0 then
  begin
    Curve[0].X:=0;
    Curve[1].X:=EnsureRange(ContrastHeight+Contrast-Brightness div 2,0,127-Brightness);
    Curve[2].X:=127-Brightness;
    Curve[3].X:=EnsureRange(255-ContrastHeight-Contrast-Brightness div 2,127-Brightness,255);
    Curve[4].X:=255;

    Curve[0].Y:=0;
    Curve[1].Y:=ContrastHeight;
    Curve[2].Y:=127;
    Curve[3].Y:=255-ContrastHeight;
    Curve[4].Y:=255;
  end
  else // Contrast<0
  begin
    Curve[0].X:=0;
    Curve[1].X:=EnsureRange(ContrastHeight+Contrast-Brightness div 2,0,127-Brightness);
    Curve[2].X:=127-Brightness;
    Curve[3].X:=EnsureRange(255-ContrastHeight-Contrast-Brightness div 2,127-Brightness,255);
    Curve[4].X:=255;

    Curve[0].Y:=0;
    Curve[1].Y:=ContrastHeight;
    Curve[2].Y:=127;
    Curve[3].Y:=255-ContrastHeight;
    Curve[4].Y:=255;
  end;
  SmoothCurve(Curve);
end;

procedure TColorMap.SetBlackWhitePoint(Black,White: Integer); // [-255 : 255]
var
  Col, First, Dist : Integer;
begin
  First:=Black;
  Dist:=White-Black;
  if Black>0 then ZeroMem(Map[0],Black)
  else Black:=0;
  if White<255 then FillChar(Map[White],256-White,255)
  else White:=255;
  if Dist>0 then for Col:=Black to White do Map[Col]:=Round((Col-First)*255/Dist);
end;

procedure TColorMap.SetMidtone(Value: Single; Point: Integer);
var
  I : Integer;
begin
  for I:=0 to Point do
    Map[I]:=Round(EnsureRange(LinearInterpolate(0,Value,I/Point),0,255));
  for I:=Point+1 to 255 do
    Map[I]:=Round(EnsureRange(LinearInterpolate(Value,255,I,Point,255),0,255));
end;

procedure TColorMap.SetGamma(Gamma: Single);
var
  Col : Byte;
  A   : Integer;
begin
  Gamma:=1/Gamma;
  Map[0]:=0;
  for Col:=1 to 255 do
  begin
    A:=Round(Power(Col/255,Gamma)*255);
    if A<=0 then Map[Col]:=0
    else if A>=255 then Map[Col]:=255
    else Map[Col]:=A;
  end;
end;

procedure TColorMap.SetScale(Scale: Single);
var
  Col : Byte;
begin
  for Col:=1 to 255 do Map[Col]:=EnsureRange(Round(Col*Scale),0,255);
end;

procedure TColorMap.Threshold(Value: Integer);
var
  Col : Byte;
begin
  if Value>=0 then for Col:=0 to 255 do Map[Col]:=Byte(Col<=Value)-1
  else for Col:=0 to 255 do Map[Col]:=Byte(Col>-Value)-1;
end;

procedure TColorMap.SetZeroPoint(Point: Integer);
var
  Col : Byte;
begin
  if Point>0 then for Col:=0 to Point do Map[Col]:=Round(255-Col/Point*255);
  if Point<255 then for Col:=Point to 255 do Map[Col]:=Round((Col-Point)/(255-Point)*255);
end;

procedure TColorMap.Quantize(Levels: Integer);
var
  Col, L : Integer;
begin
  for Col:=0 to 255 do
  begin
    L:=Trunc(Col*Levels/256)*256 div (Levels-1);
    if L>255 then L:=255;
    Map[Col]:=L;
  end;
end;

procedure TColorMap.SmoothCurve(const Points: array of TPoint);

  const
    LineSegs = 384;

  type
    TBlend = array[0..3,1..LineSegs] of Double;

  var
    FirstBlend, Blend, LastBlend : TBlend;
    SM : array[0..3] of TPoint;

  procedure InitBlend;
  var
   U, V, W : Double;
   I       : Integer;
  begin
    for I:=1 to LineSegs do
    begin
      U:=I/LineSegs; V:=U-1; W:=U+1;
      Blend[0,I]:=-U*V*(U-2)/6;
      Blend[1,I]:= W*V*(U-2)/2;
      Blend[2,I]:=-W*U*(U-2)/2;
      Blend[3,I]:= W*U*V/6;
      FirstBlend[0,I]:=-V*(V-1)*(V-2)/6;
      FirstBlend[1,I]:= U*(V-1)*(V-2)/2;
      FirstBlend[2,I]:=-U*V*(V-2)/2;
      FirstBlend[3,I]:= U*V*(V-1)/6;
      LastBlend[0,I]:=-W*U*(W-2)/6;
      LastBlend[1,I]:= (W+1)*U*(W-2)/2;
      LastBlend[2,I]:=-(W+1)*W*(W-2)/2;
      LastBlend[3,I]:= (W+1)*W*U/6;
    end;
  end;

  procedure DrawSegs(var B: TBlend);
  var
    RoundX, I, J : Integer;
    X, Y : Double;
  begin
    for I:=1 to LineSegs do
    begin
      X:=0; Y:=0;
      for J:=0 to 3 do
      begin
        X:=X+SM[J].X*B[J,I];
        Y:=Y+SM[J].Y*B[J,I];
      end;
      RoundX:=Round(X);
      if RoundX in [0..255] then Map[RoundX]:=ForceInRange(Round(Y),0,255);
    end;
  end;

  procedure NextSection;
  var
    I : Integer;
  begin
    for I:=0 to 2 do SM[I]:=SM[I+1];
  end;

var
  I : Integer;
begin
  Assert(Length(Points)>=5);
  InitBlend;
  for I:=0 to 3 do SM[I]:=Points[I];
  DrawSegs(FirstBlend);
  DrawSegs(Blend);
  NextSection;
  for I:=4 to High(Points)-1 do
  begin
    SM[3]:=Points[I];
    DrawSegs(Blend);
    NextSection
  end;
  SM[3]:=Points[High(Points)];
  DrawSegs(Blend);
  DrawSegs(LastBlend);
  if Points[0].X in [0..255] then Map[Points[0].X]:=Points[0].Y;
  if Points[High(Points)].X in [0..255] then Map[Points[High(Points)].X]:=Points[High(Points)].Y;
end;

procedure TColorMap.CatmullRomCurve(const Points: array of TPoint);
var
  P0, P1, P2, P3 : TPoint;
  PrevPos, Pos : record
                   X : Integer;
                   Y : Double;
                 end;  
  I, J, X, D : Integer;
  T : Double;
begin
  Assert(Length(Points)>=2);
  Map[Points[0].X]:=Points[0].Y;
  PrevPos.X:=Points[0].X;
  PrevPos.Y:=Points[0].Y;
  for I:=1 to High(Points) do
  begin
    D:=Max(1,(Points[I].X-Points[I-1].X) div 2);
    P0:=Points[Max(0,I-2)];
    P1:=Points[Max(0,I-1)];
    P2:=Points[I];
    P3:=Points[Min(High(Points),I+1)];
    for J:=0 to D do
    begin
      T:=J/D;
      Pos.X:=EnsureRange(Round(CatmullRomPoly(P0.X,P1.X,P2.X,P3.X,T)),0,255);
      Pos.Y:=EnsureRange(CatmullRomPoly(P0.Y,P1.Y,P2.Y,P3.Y,T),0,255);
      for X:=PrevPos.X+1 to Pos.X do
        Map[X]:=Round(PrevPos.Y+(Pos.Y-PrevPos.Y)*(X-PrevPos.X)/(Pos.X-PrevPos.X));
      PrevPos:=Pos;
    end;
    Map[Points[I].X]:=Points[I].Y;
  end;
end;

procedure TColorMap.Combine(var ColorMap2: TColorMap);
var
  Col : Integer;
  NewMap : TColorMapList;
begin
  for Col:=0 to 255 do NewMap[Col]:=ColorMap2.Map[Map[Col]];
  Map:=NewMap;
end;

procedure TColorMap.Apply(Image: TLinearBitmap; Plane: Integer);
var
  Planes, P : Integer;
  Pix : ^Byte;

  procedure ProcessLine(Y: Integer);
  var
    P : Integer;
    Pix : ^Byte;
  begin
    Pix:=Image.ScanLine[Y];
    for P:=0 to Image.BytesPerLine-1 do
    begin
      Pix^:=Map[Pix^];
      Inc(Pix);
    end;
  end;

begin
  if (Image.PixelFormat=pf24bit) and (Plane<>AllPlanes) then Planes:=3
  else if Image.Size>2*1024*1024 then
  begin
    ParallelFor(0,Image.Height-1,@ProcessLine);
    Exit;
  end
  else
  begin
    Plane:=0;
    Planes:=1;
  end;
  Pix:=@Image.Map^[Plane];
  for P:=1 to Image.Size div Planes do
  begin
    Pix^:=Map[Pix^];
    Inc(Pix,Planes);
  end;
end;

procedure TColorMap.Apply(Image: TLinearBitmap; Rect: TRect; Plane: Integer);
var
  Planes, X, Y : Integer;
  Pix : ^Byte;
begin
  Rect.Right:=Rect.Right-Rect.Left+1;
  if Plane=AllPlanes then
  begin
    Rect.Left:=Rect.Left*Image.PixelSize;
    Rect.Right:=Rect.Right*Image.PixelSize;
    Planes:=1;
  end
  else if Image.PixelFormat=pf8bit then Planes:=1
  else
  begin
    Planes:=Image.PixelSize;
    Rect.Left:=Rect.Left*Image.PixelSize+Plane;
  end;

  for Y:=Rect.Top to Rect.Bottom do
  begin
    Pix:=@PByteArray(Image.ScanLine[Y])^[Rect.Left];
    for X:=1 to Rect.Right do
    begin
      Pix^:=Map[Pix^];
      Inc(Pix,Planes);
    end;
  end;
end;

procedure TColorMap.Apply(Palette: PPalette; Plane: Integer);
var
  Planes, P : Integer;
  Pix : ^Byte;
begin
  if Plane=AllPlanes then
  begin
    Plane:=0;
    Planes:=1;
    P:=768;
  end
  else
  begin
    Planes:=3;
    P:=256;
  end;
  Pix:=@Palette^[Plane];
  for P:=1 to P do
  begin
    Pix^:=Map[Pix^];
    Inc(Pix,Planes);
  end;
end;

procedure TColorMap.Apply(OrgMap: PByteArray; NewImage: TLinearBitmap; Plane: Integer);
var
  Planes, P : Integer;
  Pix, NewPix : ^Byte;
begin
  if Plane=AllPlanes then
  begin
    Plane:=0;
    Planes:=1;
  end
  else if NewImage.PixelFormat=pf24bit then Planes:=3
  else Planes:=1;

  Pix:=@OrgMap^[Plane];
  NewPix:=@NewImage.Map^[Plane];
  for P:=1 to NewImage.Size div Planes do
  begin
    NewPix^:=Map[Pix^];
    Inc(Pix,Planes);
    Inc(NewPix,Planes);
  end;
end;

procedure TColorMap.SaveToStream(Stream: TBaseStream);
begin
  Stream.Write(Map,SizeOf(Map));
end;

procedure TColorMap.LoadFromStream(Stream: TBaseStream);
begin
  Stream.Read(Map,SizeOf(Map));
end;

procedure TColorMap.SaveToFile(const FileName: string);
var
  Stream : TFileStream;
begin
  Stream:=TFileStream.Create(FileName,fsRewrite);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TColorMap.LoadFromFile(const FileName: string);
var
  Stream : TFileStream;
  {$IFDEF FloatMapFileSupport}
  Vector : TVector;
  I : Integer;
  {$ENDIF}
begin
  {$IFDEF FloatMapFileSupport}
  if Pos('*.'+ExtractFileExtNoDotUpper(FileName),UpperCase(rsMapLoadFilter))>0 then
  begin
    Vector:=TVector.Create;
    try
      Vector.LoadFromFile(FileName);
      if Vector.Length<>256 then raise Exception.Create(rsFileMustContain256Values);
      if Vector.Max<=1 then Vector.Multiply(255);
      for I:=0 to 255 do Map[I]:=EnsureRange(Round(Vector.Map^[I]),0,255);
    finally
      Vector.Free;
    end
  end
  else
  {$ENDIF}
  begin
    Stream:=TFileStream.Create(FileName,[fsRead,fsShareRead]);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

end.

