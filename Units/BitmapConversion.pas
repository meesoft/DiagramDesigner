////////////////////////////////////////////////////////////////////////////////
//
// BitmapConversion.pas - Conversion utilities for TLinearBitmap
// -------------------------------------------------------------
// Version:   2005-10-02
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
unit BitmapConversion;

interface

uses SysUtils, Windows, Graphics, MemUtils, LinarBitmap, Math, ExpressionEval,
  MatrixMath, MathUtils, SortUtils, FloatMap;

const
  BluePlane  = 0;
  GreenPlane = 1;
  RedPlane   = 2;
  AllPlanes  = 3;

const
  RGBTransform : TMatrix4x4      = ((1,0,0,0),
                                    (0,1,0,0),
                                    (0,0,1,0),
                                    (0,0,0,1));

  CMYTransform : TMatrix4x4      = (( -1,  0,  0,255),
                                    (  0, -1,  0,255),
                                    (  0,  0, -1,255),
                                    (  0,  0,  0,  1));


// Color spaces FAQ - David Bourgin
// Date: 28/9/94
//  Y=  0.2989*Red+0.5866*Green+0.1145*Blue	| Red=  Y+0.0000*Cb+1.4022*Cr
//  Cb=-0.1687*Red-0.3312*Green+0.5000*Blue	| Green=Y-0.3456*Cb-0.7145*Cr
//  Cr= 0.5000*Red-0.4183*Green-0.0816*Blue	| Blue= Y+1.7710*Cb+0.0000*Cr
  YCbCrTransform601 : TMatrix4x4    = (( 0.2989, 0.5866, 0.1145,  0  ),     // Rec 601-1 specs (whatever that is)
                                       (-0.1687,-0.3312, 0.5   ,127.5),
                                       ( 0.5   ,-0.4183,-0.0816,127.5),
                                       ( 0     , 0,      0     ,  1  ));

  YCbCrTransform : TMatrix4x4    = (( 0.299  , 0.587  , 0.114  ,  0  ),  // JPEG/JPEG 2000 standard transform (I think)
                                    (-0.16875,-0.33126, 0.5    ,127.5),
                                    ( 0.5    ,-0.41869,-0.08131,127.5),
                                    ( 0      , 0,       0      ,  1  ));

  YCbCrTransformCompressed : TMatrix4x4 = (( 65.481/255, 128.553/255,  24.966/255, 16), // From Matlab
                                           (-37.797/255,- 74.203/255, 112.000/255,128),
                                           (112.000/255,- 93.786/255,- 18.214/255,128),
                                           (          0,           0,           0,  1));

  YIQTransform : TMatrix4x4      = (( 0.299, 0.587, 0.114,    0),
                                    ( 0.569,-0.275,-0.321,127.5),
                                    ( 0.212,-0.523, 0.311,127.5),
                                    (     0,     0,     0,    1));

var
  InvYCbCrTransform, InvYIQTransform : TMatrix4x4;

type
  T3PlanesArray = array[1..3] of TLinearBitmap;
  T4PlanesArray = array[1..4] of TLinearBitmap;

// Set every second pixel to 255
procedure Pattern2x2(Image: TLinarBitmap);

// Make palette from the 256 most frequently used colors
procedure OptimizePalette(Image: TLinarBitmap; out NewPal: TPalette; MaxSearchColors: Integer);

// Convert 8 or 24 bit image to 8 bit grayscale
procedure ConvertToGrayscale(const Image,GrayImage: TLinarBitmap; ImageGamma: Single=1); overload;
procedure ConvertToGrayscale(const Image: TLinarBitmap; ImageGamma: Single=1); overload;

// Convert image to 256 color palette
// If a palette is not specified, it is constructed from the first 256 colors found in the image
procedure MakePaletteImage(Image: TLinarBitmap; Palette: PPalette=nil);

// |X|     |A|
// |Y| = T·|B|   (bottom row of T must be [0 0 0 1])
// |Z|     |C|
// |0|     |1|
procedure ColorTransform(A,B,C: Byte; out X,Y,Z: Byte; const T: TMatrix4x4); overload;
procedure ColorTransform(A,B,C: Byte; out X,Y,Z: Single; const T: TMatrix4x4); overload;
procedure ColorTransform(A,B,C: Byte; out X,Y,Z: Double; const T: TMatrix4x4); overload;
procedure ColorTransform(const A,B,C: Double; out X,Y,Z: Byte; const T: TMatrix4x4); overload;
procedure ColorTransform(A,B,C: Float; out X,Y,Z: Single; const T: TMatrix4x4); overload;
procedure ColorTransform(A,B,C: Float; out X,Y,Z: Double; const T: TMatrix4x4); overload;

type TColorTransformProc = procedure(R,G,B: Byte; out V0,V1,V2: Byte);

procedure ColorTransformHSI2RGB(H,S,I: Byte; out R,G,B: Byte);
procedure ColorTransformRGB2HSI(R,G,B: Byte; out H,S,I: Byte);
procedure ColorTransformRGB2Lab(R,G,B: Byte; out L,a_,b_: Byte);
procedure ColorTransformLab2RGB(L,a_,b_: Byte; out R,G,B: Byte);
procedure ColorTransformRGB2LOCO(R,G,B: Byte; out S0,S1,S2: Byte); // Used in MNG
procedure ColorTransformLOCO2RGB(S0,S1,S2: Byte; out R,G,B: Byte);

function GetPCATransform(const Image: TLinarBitmap; AutoStretch: Boolean=False;
                         C1Var: PDouble=nil; C2Var: PDouble=nil; C3Var: PDouble=nil;
                         ShowResult: Boolean=False): TMatrix4x4Obj;
function GetDecorrelationStretchTransform(Image: TLinearBitmap; AllowComponentOffset: Boolean): TMatrix4x4Obj;

// Split color planes
procedure SplitColorPlanes(Image,Plane1,Plane2,Plane3: TLinarBitmap; const T: TMatrix4x4); overload;
procedure SplitColorPlanes(Image,Plane1,Plane2,Plane3: TLinarBitmap; T: TColorTransformProc); overload;
procedure SplitColorPlanesRGB(Image,RedPlane,GreenPlane,BluePlane: TLinarBitmap);
procedure SplitColorPlanesCMYK(Image,CyanPlane,MagentaPlane,YellowPlane,BlackPlane: TLinarBitmap; BlackPlaneGamma,ImageGamma: Single);
// Create TLinearBitmap for output array
procedure CreateColorPlanes(var Planes: T3PlanesArray); overload;
procedure CreateColorPlanes(var Planes: T4PlanesArray); overload;
procedure FreeColorPlanes(var Planes: T3PlanesArray); overload;
procedure FreeColorPlanes(var Planes: T4PlanesArray); overload;
procedure SplitColorPlanesCreate(Image: TLinarBitmap; var Planes: T3PlanesArray; const T: TMatrix4x4);
procedure SplitColorPlanesRGBCreate(Image: TLinarBitmap; var Planes: T3PlanesArray);

// Combine color planes
procedure CombineColorPlanes(Plane1,Plane2,Plane3,Image: TLinarBitmap; const T: TMatrix4x4); overload;
procedure CombineColorPlanes(Plane1,Plane2,Plane3,Image: TLinarBitmap; T: TColorTransformProc); overload;
procedure CombineColorPlanesRGB(Plane1,Plane2,Plane3,Image: TLinarBitmap);

procedure ConvertColorSpace(Image: TLinarBitmap; const T: TMatrix4x4; NewImage: TLinarBitmap=nil); overload;
procedure ConvertColorSpace(Image: TLinarBitmap; ColorTransform: TColorTransformProc; NewImage: TLinarBitmap=nil); overload;
procedure ConvertColorSpaceCMYK2RGB(Image: TLinarBitmap; NewImage: TLinarBitmap=nil);

type TImageOpetator = (boSub,boAdd,boAddSub,boMultiply,boAND,boOR,boXOR,boMin,boMax);

// Bitmap calculations
procedure BitmapCalculate(Plane1,Plane2,Plane3,Image: TLinarBitmap; Offset: Integer; Operator: TImageOpetator); overload;
procedure BitmapCalculate(Plane1,Plane2,Image: TLinarBitmap; Offset: Integer; Operator: TImageOpetator); overload;

// Combine images with a function of p1 and p2. Example: Expression='(p1+p2)/2' for average
procedure BitmapCalculateExpression(Plane1,Plane2,Image: TLinarBitmap; const Expression: string; Defines: TSymbolTable=nil);

procedure WeightedCombineImages(Image1,Image2,Weights,Result: TLinarBitmap; MaxWeight: Integer=255); overload;
procedure WeightedCombineImages(Image1,Image2: TLinearBitmap; Weights: TFloatMap32; Result: TLinarBitmap); overload;

implementation

uses ColorMapper, BitmapGammaInterpolation, MultiCoreProcessing;

procedure Pattern2x2(Image: TLinarBitmap);
var
  X, Y, P, Planes : Integer;
begin
  if Image.PixelFormat=pf24bit then Planes:=3
  else Planes:=1;
  for P:=0 to Planes-1 do
    for Y:=0 to Image.Height-1 do
      for X:=0 to Image.Width-1 do
        if Boolean((X xor Y) and 1) then TByteArray(Image.ScanLine[Y]^)[X*Planes+P]:=Byte(P=1)-1;
end;

// Convert to 8-bit grayscale image
procedure ConvertToGrayscale(const Image,GrayImage: TLinarBitmap; ImageGamma: Single);
var
  GammaConverter : TGammaConverter;

  procedure Convert(Y: Integer);
  var
    Pix : ^RGBRec;
    NewPix : ^Byte;
    X : Integer;
  begin
    Pix:=Image.ScanLine[Y];
    NewPix:=GrayImage.ScanLine[Y];
    for X:=0 to Image.Width-1 do
    begin
      NewPix^:=Round((Pix^.R*2+Pix^.G*3+Pix^.B)*(1/6));
      Inc(Pix); Inc(NewPix);
    end;
  end;

  procedure ConvertGamma(Y: Integer);
  var
    Pix : ^RGBRec;
    NewPix : ^Byte;
    X : Integer;
  begin
    Pix:=Image.ScanLine[Y];
    NewPix:=GrayImage.ScanLine[Y];
    for X:=0 to Image.Width-1 do
    begin
      NewPix^:=GammaConverter.ConvertToGrayscale(Pix^);
      Inc(Pix); Inc(NewPix);
    end;
  end;

begin
  if Image.PixelFormat=pf24bit then
  begin
    GrayImage.New(Image.Width,Image.Height,pf8bit);
    GrayImage.Palette^:=GrayPal;
    if ImageGamma=1 then
    begin
      ParallelFor(0,Image.Height-1,@Convert);
    end
    else
    begin
      GammaConverter.Prepare(ImageGamma);
      ParallelFor(0,Image.Height-1,@ConvertGamma);
    end;
  end
  else
  begin
    Image.AssignTo(GrayImage);
    ConvertToGrayscale(GrayImage,ImageGamma);
  end;
end;

// Convert to 8-bit grayscale image
procedure ConvertToGrayscale(const Image: TLinarBitmap; ImageGamma: Single);
var
  Pix : ^Byte;
  P : Integer;
  LUT : array[0..255] of Byte;
  NewImage : TLinarBitmap;
  GammaConverter : TGammaConverter;
begin
  if Image.PixelFormat=pf32bit then Image.PixelFormat:=pf24bit;
  if Image.PixelFormat=pf24bit then
  begin
    NewImage:=TLinarBitmap.Create;
    ConvertToGrayscale(Image,NewImage,ImageGamma);
    Image.TakeOver(NewImage);
    NewImage.Free;
  end
  else if Image.PixelFormat=pf8bit then
  begin
    GammaConverter.Prepare(ImageGamma);
    for P:=0 to 255 do LUT[P]:=GammaConverter.ConvertToGrayscale(Image.Palette^[P]);
    Pointer(Pix):=Image.Map;
    for P:=1 to Image.Size do
    begin
      Pix^:=LUT[Pix^];
      Inc(Pix);
    end;
    Image.Palette^:=GrayPal;
  end
  else raise Exception.Create(rsInvalidPixelFormat);
end;


// Make palette from the 256 most frequently used colors
procedure OptimizePalette(Image: TLinarBitmap; out NewPal: TPalette; MaxSearchColors: Integer);
var
   Pix               : ^RGBRec;
   I, P, ColorCount  : Integer;
   Found             : Boolean;
   Palette           : array of TDataRecord4;
begin
  ZeroMem(Palette,SizeOf(Palette));
  ColorCount:=0;
  Pointer(Pix):=Image.Map;
  SetLength(Palette,Min(512,MaxSearchColors));
  for P:=0 to Image.Width*Image.Height-1 do
  begin
    Found:=False;
    for I:=0 to ColorCount-1 do
      with TRGBQuad(Palette[I].Data) do
        if (rgbBlue=Pix^.B) and (rgbGreen=Pix^.G) and (rgbRed=Pix^.R) then
        begin
          Inc(Palette[I].Value);
          Found:=True;
          Break;
        end;

    if not Found then
    begin
      PRGBRec(@Palette[ColorCount].Data)^:=Pix^;
      Palette[ColorCount].Value:=1;
      Inc(ColorCount);
      if ColorCount>=Length(Palette) then
      begin
        if ColorCount>=MaxSearchColors then Break; // Don't look any further
        SetLength(Palette,Min(Length(Palette)*2,MaxSearchColors));
      end;
    end;
    Inc(Pix);
  end;
  QuickSortDataRecord4(Palette,ColorCount);
  if ColorCount<256 then ZeroMem(NewPal,SizeOf(NewPal));
  for I:=0 to Min(256,ColorCount)-1 do NewPal[I]:=PRGBRec(@Palette[I].Data)^;
end;

procedure SplitColorPlanesRGB(Image,RedPlane,GreenPlane,BluePlane: TLinarBitmap);
var
  Pix : ^RGBRec;
  RedPix, GreenPix, BluePix : ^Byte;
  P : Integer;
begin
  RedPlane.New(Image.Width,Image.Height,pf8bit);
  GreenPlane.New(Image.Width,Image.Height,pf8bit);
  BluePlane.New(Image.Width,Image.Height,pf8bit);
  Pointer(Pix):=Image.Map;
  Pointer(RedPix):=RedPlane.Map;
  Pointer(GreenPix):=GreenPlane.Map;
  Pointer(BluePix):=BluePlane.Map;
  for P:=1 to RedPlane.Size do
  begin
    RedPix^:=Pix^.R;
    GreenPix^:=Pix^.G;
    BluePix^:=Pix^.B;
    Inc(Pix);
    Inc(RedPix);
    Inc(GreenPix);
    Inc(BluePix);
  end;
end;

procedure SplitColorPlanesCMYK(Image,CyanPlane,MagentaPlane,YellowPlane,BlackPlane: TLinarBitmap; BlackPlaneGamma, ImageGamma: Single);
var
  ImageGammaMap, BlackPlaneMap : TColorMap;

  procedure ProcessLine(Y: Integer);
  var
    Pix : PRGBRec;
    CyanPix, MagentaPix, YellowPix, BlackPix : ^Byte;
    X : Integer;
    Scale : Double;
  begin
    Pix:=Image.ScanLine[Y];
    CyanPix:=CyanPlane.ScanLine[Y];
    MagentaPix:=MagentaPlane.ScanLine[Y];
    YellowPix:=YellowPlane.ScanLine[Y];
    BlackPix:=BlackPlane.ScanLine[Y];
    for X:=1 to Image.Width do
    begin
      CyanPix^:=255-ImageGammaMap.Map[Pix^.R];
      MagentaPix^:=255-ImageGammaMap.Map[Pix^.G];
      YellowPix^:=255-ImageGammaMap.Map[Pix^.B];

      BlackPix^:=BlackPlaneMap.Map[Min(Min(CyanPix^,MagentaPix^),YellowPix^)];

      if BlackPix^<255 then
      begin
        Scale:=255/(255-BlackPix^);
        CyanPix^:=Round((CyanPix^-BlackPix^)*Scale);
        MagentaPix^:=Round((MagentaPix^-BlackPix^)*Scale);
        YellowPix^:=Round((YellowPix^-BlackPix^)*Scale);
      end
      else
      begin
        CyanPix^:=0;
        MagentaPix^:=0;
        YellowPix^:=0;
      end;

      Inc(Pix);
      Inc(CyanPix);
      Inc(MagentaPix);
      Inc(YellowPix);
      Inc(BlackPix);
    end;
  end;

begin
  try
    if BlackPlaneGamma=0 then ZeroMem(BlackPlaneMap.Map,SizeOf(BlackPlaneMap.Map))
    else BlackPlaneMap.SetGamma(BlackPlaneGamma);
  except
    ZeroMem(BlackPlaneMap.Map,SizeOf(BlackPlaneMap.Map));
  end;
  ImageGammaMap.SetGamma(1/ImageGamma);
  CyanPlane.New(Image.Width,Image.Height,pf8bit);
  MagentaPlane.New(Image.Width,Image.Height,pf8bit);
  YellowPlane.New(Image.Width,Image.Height,pf8bit);
  BlackPlane.New(Image.Width,Image.Height,pf8bit);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure CreateColorPlanes(var Planes: T3PlanesArray);
var I : Integer;
begin
  for I:=1 to 3 do Planes[I]:=TLinearBitmap.Create;
end;

procedure FreeColorPlanes(var Planes: T3PlanesArray);
var I : Integer;
begin
  for I:=1 to 3 do FreeAndNil(Planes[I]);
end;

procedure CreateColorPlanes(var Planes: T4PlanesArray);
var I : Integer;
begin
  for I:=1 to 4 do Planes[I]:=TLinearBitmap.Create;
end;

procedure FreeColorPlanes(var Planes: T4PlanesArray);
var I : Integer;
begin
  for I:=1 to 4 do FreeAndNil(Planes[I]);
end;

procedure SplitColorPlanesRGBCreate(Image: TLinarBitmap; var Planes: T3PlanesArray);
begin
  CreateColorPlanes(Planes);
  try
    SplitColorPlanesRGB(Image,Planes[1],Planes[2],Planes[3]);
  except
    FreeColorPlanes(Planes);
    raise;
  end;
end;

procedure SplitColorPlanesCreate(Image: TLinarBitmap; var Planes: T3PlanesArray; const T: TMatrix4x4);
begin
  CreateColorPlanes(Planes);
  try
    SplitColorPlanes(Image,Planes[1],Planes[2],Planes[3],T);
  except
    FreeColorPlanes(Planes);
    raise;
  end;
end;

procedure ColorTransform(A,B,C: Float; out X,Y,Z: Single; const T: TMatrix4x4); overload;
begin
  X:=A*T[1,1]+B*T[1,2]+C*T[1,3]+T[1,4];
  Y:=A*T[2,1]+B*T[2,2]+C*T[2,3]+T[2,4];
  Z:=A*T[3,1]+B*T[3,2]+C*T[3,3]+T[3,4];
end;

procedure ColorTransform(A,B,C: Float; out X,Y,Z: Double; const T: TMatrix4x4); overload;
begin
  X:=A*T[1,1]+B*T[1,2]+C*T[1,3]+T[1,4];
  Y:=A*T[2,1]+B*T[2,2]+C*T[2,3]+T[2,4];
  Z:=A*T[3,1]+B*T[3,2]+C*T[3,3]+T[3,4];
end;

procedure ColorTransform(A,B,C: Byte; out X,Y,Z: Single; const T: TMatrix4x4); overload;
begin
  X:=A*T[1,1]+B*T[1,2]+C*T[1,3]+T[1,4];
  Y:=A*T[2,1]+B*T[2,2]+C*T[2,3]+T[2,4];
  Z:=A*T[3,1]+B*T[3,2]+C*T[3,3]+T[3,4];
end;

procedure ColorTransform(A,B,C: Byte; out X,Y,Z: Double; const T: TMatrix4x4); overload;
begin
  X:=A*T[1,1]+B*T[1,2]+C*T[1,3]+T[1,4];
  Y:=A*T[2,1]+B*T[2,2]+C*T[2,3]+T[2,4];
  Z:=A*T[3,1]+B*T[3,2]+C*T[3,3]+T[3,4];
end;

procedure ColorTransform(A,B,C: Byte; out X,Y,Z: Byte; const T: TMatrix4x4);
var D : Integer;
begin
  D:=Round(A*T[1,1]+B*T[1,2]+C*T[1,3]+T[1,4]);
  if D>255 then
    D:=255
  else if D<0 then
    D:=0;
  X:=D;
  D:=Round(A*T[2,1]+B*T[2,2]+C*T[2,3]+T[2,4]);
  if D>255 then
    D:=255
  else if D<0 then
    D:=0;
  Y:=D;
  D:=Round(A*T[3,1]+B*T[3,2]+C*T[3,3]+T[3,4]);
  if D>255 then
    D:=255
  else if D<0 then
    D:=0;
  Z:=D;
end;

procedure ColorTransform(const A,B,C: Double; out X,Y,Z: Byte; const T: TMatrix4x4);
var D : Integer;
begin
  D:=Round(A*T[1,1]+B*T[1,2]+C*T[1,3]+T[1,4]);
  if D>255 then D:=255 else if D<0 then D:=0;
  X:=D;
  D:=Round(A*T[2,1]+B*T[2,2]+C*T[2,3]+T[2,4]);
  if D>255 then D:=255 else if D<0 then D:=0;
  Y:=D;
  D:=Round(A*T[3,1]+B*T[3,2]+C*T[3,3]+T[3,4]);
  if D>255 then D:=255 else if D<0 then D:=0;
  Z:=D;
end;

procedure ColorTransformRGB2HSI(R,G,B: Byte; out H,S,I: Byte);
var
  D : Byte;
  RR, RG, RB, RH : Double;
begin
  I:=Round((R+G+B)/3);
  if I=0 then
  begin
    S:=0;
    H:=0;
  end
  else
  begin
    D:=R;
    if G<D then D:=G;
    if B<D then D:=B;
    RR:=R/255; RG:=G/255; RB:=B/255;
    S:=Round(255-3/(RR+RG+RB)*D);
    if S=0 then H:=0
    else
    begin
      RH:=ArcCos(0.5*((RR-RG)+(RR-RB))/Sqrt(Sqr(RR-RG)+(RR-RB)*(RG-RB)));
      if B>G then RH:=(2*Pi)-RH;
      H:=Round(RH*(255/(2*Pi)));
    end;
  end
end;

procedure ColorTransformHSI2RGB(H,S,I: Byte; out R,G,B: Byte);
var
  RR, RG, RB, RH, RS : Double;
  D : Integer;
begin
  RS:=S/255;
  if H<=85 then // 0°<H<=120°
  begin
    RH:=H*(Pi*2/255);
    RB:=1-RS;
    RR:=1+RS*Cos(RH)/Cos((60/180*Pi)-RH);
    RG:=3-(RR+RB);
  end
  else if H<170 then // 120°<H<=240°
  begin
    RH:=(H-85)*(Pi*2/255);
    RR:=1-RS;
    RG:=1+RS*Cos(RH)/Cos((60/180*Pi)-RH);
    RB:=3-(RR+RG);
  end
  else // 240°<H<=360°
  begin
    RH:=(H-170)*(Pi*2/255);
    RG:=1-RS;
    RB:=1+RS*Cos(RH)/Cos((60/180*Pi)-RH);
    RR:=3-(RB+RG);
  end;
  D:=Round(RR*I);
  if D>255 then D:=255; R:=D;
  D:=Round(RG*I);
  if D>255 then D:=255; G:=D;
  D:=Round(RB*I);
  if D>255 then D:=255; B:=D;
end;

procedure ColorTransformRGB2Lab(R,G,B: Byte; out L,a_,b_: Byte);
const
  XYZTransform : TMatrix3x3 = ((0.412453, 0.357580, 0.180423),
                               (0.212671, 0.715160, 0.072169),
                               (0.019334, 0.119193, 0.950227));
  T = 0.008856;
var
  X, Y, Z, fX, fY, fZ, Y3, DL, Da, Db : Double;
begin
  X:=(XYZTransform[1,1]*R+XYZTransform[1,2]*G+XYZTransform[1,3]*B)/(0.950456*255);
  Y:=(XYZTransform[2,1]*R+XYZTransform[2,2]*G+XYZTransform[2,3]*B)/255;
  Z:=(XYZTransform[3,1]*R+XYZTransform[3,2]*G+XYZTransform[3,3]*B)/(1.088754*255);

  Y3:=Power(Y,1/3);
  if X>T then fX:=Power(X,1/3) else fX:=7.787*X + 16/116; // [0.137931034483;1]
  if Y>T then fY:=Y3           else fY:=7.787*Y + 16/116;
  if Z>T then fZ:=Power(Z,1/3) else fZ:=7.787*Z + 16/116;

  if Y>T then DL:=116*Y3-16.0  else DL:=903.3*Y;          // [0;100]
  Da:=500*(fX-fY);                                        // [-431;431]
  Db:=200*(fY-fZ);                                        // [-172.4;172.4]

  L:=ForceInRange(Round(DL*(255/100)),0,255);
  a_:=ForceInRange(Round(Da*(127.5/100)+127.5),0,255);
  b_:=ForceInRange(Round(Db*(127.5/100)+127.5),0,255);
end;

procedure ColorTransformLab2RGB(L,a_,b_: Byte; out R,G,B: Byte);
const
  InvXYZTransform : TMatrix3x3 = (( 3.240479,-1.537150,-0.498535),
                                  (-0.969256, 1.875992, 0.041556),
                                  ( 0.055648,-0.204043, 1.057311));
  T1 = 0.008856;
  T2 = 0.206893;
var
  X, Y, Z, fX, fY, fZ, DL, Da, Db : Double;
begin
  DL:=L*(100/255);
  Da:=(a_-127.5)*(100/127.5);
  Db:=(b_-127.5)*(100/127.5);

  // Compute Y
  fY:=Power((DL+16)/116,3);
  if fY<=T1 then fY:=DL/903.3;
  Y:=fY;
  // Alter fY slightly for further calculations
  if fY>T1 then fY:=Power(fY,1/3)
  else fY:=(7.787*fY+16/116);

  // Compute X
  fX:=fY+Da/500;
  if fX>T2 then X:=Power(fX,3)
  else X:=(fX-16/116)/7.787;

  // Compute Z
  fZ:=fY-Db/200;
  if fZ>T2 then Z:=Power(fZ,3)
  else Z:=(fZ-16/116)/7.787;

  X:=X*0.950456;
  Z:=Z*1.088754;

  R:=ForceInRange(Round(255*(InvXYZTransform[1,1]*X+InvXYZTransform[1,2]*Y+InvXYZTransform[1,3]*Z)),0,255);
  G:=ForceInRange(Round(255*(InvXYZTransform[2,1]*X+InvXYZTransform[2,2]*Y+InvXYZTransform[2,3]*Z)),0,255);
  B:=ForceInRange(Round(255*(InvXYZTransform[3,1]*X+InvXYZTransform[3,2]*Y+InvXYZTransform[3,3]*Z)),0,255);
end;

procedure ColorTransformRGB2LOCO(R,G,B: Byte; out S0,S1,S2: Byte);
begin
  S0:=R-G;
  S1:=G;
  S2:=B-G;
  {S0:=(R+2*G+B) div 4; // JPEG2000 RCT
  S1:=R-G;
  S2:=B-G;}
end;

procedure ColorTransformLOCO2RGB(S0,S1,S2: Byte; out R,G,B: Byte);
begin
  R:=S0+S1;
  G:=S1;
  B:=S2+S1;
end;

procedure SplitColorPlanes(Image,Plane1,Plane2,Plane3: TLinarBitmap; T: TColorTransformProc);

  procedure ProcessLine(Y: Integer);
  var
    Pix : ^RGBRec;
    Pix1, Pix2, Pix3 : ^Byte;
    X : Integer;
  begin
    Pix:=Image.ScanLine[Y];
    Pix1:=Plane1.ScanLine[Y];
    Pix2:=Plane2.ScanLine[Y];
    Pix3:=Plane3.ScanLine[Y];
    for X:=1 to Image.Width do
    begin
      T(Pix^.R,Pix^.G,Pix^.B,Pix1^,Pix2^,Pix3^);
      Inc(Pix);
      Inc(Pix1); Inc(Pix2); Inc(Pix3);
    end;
  end;

begin
  Plane1.New(Image.Width,Image.Height,pf8bit);
  Plane2.New(Image.Width,Image.Height,pf8bit);
  Plane3.New(Image.Width,Image.Height,pf8bit);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure CombineColorPlanes(Plane1,Plane2,Plane3,Image: TLinarBitmap; T: TColorTransformProc);

  procedure ProcessLine(Y: Integer);
  var
    Pix : ^RGBRec;
    Pix1, Pix2, Pix3 : ^Byte;
    X : Integer;
  begin
    Pix:=Image.ScanLine[Y];
    Pix1:=Plane1.ScanLine[Y];
    Pix2:=Plane2.ScanLine[Y];
    Pix3:=Plane3.ScanLine[Y];
    for X:=1 to Image.Width do
    begin
      T(Pix1^,Pix2^,Pix3^,Pix^.R,Pix^.G,Pix^.B);
      Inc(Pix);
      Inc(Pix1); Inc(Pix2); Inc(Pix3);
    end;
  end;

begin
  Image.New(Plane1.Width,Plane1.Height,pf24bit);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure SplitColorPlanes(Image,Plane1,Plane2,Plane3: TLinarBitmap; const T: TMatrix4x4);
var
  Pix : ^RGBRec;
  Pix1, Pix2, Pix3 : ^Byte;
  P : Integer;
begin
  Plane1.New(Image.Width,Image.Height,pf8bit);
  Plane2.New(Image.Width,Image.Height,pf8bit);
  Plane3.New(Image.Width,Image.Height,pf8bit);
  Pointer(Pix):=Image.Map;
  Pointer(Pix1):=Plane1.Map;
  Pointer(Pix2):=Plane2.Map;
  Pointer(Pix3):=Plane3.Map;
  for P:=1 to Plane1.Size do
  begin
    ColorTransform(Pix^.R,Pix^.G,Pix^.B,Pix1^,Pix2^,Pix3^,T);
    Inc(Pix);
    Inc(Pix1); Inc(Pix2); Inc(Pix3);
  end;
end;

procedure ConvertColorSpace(Image: TLinarBitmap; const T: TMatrix4x4; NewImage: TLinarBitmap);
var
  Pix, NewPix : ^RGBRec;
  P : Integer;
begin
  if Image.PixelFormat<>pf24bit then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  Pix:=Pointer(Image.Map);
  if (NewImage=nil) or (NewImage=Image) then
    for P:=1 to Image.Width*Image.Height do
    begin
      ColorTransform(Pix^.R,Pix^.G,Pix^.B,Pix^.R,Pix^.G,Pix^.B,T);
      Inc(Pix);
    end
  else
  begin
    NewImage.New(image.Width,Image.Height,pf24bit);
    NewPix:=Pointer(NewImage.Map);
    for P:=1 to Image.Width*Image.Height do
    begin
      ColorTransform(Pix^.R,Pix^.G,Pix^.B,NewPix^.R,NewPix^.G,NewPix^.B,T);
      Inc(Pix);
      Inc(NewPix);
    end
  end;
end;

procedure ConvertColorSpace(Image: TLinarBitmap; ColorTransform: TColorTransformProc; NewImage: TLinarBitmap=nil);
var
  Pix, NewPix : ^RGBRec;
  P : Integer;
begin
  Pix:=Pointer(Image.Map);
  if (NewImage=nil) or (NewImage=Image) then
    for P:=1 to Image.Width*Image.Height do
    begin
      ColorTransform(Pix^.R,Pix^.G,Pix^.B,Pix^.R,Pix^.G,Pix^.B);
      Inc(Pix);
    end
  else
  begin
    NewImage.New(image.Width,Image.Height,pf24bit);
    NewPix:=Pointer(NewImage.Map);
    for P:=1 to Image.Width*Image.Height do
    begin
      ColorTransform(Pix^.R,Pix^.G,Pix^.B,NewPix^.R,NewPix^.G,NewPix^.B);
      Inc(Pix);
      Inc(NewPix);
    end
  end;
end;

function CMYK2RGB(Col,Black: Integer): Integer;
const
  ScaleCK = 0.50;
  ScaleK  = 0.20;
  ScaleC  = 1-ScaleCK-ScaleK;
begin
  Col:=255-Col;
  Black:=255-Black;
  Result:=Round(Col*ScaleC + Black*ScaleK + Col*Black*ScaleCK/255);
  if Result<0 then Result:=0
  else if Result>255 then Result:=255;
end;

procedure ConvertColorSpaceCMYK2RGB(Image: TLinarBitmap; NewImage: TLinarBitmap);

var
  TakeOver : Boolean;
  Pix : PPaletteEntry;
  NewPix : ^RGBRec;
  P : Integer;
begin
  if Image.PixelFormat<>pf32bit then raise Exception.Create(rsUnsupportedBitmapFormat);
  TakeOver:=NewImage=nil;
  if TakeOver then NewImage:=TLinearBitmap.Create;
  NewImage.New(Image.Width,Image.Height,pf24bit);
  Pix:=Pointer(Image.Map);
  NewPix:=Pointer(NewImage.Map);
  for P:=1 to Image.Width*Image.Height do
  begin
    NewPix.R:=CMYK2RGB(Pix.peRed,Pix.peFlags);
    NewPix.G:=CMYK2RGB(Pix.peGreen,Pix.peFlags);
    NewPix.B:=CMYK2RGB(Pix.peBlue,Pix.peFlags);
    Inc(Pix);
    Inc(NewPix);
  end;
  if TakeOver then
  begin
    Image.TakeOver(NewImage);
    NewImage.Free;
  end;
end;

procedure CombineColorPlanes(Plane1,Plane2,Plane3,Image: TLinarBitmap; const T: TMatrix4x4);

  procedure ProcessLine(Y: Integer);
  var
    Pix : ^RGBRec;
    Pix1, Pix2, Pix3 : ^Byte;
    X : Integer;
  begin
    Pointer(Pix):=Image.ScanLine[Y];
    Pointer(Pix1):=Plane1.ScanLine[Y];
    Pointer(Pix2):=Plane2.ScanLine[Y];
    Pointer(Pix3):=Plane3.ScanLine[Y];
    for X:=1 to Plane1.Width do
    begin
      ColorTransform(Pix1^,Pix2^,Pix3^,Pix^.R,Pix^.G,Pix^.B,T);
      Inc(Pix);
      Inc(Pix1); Inc(Pix2); Inc(Pix3);
    end;
  end;

begin
  Assert((Plane1.PixelFormat=pf8bit) and (Plane2.PixelFormat=pf8bit) and (Plane2.PixelFormat=pf8bit));
  Assert((Plane1.Width=Plane2.Width) and (Plane1.Height=Plane2.Height));
  Assert((Plane1.Width=Plane3.Width) and (Plane1.Height=Plane3.Height));
  Image.New(Plane1.Width,Plane1.Height,pf24bit);
  ParallelFor(0,Image.Height-1,@ProcessLine);
end;

procedure CombineColorPlanesRGB(Plane1,Plane2,Plane3,Image: TLinarBitmap);
var
  Pix : ^RGBRec;
  Pix1, Pix2, Pix3 : ^Byte;
  P : Integer;
begin
  Assert((Plane1.PixelFormat=pf8bit) and (Plane2.PixelFormat=pf8bit) and (Plane2.PixelFormat=pf8bit)); 
  Assert((Plane1.Width=Plane2.Width) and (Plane1.Height=Plane2.Height));
  Assert((Plane1.Width=Plane3.Width) and (Plane1.Height=Plane3.Height));
  Image.New(Plane1.Width,Plane1.Height,pf24bit);
  Pointer(Pix):=Image.Map;
  Pointer(Pix1):=Plane1.Map;
  Pointer(Pix2):=Plane2.Map;
  Pointer(Pix3):=Plane3.Map;
  for P:=1 to Plane1.Size do
  begin
    Pix^.R:=Pix1^; Pix^.G:=Pix2^; Pix^.B:=Pix3^;
    Inc(Pix);
    Inc(Pix1); Inc(Pix2); Inc(Pix3);
  end;
end;

procedure BitmapCalculateExpression(Plane1,Plane2,Image: TLinarBitmap; const Expression: string; Defines: TSymbolTable);
var
  P1Symbol, P2Symbol : TSymbolValue;
  I, P1, P2 : Integer;
  Pix1, Pix2, P : PByte;
  CombineMap : array[0..255,0..255] of Byte;
  E : EvalFloat;
begin
  if Defines=nil then Defines:=DefaultDefines;
  Image.New(Plane1.Width,Plane1.Height,Plane1.PixelFormat);
  P1Symbol:=TSymbolValue(Defines.Define('p1',TSymbolValue.Create));
  P2Symbol:=TSymbolValue(Defines.Define('p2',TSymbolValue.Create));
  try
    for P1:=0 to 255 do
      for P2:=0 to 255 do
      begin
        P1Symbol.Value:=P1; P2Symbol.Value:=P2;
        E:=EvaluateExpression(Expression,Defines);
        if E<=0 then CombineMap[P1,P2]:=0
        else if E>=255 then CombineMap[P1,P2]:=255
        else CombineMap[P1,P2]:=Round(E);
      end;
  finally
    Defines.Remove('p1');
    Defines.Remove('p2');
  end;

  Pix1:=@Plane1.Map^[0]; Pix2:=@Plane2.Map^[0]; P:=@Image.Map^[0];
  for I:=1 to Image.Size do
  begin
    P^:=CombineMap[Pix1^,Pix2^];
    Inc(P); Inc(Pix1); Inc(Pix2);
  end;
  if Image.PixelFormat=pf8bit then Image.Palette^:=Plane1.Palette^;
end;

procedure BitmapCalculate(Plane1,Plane2,Plane3,Image: TLinarBitmap; Offset: Integer; Operator: TImageOpetator);
var
  Pix, Pix1, Pix2, Pix3 : ^Byte;
  P, D : Integer;
begin
  Image.New(Plane1.Width,Plane1.Height,Plane1.PixelFormat);
  Pointer(Pix):=Image.Map;
  Pointer(Pix1):=Plane1.Map;
  Pointer(Pix2):=Plane2.Map;
  Pointer(Pix3):=Plane3.Map;
  case Operator of
    boAddSub   : for P:=1 to Plane1.Size do
                 begin
                   D:=Integer(Pix1^)+Pix2^-Pix3^+Offset;
                   if D>255 then D:=255 else if D<0 then D:=0;
                   Pix^:=D;

                   Inc(Pix); Inc(Pix1); Inc(Pix2); Inc(Pix3);
                 end;
    boMultiply : for P:=1 to Plane1.Size do
                 begin
                   D:=Integer(Pix1^)*Pix2^*Pix3^+Offset;
                   if D>255 then D:=255 else if D<0 then D:=0;
                   Pix^:=D;

                   Inc(Pix); Inc(Pix1); Inc(Pix2); Inc(Pix3);
                 end;
    boAND      : for P:=1 to Plane1.Size do
                 begin
                   D:=Integer(Pix1^) and Pix2^ and Pix3^+Offset;
                   if D>255 then D:=255 else if D<0 then D:=0;
                   Pix^:=D;

                   Inc(Pix); Inc(Pix1); Inc(Pix2); Inc(Pix3);
                 end;
    boOR       : for P:=1 to Plane1.Size do
                 begin
                   D:=Integer(Pix1^) or Pix2^ or Pix3^+Offset;
                   if D>255 then D:=255 else if D<0 then D:=0;
                   Pix^:=D;

                   Inc(Pix); Inc(Pix1); Inc(Pix2); Inc(Pix3);
                 end;
    boXOR      : for P:=1 to Plane1.Size do
                 begin
                   D:=Integer(Pix1^) xor Pix2^ xor Pix3^+Offset;
                   if D>255 then D:=255 else if D<0 then D:=0;
                   Pix^:=D;

                   Inc(Pix); Inc(Pix1); Inc(Pix2); Inc(Pix3);
                 end;
  else raise ELinearBitmap.Create('Operation not supported');
  end;
end;

procedure BitmapCalculate(Plane1,Plane2,Image: TLinarBitmap; Offset: Integer; Operator: TImageOpetator);
var
  Pix, Pix1, Pix2 : ^Byte;
  P, D : Integer;
begin
  Assert(Plane1.PixelFormat=Plane2.PixelFormat);
  Image.New(Plane1.Width,Plane1.Height,Plane1.PixelFormat);
  Pointer(Pix):=Image.Map;
  Pointer(Pix1):=Plane1.Map;
  Pointer(Pix2):=Plane2.Map;
  case Operator of
    boSub   : for P:=1 to Plane1.Size do
              begin
                D:=Integer(Pix1^)-Pix2^+Offset;
                if D>255 then D:=255 else if D<0 then D:=0;
                Pix^:=D;

                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
    boAdd   : for P:=1 to Plane1.Size do
              begin
                D:=Integer(Pix1^)+Pix2^+Offset;
                if D>255 then D:=255 else if D<0 then D:=0;
                Pix^:=D;

                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
    boAND   : for P:=1 to Plane1.Size do
              begin
                Pix^:=Pix1^ and Pix2^;
                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
    boOR    : for P:=1 to Plane1.Size do
              begin
                Pix^:=Pix1^ or Pix2^;
                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
    boXOR   : for P:=1 to Plane1.Size do
              begin
                Pix^:=Pix1^ xor Pix2^;
                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
    boMin   : for P:=1 to Plane1.Size do
              begin
                if Pix1^<=Pix2^ then Pix^:=Pix1^
                else Pix^:=Pix2^;
                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
    boMax   : for P:=1 to Plane1.Size do
              begin
                if Pix1^>=Pix2^ then Pix^:=Pix1^
                else Pix^:=Pix2^;
                Inc(Pix); Inc(Pix1); Inc(Pix2);
              end;
  else raise ELinearBitmap.Create('Operation not supported');
  end;
end;

function GetPCATransform(const Image: TLinarBitmap; AutoStretch: Boolean; C1Var,C2Var,C3Var: PDouble; ShowResult: Boolean): TMatrix4x4Obj;

  function ComponentCovariance(P1,P2: PByte): Double;
  var
    I, Count, Sum1, Sum2  : Integer;
    Sum12 : Int64;
  begin
    Count:=Image.Width*Image.Height;
    Sum1:=0; Sum2:=0; Sum12:=0; 
    for I:=1 to Count do
    begin
      Inc(Sum1,P1^);
      Inc(Sum2,P2^);
      Inc(Sum12,Integer(P1^)*P2^);
      Inc(P1,3); Inc(P2,3);
    end;
    try
      Result:=Sum12/Count-Sum1*Sum2/Count;
    except
      Result:=0;
    end;
  end;

var
  Covariance, V, d : TMatrix;
  r, c : Integer;
  Min, Max, Range : Double;
begin
  if Image.PixelFormat<>pf24bit then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  Covariance:=TMatrix.Create(3,3);
  V:=TMatrix.Create(3,3);
  d:=TMatrix.Create(3);
  try
    for r:=1 to 3 do
      for c:=r to 3 do
      begin
        Covariance[r,c]:=ComponentCovariance(@Image.Map^[r-1],@Image.Map^[c-1]);
        Covariance[c,r]:=Covariance[r,c]; // Mirror
      end;
    Covariance.Eigen(V,d);

    if Assigned(C1Var) then C1Var^:=d.Matrix^[0]/d.Sum;
    if Assigned(C2Var) then C2Var^:=d.Matrix^[1]/d.Sum;
    if Assigned(C3Var) then C3Var^:=d.Matrix^[2]/d.Sum;

    V.Transpose;

    Result.Eye;
    for r:=1 to 3 do
    begin
      if AutoStretch then // Normalize rows and move to [0;255]
      begin
        Min:=0; Max:=0;
        for c:=1 to 3 do
        begin
          if V[r,c]<0 then Min:=Min+V[r,c]
          else Max:=Max+V[r,c];
        end;
        Range:=Max-Min;
        Result.M[r,4]:=-Min/Range*255;
      end
      else Range:=1;
      for c:=1 to 3 do Result.M[r,c]:=V[r,c]/Range;
    end;
             
    // Show result
    {$IFDEF FloatMapFileSupport}
    if ShowResult then
    begin
      Covariance.Show('Covariance');
      //V.Show('V');
      V.New(4,4);
      Move(Result.M,V.Matrix^,SizeOf(Result.M));
      V.Show('T');
    end;
    {$ENDIF}
  finally
    d.Free;
    V.Free;
    Covariance.Free;
  end;
end;

function GetDecorrelationStretchTransform(Image: TLinearBitmap; AllowComponentOffset: Boolean): TMatrix4x4Obj;
var
  M : TMatrix4x4;
  T : TMatrix4x4Obj;
  MinVal, MaxVal, Scale, Comp : array[1..3] of Double;
  MaxComp, MinComp : Double;
  I, r, c : Integer;
  Pix : PRGBRec;
begin
  // Compute PCA transform matrix
  M:=GetPCATransform(Image,AllowComponentOffset).M;
  // Determine value range in each component
  for r:=1 to 3 do
  begin
    MinVal[r]:=Infinity;
    MaxVal[r]:=NegInfinity;
  end;
  Pix:=@Image.Map^[0];
  for I:=1 to Image.Width*Image.Height do
  begin
    ColorTransform(Pix^.R,Pix^.G,Pix^.B,Comp[1],Comp[2],Comp[3],M);
    for r:=1 to 3 do
    begin
      MinVal[r]:=Min(MinVal[r],Comp[r]);
      MaxVal[r]:=Max(MaxVal[r],Comp[r]);
    end;
    Inc(Pix);
  end;
  // Determine stretching matrix
  Result.M:=M;
  if AllowComponentOffset then // Stretch all components to [0;255]
  begin
    T.Translate(-MinVal[1],-MinVal[2],-MinVal[3]); Result.Transform(T.M);
    T.Scale(255/(MaxVal[1]-MinVal[1]),255/(MaxVal[2]-MinVal[2]),255/(MaxVal[3]-MinVal[3])); Result.Transform(T.M);
  end
  else // Only scale components
  begin
    for r:=1 to 3 do
    begin
      MinComp:=0;
      MaxComp:=0;
      for c:=1 to 3 do
        if M[r,c]<0 then MinComp:=MinComp+M[r,c]
        else MaxComp:=MaxComp+M[r,c];
      MinComp:=MinComp*255;
      MaxComp:=MaxComp*255;
      Scale[r]:=Infinity;
      if (MaxComp>0) and (MaxVal[r]>0) then Scale[r]:=Min(Scale[r],MaxComp/MaxVal[r]);
      if (MinComp<0) and (MinVal[r]<0) then Scale[r]:=Min(Scale[r],MinComp/MinVal[r]);
    end;
    T.Scale(Scale[1],Scale[2],Scale[3]); Result.Transform(T.M);
  end;
  Result.Transform(InvertTransform4x4(M));
end;

procedure MakePaletteImage(Image: TLinarBitmap; Palette: PPalette);
var
   Pix               : ^RGBRec;
   NewPix            : ^Byte;
   OldMap            : PByteArray;
   Dist, BestDist, P : Integer;
   I, Col, Best      : Integer;
   Pal               : TPalette;
   Found             : Boolean;
   ColLUT            : array[0..255] of Integer;
begin
  if Image.PixelFormat in [pf16bit,pf32bit] then Image.PixelFormat:=pf24bit;
  if Image.PixelFormat=pf24bit then
  begin
    if Assigned(Palette) then Pal:=Palette^
    else
    begin // Make palette for 24 bit image
      Col:=-1; Pointer(Pix):=Image.Map;
      for P:=0 to Image.Width*Image.Height-1 do
      begin
        Found:=False;
        for I:=0 to Col do if (Pal[I].R=Pix^.R) and (Pal[I].G=Pix^.G) and (Pal[I].B=Pix^.B) then
        begin
          Found:=True;
          Break;
        end;
        if not Found then
        begin
          Inc(Col);
          Pal[Col]:=Pix^;
          if Col=255 then Break; // No reason to look any further
        end;
        Inc(Pix);
      end;
      for I:=Col+1 to 255 do Pal[I]:=BlackPix24;
    end;
    // Create image with new palette
    Pointer(Pix):=Image.Map;
    OldMap:=Image.Map;
    Image.Map:=nil;
    Image.New(Image.Width,Image.Height,pf8bit);
    Pointer(NewPix):=Image.Map;
    Best:=0;  
    for P:=0 to Image.Size-1 do
    begin
      BestDist:=High(Integer);
      for I:=0 to 255 do
      begin
        Dist:=Sqr(Pix^.R-Pal[I].R)+Sqr(Pix^.G-Pal[I].G)+Sqr(Pix^.B-Pal[I].B);
        if Dist<BestDist then
        begin
          Best:=I;
          if Dist=0 then Break;
          BestDist:=Dist;
        end;
      end;
      NewPix^:=Best;
      Inc(Pix); Inc(NewPix);
    end;
    FreeMem(OldMap);
    Image.Palette^:=Pal;
  end
  else if Image.PixelFormat=pf8bit then
  begin
    if Assigned(Palette) and not CompareMem(Image.Palette,Palette,SizeOf(TPalette)) then // Assign new palette to image
    begin
      Best:=0;
      for Col:=0 to 255 do
      begin
        BestDist:=High(Integer);
        for I:=0 to 255 do
        begin
          Dist:=Sqr(Image.Palette^[Col].R-Palette^[I].R)+
                Sqr(Image.Palette^[Col].G-Palette^[I].G)+
                Sqr(Image.Palette^[Col].B-Palette^[I].B);
          if Dist<BestDist then
          begin
            Best:=I;
            if Dist=0 then Break;
            BestDist:=Dist;
          end;
        end;
        ColLUT[Col]:=Best;
      end;
      NewPix:=Pointer(Image.Map);
      for P:=1 to Image.Size do
      begin
        NewPix^:=ColLUT[NewPix^];
        Inc(NewPix);
      end;
      Image.Palette^:=Palette^;
    end;
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

procedure WeightedCombineImages(Image1,Image2,Weights,Result: TLinarBitmap; MaxWeight: Integer=255);
var
  I, I24 : Integer;
  W : Single;
  WeightScale : Double;
begin
  if (Image1.PixelFormat<>Image2.PixelFormat) or (Weights.PixelFormat<>pf8bit) then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  WeightScale:=1/MaxWeight;
  Result.New(Image1.Width,Image1.Height,Image1.PixelFormat);
  if Image1.PixelFormat=pf8bit then
  begin
    for I:=0 to Weights.Size-1 do
    begin
      if Weights.Map^[I]>=MaxWeight then Result.Map^[I]:=Image2.Map^[I]
      else
      begin
        W:=Weights.Map^[I]*WeightScale;
        Result.Map^[I]:=Round(Image1.Map^[I]*(1-W)+Image2.Map^[I]*W);
      end;
    end;
  end
  else if Image1.PixelFormat=pf24bit then
  begin
    I24:=0;
    for I:=0 to Weights.Size-1 do
    begin
      if Weights.Map^[I]>=MaxWeight then
      begin
        Result.Map^[I24+0]:=Image2.Map^[I24+0];
        Result.Map^[I24+1]:=Image2.Map^[I24+1];
        Result.Map^[I24+2]:=Image2.Map^[I24+2];
      end
      else
      begin
        W:=Weights.Map^[I]*WeightScale;
        Result.Map^[I24+0]:=Round(Image1.Map^[I24+0]*(1-W)+Image2.Map^[I24+0]*W);
        Result.Map^[I24+1]:=Round(Image1.Map^[I24+1]*(1-W)+Image2.Map^[I24+1]*W);
        Result.Map^[I24+2]:=Round(Image1.Map^[I24+2]*(1-W)+Image2.Map^[I24+2]*W);
      end;
      Inc(I24,3);
    end;
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

// Weight=0 gives Image1 pixel, weight=1 gives Image2 pixel
procedure WeightedCombineImages(Image1,Image2: TLinearBitmap; Weights: TFloatMap32; Result: TLinarBitmap);

  procedure ProcessLine8(Y: Integer);
  var
    Line1, Line2, NewLine : PByteArray;
    WeightLine : PSingleArray;
    X : Integer;
    W : Single;
  begin
    Line1:=Image1.ScanLine[Y];
    Line2:=Image2.ScanLine[Y];
    NewLine:=Result.ScanLine[Y];
    WeightLine:=Weights.ScanLine[Y];
    for X:=0 to Result.BytesPerLine-1 do
    begin
      W:=WeightLine^[X];
      NewLine[X]:=Round(Line1^[X]*(1-W) + Line2^[X]*W);
    end;
  end;

  procedure ProcessLine24(Y: Integer);
  var
    Line1, Line2, NewLine : PByteArray;
    WeightLine : PSingleArray;
    X, I : Integer;
    W : Single;
  begin
    Line1:=Image1.ScanLine[Y];
    Line2:=Image2.ScanLine[Y];
    NewLine:=Result.ScanLine[Y];
    WeightLine:=Weights.ScanLine[Y];
    I:=0;
    for X:=0 to Result.Width-1 do
    begin
      W:=WeightLine^[X];
      NewLine[I]:=Round(Line1^[I]*(1-W) + Line2^[I]*W); Inc(I);
      NewLine[I]:=Round(Line1^[I]*(1-W) + Line2^[I]*W); Inc(I);
      NewLine[I]:=Round(Line1^[I]*(1-W) + Line2^[I]*W); Inc(I);
    end;
  end;

begin
  Result.New(Image1.Width,Image1.Height,Image1.PixelFormat);
  if Result.PixelFormat=pf24bit then ParallelFor(0,Result.Height-1,@ProcessLine24)
  else if Result.PixelFormat=pf8bit then ParallelFor(0,Result.Height-1,@ProcessLine8)
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

initialization
  InvYCbCrTransform:=InvertTransform4x4(YCbCrTransform);
  InvYIQTransform:=InvertTransform4x4(YIQTransform);
end.

