////////////////////////////////////////////////////////////////////////////////
//
// BitmapRotate.pas - Bitmap rotation and warping
// ----------------------------------------------
// Version:   2009-12-30
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
// Last changes:
//   Optimization, Rotate90 added
//
unit BitmapRotate;

interface

uses
  Windows, SysUtils, Graphics, LinarBitmap, Math, MathUtils, MatrixMath, FileMappedBitmap, ColorMapper;

// Rotate and resize Image, Angle is radians, counter-clockwise
procedure Rotate(Image: TLinearBitmap; Angle: Double; Background: TColor=0; ImageGamma: Single=1);
// Rotate 90°
procedure Rotate90(Image: TLinearBitmap);
// Rotate 180°
procedure Rotate180(Image: TLinearBitmap);
// Rotate 270°
procedure Rotate270(Image: TLinearBitmap);
// Image will be rotatet Angle radians and centered in NewImage
procedure RotateAny(Image,NewImage: TLinearBitmap; Angle: Double);
procedure RotateAnyBilinear(Image,NewImage: TLinearBitmap; Angle: Double; AutoFillCorners: Boolean=False); overload;
procedure RotateAnyBilinear(Image,NewImage: TLinearBitmap; Angle,Gamma: Double); overload;
procedure RotateAnyMonochrome(Image1: TFileMappedBitmap; NewImage: TLinearBitmap; SourceSize: TPoint; Angle: Double);
// Determine size of image after it has been rotated and scaled
function RotatedImageSize(Image: TLinearBitmap; const Angle: Double; const Scale: Double=1): TPoint;
// Mirror left <-> right
procedure Mirror(Image: TLinearBitmap);
// Flip top <-> bottom
procedure Flip(Image: TLinearBitmap);
// Deskew image based on horizontal lines (returns rotation angle)
function Deskew(Image: TLinearBitmap; BackgroundColor: TColor=-1; ImageGamma: Single=1): Double;

type
  TTextureVertrix = record
                      Source : TPoint;
                      Dest   : TPoint;
                    end;

// Copy and transform triangle from Image to NewImage
procedure TransformTriangle(Image,NewImage: TLinearBitmap; Vert1,Vert2,Vert3: TTextureVertrix);
// Warp bitmap with bilinear interpolation using the linear transform F. Result is origin point
type TWarpCrop = (wcFullImage,wcAutoCrop,wcUseDestination);
function WarpBitmap(Image,NewImage: TLinearBitmap; F: TMatrix3x3; CropOption: TWarpCrop=wcAutoCrop; Background: TColor=0): TPoint;

implementation

uses
  Types, SyncObjs, MultiCoreProcessing, Classes, MemUtils, BitmapGammaInterpolation,
  FloatMapUtils, BitmapConversion, PerformanceTimer;

function WarpBitmap(Image,NewImage: TLinearBitmap; F: TMatrix3x3; CropOption: TWarpCrop; Background: TColor): TPoint;

var
  BytesPerLine : Integer;

  procedure ProcessLine8(Y: Integer);
  var
    P : TFloatPoint;
    X, TX, TY, B : Integer;
    Pix8 : PByte;
  begin
    Pix8:=NewImage.ScanLine[Y];
    for X:=0 to NewImage.Width-1 do
    begin
      P:=Transform2D(X+Result.X,Y+Result.Y,F);
      TX:=Floor(P.X);
      TY:=Floor(P.Y);
      if (TX>=0) and (TY>=0) then
      begin
        if (TX<Image.Width-1) and (TY<Image.Height-1) then
        begin
          B:=Integer(Image.Map)+TX+TY*BytesPerLine;
          P.X:=(P.X-TX); P.Y:=(P.Y-TY);
          Pix8^:=Round(PByte(B)^*(1-P.X)*(1-P.Y)+
                       PByte(B+1)^*P.X*(1-P.Y)+
                       PByte(B+BytesPerLine)^*P.Y*(1-P.X)+
                       PByte(B+BytesPerLine+1)^*P.Y*P.X);
        end
        else if (TX<Image.Width) and (TY<Image.Height) then Pix8^:=PByte(Image.Pixel[TX,TY])^;
      end;
      Inc(Pix8);
    end;
  end;

  procedure ProcessLine24(Y: Integer);
  var
    P : TFloatPoint;
    X, TX, TY, B : Integer;
    Pix : PRGBRec;
  begin
    Pix:=NewImage.ScanLine[Y];
    for X:=0 to NewImage.Width-1 do
    begin
      P:=Transform2D(X+Result.X,Y+Result.Y,F);
      TX:=Floor(P.X);
      TY:=Floor(P.Y);
      if (TX>=0) and (TY>=0) then
      begin
        if (TX<Image.Width-1) and (TY<Image.Height-1) then
        begin
          B:=Integer(Image.Map)+TX*3+TY*BytesPerLine;

          P.X:=(P.X-TX); P.Y:=(P.Y-TY);
          Pix^.B:=Round(PByte(B)^*(1-P.X)*(1-P.Y)+
                        PByte(B+3)^*P.X*(1-P.Y)+
                        PByte(B+BytesPerLine)^*P.Y*(1-P.X)+
                        PByte(B+BytesPerLine+3)^*P.Y*P.X);

          Inc(B);
          Pix^.G:=Round(PByte(B)^*(1-P.X)*(1-P.Y)+
                        PByte(B+3)^*P.X*(1-P.Y)+
                        PByte(B+BytesPerLine)^*P.Y*(1-P.X)+
                        PByte(B+BytesPerLine+3)^*P.Y*P.X);

          Inc(B);
          Pix^.R:=Round(PByte(B)^*(1-P.X)*(1-P.Y)+
                        PByte(B+3)^*P.X*(1-P.Y)+
                        PByte(B+BytesPerLine)^*P.Y*(1-P.X)+
                        PByte(B+BytesPerLine+3)^*P.Y*P.X);
        end
        else if (TX<Image.Width) and (TY<Image.Height) then Pix^:=PRGBRec(Image.Pixel[TX,TY])^;
      end;
      Inc(Pix);
    end;
  end;

var
  CDest : array[0..3] of TFloatPoint;
  MinX, MaxX, MinY, MaxY : Double;
  OMinX, OMaxX, OMinY, OMaxY : Double;
  I : Integer;
begin
  if (NewImage=Image) or (NewImage=nil) then
  begin
    NewImage:=TLinearBitmap.Create;
    try
      if Background<0 then
      begin
        Assert(CropOption=wcUseDestination,'Background<0 only works with wcUseDestination');
        NewImage.Assign(Image);
        Result:=WarpBitmap(NewImage,Image,F,wcUseDestination,-1);
      end
      else
      begin
        if CropOption=wcUseDestination then NewImage.New(Image.Width,Image.Height,Image.PixelFormat);
        Result:=WarpBitmap(Image,NewImage,F,CropOption,Background);
        Image.TakeOver(NewImage);
      end;
    finally
      NewImage.Free;
    end;
    Exit;
  end;

  if CropOption=wcUseDestination then // No translation, use size of NewImage
  begin
    Assert(NewImage.Width>0);
    Result.X:=0;
    Result.Y:=0;
  end
  else
  begin
    // Transform image corners
    CDest[0]:=FloatPoint(0,0);
    CDest[1]:=FloatPoint(0,Image.Height-1);
    CDest[2]:=FloatPoint(Image.Width-1,Image.Height-1);
    CDest[3]:=FloatPoint(Image.Width-1,0);
    MaxX:=-MaxDouble; MinY:=MaxDouble;
    MaxY:=-MaxDouble; MinX:=MaxDouble;
    OMaxX:=-MaxDouble; OMinY:=MaxDouble;
    OMaxY:=-MaxDouble; OMinX:=MaxDouble;
    for I:=0 to 3 do
    begin
      CDest[I]:=Transform2D(CDest[I].X,CDest[I].Y,F);
      // Find min and max
      if CropOption=wcAutoCrop then
      begin
        if CDest[I].X<OMinX then
        begin
          MinX:=OMinX;
          OMinX:=CDest[I].X;
        end
        else if CDest[I].X<MinX then MinX:=CDest[I].X;
        if CDest[I].X>OMaxX then
        begin
          MaxX:=OMaxX;
          OMaxX:=CDest[I].X;
        end
        else if CDest[I].X>MaxX then MaxX:=CDest[I].X;
        if CDest[I].Y<OMinY then
        begin
          MinY:=OMinY;
          OMinY:=CDest[I].Y;
        end
        else if CDest[I].Y<MinY then MinY:=CDest[I].Y;
        if CDest[I].Y>OMaxY then
        begin
          MaxY:=OMaxY;
          OMaxY:=CDest[I].Y;
        end
        else if CDest[I].Y>MaxY then MaxY:=CDest[I].Y;
      end
      else // CropOption=wcFullImage
      begin
        if CDest[I].X<MinX then MinX:=CDest[I].X;
        if CDest[I].X>MaxX then MaxX:=CDest[I].X;
        if CDest[I].Y<MinY then MinY:=CDest[I].Y;
        if CDest[I].Y>MaxY then MaxY:=CDest[I].Y;
      end;
    end;                                
    if Max(Ceil(MaxX-MinX),Ceil(MaxY-MinY))>50000 then raise Exception.Create('Invalid transformation');
    Result.X:=Round(MinX);
    Result.Y:=Round(MinY);
    NewImage.New(Round(MaxX-MinX+1),Round(MaxY-MinY+1),Image.PixelFormat);
  end;
  InvertMatrix3x3(F);
  if Background>=0 then NewImage.ClearColor(Background);
  BytesPerLine:=Image.BytesPerLine;
  if Image.PixelFormat=pf24bit then // 24-bit
  begin
    ParallelFor(0,NewImage.Height-1,@ProcessLine24);
  end
  else if Image.IsGrayScale then // 8-bit grayscale
  begin
    NewImage.Palette^:=Image.Palette^;
    ParallelFor(0,NewImage.Height-1,@ProcessLine8);
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

type
  TTriPoint = record
                X, Y : Integer;
                MapPoint : TPoint;
              end;

function RotatedImageSize(Image: TLinearBitmap; const Angle,Scale: Double): TPoint;
var
  I : Integer;
  P : array[0..3] of TTriPoint;
  CX, CY : Double;
begin
  CX:=Image.Width/2; CY:=Image.Height/2;

  // Define triangle corner points
  P[0].MapPoint.X:=0;           P[0].MapPoint.Y:=0;
  P[1].MapPoint.X:=Image.Width; P[1].MapPoint.Y:=0;
  P[2].MapPoint.X:=0;           P[2].MapPoint.Y:=Image.Height;
  P[3].MapPoint.X:=Image.Width; P[3].MapPoint.Y:=Image.Height;

  // Rotate points
  Result:=Point(0,0);
  for I:=0 to 3 do
  begin
    P[I].X:=Round(((P[I].MapPoint.X-CX)*Cos(Angle)-(P[I].MapPoint.Y-CY)*Sin(Angle))*Scale*2);
    P[I].Y:=Round(((P[I].MapPoint.X-CX)*Sin(Angle)+(P[I].MapPoint.Y-CY)*Cos(Angle))*Scale*2);

    if Abs(P[I].X)>Result.X then Result.X:=Abs(P[I].X);
    if Abs(P[I].Y)>Result.Y then Result.Y:=Abs(P[I].Y);
  end;
end;

procedure Rotate(Image: TLinearBitmap; Angle: Double; Background: TColor; ImageGamma: Single);
var
  NewImage : TLinearBitmap;
begin
  Angle:=NormalizeAngle(Angle);
  if Abs(Angle-Pi)<0.0001 then Rotate180(Image)
  else if Abs(Angle-Pi*3/2)<0.0001 then Rotate270(Image)
  else if Abs(Angle-Pi/2)<0.0001 then Rotate90(Image)
  else if (Abs(Angle)>0.0001) and (Abs(Angle-2*Pi)>0.0001) then
  begin
    NewImage:=TLinearBitmap.Create;
    try
      with RotatedImageSize(Image,Angle,1) do NewImage.New(X,Y,Image.PixelFormat);
      if Image.PixelFormat=pf8bit then SwapDWords(NewImage.Palette,Image.Palette);
      if Background>=0 then NewImage.ClearColor(Background);
      if (Image.PixelFormat=pf8bit) and not NewImage.IsGrayScale then RotateAny(Image,NewImage,Angle)
      else if ImageGamma=1 then RotateAnyBilinear(Image,NewImage,Angle,Background<0)
      else RotateAnyBilinear(Image,NewImage,Angle,ImageGamma);
      Image.TakeOver(NewImage);
    finally
      NewImage.Free;
    end;
  end;
end;

{$WARNINGS OFF}
procedure TransformTriangle(Image,NewImage: TLinearBitmap; Vert1,Vert2,Vert3: TTextureVertrix);

  procedure TextureTriangle24(Pos1,Pos2,Pos3: TTriPoint);

    procedure HLine(X1,X2, Y: Integer; MapX1, MapY1, MapX2, MapY2: Double);
    var
     X, DX : Integer;
     T, IncMapX, IncMapY : Double;
     OutCol : ^RGBRec;
    begin
     if (Y<0) or (Y>=NewImage.Height) then Exit;

     if X1>X2 then
     begin
      X:=X1; X1:=X2; X2:=X;
      T:=MapX1; MapX1:=MapX2; MapX2:=T; T:=MapY1; MapY1:=MapY2; MapY2:=T;
     end;
     DX:=X2-X1;
     if DX<>0 then
     begin
      IncMapX:=(MapX2-MapX1)/DX; IncMapY:=(MapY2-MapY1)/DX;
     end;
     OutCol:=Pointer(Integer(NewImage.ScanLine[Y])+X1*3);
     if X2>=NewImage.Width then X2:=NewImage.Width-1;
     for X:=X1 to X2 do
     begin
      if X>=0 then OutCol^:=RGBRec(Pointer(@Image.Map^[Round(MapY1)*Image.BytesPerLine+Round(MapX1)*3])^);

      MapX1:=MapX1+IncMapX; MapY1:=MapY1+IncMapY;
      Inc(OutCol);
     end;
    end;
  
  var
   X1, X2, IncX1, IncX2,
   MapX1, MapX2, IncMapX1, IncMapX2, MapY1, MapY2, IncMapY1, IncMapY2 : Double;
   Y, DY : Integer;
   T : TTriPoint;
  begin
   if Pos2.Y<Pos1.Y then {Sorter punkter efter Y-værdi}
   begin
    T:=Pos1; Pos1:=Pos2; Pos2:=T;
   end;
   if Pos3.Y<Pos2.Y then
   begin
    T:=Pos2; Pos2:=Pos3; Pos3:=T;
    if Pos2.Y<Pos1.Y then
    begin
     T:=Pos1; Pos1:=Pos2; Pos2:=T;
    end;
   end;
   DY:=Pos3.Y-Pos1.Y;
   if DY=0 then
   begin
    HLine(Pos1.X,Pos2.X,Pos1.Y,
          Pos1.MapPoint.X,Pos1.MapPoint.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y);
    HLine(Pos2.X,Pos3.X,Pos1.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y,
          Pos3.MapPoint.X,Pos3.MapPoint.Y);
    Exit;
   end;
   X1:=Pos1.X; {Del op i to retvinklede trekanter}
   IncX1:=(Pos3.X-X1)/DY;
   MapX1:=Pos1.MapPoint.X; MapY1:=Pos1.MapPoint.Y;
   IncMapX1:=(Pos3.MapPoint.X-MapX1)/DY; IncMapY1:=(Pos3.MapPoint.Y-MapY1)/DY;

   DY:=Pos2.Y-Pos1.Y;
   if DY=0 then
   begin
    X2:=Pos2.X;
    MapX2:=Pos2.MapPoint.X; MapY2:=Pos2.MapPoint.Y;
   end
   else
   begin
    X2:=X1;
    IncX2:=(Pos2.X-X1)/DY;
    MapX2:=MapX1; IncMapX2:=(Pos2.MapPoint.X-MapX1)/DY;
    MapY2:=MapY1; IncMapY2:=(Pos2.MapPoint.Y-MapY1)/DY;
   end;
   for Y:=Pos1.Y to Pos2.Y do {Tegn nr. 1}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
   DY:=Pos3.Y-Pos2.Y;
   if DY=0 then Exit;
   IncX2:=(Pos3.X-Pos2.X)/DY;
   X2:=Pos2.X+IncX2;
   IncMapX2:=(Pos3.MapPoint.X-Pos2.MapPoint.X)/DY; MapX2:=Pos2.MapPoint.X+IncMapX2;
   IncMapY2:=(Pos3.MapPoint.Y-Pos2.MapPoint.Y)/DY; MapY2:=Pos2.MapPoint.Y+IncMapY2;
   for Y:=Pos2.Y+1 to Pos3.Y do {Tegn nr. 2}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
  end;

  procedure TextureTriangle8(Pos1,Pos2,Pos3: TTriPoint);

    procedure HLine(X1,X2: Integer; Y: Integer; MapX1, MapY1, MapX2, MapY2: Double);
    var
     X, DX : Integer;
     T, IncMapX, IncMapY : Double;
     OutCol : ^Byte;
    begin
     if (Y<0) or (Y>=NewImage.Height) then Exit;

     if X1>X2 then
     begin
      X:=X1; X1:=X2; X2:=X;
      T:=MapX1; MapX1:=MapX2; MapX2:=T; T:=MapY1; MapY1:=MapY2; MapY2:=T;
     end;
     DX:=X2-X1;
     if DX<>0 then
     begin
      IncMapX:=(MapX2-MapX1)/DX; IncMapY:=(MapY2-MapY1)/DX;
     end;
     OutCol:=Pointer(Integer(NewImage.ScanLine[Y])+X1);

     if X2>=NewImage.Width then X2:=NewImage.Width-1;
     for X:=X1 to X2 do
     begin
      if X>=0 then OutCol^:=Image.Map^[Round(MapY1)*Image.BytesPerLine+Round(MapX1)];

      MapX1:=MapX1+IncMapX; MapY1:=MapY1+IncMapY;
      Inc(OutCol);
     end;
    end;

  var
   X1, X2, IncX1, IncX2,
   MapX1, MapX2, IncMapX1, IncMapX2, MapY1, MapY2, IncMapY1, IncMapY2 : Double;
   Y, DY : Integer;
   T : TTriPoint;
  begin
   if Pos2.Y<Pos1.Y then {Sorter punkter efter Y-værdi}
   begin
    T:=Pos1; Pos1:=Pos2; Pos2:=T;
   end;
   if Pos3.Y<Pos2.Y then
   begin
    T:=Pos2; Pos2:=Pos3; Pos3:=T;
    if Pos2.Y<Pos1.Y then
    begin
     T:=Pos1; Pos1:=Pos2; Pos2:=T;
    end;
   end;
   DY:=Pos3.Y-Pos1.Y;
   if DY=0 then
   begin
    HLine(Pos1.X,Pos2.X,Pos1.Y,
          Pos1.MapPoint.X,Pos1.MapPoint.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y);
    HLine(Pos2.X,Pos3.X,Pos1.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y,
          Pos3.MapPoint.X,Pos3.MapPoint.Y);
    Exit;
   end;
   X1:=Pos1.X; {Del op i to retvinklede trekanter}
   IncX1:=(Pos3.X-X1)/DY;
   MapX1:=Pos1.MapPoint.X; MapY1:=Pos1.MapPoint.Y;
   IncMapX1:=(Pos3.MapPoint.X-MapX1)/DY; IncMapY1:=(Pos3.MapPoint.Y-MapY1)/DY;

   DY:=Pos2.Y-Pos1.Y;
   if DY=0 then
   begin
    X2:=Pos2.X;
    MapX2:=Pos2.MapPoint.X; MapY2:=Pos2.MapPoint.Y;
   end
   else
   begin
    X2:=X1;
    IncX2:=(Pos2.X-X1)/DY;
    MapX2:=MapX1; IncMapX2:=(Pos2.MapPoint.X-MapX1)/DY;
    MapY2:=MapY1; IncMapY2:=(Pos2.MapPoint.Y-MapY1)/DY;
   end;
   for Y:=Pos1.Y to Pos2.Y do {Tegn nr. 1}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
   DY:=Pos3.Y-Pos2.Y;
   if DY=0 then Exit;
   IncX2:=(Pos3.X-Pos2.X)/DY;
   X2:=Pos2.X+IncX2;
   IncMapX2:=(Pos3.MapPoint.X-Pos2.MapPoint.X)/DY; MapX2:=Pos2.MapPoint.X+IncMapX2;
   IncMapY2:=(Pos3.MapPoint.Y-Pos2.MapPoint.Y)/DY; MapY2:=Pos2.MapPoint.Y+IncMapY2;
   for Y:=Pos2.Y+1 to Pos3.Y do {Tegn nr. 2}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
  end;

var
  Pos1, Pos2, Pos3 : TTriPoint;

begin
  if Image.PixelFormat<>NewImage.PixelFormat then
    raise Exception.Create('Pixel format mismatch');

  Pos1.X:=Vert1.Dest.X; Pos1.Y:=Vert1.Dest.Y;
  Pos1.MapPoint:=Vert1.Source;
  Pos2.X:=Vert2.Dest.X; Pos2.Y:=Vert2.Dest.Y;
  Pos2.MapPoint:=Vert2.Source;
  Pos3.X:=Vert3.Dest.X; Pos3.Y:=Vert3.Dest.Y;
  Pos3.MapPoint:=Vert3.Source;

  if Image.PixelFormat=pf24bit then TextureTriangle24(Pos1,Pos2,Pos3)
  else TextureTriangle8(Pos1,Pos2,Pos3);
end;
{$WARNINGS ON}

(*procedure RotateAny(Image,NewImage: TLinearBitmap; Angle: Double; Scale: Integer);

  procedure TextureTriangle24(Pos1,Pos2,Pos3: TTriPoint);

    procedure HLine(X1,X2, Y: Integer; MapX1, MapY1, MapX2, MapY2: Double);
    var
     X, DX : Integer;
     T, IncMapX, IncMapY : Double;
     OutCol : ^RGBRec;

    begin
     if (Y<0) or (Y>=NewImage.Height) then Exit;

     if X1>X2 then
     begin
      X:=X1; X1:=X2; X2:=X;
      T:=MapX1; MapX1:=MapX2; MapX2:=T; T:=MapY1; MapY1:=MapY2; MapY2:=T;
     end;
     DX:=X2-X1;
     if DX<>0 then
     begin
      IncMapX:=(MapX2-MapX1)/DX; IncMapY:=(MapY2-MapY1)/DX;
     end;
     OutCol:=Pointer(Integer(NewImage.ScanLine[Y])+X1*3);
     if X2>=NewImage.Width then X2:=NewImage.Width-1;
     for X:=X1 to X2 do
     begin
      if X>=0 then OutCol^:=RGBRec(Pointer(@Image.Map^[Round(MapY1)*Image.BytesPerLine+Round(MapX1)*3])^);

      MapX1:=MapX1+IncMapX; MapY1:=MapY1+IncMapY;
      Inc(OutCol);
     end;
    end;

  var
   X1, X2, IncX1, IncX2,
   MapX1, MapX2, IncMapX1, IncMapX2, MapY1, MapY2, IncMapY1, IncMapY2 : Double;
   Y, DY : Integer;
   T : TTriPoint;
  begin
   if Pos2.Y<Pos1.Y then {Sorter punkter efter Y-værdi}
   begin
    T:=Pos1; Pos1:=Pos2; Pos2:=T;
   end;
   if Pos3.Y<Pos2.Y then
   begin
    T:=Pos2; Pos2:=Pos3; Pos3:=T;
    if Pos2.Y<Pos1.Y then
    begin
     T:=Pos1; Pos1:=Pos2; Pos2:=T;
    end;
   end;
   DY:=Pos3.Y-Pos1.Y;
   if DY=0 then
   begin
    HLine(Pos1.X,Pos2.X,Pos1.Y,
          Pos1.MapPoint.X,Pos1.MapPoint.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y);
    HLine(Pos2.X,Pos3.X,Pos1.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y,
          Pos3.MapPoint.X,Pos3.MapPoint.Y);
    Exit;
   end;
   X1:=Pos1.X; {Del op i to retvinklede trekanter}
   IncX1:=(Pos3.X-X1)/DY;
   MapX1:=Pos1.MapPoint.X; MapY1:=Pos1.MapPoint.Y;
   IncMapX1:=(Pos3.MapPoint.X-MapX1)/DY; IncMapY1:=(Pos3.MapPoint.Y-MapY1)/DY;

   DY:=Pos2.Y-Pos1.Y;
   if DY=0 then
   begin
    X2:=Pos2.X;
    MapX2:=Pos2.MapPoint.X; MapY2:=Pos2.MapPoint.Y;
   end
   else
   begin
    X2:=X1;
    IncX2:=(Pos2.X-X1)/DY;
    MapX2:=MapX1; IncMapX2:=(Pos2.MapPoint.X-MapX1)/DY;
    MapY2:=MapY1; IncMapY2:=(Pos2.MapPoint.Y-MapY1)/DY;
   end;
   for Y:=Pos1.Y to Pos2.Y do {Tegn nr. 1}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
   DY:=Pos3.Y-Pos2.Y;
   if DY=0 then Exit;
   IncX2:=(Pos3.X-Pos2.X)/DY;
   X2:=Pos2.X+IncX2;
   IncMapX2:=(Pos3.MapPoint.X-Pos2.MapPoint.X)/DY; MapX2:=Pos2.MapPoint.X+IncMapX2;
   IncMapY2:=(Pos3.MapPoint.Y-Pos2.MapPoint.Y)/DY; MapY2:=Pos2.MapPoint.Y+IncMapY2;
   for Y:=Pos2.Y+1 to Pos3.Y do {Tegn nr. 2}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
  end;

  procedure TextureTriangle8(Pos1,Pos2,Pos3: TTriPoint);

    procedure HLine(X1,X2: Integer; Y: Integer; MapX1, MapY1, MapX2, MapY2: Double);
    var
     X, DX : Integer;
     T, IncMapX, IncMapY : Double;
     OutCol : ^Byte;

    begin
     if (Y<0) or (Y>=NewImage.Height) then Exit;

     if X1>X2 then
     begin
      X:=X1; X1:=X2; X2:=X;
      T:=MapX1; MapX1:=MapX2; MapX2:=T; T:=MapY1; MapY1:=MapY2; MapY2:=T;
     end;
     DX:=X2-X1;
     if DX<>0 then
     begin
      IncMapX:=(MapX2-MapX1)/DX; IncMapY:=(MapY2-MapY1)/DX;
     end;
     OutCol:=Pointer(Integer(NewImage.ScanLine[Y])+X1);

     if X2>=NewImage.Width then X2:=NewImage.Width-1;
     for X:=X1 to X2 do
     begin
      if X>=0 then OutCol^:=Image.Map^[Round(MapY1)*Image.BytesPerLine+Round(MapX1)];

      MapX1:=MapX1+IncMapX; MapY1:=MapY1+IncMapY;
      Inc(OutCol);
     end;
    end;

  var
   X1, X2, IncX1, IncX2,
   MapX1, MapX2, IncMapX1, IncMapX2, MapY1, MapY2, IncMapY1, IncMapY2 : Double;
   Y, DY : Integer;
   T : TTriPoint;
  begin
   if Pos2.Y<Pos1.Y then {Sorter punkter efter Y-værdi}
   begin
    T:=Pos1; Pos1:=Pos2; Pos2:=T;
   end;
   if Pos3.Y<Pos2.Y then
   begin
    T:=Pos2; Pos2:=Pos3; Pos3:=T;
    if Pos2.Y<Pos1.Y then
    begin
     T:=Pos1; Pos1:=Pos2; Pos2:=T;
    end;
   end;
   DY:=Pos3.Y-Pos1.Y;
   if DY=0 then
   begin
    HLine(Pos1.X,Pos2.X,Pos1.Y,
          Pos1.MapPoint.X,Pos1.MapPoint.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y);
    HLine(Pos2.X,Pos3.X,Pos1.Y,
          Pos2.MapPoint.X,Pos2.MapPoint.Y,
          Pos3.MapPoint.X,Pos3.MapPoint.Y);
    Exit;
   end;
   X1:=Pos1.X; {Del op i to retvinklede trekanter}
   IncX1:=(Pos3.X-X1)/DY;
   MapX1:=Pos1.MapPoint.X; MapY1:=Pos1.MapPoint.Y;
   IncMapX1:=(Pos3.MapPoint.X-MapX1)/DY; IncMapY1:=(Pos3.MapPoint.Y-MapY1)/DY;

   DY:=Pos2.Y-Pos1.Y;
   if DY=0 then
   begin
    X2:=Pos2.X;
    MapX2:=Pos2.MapPoint.X; MapY2:=Pos2.MapPoint.Y;
   end
   else
   begin
    X2:=X1;
    IncX2:=(Pos2.X-X1)/DY;
    MapX2:=MapX1; IncMapX2:=(Pos2.MapPoint.X-MapX1)/DY;
    MapY2:=MapY1; IncMapY2:=(Pos2.MapPoint.Y-MapY1)/DY;
   end;
   for Y:=Pos1.Y to Pos2.Y do {Tegn nr. 1}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
   DY:=Pos3.Y-Pos2.Y;
   if DY=0 then Exit;
   IncX2:=(Pos3.X-Pos2.X)/DY;
   X2:=Pos2.X+IncX2;
   IncMapX2:=(Pos3.MapPoint.X-Pos2.MapPoint.X)/DY; MapX2:=Pos2.MapPoint.X+IncMapX2;
   IncMapY2:=(Pos3.MapPoint.Y-Pos2.MapPoint.Y)/DY; MapY2:=Pos2.MapPoint.Y+IncMapY2;
   for Y:=Pos2.Y+1 to Pos3.Y do {Tegn nr. 2}
   begin
    HLine(Round(X1),Round(X2),Y,MapX1,MapY1,MapX2,MapY2);
    X1:=X1+IncX1; X2:=X2+IncX2;
    MapX1:=MapX1+IncMapX1; MapY1:=MapY1+IncMapY1; MapX2:=MapX2+IncMapX2; MapY2:=MapY2+IncMapY2;
   end;
  end;

var
  P : array[0..5] of TTriPoint;
  CX, CY, NewCX, NewCY : Double;
  I : Integer;

begin
  if Image.PixelFormat<>NewImage.PixelFormat then
    raise Exception.Create('Pixel format mismatch');

  CX:=(Image.Width-1)/2;       CY:=(Image.Height-1)/2;
  NewCX:=(NewImage.Width-1)/2; NewCY:=(NewImage.Height-1)/2;

  // Define triangle corner points
  P[0].MapPoint.X:=0;             P[0].MapPoint.Y:=0;
  P[1].MapPoint.X:=Image.Width-1; P[1].MapPoint.Y:=0;
  P[2].MapPoint.X:=0;             P[2].MapPoint.Y:=Image.Height-1;
  P[3].MapPoint.X:=0;             P[3].MapPoint.Y:=Image.Height-1;
  P[4].MapPoint.X:=Image.Width-1; P[4].MapPoint.Y:=0;
  P[5].MapPoint.X:=Image.Width-1; P[5].MapPoint.Y:=Image.Height-1;

  // Rotate points
  for I:=0 to 5 do
  begin
    P[I].X:=Round(((P[I].MapPoint.X-CX)*Cos(Angle)-(P[I].MapPoint.Y-CY)*Sin(Angle))*Scale+NewCX);
    P[I].Y:=Round(((P[I].MapPoint.X-CX)*Sin(Angle)+(P[I].MapPoint.Y-CY)*Cos(Angle))*Scale+NewCY);
  end;

  // Draw two triangles
  if Image.PixelFormat=pf24bit then
  begin
    TextureTriangle24(P[0],P[1],P[2]);
    TextureTriangle24(P[3],P[4],P[5]);
  end
  else
  begin
    TextureTriangle8(P[0],P[1],P[2]);
    TextureTriangle8(P[3],P[4],P[5]);
  end;
end;(**)

procedure RotateAnyRect(Image,NewImage: TLinearBitmap; Angle: Double; const DestRect: TRect);
var
  jOriginalMin   : Integer;
  jOriginalMax   : Integer;
  iRotationAxis  : Integer;
  jRotationAxis  : Integer;
  sinTheta       : DOUBLE;
  cosTheta       : DOUBLE;
  NewCX, NewCY   : Integer;

  procedure UpdateOriginalMinMax(i,j: Integer);
  var
    iPrime         : Integer;
    jPrime         : Integer;
    jOriginal      : Integer;
  begin
    jPrime := j - NewCY;
    iPrime := i - NewCX;
    jOriginal := ROUND(iPrime * sinTheta + jPrime * cosTheta) + jRotationAxis;
    if jOriginal<jOriginalMin then jOriginalMin:=jOriginal;
    if jOriginal>jOriginalMax then jOriginalMax:=jOriginal;
  end;

  procedure ProcessLine(j: Integer);
  var
    i              : Integer;
    iPrime         : Integer;
    jPrime         : Integer;
    iOriginal      : Integer;
    jOriginal      : Integer;
    RowOriginal8   : PByteArray;
    RowRotated8    : PByteArray;
  BEGIN
    RowRotated8 := @NewImage.Map^[DWord(j)*DWord(NewImage.BytesPerLine)];
    jPrime := j - NewCY;
    FOR i := DestRect.Left to DestRect.Right DO
    BEGIN
      iPrime := i - NewCX;
      iOriginal := ROUND(iPrime * CosTheta - jPrime * sinTheta) + iRotationAxis;
      jOriginal := ROUND(iPrime * sinTheta + jPrime * cosTheta) + jRotationAxis;
      IF (iOriginal >= 0) AND (iOriginal < Image.Width) AND
         (jOriginal >= 0) AND (jOriginal < Image.Height) THEN
      BEGIN
        RowOriginal8 := @Image.Map^[DWord(jOriginal)*DWord(Image.BytesPerLine)];
        RowRotated8[i] := RowOriginal8[iOriginal];
      END;
    END
  END;

begin
  if Image.PixelFormat<>pf8bit then raise Exception.Create(rsInvalidPixelFormat);
  Assert((DestRect.Left>=0) and (DestRect.Right<NewImage.Width));
  Assert((DestRect.Top>=0) and (DestRect.Bottom<NewImage.Height));

  // Axis of rotation is normally center of image
  iRotationAxis := Image.Width div 2;
  jRotationAxis := Image.Height div 2;

  NewCX:=NewImage.Width div 2;
  NewCY:=NewImage.Height div 2;

  SinCosDouble(Angle,sinTheta,cosTheta);

  if (Image is TFileMappedBitmap) and TFileMappedBitmap(Image).MustUpdateScanLine then
  begin
    jOriginalMin:=Image.Height-1;
    jOriginalMax:=0;
    UpdateOriginalMinMax(DestRect.Left,DestRect.Top);
    UpdateOriginalMinMax(DestRect.Right,DestRect.Top);
    UpdateOriginalMinMax(DestRect.Left,DestRect.Bottom);
    UpdateOriginalMinMax(DestRect.Right,DestRect.Bottom);
    jOriginalMin:=Max(0,jOriginalMin);
    jOriginalMax:=Min(Image.Height-1,jOriginalMax);
    TFileMappedBitmap(Image).MapLines(jOriginalMin,jOriginalMax-jOriginalMin+1);
  end;
  if (NewImage is TFileMappedBitmap) and TFileMappedBitmap(NewImage).MustUpdateScanLine then
  begin
    TFileMappedBitmap(NewImage).MapLines(DestRect.Top,DestRect.Bottom-DestRect.Top+1);
  end;

  // Step through each row of rotated image.
  ParallelFor(DestRect.Top,DestRect.Bottom,@ProcessLine);
END;

procedure RotateAny(Image,NewImage: TLinearBitmap; Angle: Double);
const
  BlockSize = 1024;
var
  i              :  INTEGER;
  iRotationAxis  :  INTEGER;
  iOriginal      :  INTEGER;
  iPrime         :  INTEGER;
  j              :  INTEGER;
  jRotationAxis  :  INTEGER;
  jOriginal      :  INTEGER;
  jPrime         :  INTEGER;
  RowOriginal24  :  PRGBArray;
  RowRotated24   :  PRGBArray;
  sinTheta       :  DOUBLE;
  cosTheta       :  DOUBLE;
  NewCX, NewCY   : Integer;
  Rect : TRect;
begin
  if Image.PixelFormat=pf8bit then
  begin
    if (Image is TFileMappedBitmap) and TFileMappedBitmap(Image).MustUpdateScanLine then
    begin
      j:=0;
      while j<NewImage.Height do
      begin
        Rect.Top:=j;
        Rect.Bottom:=Min(j+BlockSize,NewImage.Height)-1;
        i:=0;
        while i<NewImage.Width do
        begin
          Rect.Left:=i;
          Rect.Right:=Min(i+BlockSize,NewImage.Width)-1;
          RotateAnyRect(Image,NewImage,Angle,Rect);
          i:=Rect.Right+1;
        end;
        j:=Rect.Bottom+1;
      end;
    end
    else if (NewImage is TFileMappedBitmap) and TFileMappedBitmap(NewImage).MustUpdateScanLine then
    begin
      Rect.Left:=0;
      Rect.Right:=NewImage.Width-1;
      j:=0;
      while j<NewImage.Height do
      begin
        Rect.Top:=j;
        Rect.Bottom:=Min(j+BlockSize,NewImage.Height)-1;
        RotateAnyRect(Image,NewImage,Angle,Rect);
        j:=Rect.Bottom+1;
      end;
    end
    else
    begin
      RotateAnyRect(Image,NewImage,Angle,Types.Rect(0,0,NewImage.Width-1,NewImage.Height-1));
    end;
  end
  else
  begin
    // Axis of rotation is normally center of image
    iRotationAxis := Image.Width div 2;
    jRotationAxis := Image.Height div 2;

    NewCX:=NewImage.Width div 2;
    NewCY:=NewImage.Height div 2;

    // Convert degrees to radians.  Use minus sign to force clockwise rotation.
    sinTheta := SIN(Angle);
    cosTheta := COS(Angle);

    // Step through each row of rotated image.
    FOR j := 0 to NewImage.Height-1 DO
    BEGIN
      RowRotated24  := NewImage.ScanlineSafe[j];

      // Assume the bitmap has an even number of pixels in both dimensions and
      // the axis of rotation is to be the exact middle of the image -- so this
      // axis of rotation is not at the middle of any pixel.

      // The transformation (i,j) to (iPrime, jPrime) puts the center of each
      // pixel at odd-numbered coordinates.  The left and right sides of each
      // pixel (as well as the top and bottom) then have even-numbered coordinates.

      // The point (iRotationAxis, jRotationAxis) identifies the axis of rotation.

      // For a 640 x 480 pixel image, the center point is (320, 240).  Pixels
      // numbered (index i) 0..319 are left of this point along the "X" axis and
      // pixels numbered 320..639 are right of this point.  Likewise, vertically
      // pixels are numbered (index j) 0..239 above the axis of rotation and
      // 240..479 below the axis of rotation.

      // The subtraction (i, j) - (iRotationAxis, jRotationAxis) moves the axis of
      // rotation from (i, j) to (iRotationAxis, jRotationAxis), which is the
      // center of the bitmap in this implementation.

      jPrime := 2*(j - NewCY{jRotationAxis}) + 1;

      FOR i := 0 to NewImage.Width-1 DO
      BEGIN
        iPrime := 2*(i - NewCX{iRotationAxis}) + 1;

        // Rotate (iPrime, jPrime) to location of desired pixel
        // Note:  There is negligible difference between floating point and
        // scaled integer arithmetic here.
        // Transform back to pixel coordinates of image, including translation
        // of origin from axis of rotation to origin of image.
        iOriginal := (ROUND(iPrime * CosTheta - jPrime * sinTheta) - 1) DIV 2 + iRotationAxis;
        jOriginal := (ROUND(iPrime * sinTheta + jPrime * cosTheta) - 1) DIV 2 + jRotationAxis;

        // Make sure (iOriginal, jOriginal) is in BitmapOriginal.
        IF   (iOriginal >= 0) AND (iOriginal < Image.Width) AND
             (jOriginal >= 0) AND (jOriginal < Image.Height) THEN
        BEGIN
          // Assign pixel from rotated space to current pixel in BitmapRotated
          RowOriginal24 := Image.Scanline[jOriginal];
          RowRotated24[i]  := RowOriginal24[iOriginal]
        END;
      END
    END
  end;
END;

procedure RotateAnyBilinear(Image,NewImage: TLinearBitmap; Angle: Double; AutoFillCorners: Boolean);
var
  i              : INTEGER;
  j              : INTEGER;
  iRotationAxis  : INTEGER;
  jRotationAxis  : INTEGER;
  iOriginal      : INTEGER;
  jOriginal      : INTEGER;
  iPrime         : INTEGER;
  jPrime         : INTEGER;
  fPrimeRotated  : DOUBLE;
  iWeight        : INTEGER;
  jWeight        : INTEGER;
  RowOriginal8   : PByteArray;
  RowRotated8    : PByteArray;
  RowRotated24   : PRGBArray;
  sinTheta       : DOUBLE;
  cosTheta       : DOUBLE;
  NewCX, NewCY   : Integer;
begin
  // Axis of rotation is normally center of image
  iRotationAxis := Image.Width div 2;
  jRotationAxis := Image.Height div 2;

  NewCX:=NewImage.Width div 2;
  NewCY:=NewImage.Height div 2;

  // Convert degrees to radians.  Use minus sign to force clockwise rotation.
  sinTheta := SIN(Angle);
  cosTheta := COS(Angle);

  // Step through each row of rotated image.
  if Image.PixelFormat=pf24bit then
  begin
    FOR j := 0 to NewImage.Height-1 DO
    BEGIN
      RowRotated24  := NewImage.ScanlineSafe[j];
      jPrime := j - NewCY;
      FOR i := 0 to NewImage.Width-1 DO
      BEGIN
        iPrime := i - NewCX;
        fPrimeRotated := iPrime * CosTheta - jPrime * sinTheta + iRotationAxis;
        iOriginal := Trunc(fPrimeRotated);
        if iOriginal=Image.Width-1 then
        begin
          Dec(iOriginal);
          iWeight:=256;
        end
        else
        begin
          iWeight:=Trunc((fPrimeRotated-iOriginal)*256+0.5);
          if iWeight<0 then iWeight:=0;
        end;
        fPrimeRotated := iPrime * sinTheta + jPrime * cosTheta + jRotationAxis;
        jOriginal := Trunc(fPrimeRotated);
        if jOriginal=Image.Height-1 then
        begin
          Dec(jOriginal);
          jWeight:=256;
        end
        else
        begin
          jWeight:=Trunc((fPrimeRotated-jOriginal)*256+0.5);
          if jWeight<0 then jWeight:=0;
        end;
        IF   (iOriginal >= 0) AND (iOriginal < Image.Width) AND
             (jOriginal >= 0) AND (jOriginal < Image.Height) THEN
        BEGIN
          RowOriginal8 := Image.Scanline[jOriginal];
          iOriginal:=iOriginal*3;
          RowRotated24[i].B:=((RowOriginal8[iOriginal+0] * (256-iWeight) +
                              RowOriginal8[iOriginal+3] * (iWeight)) * (256-jWeight) +
                             (RowOriginal8[iOriginal+Image.BytesPerLine+0] * (256-iWeight) +
                              RowOriginal8[iOriginal+Image.BytesPerLine+3] * (iWeight)) * jWeight ) shr 16;
          RowRotated24[i].G:=((RowOriginal8[iOriginal+1] * (256-iWeight) +
                              RowOriginal8[iOriginal+4] * (iWeight)) * (256-jWeight) +
                             (RowOriginal8[iOriginal+Image.BytesPerLine+1] * (256-iWeight) +
                              RowOriginal8[iOriginal+Image.BytesPerLine+4] * (iWeight)) * jWeight ) shr 16;
          RowRotated24[i].R:=((RowOriginal8[iOriginal+2] * (256-iWeight) +
                              RowOriginal8[iOriginal+5] * (iWeight)) * (256-jWeight) +
                             (RowOriginal8[iOriginal+Image.BytesPerLine+2] * (256-iWeight) +
                              RowOriginal8[iOriginal+Image.BytesPerLine+5] * (iWeight)) * jWeight ) shr 16;
        END
        else if AutoFillCorners then
        begin
          RowOriginal8 := Image.Scanline[EnsureRange(jOriginal,0,Image.Height-1)];
          iOriginal:=EnsureRange(iOriginal,0,Image.Width-1)*3;
          RowRotated24[i].B:=RowOriginal8[iOriginal+0];
          RowRotated24[i].G:=RowOriginal8[iOriginal+1];
          RowRotated24[i].R:=RowOriginal8[iOriginal+2];
        end;
      END;
    END;
  end
  else // 8 bit
  begin
    FOR j := 0 to NewImage.Height-1 DO
    BEGIN
      RowRotated8  := NewImage.ScanlineSafe[j];
      jPrime := j - NewCY;
      FOR i := 0 to NewImage.Width-1 DO
      BEGIN
        iPrime := i - NewCX;
        fPrimeRotated := iPrime * CosTheta - jPrime * sinTheta + iRotationAxis;
        iOriginal := Trunc(fPrimeRotated);
        if iOriginal=Image.Width-1 then
        begin
          Dec(iOriginal);
          iWeight:=256;
        end
        else
        begin
          iWeight:=Trunc((fPrimeRotated-iOriginal)*256+0.5);
          if iWeight<0 then iWeight:=0;
        end;
        fPrimeRotated := iPrime * sinTheta + jPrime * cosTheta + jRotationAxis;
        jOriginal := Trunc(fPrimeRotated);
        if jOriginal=Image.Height-1 then
        begin
          Dec(jOriginal);
          jWeight:=256;
        end
        else
        begin
          jWeight:=Trunc((fPrimeRotated-jOriginal)*256+0.5);
          if jWeight<0 then jWeight:=0;
        end;
        IF   (iOriginal >= 0) AND (iOriginal < Image.Width) AND
             (jOriginal >= 0) AND (jOriginal < Image.Height) THEN

        BEGIN
          RowOriginal8 := Image.Scanline[jOriginal];
          RowRotated8[i]:=((RowOriginal8[iOriginal] * (256-iWeight) +
                            RowOriginal8[iOriginal+1] * (iWeight)) * (256-jWeight) +
                           (RowOriginal8[iOriginal+Image.BytesPerLine] * (256-iWeight) +
                            RowOriginal8[iOriginal+Image.BytesPerLine+1] * (iWeight)) * jWeight ) shr 16;
        END
        else if AutoFillCorners then
        begin
          RowOriginal8 := Image.Scanline[EnsureRange(jOriginal,0,Image.Height-1)];
          iOriginal:=EnsureRange(iOriginal,0,Image.Width-1);
          RowRotated8[i]:=RowOriginal8[iOriginal];
        end;
      END;
    END;
  end;
END;

procedure RotateAnyBilinear(Image,NewImage: TLinearBitmap; Angle,Gamma: Double);
var
  GammaLUT       : T8bitToSingleGammaLUT;
  InvGammaLUT    : T16bitTo8bitGammaLUT;
  iRotationAxis  : INTEGER;
  jRotationAxis  : INTEGER;
  sinTheta       : DOUBLE;
  cosTheta       : DOUBLE;
  NewCX, NewCY   : Integer;

  procedure ProcessLine24(j: Integer);
  var
    i              : INTEGER;
    RowOriginal    : PByteArray;
    RowRotated24   : PRGBArray;
    iOriginal      : INTEGER;
    jOriginal      : INTEGER;
    iPrime         : INTEGER;
    jPrime         : INTEGER;
    fPrimeRotated  : DOUBLE;
    iWeight        : Double;
    jWeight        : Double;
    NewPix         : Double;
  begin
    RowRotated24  := NewImage.ScanlineSafe[j];
    jPrime := j - NewCY;
    FOR i := 0 to NewImage.Width-1 DO
    BEGIN
      iPrime := i - NewCX;
      fPrimeRotated := iPrime * CosTheta - jPrime * sinTheta + iRotationAxis;
      iOriginal := Trunc(fPrimeRotated);
      if iOriginal=Image.Width-1 then
      begin
        Dec(iOriginal);
        iWeight:=1;
      end
      else
      begin
        iWeight:=fPrimeRotated-iOriginal;
        if iWeight<0 then iWeight:=0;
      end;
      fPrimeRotated := iPrime * sinTheta + jPrime * cosTheta + jRotationAxis;
      jOriginal := Trunc(fPrimeRotated);
      if jOriginal=Image.Height-1 then
      begin
        Dec(jOriginal);
        jWeight:=1;
      end
      else
      begin
        jWeight:=fPrimeRotated-jOriginal;
        if jWeight<0 then jWeight:=0;
      end;
      IF   (iOriginal >= 0) AND (iOriginal < Image.Width) AND
           (jOriginal >= 0) AND (jOriginal < Image.Height) THEN
      BEGIN
        RowOriginal := Image.Scanline[jOriginal];
        iOriginal:=iOriginal*3;
        NewPix:=((GammaLUT[RowOriginal[iOriginal+0]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+3]] * (iWeight)) * (1-jWeight) +
                 (GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+0]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+3]] * (iWeight)) * jWeight);
        if NewPix<0 then RowRotated24[i].B:=0
        else if NewPix>=255 then RowRotated24[i].B:=255
        else RowRotated24[i].B:=InvGammaLUT[Round(NewPix*256)];
        NewPix:=((GammaLUT[RowOriginal[iOriginal+1]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+4]] * (iWeight)) * (1-jWeight) +
                 (GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+1]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+4]] * (iWeight)) * jWeight);
        if NewPix<0 then RowRotated24[i].G:=0
        else if NewPix>=255 then RowRotated24[i].G:=255
        else RowRotated24[i].G:=InvGammaLUT[Round(NewPix*256)];
        NewPix:=((GammaLUT[RowOriginal[iOriginal+2]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+5]] * (iWeight)) * (1-jWeight) +
                 (GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+2]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+5]] * (iWeight)) * jWeight);
        if NewPix<0 then RowRotated24[i].R:=0
        else if NewPix>=255 then RowRotated24[i].R:=255
        else RowRotated24[i].R:=InvGammaLUT[Round(NewPix*256)];
      END
    end;
  end;

  procedure ProcessLine8(j: Integer);
  var
    i              : INTEGER;
    RowOriginal    : PByteArray;
    RowRotated8    : PByteArray;
    iOriginal      : INTEGER;
    jOriginal      : INTEGER;
    iPrime         : INTEGER;
    jPrime         : INTEGER;
    fPrimeRotated  : DOUBLE;
    iWeight        : Double;
    jWeight        : Double;
    NewPix         : Double;
  begin
    RowRotated8  := NewImage.ScanlineSafe[j];
    jPrime := j - NewCY;
    FOR i := 0 to NewImage.Width-1 DO
    BEGIN
      iPrime := i - NewCX;
      fPrimeRotated := iPrime * CosTheta - jPrime * sinTheta + iRotationAxis;
      iOriginal := Trunc(fPrimeRotated);
      if iOriginal=Image.Width-1 then
      begin
        Dec(iOriginal);
        iWeight:=1;
      end
      else
      begin
        iWeight:=fPrimeRotated-iOriginal;
        if iWeight<0 then iWeight:=0;
      end;
      fPrimeRotated := iPrime * sinTheta + jPrime * cosTheta + jRotationAxis;
      jOriginal := Trunc(fPrimeRotated);
      if jOriginal=Image.Height-1 then
      begin
        Dec(jOriginal);
        jWeight:=1;
      end
      else
      begin
        jWeight:=fPrimeRotated-jOriginal;
        if jWeight<0 then jWeight:=0;
      end;
      IF   (iOriginal >= 0) AND (iOriginal < Image.Width) AND
           (jOriginal >= 0) AND (jOriginal < Image.Height) THEN
      BEGIN
        RowOriginal:=Image.Scanline[jOriginal];
        NewPix:=((GammaLUT[RowOriginal[iOriginal]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+1]] * (iWeight)) * (1-jWeight) +
                 (GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine]] * (1-iWeight) +
                  GammaLUT[RowOriginal[iOriginal+Image.BytesPerLine+1]] * iWeight) * jWeight);
        if NewPix<0 then RowRotated8[i]:=0
        else if NewPix>=255 then RowRotated8[i]:=255
        else RowRotated8[i]:=InvGammaLUT[Round(NewPix*256)];
      END
    END;
  end;

begin
  CreateGammaLUT(1/Gamma,GammaLUT);
  CreateGammaLUT(Gamma,InvGammaLUT);
  SinCosDouble(Angle,sinTheta,cosTheta);
  // Axis of rotation is normally center of image
  iRotationAxis := Image.Width div 2;
  jRotationAxis := Image.Height div 2;
  NewCX:=NewImage.Width div 2;
  NewCY:=NewImage.Height div 2;
  // Step through each row of rotated image.
  if Image.PixelFormat=pf24bit then
  begin
    ParallelFor(0,NewImage.Height-1,@ProcessLine24);
  end
  else // 8 bit
  begin
    ParallelFor(0,NewImage.Height-1,@ProcessLine8);
  end;
END;

// Rotate image assuming that it is only black and white
procedure RotateAnyMonochrome(Image1: TFileMappedBitmap; NewImage: TLinearBitmap; SourceSize: TPoint; Angle: Double);
const
  BlockSize = 2048;
var
  iRotationAxis  : Integer;
  jRotationAxis  : Integer;
  sinTheta       : DOUBLE;
  cosTheta       : DOUBLE;
  NewCX, NewCY   : Integer;
  Image1IsLarge  : Boolean;
  NewImageIsLarge: Boolean;

  procedure RotateRect(const DestRect: TRect);
  var
    jOriginalMin   : Integer;
    jOriginalMax   : Integer;

    procedure UpdateOriginalMinMax(i,j: Integer);
    var
      iPrime         : Integer;
      jPrime         : Integer;
      jOriginal      : Integer;
    begin
      jPrime := 2*(j - NewCY) + 1;
      iPrime := 2*(i - NewCX) + 1;
      jOriginal := (ROUND(iPrime * sinTheta + jPrime * cosTheta) - 1) DIV 2 + jRotationAxis;
      if jOriginal<jOriginalMin then jOriginalMin:=jOriginal;
      if jOriginal>jOriginalMax then jOriginalMax:=jOriginal;
    end;

    procedure ProcessLine(j: Integer);
    var
      i              : Integer;
      iOriginal      : Integer;
      jOriginal      : Integer;
      iPrime         : Integer;
      jPrime         : Integer;
      RowRotated     : PByteArray;
      RowOriginal    : PByteArray;
    begin
      RowRotated := @NewImage.Map^[DWord(j)*DWord(NewImage.BytesPerLine)];
      jPrime := 2*(j - NewCY) + 1;
      for i := DestRect.Left to DestRect.Right do
      begin
        iPrime := 2*(i - NewCX) + 1;
        iOriginal := (ROUND(iPrime * CosTheta - jPrime * sinTheta) - 1) DIV 2 + iRotationAxis;
        jOriginal := (ROUND(iPrime * sinTheta + jPrime * cosTheta) - 1) DIV 2 + jRotationAxis;
        if   (iOriginal >= 0) and (iOriginal < SourceSize.X) and
             (jOriginal >= 0) and (jOriginal < SourceSize.Y) then
        begin
          RowOriginal := @Image1.Map^[DWord(jOriginal)*DWord(Image1.BytesPerLine)];
          RowRotated[i] := not ((RowOriginal[iOriginal shr 3] shr (iOriginal and 7)) and 1-1);
        end;
      end
    end;

  begin
    if Image1IsLarge then
    begin
      jOriginalMin:=Image1.Height-1;
      jOriginalMax:=0;
      UpdateOriginalMinMax(DestRect.Left,DestRect.Top);
      UpdateOriginalMinMax(DestRect.Right,DestRect.Top);
      UpdateOriginalMinMax(DestRect.Left,DestRect.Bottom);
      UpdateOriginalMinMax(DestRect.Right,DestRect.Bottom);
      jOriginalMin:=Max(0,jOriginalMin);
      jOriginalMax:=Min(Image1.Height-1,jOriginalMax);
      Image1.MapLines(jOriginalMin,jOriginalMax-jOriginalMin+1);
    end;
    if NewImageIsLarge then
    begin
      TFileMappedBitmap(NewImage).MapLines(DestRect.Top,DestRect.Bottom-DestRect.Top+1);
    end;
    ParallelFor(DestRect.Top,DestRect.Bottom,@ProcessLine);
  end;

var
  X, Y : Integer;
  DestRect : TRect;
begin
  // Rotate
  Image1IsLarge:=Image1.MustUpdateScanLine;
  NewImageIsLarge:=(NewImage is TFileMappedBitmap) and TFileMappedBitmap(NewImage).MustUpdateScanLine;
  iRotationAxis := SourceSize.X div 2;
  jRotationAxis := SourceSize.Y div 2;
  NewCX:=NewImage.Width div 2;
  NewCY:=NewImage.Height div 2;
  SinCosDouble(Angle,sinTheta,cosTheta);
  if Image1IsLarge then
  begin
    Y:=0;
    while Y<NewImage.Height do
    begin
      DestRect.Top:=Y;
      DestRect.Bottom:=Min(Y+BlockSize,NewImage.Height)-1;
      X:=0;
      while X<NewImage.Width do
      begin
        DestRect.Left:=X;
        DestRect.Right:=Min(X+BlockSize,NewImage.Width)-1;
        RotateRect(DestRect);
        X:=DestRect.Right+1;
      end;
      Y:=DestRect.Bottom+1;
    end;
  end
  else
  begin
    Y:=0;
    DestRect.Left:=0;
    DestRect.Right:=NewImage.Width-1;
    while Y<NewImage.Height do
    begin
      DestRect.Top:=Y;
      DestRect.Bottom:=Min(Y+BlockSize,NewImage.Height)-1;
      RotateRect(DestRect);
      Y:=DestRect.Bottom+1;
    end;
  end;
end;

// Mirror left <-> right
procedure Mirror(Image: TLinearBitmap);
var
 X, Y : Integer;
 Col24 : RGBRec;
 Col : Byte;
 Pix8L, Pix8R : ^Byte;
 Pix24L, Pix24R : ^RGBRec;
begin
  with Image do
  begin
    if not Present then Exit;
    case PixelFormat of
      pf8bit  : for Y:=0 to Height-1 do
                begin
                  Pix8L:=ScanLineSafe[Y];
                  Pix8R:=Pointer(Integer(Pix8L)+BytesPerLine);
                  for X:=0 to (Width-2) div 2 do
                  begin
                    Dec(Pix8R);
                    Col:=Pix8R^;
                    Pix8R^:=Pix8L^;
                    Pix8L^:=Col;
                    Inc(Pix8L);
                  end;
                end;
      pf24bit : for Y:=0 to Height-1 do
                begin
                  Pix24L:=ScanLineSafe[Y];
                  Pix24R:=Pointer(Integer(Pix24L)+BytesPerLine);
                  for X:=0 to (Width-2) div 2 do
                  begin
                    Dec(Pix24R);
                    Col24:=Pix24R^;
                    Pix24R^:=Pix24L^;
                    Pix24L^:=Col24;
                    Inc(Pix24L);
                  end;
                end;
      else raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
  end;
end;

// Flip top <-> bottom
procedure Flip(Image: TLinearBitmap);
var
 Y : Integer;
 Line : PByteArray;
begin
  with Image do
  begin
    if not Present then Exit;
    GetMem(Line,BytesPerLine);
    try
      for Y:=0 to (Height-2) div 2 do
      begin
        Move(ScanLine[Y]^,Line^,BytesPerLine);
        Move(ScanLine[Height-1-Y]^,ScanLine[Y]^,BytesPerLine);
        Move(Line^,ScanLine[Height-1-Y]^,BytesPerLine);
      end;
    finally
      FreeMem(Line);
    end;
  end;
end;

procedure Rotate180(Image: TLinearBitmap);
begin
  Flip(Image);
  Mirror(Image);
end;

procedure Rotate90(Image: TLinearBitmap);
var
  NewImage : TLinearBitmap;
  X, Y : Integer;
  OrgPix24, NewPix24 : ^RGBRec;
  OrgPix8, NewPix8 : ^Byte;
begin
  NewImage:=TLinearBitmap.Create;
  try
    NewImage.New(Image.Height,Image.Width,Image.PixelFormat);
    case Image.PixelFormat of
      pf8bit  : begin
                  OrgPix8:=@Image.Map^[Image.Size-1];
                  for Y:=Image.Height-1 downto 0 do
                  begin
                    NewPix8:=@NewImage.Map^[Y];
                    for X:=0 to Image.Width-1 do
                    begin
                      NewPix8^:=OrgPix8^;
                      Dec(OrgPix8);
                      Inc(NewPix8,NewImage.Width);
                    end;
                  end;
                  NewImage.Palette^:=Image.Palette^;
                end;
      pf24bit : begin
                  OrgPix24:=@Image.Map^[Image.Size-3];
                  for Y:=Image.Height-1 downto 0 do
                  begin
                    NewPix24:=@NewImage.Map^[Y*3];
                    for X:=0 to Image.Width-1 do
                    begin
                      NewPix24^:=OrgPix24^;
                      Dec(OrgPix24);
                      Inc(NewPix24,NewImage.Width);
                    end;
                  end;
                end;
      else raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
    Image.TakeOver(NewImage);
  finally
    NewImage.Free;
  end;
end;

procedure Rotate270(Image: TLinearBitmap);
var
  NewImage : TLinearBitmap;
  X, Y : Integer;
  OrgPix24, NewPix24 : ^RGBRec;
  OrgPix8, NewPix8 : ^Byte;
begin
  NewImage:=TLinearBitmap.Create;
  try
    NewImage.New(Image.Height,Image.Width,Image.PixelFormat);
    case Image.PixelFormat of
      pf8bit  : begin
                  OrgPix8:=@Image.Map^[0];
                  for Y:=0 to Image.Height-1 do
                  begin
                    NewPix8:=@NewImage.Map^[NewImage.Width-1-Y];
                    for X:=0 to Image.Width-1 do
                    begin
                      NewPix8^:=OrgPix8^;
                      Inc(OrgPix8);
                      Inc(NewPix8,NewImage.Width);
                    end;
                  end;
                  NewImage.Palette^:=Image.Palette^;
                end;
      pf24bit : begin
                  OrgPix24:=@Image.Map^[0];
                  for Y:=0 to Image.Height-1 do
                  begin
                    NewPix24:=@NewImage.Map^[(NewImage.Width-1-Y)*3];
                    for X:=0 to Image.Width-1 do
                    begin
                      NewPix24^:=OrgPix24^;
                      Inc(OrgPix24);
                      Inc(NewPix24,NewImage.Width);
                    end;
                  end;
                end;
      else raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
    Image.TakeOver(NewImage);
  finally
    NewImage.Free;
  end;
end;

function Deskew(Image: TLinearBitmap; BackgroundColor: TColor; ImageGamma: Single): Double;
const
  MaxScewAngle = 15{°}/180*Pi;
  AngleStep     = 0.5{°}/180*Pi;
  
var
  BestVariance : Double;
  Sync : TCriticalSection;

  procedure TestAngle(I: Integer);
  var
    X, Line, Skew, PixCount, LineCount, Start, Sum, IY : Integer;
    DY, Variance, Y, Avarage, Angle, T : Double;
  begin
    Avarage:=0;
    Variance:=0;        
    LineCount:=0;
    Angle:=I*AngleStep;
    Skew:=Round(Tan(Angle)*Image.Width);
    DY:=Skew/Image.BytesPerLine;
    Start:=Min(-Skew div 2,0);
    for Line:=Start to Image.Height-1+Max(-Skew div 2,0) do
    begin
      Y:=Line;
      PixCount:=0;
      Sum:=0;
      for X:=0 to Image.BytesPerLine-1 do
      begin
        IY:=Round(Y);
        if (IY>=0) and (IY<Image.Height) then
        begin
          Inc(Sum,Image.Map^[IY*Image.BytesPerLine+X]);
          Inc(PixCount);
        end;
        Y:=Y+DY;
      end;

      if PixCount>0 then
      begin
        T:=Sum/PixCount;
        Variance:=Variance+Sqr(T);
        Avarage:=Avarage+T;
        Inc(LineCount);
      end;
    end;
    Avarage:=Avarage/LineCount;
    Variance:=(Variance-LineCount*Sqr(Avarage))/LineCount;
    Sync.Acquire;
    try
      if Variance>BestVariance then
      begin
        BestVariance:=Variance;
        Result:=Angle;
      end;
    finally
      Sync.Release;
    end;
  end;

begin
  Result:=0;
  if not Image.Present or (Image.Size<3) then Exit;
  BestVariance:=0;
  Sync:=TCriticalSection.Create;
  try
    ParallelFor(-Round(MaxScewAngle/AngleStep),Round(MaxScewAngle/AngleStep),@TestAngle);
  finally
    Sync.Free;
  end;
  if Result<>0 then
  begin
    if BackgroundColor<0 then BackgroundColor:=MostUsedColor(Image);
    Rotate(Image,Result,BackgroundColor,ImageGamma);
  end;
  if Assigned(ProgressUpdate) then ProgressUpdate(100);
end;

end.

