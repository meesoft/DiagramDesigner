////////////////////////////////////////////////////////////////////////////////
//
// BitmapResize.pas - Bitmap resizing utilities
// --------------------------------------------
// Version:   2005-08-19
// Maintain:  Michael Vinther         mv@logicnet·dk
//
// Last changes:
//   Resizing with non-locked aspect ratio fixed
//   Resize to height=1 fixed
//
unit BitmapResize;

interface

uses
  Windows, Graphics, SysUtils, MemUtils, LinarBitmap, Math, MathUtils;

// Resize grayscale or 24 bit image to ½ width and ½ height
procedure ResizeHalf(Image: TLinearBitmap; NewImage: TLinearBitmap=nil); overload;
procedure ResizeHalf(Image, NewImage: TLinearBitmap; ImageGamma: Single); overload;
// Resize grayscale or 24 bit image to ½ width and ½ height without lowpass filtering
procedure ResizeHalfNoFilter(Image: TLinearBitmap; NewImage: TLinearBitmap=nil);
// Resize grayscale or 24 bit image to double width and double height
procedure ResizeDouble(Image: TLinearBitmap; NewImage: TLinearBitmap=nil); overload;
procedure ResizeDouble(Image, NewImage: TLinearBitmap; ImageGamma: Single); overload;

procedure BilinearResizeImage(Image,NewImage: TLinearBitmap); overload;
procedure BilinearResizeImage(Image,NewImage: TLinearBitmap; ImageGamma: Single; Dummy: Pointer=nil); overload;

// Resize with no interpolation or filtering. If NewWidth and NewHeight is 0, size is taken from NewImage
procedure PixelResize(Image: TLinearBitmap; NewWidth,NewHeight: Integer; NewImage: TLinearBitmap=nil);

// General resize with linear interpolation if not palette color
procedure ResizeImg(Image: TLinearBitmap; X,Y: Integer; ImageGamma: Single=1);
procedure CopyResizeImg(Image, NewImage: TLinearBitmap; X,Y: Integer; ImageGamma: Single=1);

type
  TResizePlane = procedure(Image,NewImage: TLinearBitmap; ImageGamma: Single; Param: Pointer);
  TResizeLine = procedure(SrcLine,DstLine: PSingleArray; SrcWidth,DstWidth: Integer; var OptimizationData: TObject);
  TResizeLine8 = procedure(SrcLine,DstLine: PByteArray; SrcWidth,DstWidth: Integer; SrcSkip,DstSkip: Integer; var OptimizationData: TObject);

// Generel bitmap resize. TResizeLine is not thread-safe!
procedure ResizeImage(Image: TLinearBitmap; NewWidth,NewHeight: Integer; Method: TResizePlane; NewImage: TLinearBitmap=nil; ImageGamma: Single=1; Param: Pointer=nil); overload;
procedure ResizeImage(Image: TLinearBitmap; NewWidth,NewHeight: Integer; Method: TResizeLine; NewImage: TLinearBitmap=nil; ImageGamma: Single=1); overload;
procedure ResizeImage(Image: TLinearBitmap; NewWidth,NewHeight: Integer; Method: TResizeLine8; NewImage: TLinearBitmap=nil); overload;
procedure ResizeImage(Image,NewImage: TLinearBitmap; Method: TResizePlane; ImageGamma: Single=1; Param: Pointer=nil); overload;
procedure ResizeImage(Image,NewImage: TLinearBitmap; Method: TResizeLine; ImageGamma: Single=1); overload;

procedure RepeatImage(Image,NewImage: TLinearBitmap; CountX,CountY: Integer);

// Line resize
procedure FilterResampleLine(Line,NewLine: PByteArray; Width, NewWidth: Integer);

implementation

uses BitmapConversion, FloatMap,
  MultiCoreProcessing,
  BitmapGammaInterpolation, FileMappedBitmap, Interpolation, PerformanceTimer;

procedure ResizeHalf(Image,NewImage: TLinearBitmap);
type
  RGBList  = packed array[0..1] of RGBRec;
  GrayList = packed array[0..1] of Byte;
var
  OutPix_24 : ^RGBRec;
  DrawPix1_24, DrawPix2_24 : ^RGBList;
  OutPix_8 : ^Byte;
  DrawPix1_8, DrawPix2_8 : ^GrayList;
  OverwriteSource : Boolean;
  X, Y : Integer;
begin
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then NewImage:=TLinearBitmap.Create;
  NewImage.New(Image.Width div 2,Image.Height div 2,Image.PixelFormat);
  if Image.PixelFormat=pf8bit then NewImage.Palette^:=Image.Palette^;
  try
    case Image.PixelFormat of
      pf8bit  : for Y:=0 to NewImage.Height-1 do
                begin
                  OutPix_8:=NewImage.ScanLine[Y];
                  DrawPix1_8:=Image.ScanLine[Y*2];
                  DrawPix2_8:=Image.ScanLine[Y*2+1];
                  for X:=1 to NewImage.Width do
                  begin
                    OutPix_8^:=(DrawPix1_8^[0]+DrawPix2_8^[0]+DrawPix1_8^[1]+DrawPix2_8^[1]+2) shr 2;
                    Inc(OutPix_8);
                    Inc(DrawPix1_8); Inc(DrawPix2_8);
                  end;
                end;
      pf24bit : for Y:=0 to NewImage.Height-1 do
                begin
                  OutPix_24:=NewImage.ScanLine[Y];
                  DrawPix1_24:=Image.ScanLine[Y*2];
                  DrawPix2_24:=Image.ScanLine[Y*2+1];
                  for X:=1 to NewImage.Width do
                  begin
                    OutPix_24^.B:=(DrawPix1_24^[0].B+DrawPix2_24^[0].B+DrawPix1_24^[1].B+DrawPix2_24^[1].B+2) shr 2;
                    OutPix_24^.G:=(DrawPix1_24^[0].G+DrawPix2_24^[0].G+DrawPix1_24^[1].G+DrawPix2_24^[1].G+2) shr 2;
                    OutPix_24^.R:=(DrawPix1_24^[0].R+DrawPix2_24^[0].R+DrawPix1_24^[1].R+DrawPix2_24^[1].R+2) shr 2;
                    Inc(OutPix_24);
                    Inc(DrawPix1_24); Inc(DrawPix2_24);
                  end;
                end;
      else raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
  finally
    if OverwriteSource then
    begin
      Image.TakeOver(NewImage);
      NewImage.Free;
    end;
  end;
end;

procedure ResizeHalf(Image,NewImage: TLinearBitmap; ImageGamma: Single);
type
  RGBList  = packed array[0..1] of RGBRec;
  GrayList = packed array[0..1] of Byte;
var
  OutPix_24 : ^RGBRec;
  DrawPix1_24, DrawPix2_24 : ^RGBList;
  OutPix_8 : ^Byte;
  DrawPix1_8, DrawPix2_8 : ^GrayList;
  OverwriteSource : Boolean;
  GammaConverter : TGammaConverter;
  X, Y : Integer;
begin
  if ImageGamma=1 then
  begin
    ResizeHalf(Image,NewImage);
    Exit;
  end;
  GammaConverter.Prepare(ImageGamma);
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then NewImage:=TLinearBitmap.Create;
  NewImage.New(Image.Width div 2,Image.Height div 2,Image.PixelFormat);
  if Image.PixelFormat=pf8bit then NewImage.Palette^:=Image.Palette^;
  try
    case Image.PixelFormat of
      pf8bit  : for Y:=0 to NewImage.Height-1 do
                begin
                  OutPix_8:=NewImage.ScanLine[Y];
                  DrawPix1_8:=Image.ScanLine[Y*2];
                  DrawPix2_8:=Image.ScanLine[Y*2+1];
                  for X:=1 to NewImage.Width do
                  begin
                    OutPix_8^:=GammaConverter.Average4(DrawPix1_8^[0],DrawPix2_8^[0],DrawPix1_8^[1],DrawPix2_8^[1]);
                    Inc(OutPix_8);
                    Inc(DrawPix1_8); Inc(DrawPix2_8);
                  end;
                end;
      pf24bit : for Y:=0 to NewImage.Height-1 do
                begin
                  OutPix_24:=NewImage.ScanLine[Y];
                  DrawPix1_24:=Image.ScanLine[Y*2];
                  DrawPix2_24:=Image.ScanLine[Y*2+1];
                  for X:=1 to NewImage.Width do
                  begin
                    OutPix_24^.B:=GammaConverter.Average4(DrawPix1_24^[0].B,DrawPix2_24^[0].B,DrawPix1_24^[1].B,DrawPix2_24^[1].B);
                    OutPix_24^.G:=GammaConverter.Average4(DrawPix1_24^[0].G,DrawPix2_24^[0].G,DrawPix1_24^[1].G,DrawPix2_24^[1].G);
                    OutPix_24^.R:=GammaConverter.Average4(DrawPix1_24^[0].R,DrawPix2_24^[0].R,DrawPix1_24^[1].R,DrawPix2_24^[1].R);
                    Inc(OutPix_24);
                    Inc(DrawPix1_24); Inc(DrawPix2_24);
                  end;
                end;
      else raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
  finally
    if OverwriteSource then
    begin
      Image.TakeOver(NewImage);
      NewImage.Free;
    end;
  end;
end;

procedure ResizeHalfNoFilter(Image,NewImage: TLinearBitmap);
type
  RGBList  = packed array[0..1] of RGBRec;
  GrayList = packed array[0..1] of Byte;
var
  Pix24, NewPix24 : ^RGBRec;
  Pix8, NewPix8 : PByte;
  OverwriteSource : Boolean;
  X, Y : Integer;
begin
  OverwriteSource:=(NewImage=Image) or (NewImage=nil);
  if OverwriteSource then NewImage:=TLinearBitmap.Create;
  NewImage.New(Image.Width div 2,Image.Height div 2,Image.PixelFormat);
  if Image.PixelFormat=pf8bit then NewImage.Palette^:=Image.Palette^;
  try
    case Image.PixelFormat of
      pf8bit  : for Y:=0 to NewImage.Height-1 do
                begin
                  Pix8:=Pointer(DWord(Image.ScanLine[2*Y+1])+1);
                  NewPix8:=NewImage.ScanLine[Y];
                  for X:=1 to NewImage.Width do
                  begin
                    NewPix8^:=Pix8^;
                    Inc(NewPix8);
                    Inc(Pix8,2);
                  end;
                end;
      pf24bit : for Y:=0 to NewImage.Height-1 do
                begin
                  Pix24:=Pointer(DWord(Image.ScanLine[2*Y+1])+3);
                  NewPix24:=NewImage.ScanLine[Y];
                  for X:=1 to NewImage.Width do
                  begin
                    NewPix24^:=Pix24^;
                    Inc(NewPix24);
                    Inc(Pix24,2);
                  end;
                end;
    end;
  finally
    if OverwriteSource then
    begin
      Image.TakeOver(NewImage);
      NewImage.Free;
    end;
  end;
end;

procedure ResizeDouble(Image,NewImage: TLinearBitmap);
var
  X, Y, Plane, Planes, NextLine, NextLineNextPix : Cardinal;
  OverwriteSource : Boolean;
  NewPix, Pix : PByte;
  List : array[0..3] of Cardinal;
begin
  with Image do
  begin
    OverwriteSource:=(NewImage=Image) or (NewImage=nil);
    if OverwriteSource then NewImage:=TLinearBitmap.Create;
    NewImage.New(Width*2,Height*2,PixelFormat);
    if PixelFormat=pf8bit then NewImage.Palette^:=Palette^;
    try
      if Image.PixelFormat=pf24bit then Planes:=3
      else Planes:=1;

      NextLine:=NewImage.BytesPerLine;
      NextLineNextPix:=NextLine+Planes;

      for Plane:=0 to Planes-1 do
      begin
        for Y:=0 to Height-2 do
        begin
          // [0 1]
          // [   ]
          // [2 3]
          Pix:=@Map^[Y*Cardinal(BytesPerLine)+Plane];
          List[1]:=Pix^;
          List[3]:=PByte(DWord(Pix)+DWord(BytesPerLine))^;
          Inc(Pix,Planes);

          NewPix:=@NewImage.Map^[(2*Y+1)*Cardinal(NewImage.BytesPerLine)+Plane];
          NewPix^:=List[1];
          PByte(DWord(NewPix)+NextLine)^:=(List[1]+List[3]+1) div 2;
          Inc(NewPix,Planes);
          for X:=0 to Width-2 do
          begin
            List[0]:=List[1];
            List[2]:=List[3];
            List[1]:=Pix^;
            List[3]:=PByte(DWord(Pix)+DWord(BytesPerLine))^;

            NewPix^:=List[0];
            PByte(DWord(NewPix)+Planes)^:=(List[0]+List[1]+1) div 2;
            PByte(DWord(NewPix)+NextLine)^:=(List[0]+List[2]+1) div 2;
            PByte(DWord(NewPix)+NextLineNextPix)^:=(List[0]+List[1]+List[2]+List[3]+2) div 4;

            Inc(Pix,Planes);
            Inc(NewPix,Planes*2);
          end;
          NewPix^:=List[1];
          PByte(DWord(NewPix)+NextLine)^:=(List[1]+List[3]+1) div 2;
        end;
      end;
      Move(NewImage.ScanLine[1]^,NewImage.ScanLine[0]^,NextLine);
      Move(NewImage.ScanLine[NewImage.Height-2]^,NewImage.ScanLine[NewImage.Height-1]^,NextLine);
    finally
      if OverwriteSource then
      begin
        Image.TakeOver(NewImage);
        NewImage.Free;
      end;
    end;
  end;
end;

procedure ResizeDouble(Image,NewImage: TLinearBitmap; ImageGamma: Single);
var
  X, Y, Plane, Planes, NextLine, NextLineNextPix : Cardinal;
  OverwriteSource : Boolean;
  NewPix, Pix : PByte;
  List : array[0..3] of Cardinal;
  Average2GammaLUT : TAverage2GammaLUT;
  GammaConverter : TGammaConverter;
begin
  if ImageGamma=1 then ResizeDouble(Image,NewImage)
  else
  with Image do
  begin
    OverwriteSource:=(NewImage=Image) or (NewImage=nil);
    if OverwriteSource then NewImage:=TLinearBitmap.Create;
    NewImage.New(Width*2,Height*2,PixelFormat);
    if PixelFormat=pf8bit then NewImage.Palette^:=Palette^;
    try
      if Image.PixelFormat=pf24bit then Planes:=3
      else Planes:=1;

      GammaConverter.Prepare(ImageGamma);
      CreateGammaLUT(ImageGamma,Average2GammaLUT);

      NextLine:=NewImage.BytesPerLine;
      NextLineNextPix:=NextLine+Planes;

      for Plane:=0 to Planes-1 do
      begin
        for Y:=0 to Height-2 do
        begin
          // [0 1]
          // [   ]
          // [2 3]
          Pix:=@Map^[Y*Cardinal(BytesPerLine)+Plane];
          List[1]:=Pix^;
          List[3]:=PByte(DWord(Pix)+DWord(BytesPerLine))^;
          Inc(Pix,Planes);

          NewPix:=@NewImage.Map^[(2*Y+1)*Cardinal(NewImage.BytesPerLine)+Plane];
          NewPix^:=List[1];
          PByte(DWord(NewPix)+NextLine)^:=Average2GammaLUT[List[1],List[3]];
          Inc(NewPix,Planes);
          for X:=0 to Width-2 do
          begin
            List[0]:=List[1];
            List[2]:=List[3];
            List[1]:=Pix^;
            List[3]:=PByte(DWord(Pix)+DWord(BytesPerLine))^;

            NewPix^:=List[0];
            PByte(DWord(NewPix)+Planes)^:=Average2GammaLUT[List[0],List[1]];
            PByte(DWord(NewPix)+NextLine)^:=Average2GammaLUT[List[0],List[2]];
            PByte(DWord(NewPix)+NextLineNextPix)^:=GammaConverter.Average4(List[0],List[1],List[2],List[3]);

            Inc(Pix,Planes);
            Inc(NewPix,Planes*2);
          end;
          NewPix^:=List[1];
          PByte(DWord(NewPix)+NextLine)^:=Average2GammaLUT[List[1],List[3]];
        end;
      end;
      Move(NewImage.ScanLine[1]^,NewImage.ScanLine[0]^,NextLine);
      Move(NewImage.ScanLine[NewImage.Height-2]^,NewImage.ScanLine[NewImage.Height-1]^,NextLine);
    finally
      if OverwriteSource then
      begin
        Image.TakeOver(NewImage);
        NewImage.Free;
      end;
    end;
  end;
end;

procedure FilterResampleLine(Line,NewLine: PByteArray; Width, NewWidth: Integer);
var
  NewI, FI, HFilterWidth, Sum, I : Integer;
  Scale, InvScale, NeighbourWeight, CenterWeight : Double;
begin
  Scale:=NewWidth/Width; // Scale>1
  HFilterWidth:=Ceil(Scale/2);
  CenterWeight:=1/Scale;
  if HFilterWidth>0 then NeighbourWeight:=(1-CenterWeight)/(2*HFilterWidth)
  else NeighbourWeight:=0;

  InvScale:=(Width-1)/(NewWidth-1);
  for NewI:=0 to NewWidth-1 do
  begin
    Sum:=0;
    for FI:=1 to HFilterWidth do
    begin
      I:=Round((NewI-FI)*InvScale);
      if I<0 then Inc(Sum,Line^[0])
      else Inc(Sum,Line^[I]);
      I:=Round((NewI+FI)*InvScale);
      if I>=Width then Inc(Sum,Line^[Width-1])
      else Inc(Sum,Line^[I]);
    end;
    NewLine^[NewI]:=Round(Sum*NeighbourWeight+Line^[Round(NewI*InvScale)]*CenterWeight);
  end;
end;

procedure PixelResize(Image: TLinearBitmap; NewWidth,NewHeight: Integer; NewImage: TLinearBitmap=nil);
var
  NX, NY, IY : Integer;
  X, Y, XInc, YInc : Double;
  TempImage : Boolean;
begin
  TempImage:=NewImage=nil;
  if TempImage then NewImage:=TLinearBitmap.Create(NewWidth,NewHeight,Image.PixelFormat)
  else if (NewWidth>0) and (NewHeight>0) then NewImage.New(NewWidth,NewHeight,Image.PixelFormat);
  try
    XInc:=Image.Width/NewImage.Width;
    YInc:=Image.Height/NewImage.Height;
    if Image.PixelFormat=pf24bit then
    begin
      Y:=0;
      for NY:=0 to NewImage.Height-1 do
      begin
        X:=0;
        IY:=Trunc(Y)*Image.BytesPerLine;
        for NX:=0 to NewImage.Width-1 do
        begin
          PRGBRec(@NewImage.Map^[NX*3+NY*NewImage.BytesPerLine])^:=PRGBRec(@Image.Map^[Trunc(X)*3+IY])^;
          X:=X+XInc;
        end;
        Y:=Y+YInc;
      end;
    end
    else // 8 bit
    begin
      NewImage.Palette^:=Image.Palette^;
      Y:=0;
      for NY:=0 to NewImage.Height-1 do
      begin
        X:=0;
        IY:=Trunc(Y)*Image.BytesPerLine;
        for NX:=0 to NewImage.Width-1 do
        begin
          NewImage.Map^[NX+NY*NewImage.BytesPerLine]:=Image.Map^[Trunc(X)+IY];
          X:=X+XInc;
        end;
        Y:=Y+YInc;
      end;
    end;
    if TempImage then Image.TakeOver(NewImage);
  finally
    if TempImage then NewImage.Free;
  end;
end;

procedure ReduceGrayscale(Image,NewImage: TLinearBitmap); overload;
var
 X, Y, X1, Y1, LX, LY, XP, YP, C : Integer;
 XInc, YInc, GX, GY : Double;
 NewPix : ^Byte;
begin
  if NewImage.Width=1 then XInc:=0
  else XInc:=(Image.Width-1)/(NewImage.Width-1);
  if NewImage.Height=1 then YInc:=0
  else YInc:=(Image.Height-1)/(NewImage.Height-1);
  GY:=0; LY:=0;
  NewPix:=@NewImage.Map^;
  for Y:=0 to NewImage.Height-1 do
  begin
    GX:=0; LX:=0;
    Y1:=Round(GY);
    for X:=0 to NewImage.Width-1 do
    begin
     X1:=Round(GX);
     C:=0;
     for YP:=LY to Y1 do
       for XP:=LX to X1 do Inc(C,Image.Map^[YP*Image.BytesPerLine+XP]);
     NewPix^:=Round(C/((X1-LX+1)*(Y1-LY+1)));

     LX:=X1+1;
     GX:=GX+XInc;
     Inc(NewPix);
     if Assigned(ProgressUpdate) then ProgressUpdate((Y+1)*100 div NewImage.Height);
    end;
    LY:=Y1+1;
    GY:=GY+YInc;
  end;
end;

procedure ReduceGrayscale(Image,NewImage: TLinearBitmap; ImageGamma: Single); overload;
var
 X, Y, X1, Y1, LX, LY, XP, YP : Integer;
 C : Single;
 XInc, YInc, GX, GY : Double;
 NewPix : ^Byte;
 GammaConverter : TGammaConverter;
begin
  if ImageGamma=1 then
  begin
    ReduceGrayscale(Image,NewImage);
    Exit;
  end;
  GammaConverter.Prepare(ImageGamma);
  if NewImage.Width=1 then XInc:=0
  else XInc:=(Image.Width-1)/(NewImage.Width-1);
  if NewImage.Height=1 then YInc:=0
  else YInc:=(Image.Height-1)/(NewImage.Height-1);
  GY:=0; LY:=0;
  NewPix:=@NewImage.Map^;
  for Y:=0 to NewImage.Height-1 do
  begin
    GX:=0; LX:=0;
    Y1:=Round(GY);
    for X:=0 to NewImage.Width-1 do
    begin
     X1:=Round(GX);
     C:=0;
     for YP:=LY to Y1 do
       for XP:=LX to X1 do C:=C+GammaConverter.InvGammaLUT[Image.Map^[YP*Image.BytesPerLine+XP]];
     NewPix^:=GammaConverter.GammaLUT[Round(C*256/((X1-LX+1)*(Y1-LY+1)))];
     LX:=X1+1;
     GX:=GX+XInc;
     Inc(NewPix);
     if Assigned(ProgressUpdate) then ProgressUpdate((Y+1)*100 div NewImage.Height);
    end;
    LY:=Y1+1;
    GY:=GY+YInc;
  end;
end;

type
  TBilinearResizeProcess = class(TMultiCoreProcess)
  protected
    Image, NewImage: TLinearBitmap;
    class procedure SetupScaling(Size,NewSize: Integer; out Offset,Step: Double);
  end;

  TBilinearGammaResizeProcess = class(TBilinearResizeProcess)
  protected
    GammaConverter : TGammaConverter;
  end;

  TBilinearResizeProcess8bit = class(TBilinearResizeProcess) // 8 bit grayscale
  protected
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

  TBilinearGammaResizeProcess8bit = class(TBilinearGammaResizeProcess) // 8 bit grayscale with gamma interpolation
  protected
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

  TBilinearResizeProcess16bit = class(TBilinearResizeProcess) // 16 bit grayscale
  protected
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

  TBilinearResizeProcess24bit = class(TBilinearResizeProcess) // 24 bit color
  protected
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

  TBilinearResizeProcess32bit = class(TBilinearResizeProcess) // 32 bit color
  protected
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

  TBilinearGammaResizeProcess24bit = class(TBilinearGammaResizeProcess) // 24 bit color with gamma interpolation
  protected
    procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
  end;

class procedure TBilinearResizeProcess.SetupScaling(Size, NewSize: Integer; out Offset, Step: Double);
var
  Scale : Double;
begin
  if NewSize<=1 then
  begin
    Offset:=0.5;
    Step:=0;
  end
  else
  if NewSize>=Size then // >=100%
  begin
    Offset:=0;
    Step:=(Size-1)/(NewSize-1);
  end
  else // 50% - 100%
  begin
    Scale:=NewSize/Size;
    if Scale>=0.5 then
    begin
      Offset:=1-Scale;
      Step:=(Size-1-2*Offset)/(NewSize-1);
    end
    else // <50%
    begin
      Offset:=0.5;
      Step:=(Size-2)/(NewSize-1);;
    end;
  end;
end;

procedure TBilinearResizeProcess8bit.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NX, NY, TruncX, TruncY : Integer;
  NewPix : PByte;
  Line : PByteArray;
  X, Y, XOffset, YOffset, DX, DY, FracX, FracY : Double;
  ThreadData : PThreadDataStartStop;
begin
  ThreadData:=Data;
  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  Y:=ThreadData.Start*DY+YOffset;
  with Image do
    for NY:=ThreadData.Start to ThreadData.Stop do
    begin
      NewPix:=NewImage.ScanLineSafe[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*BytesPerLine];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        NewPix^:=Round(Line^[TruncX]*(1-FracX)*(1-FracY)              + Line^[TruncX+1]*(  FracX)*(1-FracY) +
                       Line^[TruncX+BytesPerLine]*(1-FracX)*(  FracY) + Line^[TruncX+BytesPerLine+1]*(  FracX)*(  FracY));
        X:=X+DX;
        Inc(NewPix);
      end;
      Y:=Y+DY;
    end
end;

procedure TBilinearGammaResizeProcess8bit.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NX, NY, TruncX, TruncY : Integer;
  NewPix : PByte;
  Line : PByteArray;
  X, Y, XOffset, YOffset, DX, DY, FracX, FracY : Double;
  ThreadData : PThreadDataStartStop;
begin
  ThreadData:=Data;
  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  Y:=ThreadData.Start*DY+YOffset;
  with Image do
    for NY:=ThreadData.Start to ThreadData.Stop do
    begin
      NewPix:=NewImage.ScanLineSafe[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*BytesPerLine];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        NewPix^:=GammaConverter.GammaLUT[Round(256*
                 (GammaConverter.InvGammaLUT[Line^[TruncX]]*(1-FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Line^[TruncX+1]]*(FracX)*(1-FracY) +
                  GammaConverter.InvGammaLUT[Line^[TruncX+BytesPerLine]]*(1-FracX)*(FracY) +
                  GammaConverter.InvGammaLUT[Line^[TruncX+BytesPerLine+1]]*(FracX)*(FracY)))];
        X:=X+DX;
        Inc(NewPix);
      end;
      Y:=Y+DY;
    end
end;

procedure TBilinearResizeProcess16bit.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NX, NY, TruncX, TruncY : Integer;
  NewPix : PByte;
  NewPix16 : PWord absolute NewPix;
  Line : PByteArray;
  Line16 : PWordArray absolute Line;
  X, Y, XOffset, YOffset, DX, DY, FracX, FracY : Double;
  ThreadData : PThreadDataStartStop;
begin
  ThreadData:=Data;
  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  Y:=ThreadData.Start*DY+YOffset;
  with Image do
    for NY:=ThreadData.Start to ThreadData.Stop do
    begin
      NewPix:=NewImage.ScanLineSafe[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*BytesPerLine];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        NewPix16^:=Round(Line16^[TruncX]*(1-FracX)*(1-FracY)       + Line16^[TruncX+1]*(  FracX)*(1-FracY) +
                         Line16^[TruncX+Width]*(1-FracX)*(  FracY) + Line16^[TruncX+Width+1]*(  FracX)*(  FracY));
        X:=X+DX;
        Inc(NewPix16);
      end;
      Y:=Y+DY;
    end;
end;

procedure TBilinearResizeProcess24bit.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NX, NY, TruncX, TruncY : Integer;
  NewPix : PByte;
  Pix, Line : PByteArray;
  X, Y, XOffset, YOffset, DX, DY, FracX, FracY : Double;
  ThreadData : PThreadDataStartStop;
begin
  ThreadData:=Data;
  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  Y:=ThreadData.Start*DY+YOffset;
  with Image do
    for NY:=ThreadData.Start to ThreadData.Stop do
    begin
      NewPix:=NewImage.ScanLineSafe[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*BytesPerLine];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        Pix:=@Line^[TruncX*3];
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[3]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+3]*(  FracX)*(  FracY));
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[3]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+3]*(  FracX)*(  FracY));
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[3]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+3]*(  FracX)*(  FracY));
        Inc(NewPix);

        X:=X+DX;
      end;
      Y:=Y+DY;
    end
end;

procedure TBilinearResizeProcess32bit.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NX, NY, TruncX, TruncY : Integer;
  NewPix : PByte;
  Pix, Line : PByteArray;
  X, Y, XOffset, YOffset, DX, DY, FracX, FracY : Double;
  ThreadData : PThreadDataStartStop;
begin
  ThreadData:=Data;
  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  Y:=ThreadData.Start*DY+YOffset;
  with Image do
    for NY:=ThreadData.Start to ThreadData.Stop do
    begin
      NewPix:=NewImage.ScanLineSafe[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*BytesPerLine];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        Pix:=@Line^[TruncX*4];
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[4]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+4]*(  FracX)*(  FracY));
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[4]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+4]*(  FracX)*(  FracY));
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[4]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+4]*(  FracX)*(  FracY));
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=Round(Pix^[0]*           (1-FracX)*(1-FracY) + Pix^[4]*             (  FracX)*(1-FracY) +
                       Pix^[BytesPerLine]*(1-FracX)*(  FracY) + Pix^[BytesPerLine+4]*(  FracX)*(  FracY));
        Inc(NewPix);

        X:=X+DX;
      end;
      Y:=Y+DY;
    end
end;

procedure TBilinearGammaResizeProcess24bit.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NX, NY, TruncX, TruncY : Integer;
  NewPix : PByte;
  Pix, Line : PByteArray;
  X, Y, XOffset, YOffset, DX, DY, FracX, FracY : Double;
  ThreadData : PThreadDataStartStop;
begin
  ThreadData:=Data;
  SetupScaling(Image.Width,NewImage.Width,XOffset,DX);
  SetupScaling(Image.Height,NewImage.Height,YOffset,DY);
  Y:=ThreadData.Start*DY+YOffset;
  with Image do
    for NY:=ThreadData.Start to ThreadData.Stop do
    begin
      NewPix:=NewImage.ScanLineSafe[NY];
      X:=XOffset;
      TruncY:=Trunc(Y);
      if TruncY>=Height-1 then
      begin
        TruncY:=Height-2;
        FracY:=1;
      end
      else FracY:=Frac(Y);

      Line:=@Map^[TruncY*BytesPerLine];
      for NX:=0 to NewImage.Width-1 do
      begin
        TruncX:=Trunc(X);
        FracX:=Frac(X);
        Pix:=@Line^[TruncX*3];
        NewPix^:=GammaConverter.GammaLUT[Round(256*
                 (GammaConverter.InvGammaLUT[Pix^[0]]*(1-FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Pix^[3]]*(FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Pix^[BytesPerLine]]*(1-FracX)*(FracY)+
                  GammaConverter.InvGammaLUT[Pix^[BytesPerLine+3]]*(FracX)*(FracY)))];
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=GammaConverter.GammaLUT[Round(256*
                 (GammaConverter.InvGammaLUT[Pix^[0]]*(1-FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Pix^[3]]*(FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Pix^[BytesPerLine]]*(1-FracX)*(FracY)+
                  GammaConverter.InvGammaLUT[Pix^[BytesPerLine+3]]*(FracX)*(FracY)))];
        Inc(NewPix); Inc(Integer(Pix));
        NewPix^:=GammaConverter.GammaLUT[Round(256*
                 (GammaConverter.InvGammaLUT[Pix^[0]]*(1-FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Pix^[3]]*(FracX)*(1-FracY)+
                  GammaConverter.InvGammaLUT[Pix^[BytesPerLine]]*(1-FracX)*(FracY)+
                  GammaConverter.InvGammaLUT[Pix^[BytesPerLine+3]]*(FracX)*(FracY)))];
        Inc(NewPix);
        X:=X+DX;
      end;
      Y:=Y+DY;
    end
end;

procedure BilinearResizeImage(Image,NewImage: TLinearBitmap);
var
  Process : TBilinearResizeProcess;
  ThreadData : TThreadDataStartStop;
begin
  if Image.PixelFormat=pf8bit then Process:=TBilinearResizeProcess8bit.Create
  else if Image.PixelFormat=pf16bit then Process:=TBilinearResizeProcess16bit.Create
  else if Image.PixelFormat=pf24bit then Process:=TBilinearResizeProcess24bit.Create
  else if Image.PixelFormat=pf32bit then Process:=TBilinearResizeProcess32bit.Create
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
  try
    Process.Image:=Image;
    Process.NewImage:=NewImage;
    if (NewImage is TFileMappedBitmap) and TFileMappedBitmap(NewImage).MustUpdateScanLine then
    begin
      ThreadData.Start:=0;
      ThreadData.Stop:=NewImage.Height-1;
      ThreadData.ThreadIndex:=0;
      Process.ProcessMethod(nil,@ThreadData);
    end
    else Process.ExecuteStartStop(0,NewImage.Height-1,64);
  finally
    Process.Free;
  end;
end;

procedure BilinearResizeImage(Image,NewImage: TLinearBitmap; ImageGamma: Single; Dummy: Pointer);
var
  Process : TBilinearGammaResizeProcess;
begin
  if ImageGamma=1 then
  begin
    BilinearResizeImage(Image,NewImage);
    Exit;
  end;
  if Image.PixelFormat=pf8bit then Process:=TBilinearGammaResizeProcess8bit.Create
  else if Image.PixelFormat=pf24bit then Process:=TBilinearGammaResizeProcess24bit.Create
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
  try
    Process.Image:=Image;
    Process.NewImage:=NewImage;
    Process.GammaConverter.Prepare(ImageGamma);
    Process.ExecuteStartStop(0,NewImage.Height-1,64);
  finally
    Process.Free;
  end;
end;

procedure Reduce24bit(Image,NewImage: TLinearBitmap); overload;
var
  X, Y, X1, Y1, LX, LY, XP, YP, R, G, B : Integer;
  XInc, YInc, GX, GY, D : Double;
  NewPix, CurPix : ^RGBRec;
begin
  if NewImage.Width=1 then XInc:=0
  else XInc:=(Image.Width-1)/(NewImage.Width-1);
  if NewImage.Height=1 then YInc:=0
  else YInc:=(Image.Height-1)/(NewImage.Height-1);
  GY:=0; LY:=0;
  NewPix:=@NewImage.Map^;
  for Y:=0 to NewImage.Height-1 do
  begin
   GX:=0; LX:=0;
   Y1:=Round(GY);
   for X:=0 to NewImage.Width-1 do
   begin
    X1:=Round(GX);
    R:=0; G:=0; B:=0;
    for YP:=LY to Y1 do
     for XP:=LX to X1 do
    begin
     CurPix:=@Image.Map^[YP*Image.BytesPerLine+XP*3];
     Inc(R,CurPix^.R); Inc(G,CurPix^.G); Inc(B,CurPix^.B);
    end;
    D:=1/((X1-LX+1)*(Y1-LY+1));
    NewPix^.R:=Round(R*D); NewPix^.G:=Round(G*D); NewPix^.B:=Round(B*D);

    LX:=X1+1;
    GX:=GX+XInc;
    Inc(NewPix);
   end;
   LY:=Y1+1;
   GY:=GY+YInc;
   if Assigned(ProgressUpdate) then ProgressUpdate((Y+1)*100 div NewImage.Height);
  end;
end;

procedure Reduce24bit(Image,NewImage: TLinearBitmap; ImageGamma: Single); overload;
var
  XInc, YInc : Double;
  GammaConverter : TGammaConverter;

  procedure ProcessLine(Y: Integer);
  var
    X, X1, Y1, LX, LY, XP, YP : Integer;
    R, G, B : Single;
    GX, GY, D : Double;
    NewPix, CurPix : ^RGBRec;
  begin
   NewPix:=NewImage.ScanLine[Y];
   GY:=Y*YInc;
   Y1:=Round(GY);
   if Y=0 then LY:=0
   else LY:=Round((Y-1)*YInc)+1;
   GX:=0; LX:=0;
   for X:=0 to NewImage.Width-1 do
   begin
    X1:=Round(GX);
    R:=0; G:=0; B:=0;
    for YP:=LY to Y1 do
      for XP:=LX to X1 do
      begin
       CurPix:=@Image.Map^[YP*Image.BytesPerLine+XP*3];
       B:=B+GammaConverter.InvGammaLUT[CurPix^.B];
       G:=G+GammaConverter.InvGammaLUT[CurPix^.G];
       R:=R+GammaConverter.InvGammaLUT[CurPix^.R];
      end;
    D:=256/((X1-LX+1)*(Y1-LY+1));
    NewPix^.R:=GammaConverter.GammaLUT[Round(R*D)];
    NewPix^.G:=GammaConverter.GammaLUT[Round(G*D)];
    NewPix^.B:=GammaConverter.GammaLUT[Round(B*D)];

    LX:=X1+1;
    GX:=GX+XInc;
    Inc(NewPix);
   end;
  end;

begin
  if ImageGamma=1 then
  begin
    Reduce24bit(Image,NewImage);
    Exit;
  end;
  GammaConverter.Prepare(ImageGamma);
  if NewImage.Width=1 then XInc:=0
  else XInc:=(Image.Width-1)/(NewImage.Width-1);
  if NewImage.Height=1 then YInc:=0
  else YInc:=(Image.Height-1)/(NewImage.Height-1);
  ParallelFor(0,NewImage.Height-1,@ProcessLine);
end;

procedure ResizeImg(Image: TLinearBitmap; X,Y: Integer; ImageGamma: Single);
var
  NewImage : TLinearBitmap;
begin
  if (Image.Width=X) and (Image.Height=Y) then Exit;
  if (X<1) or (Y<1) then raise Exception.Create('Invalid image size');
  NewImage:=TLinearBitmap.Create;
  try
    CopyResizeImg(Image,NewImage,X,Y,ImageGamma);
    Image.TakeOver(NewImage);
  finally
    NewImage.Free;
  end;
end;

procedure CopyResizeImg(Image,NewImage: TLinearBitmap; X,Y: Integer; ImageGamma: Single);
begin
  if (Image.Width<1) or (X<1) or (Y<1) then
  begin
    NewImage.Dispose;
    Exit;
  end;

  NewImage.New(X,Y,Image.PixelFormat);
  if Image.PixelFormat=pf8bit then NewImage.Palette^:=Image.Palette^;

  if (Image.Width=X) and (Image.Height=Y) then
    Move(Image.Map^,NewImage.Map^,NewImage.Size) // 100%, just copy

  else if (Image.PixelFormat=pf8bit) and not Image.IsGrayScale then // Palette image
    PixelResize(Image,X,Y,NewImage)

  else if ((X<Image.Width) and (Y>Image.Height)) or   // Strange aspect ratio
          ((X>Image.Width) and (Y<Image.Height)) or
          (Image.PixelFormat=pf16bit) then
    BilinearResizeImage(Image,NewImage,ImageGamma)

  else if (X=Image.Width div 2) and (Y=Image.Height div 2) then // 50%
    ResizeHalf(Image,NewImage,ImageGamma)

  else if (X=Image.Width*2) and (Y=Image.Height*2) then // 200%
    ResizeDouble(Image,NewImage,ImageGamma)

  else if (NewImage.Width*2<Image.Width) or (NewImage.Height*2<Image.Height) then // <50%
    begin
      if Image.PixelFormat=pf24bit then
        Reduce24bit(Image,NewImage,ImageGamma)
      else
        ReduceGrayscale(Image,NewImage,ImageGamma);
    end

  else
    BilinearResizeImage(Image,NewImage,ImageGamma); // >50%
end;

type
  TResizeImageProcess = class(TMultiCoreProcess)
    protected
      procedure ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer); override;
    public
      NewWidth,NewHeight: Integer;
      Method : TResizePlane;
      Param : Pointer;
      Planes : T3PlanesArray;
      ImageGamma : Single;
      procedure ResizeImage;
    end;

procedure TResizeImageProcess.ProcessMethod(Thread: TMultiCoreProcessThread; Data: Pointer);
var
  NewPlane : TLinearBitmap;
  I : Integer;
begin
  I:=Integer(Data);
  NewPlane:=TLinarBitmap.Create(NewWidth,NewHeight,pf8bit);
  try
    Method(Planes[I],NewPlane,ImageGamma,Param);
  finally
    SwapDWords(Planes[I],NewPlane);
    NewPlane.Free;
  end;
end;

procedure TResizeImageProcess.ResizeImage;
var
  ThreadData : TThreadDataArray;
  I : Integer;
begin
  if NumberOfCores=1 then
  begin
    for I:=1 to 3 do ProcessMethod(nil,Pointer(I));
  end
  else
  begin
    SetLength(ThreadData,3);
    for I:=1 to 3 do ThreadData[I-1]:=Pointer(I);
    ExecuteProcess(ThreadData);
  end;
end;

procedure ResizeImage(Image: TLinearBitmap; NewWidth,NewHeight: Integer; Method: TResizePlane; NewImage: TLinearBitmap; ImageGamma: Single; Param: Pointer);
var
  OverwriteSource : Boolean;
  ResizeImageProcess : TResizeImageProcess;
begin
  OverwriteSource:=(NewImage=nil) or (NewImage=Image);
  if (Image.Width=NewWidth) and (Image.Height=NewHeight) then
  begin
    if not OverwriteSource then NewImage.Assign(Image);
    Exit;
  end;

  if not Assigned(Method) then Method:=BilinearResizeImage;

  if Image.PixelFormat=pf24bit then
  begin
    if not OverwriteSource and (NewWidth=0) and (NewHeight=0) then
    begin
      NewWidth:=NewImage.Width;
      NewHeight:=NewImage.Height;
    end;
    ResizeImageProcess:=TResizeImageProcess.Create;
    try
      ResizeImageProcess.NewWidth:=NewWidth;
      ResizeImageProcess.NewHeight:=NewHeight;
      ResizeImageProcess.NewHeight:=NewHeight;
      ResizeImageProcess.ImageGamma:=ImageGamma;
      ResizeImageProcess.Method:=Method;
      ResizeImageProcess.Param:=Param;
      SplitColorPlanesRGBCreate(Image,ResizeImageProcess.Planes);
      if OverwriteSource then Image.Dispose;
      ResizeImageProcess.ResizeImage;
      if OverwriteSource then
        CombineColorPlanesRGB(ResizeImageProcess.Planes[1],ResizeImageProcess.Planes[2],ResizeImageProcess.Planes[3],Image)
      else
        CombineColorPlanesRGB(ResizeImageProcess.Planes[1],ResizeImageProcess.Planes[2],ResizeImageProcess.Planes[3],NewImage);
    finally
      FreeColorPlanes(ResizeImageProcess.Planes);
      ResizeImageProcess.Free;
    end;
  end
  else // 8 bit
  begin
    if OverwriteSource then NewImage:=TLinearBitmap.Create(NewWidth,NewHeight,pf8bit)
    else if (NewWidth>0) or (NewHeight>0) then NewImage.New(NewWidth,NewHeight,pf8bit);
    try
      Method(Image,NewImage,ImageGamma,Param);
      if OverwriteSource then
      begin
        SwapDWords(NewImage.Palette,Image.Palette);
        Image.TakeOver(NewImage);
      end
      else NewImage.Palette^:=Image.Palette^;
    finally
      if OverwriteSource then NewImage.Free;
    end;
  end;
end;

procedure ResizeImage(Image,NewImage: TLinearBitmap; Method: TResizePlane; ImageGamma: Single; Param: Pointer);
begin
  ResizeImage(Image,0,0,Method,NewImage,ImageGamma,Param);
end;

procedure ResizePlaneLineBased(Image,NewImage: TLinearBitmap; ImageGamma: Single; Param: Pointer);
var
  FloatMap1, FloatMap2 : TFloatMap32;
  OptimizationData : TObject;
  ResizeLineMethod : TResizeLine;

  procedure ResizeRow(I: Integer);
  begin
    ResizeLineMethod(FloatMap1.ScanLine[I],FloatMap2.ScanLine[I],FloatMap1.Width,FloatMap2.Width,OptimizationData);
    if Assigned(ProgressUpdate) and (GetCurrentThreadId=MainThreadID) then
      ProgressUpdate(I*50 div FloatMap1.Height);
  end;

  procedure ResizeColumn(I: Integer);
  begin
    ResizeLineMethod(FloatMap2.ScanLine[I],FloatMap1.ScanLine[I],FloatMap2.Width,FloatMap1.Width,OptimizationData);
    if Assigned(ProgressUpdate) and (GetCurrentThreadId=MainThreadID) then
      ProgressUpdate(50+I*50 div FloatMap1.Height);
  end;

begin
  ResizeLineMethod:=Param;
  FloatMap1:=TFloatMap32.Create;
  FloatMap2:=TFloatMap32.Create(NewImage.Width,Image.Height);
  try
    ConvertBitmapToFloatMap(Image,FloatMap1,1/ImageGamma);
    OptimizationData:=nil;
    ResizeLineMethod(nil,nil,FloatMap1.Width,FloatMap2.Width,OptimizationData); // Create optimization data
    try
      ParallelFor(0,FloatMap1.Height-1,@ResizeRow);
    finally
      FreeAndNil(OptimizationData);
    end;
    FloatMap2.Transpose;
    FloatMap1.New(NewImage.Height,NewImage.Width);
    ResizeLineMethod(nil,nil,FloatMap2.Width,FloatMap1.Width,OptimizationData); // Create optimization data
    try
      ParallelFor(0,FloatMap1.Height-1,@ResizeColumn);
    finally
      FreeAndNil(OptimizationData);
    end;
    FloatMap1.Transpose;
    ConvertFloatMapToBitmap(FloatMap1,NewImage,ImageGamma);
    if Assigned(ProgressUpdate) and (GetCurrentThreadId=MainThreadID) then
      ProgressUpdate(100);
  finally
    FloatMap1.Free;
    FloatMap2.Free;
  end;
end;

procedure ResizeImage(Image: TLinearBitmap; NewWidth,NewHeight: Integer; Method: TResizeLine; NewImage: TLinearBitmap; ImageGamma: Single); overload;
begin
  ResizeImage(Image,NewWidth,NewHeight,ResizePlaneLineBased,NewImage,ImageGamma,@Method);
end;

procedure ResizeImage(Image,NewImage: TLinearBitmap; Method: TResizeLine; ImageGamma: Single); overload;
begin
  ResizeImage(Image,0,0,ResizePlaneLineBased,NewImage,ImageGamma,@Method);
end;

procedure ResizeImage(Image: TLinearBitmap; NewWidth,NewHeight: Integer; Method: TResizeLine8; NewImage: TLinearBitmap=nil); overload;
var
  OverwriteSource : Boolean;
  HorzImage : TLinearBitmap;
  OptimizationData: TObject;

  procedure ProcessLine(Y: Integer);
  var
    SrcLine, DstLine : PByteArray;
  begin
    SrcLine:=Image.ScanLine[Y];
    DstLine:=HorzImage.ScanLine[Y];
    if Image.PixelFormat=pf8bit then
      Method(SrcLine,DstLine,Image.Width,NewWidth,1,1,OptimizationData)
    else if Image.PixelFormat=pf24bit then
    begin
      Method(PByteArray(@SrcLine^[0]),PByteArray(@DstLine^[0]),Image.Width,NewWidth,3,3,OptimizationData);
      Method(PByteArray(@SrcLine^[1]),PByteArray(@DstLine^[1]),Image.Width,NewWidth,3,3,OptimizationData);
      Method(PByteArray(@SrcLine^[2]),PByteArray(@DstLine^[2]),Image.Width,NewWidth,3,3,OptimizationData);
    end
    else
      raise Exception.Create(rsUnsupportedBitmapFormat);
  end;

  procedure ProcessRow(X: Integer);
  begin
    if Image.PixelFormat=pf8bit then
      Method(PByteArray(@HorzImage.Map^[X]),PByteArray(@NewImage.Map^[X]),HorzImage.Height,NewHeight,HorzImage.BytesPerLine,NewImage.BytesPerLine,OptimizationData)
    else if Image.PixelFormat=pf24bit then
    begin
      Method(PByteArray(@HorzImage.Map^[X*3]),PByteArray(@NewImage.Map^[X*3]),HorzImage.Height,NewHeight,HorzImage.BytesPerLine,NewImage.BytesPerLine,OptimizationData);
      Method(PByteArray(@HorzImage.Map^[X*3+1]),PByteArray(@NewImage.Map^[X*3+1]),HorzImage.Height,NewHeight,HorzImage.BytesPerLine,NewImage.BytesPerLine,OptimizationData);
      Method(PByteArray(@HorzImage.Map^[X*3+2]),PByteArray(@NewImage.Map^[X*3+2]),HorzImage.Height,NewHeight,HorzImage.BytesPerLine,NewImage.BytesPerLine,OptimizationData);
    end
    else
      raise Exception.Create(rsUnsupportedBitmapFormat);
  end;

begin
  OverwriteSource:=(NewImage=nil) or (NewImage=Image);
  if (Image.Width=NewWidth) and (Image.Height=NewHeight) then
  begin
    if not OverwriteSource then NewImage.Assign(Image);
    Exit;
  end;
  if OverwriteSource then NewImage:=Image;

  HorzImage:=TLinearBitmap.Create(NewWidth,Image.Height,Image.PixelFormat);
  try
    // Scale horizontally
    OptimizationData:=nil;
    try
      Method(nil,nil,Image.Width,NewWidth,0,0,OptimizationData);
      ParallelFor(0,Image.Height-1,@ProcessLine);
    finally
      FreeAndNil(OptimizationData);
    end;

    // Scale vertically
    NewImage.New(NewWidth,NewHeight,Image.PixelFormat);
    try
      Method(nil,nil,HorzImage.Height,NewHeight,0,0,OptimizationData);
      ParallelFor(0,NewWidth-1,@ProcessRow);
    finally
      FreeAndNil(OptimizationData);
    end;
  finally
    HorzImage.Free;
  end;
end;

procedure RepeatImage(Image,NewImage: TLinearBitmap; CountX,CountY: Integer);
var
  UseTempImage : Boolean;
  X, Y : Integer;
begin
  UseTempImage:=(NewImage=nil) or (NewImage=Image);
  if UseTempImage then NewImage:=TLinearBitmap.Create;
  try
    NewImage.New(Image.Width*CountX,Image.Height*CountY,Image.PixelFormat);
    if Image.PixelFormat=pf8bit then NewImage.Palette^:=Image.Palette^;
    for Y:=0 to CountY-1 do
      for X:=0 to CountX-1 do NewImage.PasteImage(Image,X*Image.Width,Y*Image.Height);
    if UseTempImage then Image.TakeOver(NewImage);
  finally
    if UseTempImage then NewImage.Free;
  end;
end;

end.

