unit DiagramAntialiasingDrawing;

interface

uses
  Windows, SysUtils,
  //GdiPlus, ActiveX,
  Graphics, FastBitmap, BitmapGammaInterpolation, DiagramBase;

// Draw diagram to bitmap doing antialiasing. Set BackgroundColor=-1 if painting on an existing bitmap
procedure DrawToBitmap(Page: TDiagramPage; Bitmap: TFastBitmap; CanvasInfo: TCanvasInfo; Antialiasing: Boolean; BackgroundColor: TColor=clWhite); overload;
procedure DrawToBitmap(Layer: TBaseObjectList; Bitmap: TFastBitmap; const CanvasInfo: TCanvasInfo; Antialiasing: Boolean; BackgroundColor: TColor=clWhite); overload;

procedure BitmapAntialiasingScaleHalf(SourceBitmap,DestBitmap: TFastBitmap);

implementation

uses
  Math, Types, MathUtils, Classes;

var
  Average2GammaLUT, WeightedAverage2GammaLUT : ^TAverage2GammaLUT;

procedure CreateWeightedGammaLUT(Gamma: Single; var LUT: TAverage2GammaLUT); overload;
var
  InvGammaLUT : T8bitToSingleGammaLUT;
  A, B : Integer;
begin
  CreateGammaLUT(1/Gamma,InvGammaLUT);
  Gamma:=1/Gamma;
  for A:=0 to 255 do
    for B:=0 to 255 do
      LUT[A,B]:=Round(Power((InvGammaLUT[A]*2+InvGammaLUT[B])/(3*255),Gamma)*255);
end;

procedure BitmapAntialiasingScaleHalf(SourceBitmap,DestBitmap: TFastBitmap);
type
  TRGBPixArray = packed array[0..3] of Byte;
  PRGBPixArray = ^TRGBPixArray;

  function BlendPix(const LPix,RPix: TRGBPixArray): TRGBPixArray;
  begin
    // Average with R/B weights similar to Window's font antialiasing
    Result[0]:=WeightedAverage2GammaLUT[RPix[0],LPix[0]];
    Result[1]:=Average2GammaLUT[LPix[1],RPix[1]];
    Result[2]:=WeightedAverage2GammaLUT[LPix[2],RPix[2]];
  end;

var
  X, Y, L : Integer;
  SrcPix, DestPix : PRGBPixArray;
  Pix1, Pix2 : TRGBPixArray;
begin
  Assert(SourceBitmap.PixelFormat=pf32bit);
  Assert(DestBitmap.PixelFormat=pf32bit);
  Assert(DestBitmap.Width*2<=SourceBitmap.Width);
  Assert(DestBitmap.Height*2<=SourceBitmap.Height);

  if Average2GammaLUT=nil then
  begin
    New(Average2GammaLUT);
    CreateGammaLUT(DefaultMonitorGamma,Average2GammaLUT^);
  end;
  if WeightedAverage2GammaLUT=nil then
  begin
    New(WeightedAverage2GammaLUT);
    CreateWeightedGammaLUT(DefaultMonitorGamma,WeightedAverage2GammaLUT^);
  end;

  L:=SourceBitmap.BytesPerLine;
  for Y:=0 to DestBitmap.Height-1 do
  begin
    SrcPix:=@SourceBitmap.ScanLine32[2*Y]^;
    DestPix:=@DestBitmap.ScanLine32[Y]^;
    for X:=1 to DestBitmap.Width do
    begin
      Pix1:=BlendPix(PRGBPixArray(Integer(SrcPix)+0)^,PRGBPixArray(Integer(SrcPix)+4)^);
      Pix2:=BlendPix(PRGBPixArray(Integer(SrcPix)+L)^,PRGBPixArray(Integer(SrcPix)+L+4)^);
      DestPix^[0]:=Average2GammaLUT[Pix1[0],Pix2[0]];
      DestPix^[1]:=Average2GammaLUT[Pix1[1],Pix2[1]];
      DestPix^[2]:=Average2GammaLUT[Pix1[2],Pix2[2]];

      Inc(SrcPix,2);
      Inc(DestPix);
    end;
  end;
end;

procedure CombineAntialiasingBitmaps(StdRenderBitmap,AntialiasingBitmap,DestBitmap: TFastBitmap);
type
  TRGBPixArray = packed array[0..3] of Byte;
  PRGBPixArray = ^TRGBPixArray;

  function BlendPixLR(const LPix,RPix: TRGBPixArray): TRGBPixArray;
  begin
    // Average with R/B weights similar to Window's font antialiasing
    Result[0]:=WeightedAverage2GammaLUT[RPix[0],LPix[0]];
    Result[1]:=Average2GammaLUT[LPix[1],RPix[1]];
    Result[2]:=WeightedAverage2GammaLUT[LPix[2],RPix[2]];
    {Result[0]:=Average2GammaLUT[LPix[0],RPix[0]];
    Result[1]:=Average2GammaLUT[LPix[1],RPix[1]];
    Result[2]:=Average2GammaLUT[LPix[2],RPix[2]];{}
  end;

  function BlendPixUD(const UPix,DPix: TRGBPixArray): TRGBPixArray;
  begin
    Result[0]:=Average2GammaLUT[UPix[0],DPix[0]];
    Result[1]:=Average2GammaLUT[UPix[1],DPix[1]];
    Result[2]:=Average2GammaLUT[UPix[2],DPix[2]];
  end;

var
  X, Y, L : Integer;
  DestPix, SrcPix, MaskPix : PDWord;
  Mask : TFastBitmap;
begin
  if Average2GammaLUT=nil then
  begin
    New(Average2GammaLUT);
    CreateGammaLUT(DefaultMonitorGamma,Average2GammaLUT^);
  end;
  if WeightedAverage2GammaLUT=nil then
  begin               
    New(WeightedAverage2GammaLUT);
    CreateWeightedGammaLUT(DefaultMonitorGamma,WeightedAverage2GammaLUT^);
  end;

  // Create mask of pixels that are different between StdRenderBitmap and DestBitmap
  Mask:=TFastBitmap.Create(StdRenderBitmap.Width,StdRenderBitmap.Height,pf32bit);
  try
    Mask.Clear;
    L:=Mask.BytesPerLine;
    for Y:=1 to DestBitmap.Height-2 do
    begin
      DestPix:=@DestBitmap.ScanLine32[Y]^;
      MaskPix:=@Mask.ScanLine32[Y]^;
      SrcPix:=@StdRenderBitmap.ScanLine32[Y]^;
      for X:=1 to DestBitmap.Width-2 do
      begin
        Inc(DestPix);
        Inc(MaskPix);
        Inc(SrcPix);
        if DestPix^<>SrcPix^ then
        begin
          FillChar(PDWord(Integer(MaskPix)-4-L)^,12,1);
          FillChar(PDWord(Integer(MaskPix)-4)^,12,1);
          FillChar(PDWord(Integer(MaskPix)-4+L)^,12,1);
        end;
      end;
    end;

    // Set pixels in DestBitmap that are marked in Mask
    L:=AntialiasingBitmap.BytesPerLine;
    for Y:=0 to DestBitmap.Height-1 do
    begin
      SrcPix:=@AntialiasingBitmap.ScanLine32[2*Y]^;
      DestPix:=@DestBitmap.ScanLine32[Y]^;
      MaskPix:=@Mask.ScanLine32[Y]^;
      for X:=1 to DestBitmap.Width do
      begin
        if MaskPix^<>0 then
        begin
          DestPix^:=DWord(BlendPixUD(BlendPixLR(PRGBPixArray(SrcPix)^,           PRGBPixArray(Integer(SrcPix)+4)^),
                                     BlendPixLR(PRGBPixArray(Integer(SrcPix)+L)^,PRGBPixArray(Integer(SrcPix)+L+4)^)));
        end;
        Inc(SrcPix,2);
        Inc(DestPix);                       
        Inc(MaskPix);
      end;
    end;
  finally
    Mask.Free;
  end;
end;

type
  TOpenDiagramPage = class(TDiagramPage);

procedure DrawToBitmap(Layer: TBaseObjectList; Bitmap: TFastBitmap; const CanvasInfo: TCanvasInfo; Antialiasing: Boolean; BackgroundColor: TColor); overload;
var
  Page : TOpenDiagramPage;
begin
  Page:=TOpenDiagramPage.Create;
  try
    Page.OwnsObjects:=False;
    Page.Add(Layer);
    DrawToBitmap(Page,Bitmap,CanvasInfo,Antialiasing,BackgroundColor);
  finally
    Page.Free;
  end;
end;

// Draw diagram to bitmap doing antialiasing. Set BackgroundColor=-1 if painting on an existing bitmap
procedure DrawToBitmap(Page: TDiagramPage; Bitmap: TFastBitmap; CanvasInfo: TCanvasInfo; Antialiasing: Boolean; BackgroundColor: TColor); overload;
var
  StdRenderBitmap, AntialiasBitmap : TFastBitmap;
  {G : IGraphics;
  Bm : IBitmap;
  BmData : BitmapData;
  Im : IImage;
  Y : Integer;
  Metafile : TMetafile;
  Canvas : TCanvas;
  MemStream: TMemoryStream;
  GUID : TGUID;{}
begin
  Assert(CanvasInfo.ZBuffer=nil);
  Assert(CanvasInfo.DrawMode=dmRender);
  if Antialiasing and (Bitmap.PixelFormat=pf32bit) then
  begin
    // Create GDI+ bitmap
    (*Bm:=NewBitmap(Bitmap.Width,Bitmap.Height,PixelFormat32bppRGB);

    G:=NewGraphics(Bm);
    G.SmoothingMode:=SmoothingModeAntiAlias;

    Canvas:=TCanvas.Create;
    Canvas.Handle:=G.GetHDC;
    Canvas.FillRect(Rect(0,0,Bitmap.Width,Bitmap.Height));

    if CanvasInfo.Container.ObjectShadows then
    begin
      CanvasInfo.DrawMode:=dmShadow;
      Page.DrawShadow(Canvas,CanvasInfo);
    end;
    CanvasInfo.DrawMode:=dmRender;
    Page.Draw(Canvas,CanvasInfo);

    G.ReleaseHDC(Canvas.Handle);
    Canvas.Free;{}

    {MemStream:=TMemoryStream.Create;
    try
      // Draw page to metafile
      Metafile:=TMetafile.Create;
      try
        Metafile.Enhanced:=True;
        Metafile.Width:=Round(Bitmap.Width);
        Metafile.Height:=Round(Bitmap.Height);
        Canvas:=TMetafileCanvas.Create(Metafile,0);
        try
          if CanvasInfo.Container.ObjectShadows then
          begin
            CanvasInfo.DrawMode:=dmShadow;
            Page.DrawShadow(Canvas,CanvasInfo);
          end;
          CanvasInfo.DrawMode:=dmRender;
          Page.Draw(Canvas,CanvasInfo);
        finally
          Canvas.Free;
        end;
        Metafile.SaveToStream(MemStream);
      finally
        Metafile.Free;
      end;
      // Create GDI+ image from metafile stream
      MemStream.Position:=0;
      Im:=NewImage(TStreamAdapter.Create(MemStream));
    finally
      MemStream.Free;
    end;
    G.DrawImage(Im,0,0,Bitmap.Width,Bitmap.Height);{}

    //GetEncoderClsid(PngFormat,GUID); Bm.Save('x:\1.png');

    Bm.LockBits(NewRect(Bm.Width,Bm.Height),0,PixelFormat32bppRGB,BmData);
    for Y:=0 to Bitmap.Height-1 do
      Move(PByteArray(BmData.Scan0)^[Y*BmData.Strid],Bitmap.ScanLine32[Bitmap.Height-1-Y]^,Bitmap.BytesPerLine);
    Bm.UnlockBits(BmData);

    Exit;(**)

    // Standard rendering
    if BackgroundColor<0 then
    begin
      BackgroundColor:=0;
      StdRenderBitmap:=TFastBitmap.Create(Bitmap.Width,Bitmap.Height,pf32bit);
      StdRenderBitmap.Canvas.Draw(0,0,Bitmap);
    end
    else StdRenderBitmap:=TFastBitmap.Create(Bitmap.Width,Bitmap.Height,pf32bit,BackgroundColor);
    try
      if (CanvasInfo.Container<>nil) and CanvasInfo.Container.ObjectShadows then
      begin
        CanvasInfo.DrawMode:=dmShadow;
        Page.DrawShadow(StdRenderBitmap.Canvas,CanvasInfo);
        CanvasInfo.DrawMode:=dmRender;
      end;
      Page.Draw(StdRenderBitmap.Canvas,CanvasInfo);
      // Render without objects that uses antialiasing
      CanvasInfo.DrawMode:=dmPreAntialiasing;
      Page.Draw(Bitmap.Canvas,CanvasInfo);
      // Render objects that uses antialiasing in double size
      AntialiasBitmap:=TFastBitmap.Create(Bitmap.Width*2,Bitmap.Height*2,pf32bit,BackgroundColor);
      try
        AntialiasBitmap.Canvas.StretchDraw(Rect(0,0,AntialiasBitmap.Width,AntialiasBitmap.Height),Bitmap);
        CanvasInfo.Scale:=VectorMult(CanvasInfo.Scale,2);
        CanvasInfo.Offset.X:=CanvasInfo.Offset.X*2+1;
        CanvasInfo.Offset.Y:=CanvasInfo.Offset.Y*2+1;
        if (CanvasInfo.Container<>nil) and (CanvasInfo.Container.ObjectShadows) then
        begin
          CanvasInfo.DrawMode:=dmShadow;
          Page.DrawShadow(AntialiasBitmap.Canvas,CanvasInfo);
        end;
        CanvasInfo.DrawMode:=dmRender;
        Page.DrawAntialiasing(AntialiasBitmap.Canvas,CanvasInfo);
        // Combine results
        {StdRenderBitmap.SaveToFile('x:\dmRender.bmp');
        AntialiasBitmap.SaveToFile('x:\AntialiasBitmap.bmp');
        Bitmap.SaveToFile('x:\dmPreAntialiasing.bmp'); {$MESSAGE WARN  'DEBUG!!'}
        CombineAntialiasingBitmaps(StdRenderBitmap,AntialiasBitmap,Bitmap);
      finally
        AntialiasBitmap.Free;
      end;
    finally
      StdRenderBitmap.Free;
    end;
  end
  else
  begin
    if (CanvasInfo.Container<>nil) and CanvasInfo.Container.ObjectShadows then
    begin
      CanvasInfo.DrawMode:=dmShadow;
      Page.DrawShadow(Bitmap.Canvas,CanvasInfo);
      CanvasInfo.DrawMode:=dmRender;
    end;
    Page.Draw(Bitmap.Canvas,CanvasInfo);
  end;
end;

initialization
finalization
  if Average2GammaLUT<>nil then Dispose(Average2GammaLUT);
  if WeightedAverage2GammaLUT<>nil then Dispose(WeightedAverage2GammaLUT);
end.
