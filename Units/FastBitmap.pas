////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// FastBitmap.pas - TBitmap descendant with fast access to bitmap memory
// ---------------------------------------------------------------------
// Changed:   2002-09-24
// Auther:  Michael Vinther
//
// Last change:
//
unit FastBitmap;

interface

uses
  SysUtils, Windows, Classes, Graphics, MemUtils, MathUtils, Types, LinarBitmap;

type
  TFastBitmap = class(TBitmap)
    private                        
      FWidth, FHeight : Integer;
      function GetScanLine(Y: Integer): Pointer;
      function GetScanLine24(Y: Integer): PRGBRec;
      function GetScanLine32(Y: Integer): PIntegerArray;
      function GetPixel24(X,Y: Integer): PRGBRec;
      function GetPixel32(X, Y: Integer): PInteger;
    public
      BytesPerLine : Integer;
      Map : PByteArray;
      Map32 : PIntegerArray;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property ScanLine[Y: Integer]: Pointer read GetScanLine;
      property ScanLine24[Y: Integer]: PRGBRec read GetScanLine24;
      property ScanLine32[Y: Integer]: PIntegerArray read GetScanLine32; // Note: 0 is bottom line
      property Pixel24[X,Y: Integer]: PRGBRec read GetPixel24;
      property Pixel32[X,Y: Integer]: PInteger read GetPixel32;
      // pfCustom means no change
      constructor Create(X,Y: Integer; PixFormat: TPixelFormat=pfCustom; BrushColor: TColor=clWhite); reintroduce; overload;
      procedure New(X,Y: Integer; PixFormat: TPixelFormat=pfCustom);
      procedure UpdateMap;

      procedure PaintTo(DestCanvas: TCanvas; const DestRect: TRect; HalftoneStretch: Boolean);
      procedure Assign(Source: TPersistent); override;

      procedure Clear; overload;
      procedure Clear(Color: TColor); overload;
      procedure Smooth(WindowSize: Integer);
    end;

// Merge Image and Alpha to 32 bit bitmap with alpha channel
function CreatePremultipliedAlphaBitmap(Image,Alpha: TLinearBitmap): TFastBitmap; overload;
procedure CreatePremultipliedAlphaBitmap(Image,Alpha: TFastBitmap); overload;
// Draw bitmap to canvas with alpha blending using alpha channel
procedure AlphaBlendDrawPremultipliedAlpha(Bitmap: TBitmap; Dest: TCanvas; const DestRect: TRect; AlphaValue: Byte=255);
// Draw bitmap to canvas with alpha blending using fixed alpha value
procedure AlphaBlendDraw(Source: TBitmap; Dest: TCanvas; const DestRect: TRect; AlphaValue: Byte);
// Set high pixel byte of 32 bit bitmap
procedure SetBitmapAlphaValue(Bitmap: TFastBitmap; Alpha: Byte);
// Draw bitmap to canvas stretching with halftoning
procedure HalftoneStretchDraw(Source: TBitmap; Dest: TCanvas; const DestRect: TRect);
// Pixel value threshold
procedure BitmapThreshold(Bitmap: TBitmap; Threshold: Integer);

implementation

// Merge Image and Alpha to 32 bit bitmap with premultiplied alpha channel
function CreatePremultipliedAlphaBitmap(Image,Alpha: TLinearBitmap): TFastBitmap;
var
  X, Y : Integer;
  SrcPix : PByte;
  DestPix : ^TRGBQuad;
begin
  Assert(Alpha.PixelFormat=pf8bit);
  Assert(Alpha.Width=Image.Width);
  Assert(Alpha.Height=Image.Height);
  Result:=TFastBitmap.Create;
  Image.AssignTo(Result);
  Result.PixelFormat:=pf32bit;
  Result.UpdateMap;
  for Y:=0 to Alpha.Height-1 do
  begin
    SrcPix:=Alpha.ScanLine[Y];
    DestPix:=@Result.ScanLine32[Alpha.Height-1-Y]^;
    for X:=1 to Alpha.Width do
    begin
      DestPix^.rgbBlue:=Round(DestPix^.rgbBlue*SrcPix^/255);
      DestPix^.rgbGreen:=Round(DestPix^.rgbGreen*SrcPix^/255);
      DestPix^.rgbRed:=Round(DestPix^.rgbRed*SrcPix^/255);
      DestPix^.rgbReserved:=SrcPix^;
      Inc(SrcPix);
      Inc(DestPix);
    end;
  end;
end;

// Merge Image and Alpha to 32 bit bitmap with premultiplied alpha channel
procedure CreatePremultipliedAlphaBitmap(Image,Alpha: TFastBitmap);
var
  X, Y : Integer;
  SrcPix : PByte;
  DestPix : ^TRGBQuad;
begin
  Assert(Alpha.PixelFormat=pf8bit);
  Assert(Alpha.Width=Image.Width);
  Assert(Alpha.Height=Image.Height);
  Image.PixelFormat:=pf32bit;
  Image.UpdateMap;
  Alpha.UpdateMap;
  for Y:=0 to Alpha.Height-1 do
  begin
    SrcPix:=Alpha.ScanLine[Y];
    DestPix:=@Image.ScanLine32[Alpha.Height-1-Y]^;
    for X:=1 to Alpha.Width do
    begin
      DestPix^.rgbBlue:=Round(DestPix^.rgbBlue*SrcPix^/255);
      DestPix^.rgbGreen:=Round(DestPix^.rgbGreen*SrcPix^/255);
      DestPix^.rgbRed:=Round(DestPix^.rgbRed*SrcPix^/255);
      DestPix^.rgbReserved:=SrcPix^;
      Inc(SrcPix);
      Inc(DestPix);
    end;
  end;
end;

// Draw bitmap to canvas with alpha blending using fixed alpha value
procedure AlphaBlendDraw(Source: TBitmap; Dest: TCanvas; const DestRect: TRect; AlphaValue: Byte);
var
  Blend : TBlendFunction;
begin
  Blend.BlendOp:=AC_SRC_OVER;
  Blend.BlendFlags:=0;
  Blend.SourceConstantAlpha:=AlphaValue;
  Blend.AlphaFormat:=0;
  Windows.AlphaBlend(Dest.Handle,DestRect.Left,DestRect.Top,DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
                     Source.Canvas.Handle,0,0,Source.Width,Source.Height,
                     Blend);
end;

// Draw bitmap to canvas with alpha blending using alpha channel
procedure AlphaBlendDrawPremultipliedAlpha(Bitmap: TBitmap; Dest: TCanvas; const DestRect: TRect; AlphaValue: Byte);
var
  Blend : TBlendFunction;
begin
  Blend.BlendOp:=AC_SRC_OVER;
  Blend.BlendFlags:=0;
  Blend.SourceConstantAlpha:=AlphaValue;
  Blend.AlphaFormat:=AC_SRC_ALPHA;
  Windows.AlphaBlend(Dest.Handle,DestRect.Left,DestRect.Top,DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
                     Bitmap.Canvas.Handle,0,0,Bitmap.Width,Bitmap.Height,
                     Blend);
end;

// Draw bitmap to canvas stretching with halftoning
procedure HalftoneStretchDraw(Source: TBitmap; Dest: TCanvas; const DestRect: TRect);
begin
  SetStretchBltMode(Dest.Handle,STRETCH_HALFTONE);
  StretchBlt(Dest.Handle,DestRect.Left,DestRect.Top,DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
             Source.Canvas.Handle,0,0,Source.Width,Source.Height,cmSrcCopy);
end;

// Set high pixel byte of 32 bit bitmap
procedure SetBitmapAlphaValue(Bitmap: TFastBitmap; Alpha: Byte);
var
  X, Y : Integer;
  Pix8 : PByte;
begin
  // Reset high byte of all pixels
  for Y:=0 to Bitmap.Height-1 do
  begin
    Pix8:=Pointer(Bitmap.ScanLine32[Y]);
    Inc(Pix8,3);
    for X:=1 to Bitmap.Width do
    begin
      Pix8^:=Alpha;
      Inc(Pix8,4);
    end;
  end;
end;

procedure BitmapThreshold(Bitmap: TBitmap; Threshold: Integer);
var
  X, Y, BytesPerLine : Integer;
  Pix : PByte;
begin
  BytesPerLine:=Bitmap.Width;
  if Bitmap.PixelFormat=pf32bit then BytesPerLine:=BytesPerLine*4
  else if Bitmap.PixelFormat=pf24bit then BytesPerLine:=BytesPerLine*3;
  for Y:=0 to Bitmap.Height-1 do
  begin
    Pix:=Bitmap.ScanLine[Y];
    for X:=1 to BytesPerLine do
    begin
      if Pix^>Threshold then Pix^:=255
      else Pix^:=0;
      Inc(Pix);
    end;
  end;
end;

//==============================================================================================================================
// TFastBitmap
//==============================================================================================================================
procedure TFastBitmap.UpdateMap;
var
  MapA, MapB : DWord;
begin
  FWidth:=inherited Width;
  FHeight:=inherited Height;
  case PixelFormat of
    pf8bit  : BytesPerLine:=Ceil4(Width);
    pf24bit : BytesPerLine:=Ceil4(Width*3);
    pf32bit : BytesPerLine:=Width*4;
  else raise Exception.Create('Unsupported pixel format');
  end;
  MapA:=DWord(inherited ScanLine[0]);
  MapB:=DWord(inherited ScanLine[Height-1]);
  if MapA<MapB then Map:=Pointer(MapA)
  else Map:=Pointer(MapB);
  Map32:=Pointer(Map);
end;

constructor TFastBitmap.Create(X, Y: Integer; PixFormat: TPixelFormat; BrushColor: TColor);
begin
  Create;
  Canvas.Brush.Color:=BrushColor;
  New(X,Y,PixFormat);
end;

procedure TFastBitmap.New(X,Y: Integer; PixFormat: TPixelFormat=pfCustom);
begin
  if PixFormat<>pfCustom then PixelFormat:=PixFormat;
  inherited Width:=X;
  inherited Height:=Y;
  UpdateMap;
end;

procedure TFastBitmap.Clear;
var
  Y : Integer;
begin
  for Y:=0 to Height-1 do ZeroMem(Pointer(Integer(Map)+Y*BytesPerLine)^,Width*4);
end;

procedure TFastBitmap.Clear(Color: TColor);
begin
  if Color=0 then Clear
  else with Canvas do
  begin
    Brush.Color:=Color;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,Width,Height));
  end;
end;

function TFastBitmap.GetScanLine32(Y: Integer): PIntegerArray;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=PIntegerArray(Integer(Map)+Y*BytesPerLine);
end;

function TFastBitmap.GetScanLine24(Y: Integer): PRGBRec;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=PRGBRec(Integer(Map)+(Height-1-Y)*BytesPerLine);
end;

function TFastBitmap.GetScanLine(Y: Integer): Pointer;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=Pointer(Integer(Map)+(Height-1-Y)*BytesPerLine);
end;

function TFastBitmap.GetPixel24(X,Y: Integer): PRGBRec;
begin
  Assert((X>=0) and (Y>=0) and (X<Width) and (Y<Height));
  Result:=PRGBRec(Integer(Map)+(Height-1-Y)*BytesPerLine+X*3);
end;

function TFastBitmap.GetPixel32(X, Y: Integer): PInteger;
begin
  Assert((X>=0) and (Y>=0) and (X<Width) and (Y<Height));
  Result:=PInteger(Integer(Map)+Y*BytesPerLine+X*4);
end;

procedure TFastBitmap.Smooth(WindowSize: Integer);
var
  NewMap : PIntegerArray;
  X, Y, DX, DY, DMax, DMin, XA, XB, YA, YB, P : Integer;
  CenterPix, ScanPix : PByte;
  Sum : Integer;
  Scale : Double;
begin
  Assert(PixelFormat=pf32bit);

  DMin:=-WindowSize div 2;
  DMax:=WindowSize+DMin-1;
  Scale:=1/Sqr(WindowSize);
  GetMem(NewMap,BytesPerLine*Height);
  for P:=0 to 2 do
  begin
    for Y:=1 to Height-2 do
    begin
      CenterPix:=Pointer(Integer(NewMap)+Y*BytesPerLine+4+P);
      YA:=Y+DMin;
      YB:=Y+DMax; if YB>=Height then YB:=Height-1;
      for X:=1 to Width-2 do
      begin
        Sum:=0;
        XA:=X+DMin;
        XB:=X+DMax;
        for DY:=YA to YB do
        begin
          ScanPix:=@Map^[DY*BytesPerLine+XA*4+P];
          for DX:=XA to XB do
          begin
            Sum:=Sum+ScanPix^;
            Inc(ScanPix,4);
          end;
        end;
        CenterPix^:=Round(Sum*Scale);
        Inc(CenterPix,4);
      end;
    end;
  end;
  Move(NewMap^,Map^,BytesPerLine*Height);
  FreeMem(NewMap);
end;

procedure TFastBitmap.PaintTo(DestCanvas: TCanvas; const DestRect: TRect; HalftoneStretch: Boolean);
begin
  if HalftoneStretch then
    SetStretchBltMode(DestCanvas.Handle,STRETCH_HALFTONE)
  else
    SetStretchBltMode(DestCanvas.Handle,STRETCH_DELETESCANS);
  Windows.StretchBlt(DestCanvas.Handle,
                     DestRect.Left,DestRect.Top,
                     DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
                     Canvas.Handle,
                     0,0,Width,Height,
                     SRCCOPY);
end;

procedure TFastBitmap.Assign(Source: TPersistent);
var
  Y, I : Integer;
begin
  if (Source is TFastBitmap) and
     (TFastBitmap(Source).Width=Width) and
     (TFastBitmap(Source).Height=Height) and
     (TFastBitmap(Source).PixelFormat=PixelFormat) then
  begin
    I:=0;
    for Y:=0 to Height-1 do
    begin
      Move(Pointer(Integer(TFastBitmap(Source).Map)+I)^,Pointer(Integer(Map)+I)^,BytesPerLine);
      Inc(I,BytesPerLine);
    end;
  end
  else inherited;
end;

end.

