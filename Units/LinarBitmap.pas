////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// LinarBitmap.pas - Bitmap handling
// ---------------------------------
// Version:   2005-11-16
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//   16 bit grayscale bitmap support
//
unit LinarBitmap;

interface

uses Windows, Classes, SysUtils, SysConst, Graphics, Streams, DelphiStream, MemUtils,
  Math, Monitor, MathUtils;

{$R-} {$Q-}

resourcestring
  rsInvalidPixelFormat      = 'Unsupported pixel format';
  rsInvalidBitmapSize       = 'Invalid bitmap size';
  rsUnsupportedFileFormat   = 'Unsupported file format';
  rsErrorInBitmapData       = 'Error in bitmap data';
  rsUnsupportedBitmapFormat = 'Unsupported bitmap format';

type
  RGBRec = packed record 
             B, G, R : Byte;
           end;
  PRGBRec = ^RGBRec;

  TRGBArray = packed array[0..0] of RGBRec;
  PRGBArray = ^TRGBArray;

  TPalette = packed array[0..255] of RGBRec;
  PPalette = ^TPalette;
  TPalEntries = array[0..255] of TPaletteEntry;

  TBitmapLoader = class;

  TLinearBitmap = class(TAssignObject)
    protected
      fWidth, fHeight : Integer;
      fPixelFormat : TPixelFormat;
      fBytesPerLine, fPixelSize : Integer;
      fSize : LongInt;
      fPresent : Boolean;
      function GetScanLine(Y: Integer): Pointer;
      function GetScanLineSafe(Y: Integer): Pointer; virtual;
      function GetPixel(X,Y: Integer): Pointer;
      function GetPixelColor(X,Y: Integer): TColor;
      function GetPixelColorInterpolate(X,Y: Single): TColor;
      procedure SetPixelColor(X,Y: Integer; Color: TColor);
      function GetPixelTone(X, Y: Integer): Integer;
      function GetPixelToneInterpolate(X, Y: Single): Integer;
      procedure SetPixelTone(X, Y: Integer; const Value: Integer);
      procedure SetPixelFormat(PixFormat: TPixelFormat); virtual;
    public
      Map : PByteArray;
      Palette : PPalette;
      property Width: Integer read fWidth;
      property Height: Integer read fHeight;
      property BytesPerLine : Integer read fBytesPerLine;
      property PixelFormat : TPixelFormat read fPixelFormat write SetPixelFormat;
      property PixelSize : Integer read fPixelSize; // Bytes per pixel
      property ScanLine[Y: Integer]: Pointer read GetScanLine;
      property ScanLineSafe[Y: Integer]: Pointer read GetScanLineSafe;
      property Pixel[X,Y: Integer]: Pointer read GetPixel; default;
      property PixelColor[X,Y: Integer]: TColor read GetPixelColor write SetPixelColor;
      property PixelColorInterpolate[X,Y: Single]: TColor read GetPixelColorInterpolate;
      property PixelTone[X,Y: Integer]: Integer read GetPixelTone write SetPixelTone;
      property PixelToneInterpolate[X,Y: Single]: Integer read GetPixelToneInterpolate;
      // Number of bytes
      property Size : Integer read fSize;
      property Present : Boolean read fPresent;

      constructor Create; overload;
      constructor Create(Width,Height: Integer; PixFormat: TPixelFormat); overload;
      constructor Create(Other: TObject); overload; // Create copy
      destructor Destroy; override;

      // Create new image
      procedure New(Width,Height: Integer; PixFormat: TPixelFormat; BytesPerLine: Integer=0); overload; virtual;
      // Release image memory
      procedure Dispose; virtual;
      // True if ScanLineSafe must be used
      function MustUpdateScanLine: Boolean; virtual;
      // Clear image with color
      procedure Clear(Value: Byte=0); overload; virtual; 
      procedure Clear(Value: Byte; Rect: TRect); overload; 
      procedure ClearColor(Color: TColor); // Red is LSB
      // Copy image to Other
      procedure AssignTo(Other: TObject); override;
      procedure Assign(Other: TObject); override;
      procedure TakeOver(Other: TLinearBitmap); virtual;
      // Make sure BytesPerLine=Width*PixelSize so no memory is wasted
      procedure OptimizeMem; virtual;
      procedure UnmapImage; virtual;
      // Copy image to/from clipboard
      procedure CopyToClipboard(EmptyExistingClipboard: Boolean=True);
      function GetFromClipboard: Boolean;
      procedure PasteImage(Source: TLinearBitmap; X,Y: Integer; SrcWidth: Integer=MaxInt; SrcHeight: Integer=MaxInt); overload; virtual;
      procedure PasteImage(Source,Mask: TLinearBitmap; X,Y: Integer); overload; virtual; 
      // Load/save, TBitmapLoaders must be assigned
      function LoadFromFile(const FileName: string): TBitmapLoader;
      function SaveToFile(const FileName: string): TBitmapLoader;
      procedure LoadFromStream(Stream: TSeekableStream; const FormatExt: string); overload;
      procedure LoadFromStream(Stream: TStream; const FormatExt: string); overload; // FormatExt must be UPPERCASE
      procedure SaveToStream(Stream: TSeekableStream; const FormatExt: string); overload;
      procedure SaveToStream(Stream: TStream; const FormatExt: string); overload; // FormatExt must be UPPERCASE
      // Return true if image is grayscale
      function IsGrayScale: Boolean;
      // Resize image canvas, old image is positioned at XPos,YPos
      procedure ResizeCanvas(XSiz,YSiz,XPos,YPos: Integer; Color: TColor=0);
      // Get image from Windows DIB
      procedure GetFromDIB(var DIB: TBitmapInfo);
      procedure GetFromHDIB(HDIB: HBitmap);
      // Make Windows DIB, return total size in bytes
      function MakeDIB(out Bitmap: PBitmapInfo): Integer;
      // Stretch and paint to TCanvas,
      procedure PaintToCanvas(Canvas: TCanvas; const Dest: TRect; HalftoneStretch: Boolean=False; DisposeAfterDraw: Boolean=False);
      procedure StretchDIBits(DC: THandle; const Dest: TRect; HalftoneStretch: Boolean=False; DisposeAfterDraw: Boolean=False);
      // Paint to TBitmap (target size and palette assumed correct)
      procedure PaintToTBitmap(Target: TBitmap);
   end;

  TLinearBitmapArray = array of TLinearBitmap;

  TLinarBitmap = TLinearBitmap; // Spelling bug correction

  TBitmapLoader = class(TMonitorObject)
    public
      function CanLoad(const Ext: string): Boolean; virtual; // Ext must be UPPERCASE
      function CanSave(const Ext: string): Boolean; virtual; // Ext must be UPPERCASE

      function GetLoadFilter: string; virtual;
      function GetSaveFilter: string; virtual;

      procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinearBitmap); virtual;
      procedure SaveToStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinearBitmap); virtual;
      procedure LoadFromFile(const FileName,FileType: string; Bitmap: TLinearBitmap); virtual;
      procedure SaveToFile(const FileName,FileType: string; Bitmap: TLinearBitmap); virtual;
    end;

  TLinearGraphic = class(TGraphic)
    protected
      FImage : TLinearBitmap;
      procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
      function GetEmpty: Boolean; override;
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;
      procedure SetHeight(Value: Integer); override;
      procedure SetWidth(Value: Integer); override;
      procedure AssignTo(Dest: TPersistent); override;
    public
      property Bitmap : TLinearBitmap read FImage;
      constructor Create; override;
      destructor Destroy; override;
      procedure LoadFromStream(Stream: TStream); override;
      procedure SaveToStream(Stream: TStream); override;
      procedure LoadFromFile(const FileName: string); override;
      procedure SaveToFile(const FileName: string); override;
      procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
      procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    end;

  TLinarGraphic = TLinearGraphic; // Spelling bug correction

  TBitmapLoaderList = array[0..0] of TBitmapLoader;
  PBitmapLoaderList = ^TBitmapLoaderList;

  TBitmapLoaders = class(TMonitorObject)
                     Loaders : PBitmapLoaderList;
                     Count, MemCount : Integer;

                     procedure AddLoader(Loader: TBitmapLoader);
                     function GetLoader(Ext: string): TBitmapLoader;
                     function GetSaver(Ext: string): TBitmapLoader;
                     function GetLoadFilter: string; // Filter for open dialog
                     function GetSaveFilter: string; // Filter for save dialog
                     destructor Destroy; override;
                   end;

  ELinearBitmap = class(Exception);
  EUnsupportedFileFormat = class(Exception);

  TProgressUpdateProc = procedure(Done: Integer) of object;

var
  BitmapLoaders : TBitmapLoaders;
  ProgressUpdate : TProgressUpdateProc = nil;
  GrayPal: TPalette; // Grayscale palette

const
  BlackPix24 : RGBRec = (B:0;G:0;R:0);
  WhitePix24 : RGBRec = (B:255;G:255;R:255);

function GrayPix24(Level: Byte): RGBRec;

// Swap red and blue color components
function RGB2BGR(const Color: TColor): TColor;
function RGB2TColor(R,G,B: Byte): TColor;
function GetRGBRec(R,G,B: Byte): RGBRec; overload;
function GetRGBRec(Color: TColor): RGBRec; overload;
function ColorTone(Col1,Col2: TColor; Tone,Max: Integer): TColor;

procedure MakeLogPalette(const Pal: TPalette; var PalEntries; ColorCount: Integer = 256);
function MakeHPalette(const Pal: TPalette; ColorCount: Integer = 256): HPALETTE;
function GetFromRGBPalette(var Pal): TPalette;
function GetFromHPalette(const HPal: HPALETTE; ColorCount: Integer=256): TPalette;
function BestColorMatch(Color: TColor; const Palette: TPalette): Byte;

procedure AddLoader(Loader: TBitmapLoader);

procedure DrawHDIBToTBitmap(HDIB: THandle; Bitmap: TBitmap);
procedure DrawDIBToTBitmap(var DIB: TBitmapInfo; Bitmap: TBitmap);

procedure MakeGrayPal(var Palette; ColorCount: Integer);
procedure DeInterleave(Image: TLinearBitmap; Source: PByteArray=nil; Planes: Integer=3);

// Copied from FileUtils to avoid including Forms unit
function ExtractFileExtNoDotUpper(const FileName: string): string;

procedure FreeLinearBitmapArray(var Images: TLinearBitmapArray);

implementation

uses FileMappedBitmap, PluginLinearBitmap;

function ExtractFileExtNoDotUpper(const FileName: string): string;
var I : Integer;
begin
  I := LastDelimiter('.\:',FileName);
  if (I>0) and (FileName[I]='.') then Result:=UpperCase(Copy(FileName,I+1,MaxInt))
  else Result:='';
end;

function GrayPix24(Level: Byte): RGBRec;
begin
  Result.R:=Level;
  Result.G:=Level;
  Result.B:=Level;
end;

function RGB2TColor(R,G,B: Byte): TColor;
begin
  Result:=R or (Integer(G) shl 8) or (Integer(B) shl 16);
end;

function GetRGBRec(R,G,B: Byte): RGBRec;
begin
  Result.R:=R; Result.G:=G; Result.B:=B;
end;

function GetRGBRec(Color: TColor): RGBRec;
begin
  Result.R:=TPaletteEntry(Color).peRed;
  Result.G:=TPaletteEntry(Color).peGreen;
  Result.B:=TPaletteEntry(Color).peBlue;
end;

function RGB2BGR(const Color: TColor): TColor;
begin
  Result:=Color;
  RGBQuad(Result).rgbRed:=RGBQuad(Color).rgbBlue;
  RGBQuad(Result).rgbBlue:=RGBQuad(Color).rgbRed;
end;

function ColorTone(Col1,Col2: TColor; Tone,Max: Integer): TColor;
var
  Scale : Single;
begin
  with TPaletteEntry(Result) do
  begin
    Scale:=1/Max;
    peRed:=Round((TPaletteEntry(Col1).peRed*Tone+TPaletteEntry(Col2).peRed*(Max-Tone))*Scale);
    peGreen:=Round((TPaletteEntry(Col1).peGreen*Tone+TPaletteEntry(Col2).peGreen*(Max-Tone))*Scale);
    peBlue:=Round((TPaletteEntry(Col1).peBlue*Tone+TPaletteEntry(Col2).peBlue*(Max-Tone))*Scale);
    peFlags:=0;
  end;
end;

procedure DrawHDIBToTBitmap(HDIB: THandle; Bitmap: TBitmap);
var BitmapInfo : ^TBitmapInfo;
begin
  BitmapInfo:=GlobalLock(HDIB);
  if Assigned(BitmapInfo) then
  try
    DrawDIBToTBitmap(BitmapInfo^,Bitmap);
  finally
    GlobalUnlock(HDIB);
  end;
end;

procedure DrawDIBToTBitmap(var DIB: TBitmapInfo; Bitmap: TBitmap); // BitmapInfo

  function DibNumColors(pv: Pointer): Integer;
  begin
    if PBitmapInfoHeader(pv)^.biSize=sizeof(TBITMAPCOREHEADER) then
      Result:=1 shl PBITMAPCOREHEADER(pv)^.bcBitCount
    else if PBitmapInfoHeader(pv)^.biClrUsed=0 then
      Result:=1 shl PBitmapInfoHeader(pv)^.biBitCount
    else Result:=PBitmapInfoHeader(pv)^.biClrUsed;

    if (Result>256) then Result:=0;
  end;

var
  Map : ^Byte;
  I : Integer;
  BytesPerLine, Colors : DWord;
  LogPal: PLogPalette;
begin
  with DIB.bmiHeader do
  begin
    case biBitCount of
       1 : Bitmap.PixelFormat:=pf1bit;
       4 : Bitmap.PixelFormat:=pf4bit;
       8 : Bitmap.PixelFormat:=pf8bit;
      16 : Bitmap.PixelFormat:=pf16bit;
      24 : Bitmap.PixelFormat:=pf24bit;
      32 : Bitmap.PixelFormat:=pf32bit;
    else
      raise Exception.Create(rsInvalidPixelFormat);
    end;
    Bitmap.Width:=biWidth;
    Bitmap.Height:=biHeight;

    Colors:=0;
    if (biBitCount<=8) then
    begin
      Colors:=DibNumColors(@DIB);
      GetMem(LogPal,SizeOf(TLogPalette)+SizeOf(TPaletteEntry)*Colors);
      try
        LogPal^.PalVersion:=$300;
        LogPal^.PalNumEntries:=Colors;
        for I:=0 to Colors-1 do with LogPal^.palPalEntry[I] do
        begin
          peRed:=DIB.bmiColors[I].rgbRed;
          peGreen:=DIB.bmiColors[I].rgbGreen;
          peBlue:=DIB.bmiColors[I].rgbBlue;
          peFlags:=PC_RESERVED;
        end;
        Bitmap.Palette:=CreatePalette(LogPal^);
      finally
        FreeMem(LogPal);
      end;
    end;

    Map:=Pointer(DWord(@DIB)+biSize+Colors*SizeOf(TRGBQUAD));

    if biCompression=BI_RGB then
    begin
      BytesPerLine:=(biWidth*biBitCount div 8+3) and $fffffffc;
      if biHeight<0 then
      for I:=0 to -biHeight-1 do
      begin
        Move(Map^,Bitmap.ScanLine[I]^,BytesPerLine);
        Inc(Map,BytesPerLine);
      end
      else
      for I:=biHeight-1 downto 0 do
      begin
        Move(Map^,Bitmap.ScanLine[I]^,BytesPerLine);
        Inc(Map,BytesPerLine);
      end;
    end
    else
    begin
      if (biBitCount in [16,32]) and (biCompression=BI_BITFIELDS) then Inc(Map,12);
      StretchDIBits(Bitmap.Canvas.Handle,
                    0,0,biWidth,biHeight,
                    0,0,biWidth,biHeight,
                    Map,DIB,
                    DIB_RGB_COLORS,SRCCOPY);
       //raise Exception.Create(rsUnsupportedBitmapFormat);
    end;
  end;
end;

procedure MakeGrayPal(var Palette; ColorCount: Integer);
var
  I, C : Integer;
begin
  Dec(ColorCount);
  For I:=0 to ColorCount do
  begin
    C:=I*255 div ColorCount;
    TPalette(Palette)[I].B:=C;
    TPalette(Palette)[I].G:=C;
    TPalette(Palette)[I].R:=C;
  end;
end;

function BestColorMatch(Color: TColor; const Palette: TPalette): Byte;
var
  I, BestDist, Dist : Integer;
begin
  with TPaletteEntry(Color) do
  begin
    Result:=Byte(Color);
    with Palette[Result] do
      if (R=peRed) and (G=peGreen) and (B=peBlue) then Exit; // Grayscale color on grayscale palette
    BestDist:=High(BestDist);
    for I:=0 to 255 do with Palette[I] do
    begin
      Dist:=Sqr(R-peRed)+Sqr(G-peGreen)+Sqr(B-peBlue);
      if Dist<BestDist then
      begin
        Result:=I;
        if Dist=0 then Exit;
        BestDist:=Dist;
      end;
    end;
  end;
end;

//==============================================================================================================================
// TLinearBitmap
//==============================================================================================================================

constructor TLinearBitmap.Create;
begin
  inherited Create;
  fPixelFormat:=pfCustom;
end;

constructor TLinearBitmap.Create(Width,Height: Integer; PixFormat: TPixelFormat);
begin
  inherited Create;
  fPixelFormat:=pfCustom;
  New(Width,Height,PixFormat);
end;

constructor TLinearBitmap.Create(Other: TObject);
begin
  inherited Create;
  fPixelFormat:=pfCustom;
  Assign(Other);
end;

procedure TLinearBitmap.New(Width,Height: Integer; PixFormat: TPixelFormat; BytesPerLine: Integer);
begin
  if FPresent and (FWidth=Width) and (FHeight=Height) and (PixelFormat=PixFormat) then Exit;

  if not (PixFormat in [pf8bit,pf16bit,pf24bit,pf32bit]) then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  if (Width<1) or (Height<1) then raise ELinearBitmap.Create(rsInvalidBitmapSize);

  if Assigned(Map) then FreeMem(Map);

  if PixFormat=pf8bit then
  begin
    fPixelSize:=1;
    if Palette=nil then GetMem(Palette,SizeOf(TPalette));
  end
  else
  begin
    if PixFormat=pf24bit then fPixelSize:=3
    else if PixFormat=pf16bit then fPixelSize:=2
    else fPixelSize:=4;
    if Assigned(Palette) then
    begin
      FreeMem(Palette);
      Palette:=nil;
    end;
  end;
  if Int64(Width)*Height*fPixelSize>=$7fffffff then raise ELinearBitmap.Create(SOutOfMemory);
  fWidth:=Width; fHeight:=Height;
  fBytesPerLine:=Width*PixelSize;
  if BytesPerLine<>0 then
  begin
    if BytesPerLine<fBytesPerLine then raise ELinearBitmap.Create(rsInvalidBitmapSize);
    fBytesPerLine:=BytesPerLine;
  end;
  fPixelFormat:=PixFormat;
  fSize:=fBytesPerLine*Height;
  try
    GetMem(Map,Size+1);
  except
    fPresent:=False; fWidth:=0; fHeight:=0; fSize:=0; fPixelFormat:=pfCustom; fPixelSize:=0;
    raise;
  end;
  fPresent:=True;
end;

procedure TLinearBitmap.Dispose;
begin
  if Assigned(Map) then
  begin
    FreeMem(Map);
    Map:=nil;
  end;
  if Assigned(Palette) then
  begin
    FreeMem(Palette);
    Palette:=nil;
  end;
  fPresent:=False;
  fWidth:=0; fHeight:=0; fSize:=0; fPixelFormat:=pfCustom; fPixelSize:=0;
end;

function TLinearBitmap.MustUpdateScanLine: Boolean;
begin
  Result:=False;
end;

procedure TLinearBitmap.Assign(Other: TObject);

  procedure GetFromTBitmap(Bitmap: TBitmap);
  var Y : Integer;
  begin
    if (Bitmap.Width=0) or (Bitmap.Height=0) then Dispose
    else
    begin
      New(Bitmap.Width,Bitmap.Height,Bitmap.PixelFormat);
      Bitmap.Canvas.Lock;
      try
        for Y:=0 to Height-1 do
           Move(Bitmap.ScanLine[Y]^,ScanLine[Y]^,BytesPerLine);
        if PixelFormat=pf8bit then Palette^:=GetFromHPalette(Bitmap.Palette);
      finally
        Bitmap.Canvas.Unlock;
      end;
    end
  end;

var
  Bitmap : TBitmap;
  Y : Integer;
begin
  if Other is TLinearBitmap then
  begin
    if TLinearBitmap(Other).Present then
    begin
      New(TLinearBitmap(Other).Width,TLinearBitmap(Other).Height,TLinearBitmap(Other).PixelFormat);
      for Y:=0 to Height-1 do Move(TLinearBitmap(Other).ScanLineSafe[Y]^,ScanLineSafe[Y]^,BytesPerLine);
      if PixelFormat=pf8bit then Palette^:=TLinearBitmap(Other).Palette^;
    end
    else Dispose;
  end
  else if (Other is TBitmap) and (TBitmap(Other).PixelFormat in [pf8bit,pf24bit,pf32bit]) then
    GetFromTBitmap(TBitmap(Other))
  else if Other is TGraphic then
  begin
    Bitmap:=TBitmap.Create;
    try
      Bitmap.Canvas.Lock;
      try
        try
          Bitmap.Assign(TPersistent(Other));
          if not (Bitmap.PixelFormat in [pf8bit,pf24bit,pf32bit]) then
            Bitmap.PixelFormat:=pf24bit;
        except
          // Assign didn't work, try Draw
          Bitmap.PixelFormat:=pf24bit;
          Bitmap.Width:=TGraphic(Other).Width;
          Bitmap.Height:=TGraphic(Other).Height;
          Bitmap.Canvas.Draw(0,0,TGraphic(Other));
        end;
      finally
        Bitmap.Canvas.Unlock;
      end;
      GetFromTBitmap(Bitmap);
    finally
      Bitmap.Free;
    end
  end
  else if Other is TPersistent then
  begin
    Bitmap:=TBitmap.Create;
    try
      Bitmap.Assign(TPersistent(Other));
      if not (Bitmap.PixelFormat in [pf8bit,pf24bit]) then Bitmap.PixelFormat:=pf24bit;
      GetFromTBitmap(Bitmap)
    finally
      Bitmap.Free;
    end
  end
  else inherited;
end;

procedure TLinearBitmap.AssignTo(Other: TObject);
begin
  if Other is TBitmap then
  begin
    if not Present then Exit;
    if PixelFormat=pf16bit then TBitmap(Other).PixelFormat:=pf8bit
    else TBitmap(Other).PixelFormat:=PixelFormat;
    if PixelFormat=pf8bit then TBitmap(Other).Palette:=MakeHPalette(Palette^)
    else if PixelFormat=pf16bit then TBitmap(Other).Palette:=MakeHPalette(GrayPal);
    TBitmap(Other).Width:=Width; TBitmap(Other).Height:=Height;
    PaintToTBitmap(TBitmap(Other));
  end
  else if Other is TPicture then AssignTo(TPicture(Other).Bitmap)
  else inherited;
end;

procedure TLinearBitmap.TakeOver(Other: TLinearBitmap);
begin
  Dispose;
  if Other.ClassType=TFileMappedBitmap then
    TFileMappedBitmap(Other).MapInPhysicalMemory
  else if Other.ClassType=TPluginLinearBitmap then
    TPluginLinearBitmap(Other).ForceLocal
  else if Other.ClassType<>TLinearBitmap then
  begin
    Assign(Other);
    Other.Dispose;
    Exit;
  end;
  fWidth:=Other.fWidth;
  fHeight:=Other.fHeight;
  fPixelFormat:=Other.fPixelFormat;
  fPixelSize:=Other.fPixelSize;
  fBytesPerLine:=Other.fBytesPerLine;
  fSize:=Other.fSize;
  fPresent:=Other.Present; Other.FPresent:=False;
  Palette:=Other.Palette; Other.Palette:=nil;
  Map:=Other.Map; Other.Map:=nil;
end;

procedure TLinearBitmap.OptimizeMem;
var
  Y, NewBytesPerLine : Integer;
  NewMap : PByteArray;
begin
  NewBytesPerLine:=Width*PixelSize;
  if Present and (BytesPerLine<>NewBytesPerLine) then
  begin
    fSize:=Height*NewBytesPerLine;
    GetMem(NewMap,Size+1);
    for Y:=0 to Height-1 do Move(Map^[Y*BytesPerLine],NewMap^[Y*NewBytesPerLine],NewBytesPerLine);
    FreeMem(Map);
    Map:=NewMap;
    fBytesPerLine:=NewBytesPerLine;
  end;
end;

procedure TLinearBitmap.Clear(Value: Byte);
begin
  FillChar(Map^,Size,Value);
end;

procedure TLinearBitmap.Clear(Value: Byte; Rect: TRect);
var
  Y, X, Len : Integer;
begin
  X:=Rect.Left*PixelSize;
  Len:=(Rect.Right-Rect.Left+1)*PixelSize;
  for Y:=Rect.Top to Rect.Bottom do FillChar(PByteArray(ScanLineSafe[Y])^[X],Len,Value);
end;

procedure TLinearBitmap.ClearColor(Color: TColor);
var
  P : Integer;
  Pix24 : PRGBRec;
begin
  if Color=0 then ZeroMem(Map^,Size)
  else if PixelFormat=pf24bit then
  begin
    Color:=RGB2BGR(Color);
    Pix24:=@Map^;
    for P:=1 to Width*Height-1 do
    begin
      PDWord(Pix24)^:=Color;
      Inc(Pix24);
    end;
    Pix24^.R:=(Color shr 16) and $ff;
    Pix24^.G:=(Color shr 8) and $ff;
    Pix24^.B:=Color and $ff;
  end
  else Clear(((Color and $ff)*2+((Color shr 8) and $ff)*3+((Color shr 16) and $ff)+3) div 6);
end;

function TLinearBitmap.GetScanLine(Y: Integer): Pointer;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=@Map^[DWord(Y)*DWord(BytesPerLine)];
end;

function TLinearBitmap.GetScanLineSafe(Y: Integer): Pointer;
begin
  Assert((Y>=0) and (Y<Height));
  Result:=@Map^[Y*BytesPerLine];
end;

function TLinearBitmap.GetPixel(X,Y: Integer): Pointer;
begin
  Assert((X>=0) and (Y>=0) and (X<Width) and (Y<Height));
  Result:=@Map^[Y*BytesPerLine+X*PixelSize];
end;

function TLinearBitmap.GetPixelColor(X,Y: Integer): TColor;
begin
  if X<0 then X:=0
  else if X>=Width then X:=Width-1;
  if Y<0 then Y:=0
  else if Y>=Height then Y:=Height-1;
  case PixelFormat of
    pf8bit  : with Palette^[Map^[Y*BytesPerLine+X]] do
                Result:=RGB2TColor(R,G,B);
    pf24bit : with PRGBRec(@Map^[Y*BytesPerLine+X*3])^ do
                Result:=RGB2TColor(R,G,B);
    pf32bit : with PRGBRec(@Map^[Y*BytesPerLine+X*4])^ do
                Result:=RGB2TColor(R,G,B);
  else raise Exception.Create(rsInvalidPixelFormat);
  end
end;

function TLinearBitmap.GetPixelColorInterpolate(X,Y: Single): TColor;
var
  P00, P10 : TRGBQuad;
  P01, P11 : TRGBQuad;
  IX, IY, DX, DY : Integer;
begin
  if X<0 then X:=0;
  if Y<0 then Y:=0;
  IX:=Trunc(X); DX:=Round((X-IX)*256);
  IY:=Trunc(Y); DY:=Round((Y-IY)*256);
  P00:=TRGBQuad(GetPixelColor(IX,IY));   P10:=TRGBQuad(GetPixelColor(IX+1,IY));
  P01:=TRGBQuad(GetPixelColor(IX,IY+1)); P11:=TRGBQuad(GetPixelColor(IX+1,IY+1));
  Result:=0;
  with TRGBQuad(Result) do
  begin
    rgbBlue:=((P00.rgbBlue*(256-DX) + P10.rgbBlue*DX) * (256-DY) +
              (P01.rgbBlue*(256-DX) + P11.rgbBlue*DX) * DY + 32768) shr 16;
    rgbGreen:=((P00.rgbGreen*(256-DX) + P10.rgbGreen*DX) * (256-DY) +
               (P01.rgbGreen*(256-DX) + P11.rgbGreen*DX) * DY + 32768) shr 16;
    rgbRed:=((P00.rgbRed*(256-DX) + P10.rgbRed*DX) * (256-DY) +
             (P01.rgbRed*(256-DX) + P11.rgbRed*DX) * DY + 32768) shr 16;
  end;
end;

procedure TLinearBitmap.SetPixelColor(X,Y: Integer; Color: TColor);
begin
  if (X>=0) and (X<Width) and (Y>=0) and (Y<Height) then
  case PixelFormat of
    pf8bit  : Map^[Y*BytesPerLine+X]:=BestColorMatch(Color,Palette^);
    pf24bit : with PRGBRec(@Map^[Y*BytesPerLine+X*3])^ do
              begin
                R:=Color;
                G:=Color shr 8;
                B:=Color shr 16;
              end;
    pf32bit : with PRGBRec(@Map^[Y*BytesPerLine+X*4])^ do
              begin
                R:=Color;
                G:=Color shr 8;
                B:=Color shr 16;
              end;
  else raise Exception.Create(rsInvalidPixelFormat);
  end;
end;

function TLinearBitmap.GetPixelToneInterpolate(X,Y: Single): Integer;
var
  P00, P10 : Integer;
  P01, P11 : Integer;
  IX, IY, DX, DY : Integer;
begin
  if X<0 then X:=0;
  if Y<0 then Y:=0;
  IX:=Trunc(X); DX:=Round((X-IX)*256);
  IY:=Trunc(Y); DY:=Round((Y-IY)*256);
  P00:=GetPixelTone(IX,IY);   P10:=GetPixelTone(IX+1,IY);
  P01:=GetPixelTone(IX,IY+1); P11:=GetPixelTone(IX+1,IY+1);
  Result:=((P00*(256-DX) + P10*DX) * (256-DY) +
           (P01*(256-DX) + P11*DX) * DY + 32768) shr 16;
end;

function TLinearBitmap.GetPixelTone(X, Y: Integer): Integer;
begin
  if X<0 then X:=0
  else if X>=Width then X:=Width-1;
  if Y<0 then Y:=0
  else if Y>=Height then Y:=Height-1;
  case PixelFormat of
    pf8bit  : Result:=Map^[Y*BytesPerLine+X];
    pf16bit : Result:=PWord(@Map^[Y*BytesPerLine+X*2])^;
    pf24bit : with PRGBRec(@Map^[Y*BytesPerLine+X*3])^ do Result:=(R*2+G*3+B+3) div 6;
  else raise Exception.Create(rsInvalidPixelFormat);
  end
end;

procedure TLinearBitmap.SetPixelTone(X, Y: Integer; const Value: Integer);
begin
  if (X>=0) and (X<Width) and (Y>=0) and (Y<Height) then
  case PixelFormat of
    pf8bit  : Map^[Y*BytesPerLine+X]:=Value;
    pf16bit : PWord(@Map^[Y*BytesPerLine+2*X])^:=Value;
    pf24bit : PRGBRec(@Map^[Y*BytesPerLine+X*3])^:=GrayPal[Value];
  else raise Exception.Create(rsInvalidPixelFormat);
  end;
end;

function TLinearBitmap.IsGrayScale: Boolean;
var
  F, C : Integer;
begin
  if Present then
  case PixelFormat of
    pf8bit : begin
               IsGrayScale:=True;
               for F:=0 to 255 do if (Palette^[F].R<>F) or (Palette^[F].G<>F) or (Palette^[F].B<>F) then
               begin
                 IsGrayScale:=False; Break;
               end;
             end;
    pf16bit : IsGrayScale:=True;
    pf24bit : begin
                IsGrayScale:=True;
                C:=0;
                for F:=0 to Width*Height-1 do
                begin
                 if (Map^[C]<>Map^[C+1]) or (Map^[C]<>Map^[C+2]) then
                 begin
                  IsGrayScale:=False; Break;
                 end;
                 Inc(C,3);
                end;
              end;
    else IsGrayScale:=False;
  end
  else IsGrayScale:=False;
end;

destructor TLinearBitmap.Destroy;
begin
  if Present then Dispose;
  inherited Destroy;
end;

function TLinearBitmap.LoadFromFile(const FileName: string): TBitmapLoader;
var
  Ext : string;
begin
  Ext:=ExtractFileExtNoDotUpper(FileName);
  Result:=BitmapLoaders.GetLoader(Ext);
  Dispose;
  Result.LoadFromFile(FileName,Ext,Self);
end;

function TLinearBitmap.SaveToFile(const FileName: string): TBitmapLoader;
var
  Ext : string;
begin
  Ext:=ExtractFileExtNoDotUpper(FileName);
  Result:=BitmapLoaders.GetSaver(Ext);
  Result.SaveToFile(FileName,Ext,Self)
end;

procedure TLinearBitmap.LoadFromStream(Stream: TSeekableStream; const FormatExt: string);
var
  BitmapLoader : TBitmapLoader;
begin
  Assert(UpperCase(FormatExt)=FormatExt);
  BitmapLoader:=BitmapLoaders.GetLoader(FormatExt);
  Dispose;
  BitmapLoader.LoadFromStream(Stream,FormatExt,Self);
end;

procedure TLinearBitmap.LoadFromStream(Stream: TStream; const FormatExt: string);
var
  TmpStream : TDelphiFilterStream;
begin
  TmpStream:=TDelphiFilterStream.Create(Stream);
  try
    LoadFromStream(TmpStream,FormatExt);
  finally
    TmpStream.Free;
  end;
end;

procedure TLinearBitmap.SaveToStream(Stream: TStream; const FormatExt: string);
var
  TmpStream : TDelphiFilterStream;
begin
  TmpStream:=TDelphiFilterStream.Create(Stream);
  try
    SaveToStream(TmpStream,FormatExt);
  finally
    TmpStream.Free;
  end;
end;

procedure TLinearBitmap.SaveToStream(Stream: TSeekableStream; const FormatExt: string);
begin
  Assert(UpperCase(FormatExt)=FormatExt);
  BitmapLoaders.GetSaver(FormatExt).SaveToStream(Stream,FormatExt,Self);
end;

// Return size
const ReserveForPalette = 256*SizeOf(TRGBQuad);
function TLinearBitmap.MakeDIB(out Bitmap: PBitmapInfo): Integer;
var
 Pix : ^Byte;
 LinLen, Y : Integer;
begin
  case PixelFormat of
    pf8bit  : begin
                LinLen:=(Width+3) and $fffffffc;
                Result:=SizeOf(TBitmapInfoHeader)+ReserveForPalette+LinLen*Height;
                GetMem(Bitmap,Result);
                ZeroMem(Bitmap^.bmiHeader,SizeOf(TBitmapInfoHeader));
                with Bitmap^.bmiHeader do  // BITMAPINFOHEADER
                begin
                  biSize:=SizeOf(TBitmapInfoHeader);
                  biWidth:=Width;
                  biHeight:=Height;
                  biPlanes:=1;
                  biBitCount:=8;
                  biClrUsed:=256;
                  biClrImportant:=256;
                  biCompression:=BI_RGB;
                  biSizeImage:=LinLen*Height;
                end;
                for Y:=0 to 255 do with Bitmap^.bmiColors[Y] do
                begin
                  rgbRed:=Palette^[Y].R; rgbGreen:=Palette^[Y].G; rgbBlue:=Palette^[Y].B;
                  rgbReserved:=0;
                end;
                Pix:=Pointer(Integer(Bitmap)+SizeOf(TBitmapInfoHeader)+ReserveForPalette);
                for Y:=Height-1 downto 0 do
                begin
                  Move(ScanLineSafe[Y]^,Pix^,BytesPerLine);
                  Inc(Pix,LinLen);
                end;
              end;
    pf24bit : begin
                LinLen:=(Width*3+3) and $fffffffc;
                Result:=SizeOf(TBitmapInfoHeader)+LinLen*Height;
                GetMem(Bitmap,Result);
                ZeroMem(Bitmap^.bmiHeader,SizeOf(TBitmapInfoHeader));
                with Bitmap^.bmiHeader do  // BITMAPINFO HEADER
                begin
                  biSize:=SizeOf(TBitmapInfoHeader);
                  biWidth:=Width;
                  biHeight:=Height;
                  biPlanes:=1;
                  biBitCount:=24;
                  biCompression:=BI_RGB;
                  biSizeImage:=LinLen*Height;
                end;
                Pix:=Pointer(Integer(Bitmap)+SizeOf(TBitmapInfoHeader));
                for Y:=Height-1 downto 0 do
                begin
                  Move(ScanLineSafe[Y]^,Pix^,BytesPerLine);
                  Inc(Pix,LinLen);
                end;
              end;
    else
    begin
      Assert(False,rsInvalidPixelFormat);
      Bitmap:=nil;
      MakeDIB:=0;
    end;
  end;
end;

procedure TLinearBitmap.GetFromDIB(var DIB: TBitmapInfo); //  Windows BITMAPINFO struct
var
  C, Y, LinInc, ColorCount : Integer;
  OPix, NPix : ^Byte;
  PalEntry : ^TRGBQuad;
  Bitmap : TBitmap;
begin
  with DIB.bmiHeader do
  if (biBitCount in [8,24]) and (biCompression=BI_RGB) then
  begin
    if biBitCount=8 then New(biWidth,Abs(biHeight),pf8bit)
    else New(biWidth,Abs(biHeight),pf24bit);
    case biBitCount of
      8 : begin
            if biClrUsed=0 then ColorCount:=256
            else ColorCount:=biClrUsed;
            PalEntry:=Pointer(DWord(@DIB)+biSize);
            for C:=0 to ColorCount-1 do
            begin
              Palette^[C].R:=PalEntry^.rgbRed; Palette^[C].G:=PalEntry^.rgbGreen; Palette^[C].B:=PalEntry^.rgbBlue;
              Inc(PalEntry);
            end;
            for C:=ColorCount to 255 do Palette^[C]:=BlackPix24;
            LinInc:=(Width+3) and $fffffffc; // Rund op til nærmeste 4 bytes
            OPix:=Pointer(PalEntry);
          end;
     24 : begin
            LinInc:=(Width*3+3) and $fffffffc; // Rund op til nærmeste 4 bytes
            OPix:=Pointer(DWord(@DIB)+biSize);
          end;
    else
      raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
    if biHeight<0 then
    for Y:=0 to Height-1 do
    begin
      NPix:=ScanLineSafe[Y];
      Move(OPix^,NPix^,BytesPerLine);
      Inc(OPix,LinInc);
    end
    else
    for Y:=Height-1 downto 0 do // Upside-down
    begin
      NPix:=ScanLineSafe[Y];
      Move(OPix^,NPix^,BytesPerLine);
      Inc(OPix,LinInc);
    end;
  end
  else
  begin
    Bitmap:=TBitmap.Create;
    try
      DrawDIBToTBitmap(DIB,Bitmap);
      {if Bitmap.PixelFormat in [pf1bit,pf4bit] then
      begin
        GetFromTBitmap(Bitmap);
        Palette^:=GetFromHPalette(Bitmap.Palette);
        Bitmap.PixelFormat:=pf8bit;
      end
      else} if not (Bitmap.PixelFormat in [pf8bit,pf24bit]) then
      begin
        Bitmap.PixelFormat:=pf24bit;
        Assign(Bitmap);
      end
      else Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TLinearBitmap.GetFromHDIB(HDIB: HBitmap);
var
  Bitmap : ^TBitmapInfo;
begin
  if HDIB<>0 then
  try
    Bitmap:=GlobalLock(HDIB);
    if Bitmap=nil then Exit;
    GetFromDIB(Bitmap^);
  finally
    GlobalUnLock(HDIB);
  end;
end;

function TLinearBitmap.GetFromClipboard;
var
  HDIB : HBitmap;
  Bitmap : TBitmap;
  Metafile : TMetafile;
begin
  if OpenClipboard(0) then
  try
    if IsClipboardFormatAvailable(CF_DIB) then // Get as bitmap
    begin
      HDIB:=GetClipboardData(CF_DIB);
      GetFromHDIB(HDIB);
      Result:=(HDIB<>0) and Present;
    end
    else if IsClipboardFormatAvailable(CF_ENHMETAFILE) then // Get as Windows metafile
    begin
      Metafile:=TMetafile.Create;
      try
        Metafile.LoadFromClipboardFormat(0,0,0);
        Bitmap:=TBitmap.Create;
        try
          Bitmap.PixelFormat:=pf24bit;
          Bitmap.Width:=Metafile.Width;
          Bitmap.Height:=Metafile.Height;
          Bitmap.Canvas.Draw(0,0,Metafile);
          Assign(Bitmap);
        finally
          Bitmap.Free;
        end;
      finally
        Metafile.Free;
      end;
      Result:=Present;
    end
    else Result:=False;
  finally
    CloseClipboard;
  end
  else Result:=False;
end;

procedure TLinearBitmap.CopyToClipboard(EmptyExistingClipboard: Boolean);
var
  PDIB : Pointer;
  BitmapInfo : PBitmapInfo;
  HDIB : THandle;
  MemSize : Integer;
begin
  if not Present then Exit;
  if not EmptyExistingClipboard or OpenClipboard(0) then
  try
    MemSize:=MakeDIB(BitmapInfo);
    try
      HDIB:=GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE,MemSize);
      PDIB:=GlobalLock(HDIB);
      try
        Move(BitmapInfo^,PDIB^,MemSize);
      finally
        GlobalUnLock(HDIB);
      end;
      if EmptyExistingClipboard then EmptyClipboard;
      SetClipboardData(CF_DIB,HDIB);
    finally
      FreeMem(BitmapInfo);
    end;
  finally
    if EmptyExistingClipboard then CloseClipboard;
  end
end;

procedure TLinearBitmap.PasteImage(Source: TLinearBitmap; X,Y,SrcWidth,SrcHeight: Integer);
var
  CopySize : Integer;
  DestRect : TRect;
  SrcLine, DestLine : PByte;
begin
  if PixelFormat<>Source.PixelFormat then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  if not IntersectRect(DestRect,Rect(0,0,Width,Height),Bounds(X,Y,Min(Source.Width,SrcWidth),Min(Source.Height,SrcHeight))) then Exit;
  SrcLine:=Source.Pixel[Max(0,-X),Max(0,-Y)];
  DestLine:=Pixel[DestRect.Left,DestRect.Top];
  CopySize:=(DestRect.Right-DestRect.Left)*PixelSize;
  for Y:=DestRect.Top to DestRect.Bottom-1 do
  begin
    Move(SrcLine^,DestLine^,CopySize);
    Inc(SrcLine,Source.BytesPerLine);
    Inc(DestLine,BytesPerLine);
  end;
end;

procedure TLinearBitmap.PasteImage(Source, Mask: TLinearBitmap; X,Y: Integer);
var
  CopySize, I : Integer;
  DestRect : TRect;
  SrcLine, MaskLine, DestLine : PByteArray;
begin
  Assert(Source.Width=Mask.Width);
  Assert(Source.Height=Mask.Height);
  Assert(Source.PixelFormat=Mask.PixelFormat);
  if PixelFormat<>Source.PixelFormat then raise ELinearBitmap.Create(rsInvalidPixelFormat);
  if not IntersectRect(DestRect,Rect(0,0,Width,Height),Bounds(X,Y,Source.Width,Source.Height)) then Exit;
  SrcLine:=Source.Pixel[Max(0,-X),Max(0,-Y)];
  MaskLine:=Mask.Pixel[Max(0,-X),Max(0,-Y)];
  DestLine:=Pixel[DestRect.Left,DestRect.Top];
  CopySize:=(DestRect.Right-DestRect.Left)*PixelSize;
  for Y:=DestRect.Top to DestRect.Bottom-1 do
  begin
    for I:=0 to CopySize-1 do if MaskLine[I]<>0 then DestLine[I]:=SrcLine[I];
    Inc(PByte(SrcLine),Source.BytesPerLine);
    Inc(PByte(MaskLine),Mask.BytesPerLine);
    Inc(PByte(DestLine),BytesPerLine);
  end;
end;

procedure TLinearBitmap.ResizeCanvas(XSiz,YSiz,XPos,YPos: Integer; Color: TColor);
var
  OldMap : PByteArray;
  Y, XStart, OldBytesPerLine, LineMove, Bottom : Integer;
  NewSize : Int64;
begin
  if (XSiz<1) or (YSiz<1) then
  begin
    Dispose;
    Exit;
  end;
  if not Present then
  begin
    New(XSiz,YSiz,pf8bit);
    Palette^:=GrayPal;
    ClearColor(Color);
    Exit;
  end;      
  if (XSiz=Width) and (YSiz=Height) and (XPos=0) and (YPos=0) then Exit;

  NewSize:=Int64(XSiz)*YSiz*fPixelSize;
  if NewSize>=$7fffffff then raise ELinearBitmap.Create(SOutOfMemory);
  GetMem(OldMap,NewSize+1);
  try
    OldBytesPerLine:=BytesPerLine;
    fBytesPerLine:=XSiz*PixelSize;
    fSize:=BytesPerLine*YSiz;
    SwapDWords(OldMap,Map);
    
    if XPos>0 then XStart:=0
    else
    begin
      XStart:=-XPos*PixelSize;
      XPos:=0;
    end;
    XPos:=XPos*PixelSize;
    LineMove:=OldBytesPerLine-XStart;
    if LineMove+XPos>BytesPerLine then LineMove:=BytesPerLine-XPos;
    Bottom:=Min(Height-1,YSiz-YPos-1);
    fWidth:=XSiz; fHeight:=YSiz;
    ClearColor(Color);
    if LineMove>0 then for Y:=Max(0,-YPos) to Bottom do
      Move(OldMap^[Y*OldBytesPerLine+XStart],Map^[(Y+YPos)*BytesPerLine+XPos],LineMove);
  finally
    FreeMem(OldMap);
  end;
end;

procedure TLinearBitmap.SetPixelFormat(PixFormat: TPixelFormat);

  procedure Make32bit;
  var
    OldMap : PByteArray;
    Pix : PRGBRec;
    NewPix : PDWord;
    P : Integer;
  begin
    if PixelFormat=pf8bit then PixelFormat:=pf24bit;
    OldMap:=Map;
    fBytesPerLine:=Width*4;
    fPixelSize:=4;
    GetMem(Map,BytesPerLine*Height);
    Pointer(Pix):=OldMap;
    Pointer(NewPix):=Map;
    for P:=1 to Width*Height do
    begin
      NewPix^:=PDWord(Pix)^ and $ffffff;
      Inc(Pix); Inc(NewPix);
    end;
    fSize:=BytesPerLine*Height;
    fPixelFormat:=pf32bit;
    FreeMem(OldMap);
    FreeAndNilData(Palette);
  end;

  procedure Make24bit;
  var
    OldMap : PByteArray;
    NewPix : ^RGBRec;
    Pix : ^Byte;
    Pix32 : PDWord;
    P : Integer;
  begin
    OldMap:=Map;
    fBytesPerLine:=Width*3;
    fPixelSize:=3;
    GetMem(Map,BytesPerLine*Height+1);
    Pointer(NewPix):=Map;
    if PixelFormat=pf8bit then
    begin
      Pointer(Pix):=OldMap;
      for P:=1 to Size do
      begin
        NewPix^:=Palette^[Pix^];
        Inc(Pix); Inc(NewPix);
      end;
      FreeAndNilData(Palette);
    end
    else if PixelFormat=pf16bit then
    begin
      Integer(Pix):=Integer(OldMap)+1;
      for P:=1 to Width*Height do
      begin
        NewPix^.B:=Pix^;
        NewPix^.G:=Pix^;
        NewPix^.R:=Pix^;
        Inc(Pix,2); Inc(NewPix);
      end;
    end
    else
    begin
      Pointer(Pix32):=OldMap;
      for P:=1 to Width*Height do
      begin
        NewPix^:=PRGBRec(Pix32)^;
        Inc(Pix32); Inc(NewPix);
      end;
    end;
    fSize:=BytesPerLine*Height;
    fPixelFormat:=pf24bit;
    FreeMem(OldMap);
  end;

  procedure Make8bit;
  var
    OldMap : PByteArray;
    NewPix : ^Byte;
    Pix : ^RGBRec;
    Pix16 : PWord;
    P, C : Integer;
  begin
    if PixelFormat=pf32bit then PixelFormat:=pf24bit;
    OldMap:=Map;
    fBytesPerLine:=Width;
    fSize:=BytesPerLine*Height;
    fPixelSize:=1;
    System.New(Palette);
    GetMem(Map,Size+1);
    Pointer(NewPix):=Map;
    if PixelFormat=pf24bit then // Use 332 palette
    begin
      Pointer(Pix):=OldMap;
      for P:=0 to Size-1 do
      begin
        NewPix^:=(Pix^.B shr 6) or ((Pix^.G shr 5) and 7 shl 2) or (Pix^.R and $e0);
        Inc(Pix); Inc(NewPix);
      end;
      for C:=0 to 255 do // Create 332 palette
      begin
        Palette^[C].R:=Round(((C shr 5) and 7)*(255/7));
        Palette^[C].G:=Round(((C shr 2) and 7)*(255/7));
        Palette^[C].B:=Round((C and 3)*(255/3));
      end;
    end
    else // 16 bit
    begin
      Pointer(Pix16):=OldMap;
      for P:=0 to Size-1 do
      begin
        NewPix^:=Hi(Pix16^);
        Inc(Pix16); Inc(NewPix);
      end;
      Palette^:=GrayPal;
    end;
    FreeMem(OldMap);
    fPixelFormat:=pf8bit;
  end;

begin
  if not Present or (PixFormat=PixelFormat) then Exit;
  case PixFormat of
    pf8bit  : Make8bit;
    pf24bit : Make24bit;
    pf32bit : Make32bit;
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
  end;
end;

procedure TLinearBitmap.PaintToTBitmap(Target: TBitmap);
var
  Y, X : Integer;
  Pix24 : ^RGBRec;
  Pix8 : ^Byte;
  Pix16 : ^Word;
  Line0, Line1 : Integer;
begin
  if not Present or (Height<1) then Exit;
  if PixelFormat=Target.PixelFormat then
  begin
    Line0:=Integer(Target.ScanLine[0]);
    Move(ScanLineSafe[0]^,Pointer(Line0)^,BytesPerLine);
    if Height>1 then
    begin
      Line1:=Integer(Target.ScanLine[1]);
      Move(ScanLineSafe[1]^,Pointer(Line1)^,BytesPerLine);
      Dec(Line1,Line0);                             
      for Y:=2 to Height-1 do Move(ScanLineSafe[Y]^,Pointer(Line0+Y*Line1)^,BytesPerLine);
    end;
  end
  else if (PixelFormat=pf8bit) and (Target.PixelFormat=pf24bit) then
  begin
    Pix8:=@Map^[0];
    for Y:=0 to Height-1 do
    begin
      Pix24:=Target.Scanline[Y];
      for X:=0 to Width-1 do
      begin
        Pix24^:=Palette^[Pix8^];
        Inc(Pix8); Inc(Pix24);
      end;
    end;
  end
  else if (PixelFormat=pf16bit) and (Target.PixelFormat=pf8bit) then
  begin
    Pix16:=@Map^[0];
    for Y:=0 to Height-1 do
    begin
      Pix8:=Target.Scanline[Y];
      for X:=0 to Width-1 do
      begin
        Pix8^:=Hi(Pix16^);
        Inc(Pix8); Inc(Pix16);
      end;
    end;
  end
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
end;

procedure TLinearBitmap.PaintToCanvas(Canvas: TCanvas; const Dest: TRect; HalftoneStretch,DisposeAfterDraw: Boolean);
begin
  StretchDIBits(Canvas.Handle,Dest,HalftoneStretch,DisposeAfterDraw);
end;

procedure TLinearBitmap.StretchDIBits(DC: THandle; const Dest: TRect; HalftoneStretch,DisposeAfterDraw: Boolean);
var
  Bitmap : PBitmapInfo;
  BitmapInfoHeader : TBitmapInfoHeader;
  BytesPerLine4, Y, X : Integer;
  HeapMap : PByteArray;                       
  pt : TPoint;
begin
  if HalftoneStretch then
  begin
    GetBrushOrgEx(DC,pt);
    if SetStretchBltMode(DC,STRETCH_HALFTONE)=0 then
      SetStretchBltMode(DC,STRETCH_DELETESCANS);
    SetBrushOrgEx(DC,pt.x,pt.y,nil);
  end
  else SetStretchBltMode(DC,STRETCH_DELETESCANS);
  case PixelFormat of
    pf8bit  : begin
                GetMem(Bitmap,SizeOf(TBitmapInfoHeader)+ReserveForPalette);
                ZeroMem(Bitmap^.bmiHeader,SizeOf(TBitmapInfoHeader));
                with Bitmap^.bmiHeader do  // BITMAPINFOHEADER
                begin
                  biSize:=SizeOf(TBitmapInfoHeader);
                  biWidth:=Width;
                  biHeight:=-Height;
                  biPlanes:=1;
                  biBitCount:=8;
                  biClrUsed:=256;
                  biClrImportant:=256;
                end;
                for Y:=0 to 255 do with Bitmap^.bmiColors[Y] do
                begin
                  rgbRed:=Palette^[Y].R; rgbGreen:=Palette^[Y].G; rgbBlue:=Palette^[Y].B;
                  rgbReserved:=0;
                end;
                BytesPerLine4:=Ceil4(BytesPerLine);
              end;
    pf16bit : begin
                GetMem(Bitmap,SizeOf(TBitmapInfoHeader)+ReserveForPalette);
                ZeroMem(Bitmap^.bmiHeader,SizeOf(TBitmapInfoHeader));
                with Bitmap^.bmiHeader do  // BITMAPINFOHEADER
                begin
                  biSize:=SizeOf(TBitmapInfoHeader);
                  biWidth:=Width;
                  biHeight:=-Height;
                  biPlanes:=1;
                  biBitCount:=8;
                  biClrUsed:=256;
                  biClrImportant:=256;
                end;
                for Y:=0 to 255 do with Bitmap^.bmiColors[Y] do
                begin
                  rgbRed:=Y; rgbGreen:=Y; rgbBlue:=Y;
                  rgbReserved:=0;
                end;
                BytesPerLine4:=Ceil4(Width);
              end;
    pf24bit : begin
                ZeroMem(BitmapInfoHeader,SizeOf(TBitmapInfoHeader));
                with BitmapInfoHeader do  // BITMAPINFOHEADER
                begin
                  biSize:=SizeOf(TBitmapInfoHeader);
                  biWidth:=Width;
                  biHeight:=-Height;
                  biPlanes:=1;
                  biBitCount:=24;
                end;
                Bitmap:=@BitmapInfoHeader;
                BytesPerLine4:=Ceil4(BytesPerLine);
              end;
    pf32bit : begin
                ZeroMem(BitmapInfoHeader,SizeOf(TBitmapInfoHeader));
                with BitmapInfoHeader do  // BITMAPINFOHEADER
                begin                      
                  biSize:=SizeOf(TBitmapInfoHeader);
                  biWidth:=Width;
                  biHeight:=-Height;
                  biPlanes:=1;
                  biBitCount:=32;
                end;
                Bitmap:=@BitmapInfoHeader;
                BytesPerLine4:=BytesPerLine;
              end;
    pfCustom : Exit; 
    else raise Exception.Create(rsInvalidPixelFormat);
  end;
  HeapMap:=HeapAlloc(GetProcessHeap,0,Height*BytesPerLine4);
  try
    if HeapMap=nil then RaiseLastOSError;
    if PixelFormat=pf16bit then
      for Y:=0 to Height-1 do
        for X:=0 to Width-1 do HeapMap^[Y*BytesPerLine4+X]:=Map^[Y*BytesPerLine+2*X+1]
    else
      for Y:=0 to Height-1 do
        Move(ScanLineSafe[Y]^,HeapMap^[Y*BytesPerLine4],BytesPerLine);
    X:=Width;
    Y:=Height;
    if DisposeAfterDraw then Dispose;
    Y:=Windows.StretchDIBits(DC,
                             Dest.Left,Dest.Top,                        // Destination Origin
                             Dest.Right-Dest.Left,Dest.Bottom-Dest.Top, // Destination Width & Height
                             0,0,                                       // Source Origin
                             X,Y,                                       // Source Width & Height
                             HeapMap,
                             Bitmap^,
                             DIB_RGB_COLORS,
                             SRCCOPY);
    if DisposeAfterDraw and (DWord(Y)=GDI_ERROR) then RaiseLastOSError;
  finally
    HeapFree(GetProcessHeap,0,HeapMap);
    if Bitmap<>@BitmapInfoHeader then FreeMem(Bitmap);
    UnmapImage;
  end;
end;

//==================================================================================================

procedure MakeLogPalette(const Pal: TPalette; var PalEntries; ColorCount: Integer = 256);
var F : Integer;
begin
  for F:=0 to ColorCount-1 do with TPalEntries(PalEntries)[F] do
  begin
    peRed:=Pal[F].R; peGreen:=Pal[F].G; peBlue:=Pal[F].B;
    peFlags:=PC_RESERVED;
  end;
end;

function MakeHPalette(const Pal: TPalette; ColorCount: Integer = 256): HPALETTE;
var LogPal: PLogPalette; // LogPalette
begin
  GetMem(LogPal,SizeOf(TLogPalette)+SizeOf(TPaletteEntry)*255);
  try
    LogPal^.PalVersion:=$300;
    LogPal^.PalNumEntries:=ColorCount;
    MakeLogPalette(Pal,LogPal^.palPalEntry,ColorCount);
    MakeHPalette:=CreatePalette(LogPal^);
  finally
    FreeMem(LogPal);
  end;
end;

function GetFromHPalette(const HPal: HPALETTE; ColorCount: Integer): TPalette;
var
 P : Integer;
 LogPal : TPalEntries;
begin
  if HPal<>0 then
  begin
    ColorCount:=GetPaletteEntries(HPal,0,ColorCount,LogPal);
    for P:=0 to ColorCount-1 do
    begin
      Result[P].R:=LogPal[P].peRed; Result[P].G:=LogPal[P].peGreen; Result[P].B:=LogPal[P].peBlue;
    end;
    for P:=ColorCount to 255 do Result[P]:=BlackPix24;
  end
  else Result:=GrayPal;
end;

function GetFromRGBPalette(var Pal): TPalette;
var
 C : ^Byte;
 F : Byte;
begin
 C:=@Pal;
 for F:=0 to 255 do
 begin
  Result[F].R:=C^; Inc(C);
  Result[F].G:=C^; Inc(C);
  Result[F].B:=C^; Inc(C);
 end;
 Result:=Result;
end;

//==================================================================================================
// TBitmapLoaders
//==================================================================================================

function TBitmapLoaders.GetLoader(Ext: string): TBitmapLoader;
var I : Integer;
begin
  Ext:=UpperCase(Ext);
  for I:=0 to Count-1 do if Loaders^[I].CanLoad(Ext) then
  begin
    Result:=Loaders^[I];
    Exit;
  end;
  raise EUnsupportedFileFormat.Create(rsUnsupportedFileFormat+': '+Ext);
end;

function TBitmapLoaders.GetSaver(Ext: string): TBitmapLoader;
var I : Integer;
begin
  Ext:=UpperCase(Ext);
  for I:=0 to Count-1 do if Loaders^[I].CanSave(Ext) then
  begin
    Result:=Loaders^[I];
    Exit;
  end;
  raise EUnsupportedFileFormat.Create(rsUnsupportedFileFormat+': '+Ext);
end;

procedure TBitmapLoaders.AddLoader(Loader: TBitmapLoader);
const
  LoadersPerBlock = 5;
var
  NewLoaders : PBitmapLoaderList;
begin
  if Count>=MemCount then
  begin // Expand loader list
    GetMem(NewLoaders,(MemCount+LoadersPerBlock)*SizeOf(TBitmapLoader));
    if Assigned(Loaders) then
    begin
      Move(Loaders^,NewLoaders^,Count*SizeOf(TBitmapLoader)); // Include old list
      FreeMem(Loaders);
    end;
    Loaders:=NewLoaders;
    Inc(MemCount,LoadersPerBlock);
  end;
  Loaders^[Count]:=Loader; // Add loader to list
  Inc(Count);
end;

procedure SplitFilterString(const Filters: string; List: TStrings);
var
  Start : PByte;
  P1, P2 : Integer;
begin
  if Filters<>'' then
  begin
    P1:=1;
    Start:=@Filters[1]; Dec(Start);
    while P1<Length(Filters) do
    begin
      P2:=FastLocateByte(Start^,P1,Length(Filters),Word('|'));
      Assert(P2>0,'Invalid filter string');
      P2:=FastLocateByte(Start^,P2+1,Length(Filters),Word('|'));
      if P2=-1 then P2:=Length(Filters)+1;
      List.Add(Copy(Filters,P1,P2-P1));
      P1:=P2+1;
    end;
  end;
end;

function TBitmapLoaders.GetLoadFilter: string;
var
  I : Integer;
  Ext, AllFormats : string;
  List : TStringList;
begin
  List:=TStringList.Create;
  try
    for I:=0 to Count-1 do SplitFilterString(Loaders^[I].GetLoadFilter,List);
    List.Sort;
    Result:='';
    for I:=0 to List.Count-1 do
    begin
      if (Result<>'') and (Result[Length(Result)]<>'|') then Result:=Result+'|'+List[I]
      else Result:=Result+List[I];
    end;
  finally
    List.Free;
  end;
  if (Result<>'') and (Result[Length(Result)]<>'|') then Result:=Result+'|';
  AllFormats:='';
  I:=0;
  repeat
    I:=FastLocate2Bytes(Result[1],I,Length(Result)-1,Byte('*')+Word(Byte('.')) shl 8)+1;
    if I>0 then
    begin
      Inc(I,2);
      Ext:='';
      Assert(I<Length(Result));
      while Result[I] in ['a'..'z','A'..'Z','0'..'9','_'] do
      begin
        Ext:=Ext+Result[I];
        Inc(I);
      end;
      Ext:='*.'+AnsiLowerCase(Ext)+';';
      if Pos(Ext,AllFormats)=0 then AllFormats:=AllFormats+Ext;
    end;
  until (I<0) or (I>=Length(Result));
  SetLength(AllFormats,Length(AllFormats)-1);
  Result:=AllFormats+')|'+AllFormats+'|'+Result;
end;

function TBitmapLoaders.GetSaveFilter: string;
var
  I : Integer;
  List : TStringList;
begin
  List:=TStringList.Create;
  try
    for I:=0 to Count-1 do SplitFilterString(Loaders^[I].GetSaveFilter,List);
    List.Sort;
    Result:='';
    for I:=0 to List.Count-1 do
    begin
      if (Result<>'') and (Result[Length(Result)]<>'|') then Result:=Result+'|'+List[I]
      else Result:=Result+List[I];
    end;
  finally
    List.Free;
  end;
end;

destructor TBitmapLoaders.Destroy;
begin
  if Assigned(Loaders) then FreeMem(Loaders);
  inherited;
end;

//==================================================================================================
// TLinarGraphic
//==================================================================================================
function TBitmapLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=(Ext<>'') and (Pos('*.'+UpperCase(Ext),UpperCase(GetLoadFilter))>0);
end;

function TBitmapLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=(Ext<>'') and (Pos('*.'+UpperCase(Ext),UpperCase(GetSaveFilter))>0);
end;

function TBitmapLoader.GetLoadFilter: string;
begin
  Result:='';
end;

function TBitmapLoader.GetSaveFilter: string;
begin
  Result:=GetLoadFilter;
end;

procedure TBitmapLoader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinearBitmap);
var
  TempPath : array[0..MAX_PATH] of Char;
  TempFile : string;
  TmpStream : TFileStream;
  NoDataExcept : Boolean;
begin
  if GetTempPath(SizeOf(TempPath),TempPath)=0 then RaiseLastOSError;
  TempFile:=TempPath+IntToStr(GetTickCount+DWord(Random(MaxInt)))+'.'+Ext;
  NoDataExcept:=Stream.NoDataExcept;
  // Make temporary file and tell Windows to keep it in memory
  TmpStream:=TFileStream.Create(
    CreateFile(PChar(TempFile),GENERIC_WRITE,FILE_SHARE_READ,nil,
               CREATE_ALWAYS,FILE_ATTRIBUTE_TEMPORARY,0));
  try
    if NoDataExcept then Stream.NoDataExcept:=False;
    CopyStream(Stream,TmpStream,100*1024*1024); // Read no more than 100 MB
    LoadFromFile(TempFile,Ext,Bitmap);
  finally
    if NoDataExcept then Stream.NoDataExcept:=True;
    TmpStream.Free;
    DeleteFile(TempFile);
  end;
end;

procedure TBitmapLoader.SaveToStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinearBitmap);
var
  TempPath : array[0..MAX_PATH] of Char;
  TempFile : string;
  Handle : THandle;
  TmpStream : TFileStream;
begin
  if GetTempPath(SizeOf(TempPath),TempPath)=0 then RaiseLastOSError;
  TempFile:=TempPath+IntToStr(GetTickCount+DWord(Random(MaxInt)))+'.'+Ext;
  // Make temporary file and tell Windows to keep it in memory
  Handle:=CreateFile(PChar(TempFile),0,0,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_TEMPORARY,0);
  if Handle=INVALID_HANDLE_VALUE then RaiseLastOSError;
  CloseHandle(Handle);
  try
    SaveToFile(TempFile,Ext,Bitmap);
    TmpStream:=TFileStream.Create(TempFile,[fsRead]);
    try
      Stream.CopyFrom(TmpStream);
    finally
      TmpStream.Free;
    end;
  finally
    DeleteFile(TempFile);
  end;
end;

procedure TBitmapLoader.LoadFromFile(const FileName,FileType: string; Bitmap: TLinearBitmap);
var
  Stream : TFileStream;
begin
  Stream:=TFileStream.Create(FileName,[fsRead,fsShareRead]);
  try
    LoadFromStream(Stream,FileType,Bitmap);
  finally
    Stream.Free;
  end;
end;

procedure TBitmapLoader.SaveToFile(const FileName,FileType: string; Bitmap: TLinearBitmap);
var
  Stream : TFileStream;
begin
  Stream:=TFileStream.Create(FileName,fsRewrite);
  try
    SaveToStream(Stream,FileType,Bitmap);
  finally
    Stream.Free;
  end;
end;

//==================================================================================================
// TLinearGraphic
//==================================================================================================

constructor TLinearGraphic.Create;
begin
  inherited Create;
  FImage:=TLinearBitmap.Create;
end;

destructor TLinearGraphic.Destroy;
begin
  FImage.Free;
end;

procedure TLinearGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  SetStretchBltMode(ACanvas.Handle,COLORONCOLOR);
  FImage.PaintToCanvas(ACanvas,Rect);{}
end;

function TLinearGraphic.GetEmpty: Boolean;
begin
  Result:=not FImage.Present;
end;

function TLinearGraphic.GetHeight: Integer;
begin
  Result:=FImage.Height;
end;

function TLinearGraphic.GetWidth: Integer;
begin
  Result:=FImage.Width;
end;

procedure TLinearGraphic.SetHeight(Value: Integer);
begin
  FImage.ResizeCanvas(FImage.Width,Value,0,0,0);
end;

procedure TLinearGraphic.SetWidth(Value: Integer);
begin
  FImage.ResizeCanvas(Value,FImage.Height,0,0,0);
end;

procedure TLinearGraphic.AssignTo(Dest: TPersistent);
begin
  if (Dest is TLinearGraphic) then FImage.AssignTo(TLinearGraphic(Dest).Bitmap)
  else FImage.AssignTo(TBitmap(Dest));
end;

procedure TLinearGraphic.LoadFromStream(Stream: TStream);
var
  Bitmap : TBitmap;
begin
  Bitmap:=TBitmap.Create;
  try
    Bitmap.LoadFromStream(Stream);
    FImage.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TLinearGraphic.SaveToStream(Stream: TStream);
var
  Bitmap : TBitmap;
begin
  Bitmap:=TBitmap.Create;
  try
    FImage.AssignTo(Bitmap);
    Bitmap.SaveToStream(Stream);
  finally
    Bitmap.Free;
  end;
end;

procedure TLinearGraphic.LoadFromFile(const FileName: string);
begin
  FImage.LoadFromFile(FileName);
end;

procedure TLinearGraphic.SaveToFile(const FileName: string);
begin
  FImage.SaveToFile(FileName);
end;

procedure TLinearGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
var
  Bitmap : TBitmap;
begin
  Bitmap:=TBitmap.Create;
  try
    Bitmap.LoadFromClipboardFormat(AFormat,AData,APalette);
    FImage.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TLinearGraphic.SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE);
var
  Bitmap : TBitmap;
begin
  Bitmap:=TBitmap.Create;
  try
    FImage.AssignTo(Bitmap);
    Bitmap.SaveToClipboardFormat(AFormat,AData,APalette);
  finally
    Bitmap.Free;
  end;
end;

//==================================================================================================

procedure FreeLinearBitmapArray(var Images: TLinearBitmapArray);
var
  I : Integer;
begin
  for I:=0 to High(Images) do
    Images[I].Free;
  SetLength(Images,0);
end;

procedure AddLoader(Loader: TBitmapLoader);
begin
  BitmapLoaders.AddLoader(Loader);
end;

procedure DeInterleave(Image: TLinearBitmap; Source: PByteArray; Planes: Integer);
var
  C, P : Integer;
  SrcPix, DestPix : PByte;
  UseTemp : Boolean;
begin
  UseTemp:=(Source=nil) or (Source=Image.Map);
  if UseTemp then
  begin
    GetMem(Source,Image.Size);
    Move(Image.Map^,Source^,Image.Size);
  end;
  SrcPix:=Pointer(Source);
  if Planes>3 then Planes:=3;
  for C:=0 to Planes-1 do
  begin
    DestPix:=@Image.Map^[C];
    for P:=1 to Image.Width*Image.Height do
    begin
      DestPix^:=SrcPix^;
      Inc(SrcPix);
      Inc(DestPix,3);
    end;
  end;
  if UseTemp then FreeMem(Source);
end;

procedure TLinearBitmap.UnmapImage;
begin
end;

var C : Integer;

initialization
  for C:=0 to 255 do begin GrayPal[C].R:=C; GrayPal[C].B:=C; GrayPal[C].G:=C; end;
  BitmapLoaders:=TBitmapLoaders.Create;
finalization
  BitmapLoaders.Free;
end.

