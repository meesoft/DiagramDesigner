////////////////////////////////////////////////////////////////////////////////
//
// BMPLoader.pas - BMP wrapper
// ---------------------------
// Version:   2007-01-07
// Maintain:  Michael Vinther   |    mv@logicnet·dk
//
// Last changes:
//   Fixed bug in Delphi's BMP implementation
//
unit BMPLoader;

interface

uses Windows, Classes, Graphics, FileMappedBitmap, LinarBitmap, Streams;

resourcestring
  rsWindowsBitmap = 'Windows bitmap';

type
  TBMPLoader = class(TFileMappedBitmapLoader)
    protected
      procedure SaveToStream8Internal(Stream: TBaseStream; Bitmap: TLinarBitmap);
      procedure SaveToStream24_32Internal(Stream: TBaseStream; Bitmap: TLinarBitmap);
    public
      function CanLoad(const Ext: string): Boolean; override;
      function CanSave(const Ext: string): Boolean; override;

      function GetLoadFilter: string; override;

      procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
      procedure SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap); override;
    end;

  TVKBitmap =class(TBitmap)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

var
  Default : TBMPLoader;

implementation

uses SysUtils, DelphiStream, UniDIB, DIBTools, MathUtils;

//==============================================================================================================================
// TVKBitmap
//==============================================================================================================================
procedure TVKBitmap.LoadFromStream(Stream: TStream);
var
  StartPos : Int64;
  DIB : TUniDIB;
begin
  Height:=0;
  StartPos:=Stream.Position;
  try
    DIB:=nil;
    try
      if (UDIBLoadBMP(Stream,DIB)<>UDIBNoError) then
        raise ELinearBitmap.Create(rsUnsupportedFileFormat);
      if DIB.IndexColor then
      begin
        PixelFormat:=pf8bit;
        SetPalette(DIB.PaletteHandle);
      end;
      Width:=Abs(DIB.Width);
      Height:=Abs(DIB.Height);
      DIB.DIBToScreen(Canvas.Handle);
    finally
      DIB.Free;
    end;
  except
    Stream.Position:=StartPos;
    inherited LoadFromStream(Stream);
  end;
end;

//==============================================================================================================================
// TBMPLoader
//==============================================================================================================================

function TBMPLoader.GetLoadFilter: string;
begin
  Result:=rsWindowsBitmap+' (*.bmp)|*.bmp';
end;

function TBMPLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=Ext='BMP';
end;

function TBMPLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=Ext='BMP';
end;

procedure TBMPLoader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
  BMP : TVKBitmap;
  DelphiStream : TSeekableDelphiStream;
begin
  BMP:=TVKBitmap.Create;
  try
    DelphiStream:=TSeekableDelphiStream.Create(Stream);
    try
      BMP.LoadFromStream(DelphiStream);
    finally
      DelphiStream.Free;
    end;
    if BMP.Pixelformat in [pf1bit,pf4bit] then BMP.Pixelformat:=pf8bit
    else if BMP.Pixelformat in [pfDevice, pf15bit, pf16bit, pf32bit, pfCustom] then BMP.Pixelformat:=pf24bit;
    Bitmap.Assign(BMP);
  finally
    BMP.Free;
  end;
end;

procedure TBMPLoader.SaveToStream8Internal(Stream: TBaseStream; Bitmap: TLinarBitmap);
var
  BMF : TBitmapFileHeader;
  BIH : TBitmapInfoHeader;
  PAL : array[0..255] of TRGBQuad;
  Y, BytesPerLine, DataPerLine, Padding : Cardinal;
  Zeros : Integer;
begin
  FillChar(BIH, sizeof(BIH), 0);
  BIH.biSize:=SizeOf(BIH);
  BIH.biWidth:=Bitmap.Width;
  BIH.biHeight:=Bitmap.Height;
  BIH.biPlanes:=1;
  BIH.biBitCount:=8;
  BIH.biClrUsed:=256;
  BIH.biClrImportant:=256;
  DataPerLine:=Bitmap.Width;
  BytesPerLine:=Ceil4(DataPerLine);
  if Int64(Bitmap.Height)*BytesPerLine>MaxDWord then raise Exception.Create('Image is too big to be stored as BMP');
  BIH.biSizeImage:=BytesPerLine*Cardinal(Bitmap.Height);
  Padding:=BytesPerLine-DataPerLine;
  Zeros:=0;
  FillChar(BMF, sizeof(BMF), 0);
  BMF.bfType:=$4D42;
  BMF.bfSize:=sizeof(BMF) + BIH.biSize + Sizeof(PAL) + BIH.biSizeImage;
  BMF.bfOffBits:=sizeof(BMF) + BIH.biSize  + Sizeof(PAL);
  for Y:=0 to 255 do with PAL[Y] do
  begin
    rgbRed:=Bitmap.Palette^[Y].R; rgbGreen:=Bitmap.Palette^[Y].G; rgbBlue:=Bitmap.Palette^[Y].B;
    rgbReserved:=0;
  end;
  Stream.Write(BMF, Sizeof(BMF));
  Stream.Write(BIH, Sizeof(BIH));
  Stream.Write(PAL, Sizeof(PAL));
  for Y:=Bitmap.Height-1 downto 0 do
  begin
    Stream.Write(Bitmap.ScanLineSafe[Y]^, DataPerLine);
    if Padding>0 then Stream.Write(Zeros, Padding);
  end;
end;

procedure TBMPLoader.SaveToStream24_32Internal(Stream: TBaseStream; Bitmap: TLinarBitmap);
var
  BMF : TBitmapFileHeader;
  BIH : TBitmapInfoHeader;
  Y, BytesPerLine, DataPerLine, Padding : Cardinal;
  Zeros : Integer;
begin
  FillChar(BIH, sizeof(BIH), 0);
  BIH.biSize:=SizeOf(BIH);
  BIH.biWidth:=Bitmap.Width;
  BIH.biHeight:=Bitmap.Height;
  BIH.biPlanes:=1;
  BIH.biBitCount:=Bitmap.PixelSize*8;
  DataPerLine:=Bitmap.Width*Bitmap.PixelSize;
  BytesPerLine:=Ceil4(DataPerLine);
  if Int64(Bitmap.Height)*BytesPerLine>MaxDWord then raise Exception.Create('Image is too big to be stored as BMP');
  BIH.biSizeImage:=BytesPerLine*Cardinal(Bitmap.Height);
  Padding:=BytesPerLine-DataPerLine;
  Zeros:=0;
  FillChar(BMF, sizeof(BMF), 0);
  BMF.bfType:=$4D42;
  BMF.bfSize:=sizeof(BMF) + BIH.biSize + BIH.biSizeImage;
  BMF.bfOffBits:=sizeof(BMF) + BIH.biSize;

  Stream.Write(BMF, Sizeof(BMF));
  Stream.Write(BIH, Sizeof(BIH));
  for Y:=Bitmap.Height-1 downto 0 do
  begin
    Stream.Write(Bitmap.ScanLineSafe[Y]^, DataPerLine);
    if Padding>0 then Stream.Write(Zeros, Padding);
  end;
end;

procedure TBMPLoader.SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap);
var
  Bitmap : TBitmap;
  DelphiStream : TSeekableDelphiStream;
begin
  if LBitmap.PixelFormat in [pf8bit] then
    SaveToStream8Internal(Stream,LBitmap)
  else if LBitmap.PixelFormat in [pf24bit,pf32bit] then
    SaveToStream24_32Internal(Stream,LBitmap)
  else
  begin
    Bitmap:=TBitmap.Create;
    try
      LBitmap.AssignTo(Bitmap);
      DelphiStream:=TSeekableDelphiStream.Create(Stream);
      try
        Bitmap.SaveToStream(DelphiStream);
      finally
        DelphiStream.Free;
      end;
    finally
      Bitmap.Free;
    end;
  end;
end;

initialization
  Default:=TBMPLoader.Create;
  LinarBitmap.AddLoader(Default);
  TPicture.RegisterFileFormat('bmp',rsWindowsBitmap,TVKBitmap);
finalization
  Default.Free;
end.

