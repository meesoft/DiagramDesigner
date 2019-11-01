///////////////////////////////////////////////////////////////////////////////////////////////
//
// AnalyzerPlugins.pas - Base unit for creating image processing plugins for Image Analyzer
// ----------------------------------------------------------------------------------------
// Version:   2005-06-21
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//
unit AnalyzerPlugins;

interface

uses Windows, SysUtils, Graphics, Classes;

type
  TBGRPalette = packed array[0..255] of packed record
                  B, G, R : Byte;
                end;

  PBGRPalette = ^TBGRPalette;
  TImageContainer = packed record
                      Width, Height : Integer;
                      BytesPerLine : Integer;
                      PixelFormat : Integer; // $00 // Map is a pointer to a zero-terminated string
                                             // $01 // 8 bit. If Palette is NULL when using this format, grayscale is assumed
                                             // $21 // 16 bit grayscale
                                             // $03 // 24 bit color BGR. Image Analyzer will always give 24 bit images in this format
                                             // $04 // 32 bit color BGRA.
                                             // $13 // 24 bit, non-interleaved
                                             // $43 // 24 bit Color RGB
                                             // $53 // 48 bit Color RGB
                                             // $54 // 64 bit Color RGBA
                                             // $64 // 32 bit CMYK
                                             // $14 // Float matrix, each pixel is a single
                                             // $18 // Float matrix, each pixel is a double
                                             // $28 // Complex matrix, two doubles per pixel: Re,Im
                                             // $34 // RGB float, non-interleaved
                      Map : PByteArray;
                      Palette : PBGRPalette;
                      Options : PChar;
                    end;
  PImageContainer = ^TImageContainer;


// Add item to program main menu
const cmdMakeMenuItem = 1;
type TMakeMenuItem = packed record // This command can only be issued from RegisterPlugin
  Menu    : PChar;
  Caption : PChar;
  Hint    : PChar;
  Tag     : Integer; // For identification in ProcessImage
end;
PMakeMenuItem = ^TMakeMenuItem;

// Create new image window
const cmdCreateImageWindow = 2;
type TCreateImageWindow = packed record
  Name  : PChar;
  Image : TImageContainer;
end;
PCreateImageWindow = ^TCreateImageWindow;

// Get image from image window
const cmdGetImageWindow = 3;
type TGetImageWindow = packed record
  WindowNumber : Integer;

  // Set by Analyzer:
  TotalWindowCount : Integer;
  Name             : PChar;
  Image            : TImageContainer;
  Selection        : TRect;
end;
PGetImageWindow = ^TGetImageWindow;

// Get handle of main window (this may change when the program is running)
const cmdGetMainWindowHandle = 4;
type TGetMainWindowHandle = packed record
  Handle : THandle;
end;
PGetMainWindowHandle = ^TGetMainWindowHandle;

// Update progress and refresh screen
const cmdUpdateProgress = 5;
type TUpdateProgress = packed record
  Progress : Integer;
end;
PUpdateProgress = ^TUpdateProgress;

// Get program version
const cmdGetProgramVersion = 6;
type TGetProgramVersion = packed record
  Version : Integer;
end;

// Set file open capability
const cmdSetFileOpenCapability = 7;
type TSetFileOpenCapability = packed record // This command can only be issued from RegisterPlugin
  Filter  : PChar;   // File format filter, e.g. '3D models (*.ply;*.3ds)|*.ply;*.3ds'
  Tag     : Integer; // For identification in ProcessImage. Image.Map will point to file name
end;
PSetFileOpenCapability = ^TSetFileOpenCapability;

// Open file
const cmdOpenFile = 8;
type TOpenFile = packed record
  FileName : PChar;
end;

// Refresh children
const cmdRefreshChildren = 9;

// Get image settings
const cmdGetImageSettings = 10;
type TGetImageSettings = packed record
  Size : Integer;
  ImageGamma : Single;
end;
PGetImageSettings = ^TGetImageSettings;

// Create new image window
const cmdReplaceActiveImage = 11;
type TReplaceActiveImage = packed record
  Image : TImageContainer;
end;
PReplaceActiveImage = ^TReplaceActiveImage;


type
// Function for sending cmdXXX commands to Image Analyzer
// TAnalyzerCallback function should return 0 on failure
TAnalyzerCallback = function(Command: Integer; Data: Pointer): LongBool; stdcall;

// RegisterPlugin function should return 0 on failure
TRegisterPlugin = function(CallBack: TAnalyzerCallback): LongBool; stdcall;

// Return codes:
//   0 : Ok, image updated
//   1 : Image unassigned/not changed
//   2 : Unable to open file
//   3 : Encoding/decoding error
//   4 : Unsupported pixel format
//   5 : Unable to close file (?)
//   6 : Operation not supported
TProcessImage = function(Tag: Integer; Image: PImageContainer): Integer; stdcall;

var
  AnalyzerCallback : TAnalyzerCallback = nil;

procedure RaisePluginError(Result: Integer; Default: string='');

procedure BitmapFromImageContainer(Bitmap: TBitmap; const Image: TImageContainer);
procedure BitmapToImageContainer(Bitmap: TBitmap; out Image: TImageContainer);
procedure FreeImageContainer(var Image: TImageContainer);

function FindImageWindowName(const Image: TImageContainer): string;
function FindImageWindowSelection(const Image: TImageContainer): TRect;
function FindImageWindowMaskSelection(const Image: TImageContainer): Pointer;
procedure RefreshChildren;
function GetImageAnalyzerGamma: Single;

implementation

resourcestring
  rsUnableToOpenFile = 'Unable to open file';
  rsErrorInBitmapData = 'Error in bitmap data';
  rsUnsupportedPixelFormat = 'Unsupported pixel format';
  rsUnableToCloseFile = 'Unable to close file';
  rsProcessFailedD = 'Process failed: %d';
  rsOperationNotSupported = 'Operation not supported';

procedure RaisePluginError(Result: Integer; Default: string);
begin
  if Result<>0 then
  begin
    case Result of
      2 : raise Exception.Create(rsUnableToOpenFile);
      3 : raise Exception.Create(rsErrorInBitmapData);
      4 : raise Exception.Create(rsUnsupportedPixelFormat);
      5 : raise Exception.Create(rsUnableToCloseFile);
      6 : raise Exception.Create(rsOperationNotSupported);
    else
      begin
        if Default='' then Default:=rsProcessFailedD;
        raise Exception.CreateFmt(Default,[Result]);
      end;
    end;
  end;
end;

procedure BitmapFromImageContainer(Bitmap: TBitmap; const Image: TImageContainer);

  type TPalEntries = array[0..255] of TPaletteEntry;

  procedure MakeLogPalette(const Pal: TBGRPalette; var PalEntries; ColorCount: Integer = 256);
  var F : Integer;
  begin
    for F:=0 to ColorCount-1 do with TPalEntries(PalEntries)[F] do
    begin
      peRed:=Pal[F].R; peGreen:=Pal[F].G; peBlue:=Pal[F].B;
      peFlags:=PC_RESERVED;
    end;
  end;

  function MakeHPalette(const Pal: TBGRPalette; ColorCount: Integer = 256): HPALETTE;
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

var
  Y : Integer;
  GrayPal : TBGRPalette;
begin
  Bitmap.Width:=0;
  case Image.PixelFormat of
    $01 : Bitmap.PixelFormat:=pf8bit;
    $03 : Bitmap.PixelFormat:=pf24bit;
  else raise Exception.Create(rsUnsupportedPixelFormat);
  end;
  Bitmap.Width:=Image.Width;
  Bitmap.Height:=Image.Height;
  if Image.PixelFormat=$01 then
  begin
    if Assigned(Image.Palette) then Bitmap.Palette:=MakeHPalette(Image.Palette^)
    else
    begin
      for Y:=0 to 255 do with GrayPal[Y] do
      begin
        R:=Y;
        G:=Y;
        B:=Y;
      end;
      Bitmap.Palette:=MakeHPalette(GrayPal);
    end;
  end;
  for Y:=0 to Image.Height-1 do Move(Image.Map^[Y*Image.BytesPerLine],Bitmap.ScanLine[Y]^,Image.Width*Image.PixelFormat);
end;

procedure BitmapToImageContainer(Bitmap: TBitmap; out Image: TImageContainer);
var
  Y : Integer;
begin
  FillChar(Image,SizeOf(Image),0);
  case Bitmap.PixelFormat of
    pf8bit  : Image.PixelFormat:=$01;
    pf24bit : Image.PixelFormat:=$03;
  else raise Exception.Create(rsUnsupportedPixelFormat);
  end;
  Image.Width:=Bitmap.Width;
  Image.Height:=Bitmap.Height;
  Image.BytesPerLine:=Image.Width*Image.PixelFormat;
  GetMem(Image.Map,Image.Height*Image.BytesPerLine);
  for Y:=0 to Image.Height-1 do Move(Bitmap.ScanLine[Y]^,Image.Map^[Y*Image.BytesPerLine],Image.BytesPerLine);
end;

procedure FreeImageContainer(var Image: TImageContainer);
begin
  if Assigned(Image.Map) then FreeMem(Image.Map); Image.Map:=nil;
  if Assigned(Image.Palette) then FreeMem(Image.Palette); Image.Palette:=nil;
end;

function FindImageWindowName(const Image: TImageContainer): string;
var
  GetImageWindow : TGetImageWindow;
begin
  GetImageWindow.WindowNumber:=0;
  while AnalyzerCallback(cmdGetImageWindow,@GetImageWindow) do
  begin
    if GetImageWindow.Image.Map=Image.Map then
    begin
      Result:=GetImageWindow.Name;
      Exit;
    end;
    Inc(GetImageWindow.WindowNumber);
  end;
end;

function FindImageWindowSelection(const Image: TImageContainer): TRect;
var
  GetImageWindow : TGetImageWindow;
begin
  GetImageWindow.WindowNumber:=0;
  while AnalyzerCallback(cmdGetImageWindow,@GetImageWindow) do
  begin
    if GetImageWindow.Image.Map=Image.Map then
    begin
      Result:=GetImageWindow.Selection;
      Exit;
    end;
    Inc(GetImageWindow.WindowNumber);
  end;
  Result:=Rect(-1,-1,-1,-1);
end;

function FindImageWindowMaskSelection(const Image: TImageContainer): Pointer;
var
  GetImageWindow : TGetImageWindow;
begin
  GetImageWindow.WindowNumber:=0;
  while AnalyzerCallback(cmdGetImageWindow,@GetImageWindow) do
  begin
    if GetImageWindow.Image.Map=Image.Map then
    begin
      Result:=GetImageWindow.Image.Options;
      Exit;
    end;
    Inc(GetImageWindow.WindowNumber);
  end;
  Result:=nil;
end;

function GetImageAnalyzerGamma: Single;
var
  Settings : TGetImageSettings;
begin
  Settings.Size:=SizeOf(Settings);
  if AnalyzerCallback(cmdGetImageSettings,@Settings) and (Settings.Size>=8) then Result:=Settings.ImageGamma
  else Result:=2.2;
end;

procedure RefreshChildren;
begin
  AnalyzerCallback(cmdRefreshChildren,nil);
end;

end.

