///////////////////////////////////////////////////////////////////////////////////////////////
//
// WMFLoader.pas - Load/save Windows WMF and EMF as TLinearBitmap
// --------------------------------------------------------------
// Version:   2004-01-04
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
// Contains:
//   (TBitmapLoader)
//     TWMFLoader
//
unit WMFLoader;

interface

uses SysUtils, Windows, Classes, Streams, Graphics, DelphiStream, ColorMapper,
  LinarBitmap, BitmapConversion;

type
  TWMFLoader = class(TBitmapLoader)
                 BackgroundColor : TColor; // Used when reading

                 constructor Create;

                 function CanLoad(const Ext: string): Boolean; override;
                 function CanSave(const Ext: string): Boolean; override;

                 function GetLoadFilter: string; override;
                 function GetSaveFilter: string; override;

                 procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
                 procedure SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap); override;
               end;

var
  Default : TWMFLoader;

resourcestring
  rsWindowsMetafile = 'Windows metafile';
  rsWindowsEnhancedMetafile = 'Windows Enhanced metafile';

implementation

constructor TWMFLoader.Create;
begin
  inherited;
  BackgroundColor:=clWhite;
end;

function TWMFLoader.GetLoadFilter: string;
begin
  Result:=rsWindowsMetafile+' (*.wmf,*.emf)|*.wmf;*.emf';
end;

function TWMFLoader.GetSaveFilter: string;
begin
  Result:=rsWindowsEnhancedMetafile+' (*.emf)|*.emf|'+
          rsWindowsMetafile+' (*.wmf)|*.wmf';
end;

function TWMFLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=(Ext='WMF') or (Ext='EMF');
end;

function TWMFLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=(Ext='WMF') or (Ext='EMF');
end;

procedure TWMFLoader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
  Metafile : TMetafile;
  DelphiStream : TSeekableDelphiStream;
  DelphiBitmap : TBitmap;
begin
  Metafile:=TMetafile.Create;
  try
    DelphiStream:=TSeekableDelphiStream.Create(Stream);
    try
      Metafile.LoadFromStream(DelphiStream);
    finally
      DelphiStream.Free;
    end;
    DelphiBitmap:=TBitmap.Create;
    try
      DelphiBitmap.PixelFormat:=pf24bit;
      DelphiBitmap.Canvas.Brush.Color:=BackgroundColor;
      DelphiBitmap.Width:=Metafile.Width;
      DelphiBitmap.Height:=Metafile.Height;
      DelphiBitmap.Canvas.Draw(0,0,Metafile);
      FreeAndNil(Metafile);
      Bitmap.Assign(DelphiBitmap);
    finally
      DelphiBitmap.Free;
    end;
  finally
    Metafile.Free;
  end;
  if CountColorsUsed(Bitmap)<=256 then
  begin
    if Bitmap.IsGrayScale then ConvertToGrayscale(Bitmap)
    else MakePaletteImage(Bitmap);
  end;
end;

procedure TWMFLoader.SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap);
var
  Metafile : TMetafile;
  Canvas : TMetafileCanvas;
  DelphiStream : TSeekableDelphiStream;
begin
  Metafile:=TMetafile.Create;
  Metafile.Width:=LBitmap.Width;
  Metafile.Height:=LBitmap.Height;
  if LBitmap.PixelFormat=pf8bit then Metafile.Palette:=MakeHPalette(LBitmap.Palette^);
  Canvas:=TMetafileCanvas.Create(Metafile,0);
  try
    LBitmap.PaintToCanvas(Canvas,Rect(0,0,LBitmap.Width,LBitmap.Height),True);
  finally
    Canvas.Free;
  end;
  Metafile.Enhanced:=UpCase(Ext[1])='E';
  DelphiStream:=TSeekableDelphiStream.Create(Stream);
  try
    Metafile.SaveToStream(DelphiStream);
  finally
    DelphiStream.Free;
    Metafile.Free;
  end;
end;

initialization
  Default:=TWMFLoader.Create;
  LinarBitmap.AddLoader(Default);
finalization
  Default.Free;
end.

