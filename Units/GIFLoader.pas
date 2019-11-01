/////////////////////////////////////////////////////////////////////////////////////////////////////
//
// GIFLoader.pas - TBitmapLoader wrapper for TGIFImage by Anders Melander
// ----------------------------------------------------------------------
// Version:   2003-01-30
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//   TransparentColor for loading images
//
unit GIFLoader;

interface

uses Windows, SysUtils, Classes, LinarBitmap, Streams, Graphics, DelphiStream, GIFImage;

resourcestring
  rsGIFImageFile = 'Graphics Interchange Format';

type
  TGIFLoader = class(TBitmapLoader)
    private
    public
      TransparentColor : TColor; // Only for reading, -1 if not transparent

      constructor Create;
      function CanLoad(const Ext: string): Boolean; override;
      function CanSave(const Ext: string): Boolean; override;
      function GetLoadFilter: string; override;
      procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
      procedure SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap); override;

      procedure SaveAnimation(Frames: array of TLinearBitmap; const FileName: string; FrameRate: Integer; Loops: Word=65535; ColorKeyIndex: Integer=-1); 
    end;

var
  Default : TGIFLoader;

implementation

constructor TGIFLoader.Create;
begin
  inherited Create;
  TransparentColor:=-1;
end;

function TGIFLoader.GetLoadFilter: string;
begin
  Result:=rsGIFImageFile+' (*.gif)|*.gif';
end;

function TGIFLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=Ext='GIF';
end;

function TGIFLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=Ext='GIF';
end;

type
  TGIFSync = class
  public
    GIF : TGifImage;
    Bitmap: TLinarBitmap;
    procedure Assign;
  end;

procedure TGIFSync.Assign;
begin
  Bitmap.Assign(GIF)
end;

procedure TGIFLoader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
  GIF : TGifImage;
  GIFSync : TGIFSync;
  DelphiStream : TSeekableDelphiStream;
begin
  GIF:=TGIFImage.Create;
  try
    DelphiStream:=TSeekableDelphiStream.Create(Stream);
    try
      GIF.LoadFromStream(DelphiStream);
    finally
      DelphiStream.Free;
    end;
    
    if GIF.IsTransparent then TransparentColor:=GIF.Images[0].Bitmap.TransparentColor
    else TransparentColor:=-1;

    if GetCurrentThreadId=MainThreadID then Bitmap.Assign(GIF)
    else
    begin
      try
        Bitmap.Assign(GIF);
      except
        // Don't really know why this is necessary, but assign sometimes fails in other threads. Not really sure if this works!
        GIFSync:=TGIFSync.Create;
        try
          GIFSync.GIF:=GIF;
          GIFSync.Bitmap:=Bitmap;
          TThread.Synchronize(nil,GIFSync.Assign);
        finally
          GIFSync.Free;
        end;
      end;
    end;
  finally
    GIF.Free;
  end;
end;

procedure ToBitmap(LBitmap: TLinarBitmap; var Bitmap: TBitmap);
var
  TempBitmap : TBitmap;
  C, ColorsUsed : Integer;
begin
  Bitmap.PixelFormat:=LBitmap.PixelFormat;
  if LBitmap.PixelFormat=pf24bit then
  begin
    GIFImageDefaultColorReductionBits:=8;
    GIFImageDefaultColorReduction:=rmQuantize;
    GIFImageDefaultDitherMode:=dmFloydSteinberg;
  end;
  LBitmap.AssignTo(Bitmap);

  if LBitmap.PixelFormat=pf8bit then
  begin
    ColorsUsed:=2;
    for C:=255 downto 1 do if (LBitmap.Palette^[C].R<>0) or
                              (LBitmap.Palette^[C].G<>0) or
                              (LBitmap.Palette^[C].B<>0) then
    begin
      ColorsUsed:=C+1;
      Break;
    end;
    if ColorsUsed<=16 then
    begin
      TempBitmap:=TBitmap.Create;
      TempBitmap.Width:=Bitmap.Width;
      TempBitmap.Height:=Bitmap.Height;
      if ColorsUsed<=2 then
      begin
        TempBitmap.PixelFormat:=pf1bit;
        TempBitmap.Palette:=MakeHPalette(LBitmap.Palette^,2)
      end
      else if ColorsUsed<=16 then
      begin
        TempBitmap.PixelFormat:=pf4bit;
        TempBitmap.Palette:=MakeHPalette(LBitmap.Palette^,16)
      end;
      TempBitmap.Canvas.Draw(0,0,Bitmap);
      Bitmap.Free;
      Bitmap:=TempBitmap;
    end;
  end;
end;

procedure TGIFLoader.SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap);
var
  Bitmap : TBitmap;
  GIF : TGifImage;
  DelphiStream : TSeekableDelphiStream;
begin
  Bitmap:=TBitmap.Create;
  try
    ToBitmap(LBitmap,Bitmap);

    GIF:=TGIFImage.Create;
    try
      GIF.Assign(Bitmap);
      Bitmap.Free; Bitmap:=nil;
      DelphiStream:=TSeekableDelphiStream.Create(Stream);
      try
        GIF.SaveToStream(DelphiStream);
      finally
        DelphiStream.Free;
      end;
    finally
      GIF.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TGIFLoader.SaveAnimation(Frames: array of TLinearBitmap; const FileName: string; FrameRate: Integer; Loops: Word; ColorKeyIndex: Integer);
var
  GIF : TGifImage;
  Bitmap : TBitmap;
  I : Integer;
  Ext : TGIFItem;
begin
  Bitmap:=TBitmap.Create;
  try
    GIF:=TGIFImage.Create;
    try
      for I:=0 to High(Frames) do
      begin
        ToBitmap(Frames[I],Bitmap);
        GIF.Add(Bitmap);
        if (FrameRate>0) or (ColorKeyIndex>=0) then
        begin
          Ext:=TGIFGraphicControlExtension.Create(GIF.Images[I]);
          if FrameRate>0 then
            TGIFGraphicControlExtension(Ext).Delay:=100 div FrameRate;
          if ColorKeyIndex>=0 then
          begin
            TGIFGraphicControlExtension(Ext).Transparent:=True;
            TGIFGraphicControlExtension(Ext).TransparentColorIndex:=ColorKeyIndex;
          end;
          GIF.Images[I].Extensions.Add(Ext);
        end;
        if Loops>0 then
        begin
          Ext:=TGIFAppExtNSLoop.Create(GIF.Images[I]);
          TGIFAppExtNSLoop(Ext).Loops:=Loops;
          GIF.Images[I].Extensions.Add(Ext);
        end;
      end;
      FreeAndNil(Bitmap);
      GIF.SaveToFile(FileName);
    finally
      GIF.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

initialization
  Default:=TGIFLoader.Create;
  LinarBitmap.AddLoader(Default);
  GIFImageDefaultDitherMode:=dmBurkes; // Set default dithering for 24 -> 8 bit conversion
  GIFImageDefaultColorReduction:=rmQuantize;
finalization
  Default.Free;
end.

