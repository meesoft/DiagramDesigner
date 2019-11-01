////////////////////////////////////////////////////////////////////////////////
//
// JPEGLoader.pas - JPEG wrapper
// -----------------------------
// Version:   2005-10-02
// Maintain:  Michael Vinther    |    mv@logicnet·dk
//
// Last changes:
//
unit JPEGLoader;

interface

uses SysUtils, LinarBitmap, Streams, Windows, Graphics, DelphiStream, JPEG,
  {$IFDEF EXIF}
  dEXIF,
  {$ENDIF}
  Classes, BitmapConversion;

resourcestring
  rsJPEGImageFile = 'Joint Photographic Experts Group';

const
  DefaultSize : TPoint = (X:MaxInt;Y:MaxInt);

type
  TJPEGLoader = class(TBitmapLoader)
    public
      // Parameters for load
      DesiredSize   : TPoint;   // LoadFromStream can save time by decoding an image of only this size
      TrueSize      : TPoint;   // Actual size of last image loaded
      // Parameters for save
      Quality       : Integer;  // Image quality, 1=worst, 100=best
      FileSizeLimit : Integer;  // If FileSizeLimit>0, SaveToStream will try to limit the file size
                                // to FileSizeLimit bytes.
      {$IFDEF EXIF}
      EXIF          : TImgData; // If EXIF is assigned it is used when saving
      {$ENDIF}

      constructor Create;

      function CanLoad(const Ext: string): Boolean; override;
      function CanSave(const Ext: string): Boolean; override;

      function GetLoadFilter: string; override;
      function GetSaveFilter: string; override;

      procedure LoadFromFile(const FileName,FileType: string; Bitmap: TLinearBitmap); override;
      procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
      procedure SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap); override;
    end;

var
  Default : TJPEGLoader;

implementation

uses
  GdiPlus, GDIPlusUtils;

{$IFOPT D+}
procedure Test;
begin
  TJPEGImage.Create.Transform(jt_FLIP_H); // Test that we have the correct JPEG DCU
end;
{$ENDIF}

constructor TJPEGLoader.Create;
begin
  inherited;
  DesiredSize:=DefaultSize;
end;

function TJPEGLoader.GetLoadFilter: string;
begin
  Result:=rsJPEGImageFile+' (*.jpg,*.jpeg,*.jpe,*.wdp)|*.jpg;*.jpeg;*.jpe';
end;

function TJPEGLoader.GetSaveFilter: string;
begin
  Result:=rsJPEGImageFile+' (*.jpg)|*.jpg';
end;

function TJPEGLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=(Ext='JPG') or (Ext='JPEG') or (Ext='JPE');
end;

function TJPEGLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=(Ext='JPG') or (Ext='JPEG');
end;

procedure TJPEGLoader.LoadFromFile(const FileName, FileType: string; Bitmap: TLinearBitmap);
var
  Bm : IBitmap;
begin
  if (DesiredSize.X<MaxInt) or (DesiredSize.Y<MaxInt) then inherited // Use TJPEGImage
  else
  try
    Bm:=NewBitmap(FileName);
    if Bm.PixelFormat=PixelFormat8bppIndexed then Abort; // Use TJPEGImage to handle grayscale correctly
    if Bm.LastResult<>Ok then raise ELinearBitmap.Create(rsErrorInBitmapData);
    TrueSize:=Point(Bm.Width,Bm.Height);
    GDIplusBitmapToLinearBitmap(Bm,Bitmap);
    if not Bitmap.Present then raise ELinearBitmap.Create(rsErrorInBitmapData);
    if Bitmap.IsGrayScale then ConvertToGrayscale(Bitmap);
  except
    inherited; // Try TJPEGImage if GDI+ fails
  end;
end;

procedure TJPEGLoader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
  DelphiStream : TSeekableDelphiStream;
  JPEG : TJPEGImage;
begin
  JPEG:=TJPEGImage.Create;
  try
    DelphiStream:=TSeekableDelphiStream.Create(Stream);
    try
      JPEG.LoadFromStream(DelphiStream);
    finally
      DelphiStream.Free;
    end;
    JPEG.Scale:=jsFullSize;
    TrueSize:=Point(JPEG.Width,JPEG.Height);
    while (JPEG.Scale<jsEighth) and (JPEG.Width div 2>=DesiredSize.X) and (JPEG.Height div 2>=DesiredSize.Y) do
      JPEG.Scale:=Succ(JPEG.Scale);
    if JPEG.Scale=jsEighth then JPEG.Performance:=jpBestSpeed // Optimize preview loading for speed
    else JPEG.Performance:=jpBestQuality;
    JPEG.PixelFormat:=jf24bit;
    Bitmap.Assign(JPEG);
    if not Bitmap.Present then raise ELinearBitmap.Create(rsErrorInBitmapData);
    if JPEG.GrayScale then ConvertToGrayscale(Bitmap)
    else if Bitmap.PixelFormat=pf32bit then Bitmap.PixelFormat:=pf24bit;
  finally
    JPEG.Free;
  end;
end;

procedure TJPEGLoader.SaveToStream(Stream: TSeekableStream; const Ext: string; LBitmap: TLinarBitmap);
var
  X, Y : Integer;
  Bitmap : TBitmap;
  JPEG : TJPEGImage;
  OutPix : ^RGBRec;
  Pix : ^Byte;
  DelphiStream : TSeekableDelphiStream;
  MemStream : TMemoryStream;
  MinAboveSizeQuality, MaxBelowSizeQuality, CurrentSize, QualityGuess : Integer;
begin
  Bitmap:=TBitmap.Create;
  try
    if LBitmap.Pixelformat=pf24bit then LBitmap.AssignTo(Bitmap)
    else if LBitmap.Pixelformat=pf8bit then
    begin
      Bitmap.PixelFormat:=pf24bit;
      Bitmap.Width:=LBitmap.Width;
      Bitmap.Height:=LBitmap.Height;
      for Y:=0 to LBitmap.Height-1 do
      begin
       Pix:=@LBitmap.ScanLine[Y]^;
       OutPix:=@Bitmap.ScanLine[Y]^;
       for X:=0 to LBitmap.Width-1 do
       begin
        OutPix^:=LBitmap.Palette^[Pix^];
        Inc(Pix); Inc(OutPix);
       end;
      end;
    end;

    JPEG:=TJPEGImage.Create;
    try
      JPEG.Scale:=jsFullSize;
      JPEG.PixelFormat:=jf24bit;
      if LBitmap.IsGrayscale then JPEG.GrayScale:=True;

      if FileSizeLimit>0 then // Optimize file size to FileSizeLimit
      begin
        if Quality<1 then Quality:=JPEG.CompressionQuality;
        QualityGuess:=Quality;
        MinAboveSizeQuality:=Quality+1;
        MaxBelowSizeQuality:=1;
        MemStream:=TMemoryStream.Create;
        try
          repeat
            MemStream.Clear;
            JPEG.CompressionQuality:=QualityGuess;
            JPEG.Assign(Bitmap);
            {$IFDEF EXIF}
            if Assigned(EXIF) then EXIF.WriteEXIFJpeg(JPEG,MemStream) else
            {$ENDIF}
            JPEG.SaveToStream(MemStream);
            CurrentSize:=MemStream.Position;
            if CurrentSize>FileSizeLimit then
            begin
              MinAboveSizeQuality:=QualityGuess;
              if QualityGuess<=1 then Break;
            end                                      
            else if CurrentSize<FileSizeLimit then
            begin
              MaxBelowSizeQuality:=QualityGuess;
              if (QualityGuess>=100) or (MaxBelowSizeQuality+1=MinAboveSizeQuality) then Break;
            end
            else Break; // CurrentSize=FileSizeLimit
            QualityGuess:=(MinAboveSizeQuality+MaxBelowSizeQuality) div 2;
          until False;

          Stream.Write(MemStream.Memory^,CurrentSize);
        finally
          MemStream.Free;
        end;
      end
      else // Use specified quality
      begin
        if Quality>=1 then JPEG.CompressionQuality:=Quality;
        JPEG.Assign(Bitmap);
        DelphiStream:=TSeekableDelphiStream.Create(Stream);
        try
          {$IFDEF EXIF}
          if Assigned(EXIF) then EXIF.WriteEXIFJpeg(JPEG,DelphiStream) else
          {$ENDIF}
          JPEG.SaveToStream(DelphiStream);
        finally
          DelphiStream.Free;
        end;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

initialization
  Default:=TJPEGLoader.Create;
  LinarBitmap.AddLoader(Default);
finalization
  Default.Free;
end.

