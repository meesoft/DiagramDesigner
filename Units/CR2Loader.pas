/////////////////////////////////////////////////////////////////////////////////////////////////////
//
// CR2Loader.pas - Canon CR2 raw preview loader. The CR2 format is a Canon-specific TIFF variant.
// ----------------------------------------------------------------------------------------------
// Version:   2009-07-02
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//
unit CR2Loader;

interface

uses
  Windows, Classes, SysUtils, LinarBitmap, Streams;

resourcestring
  rsCR2ImageFile = 'CR2 Canon raw image';
  rsDNGImageFile = 'DNG Adobe raw image';

type
  TCR2Loader = class(TBitmapLoader)
    private
      procedure Load(Stream: TSeekableStream; const FileName: string; Bitmap: TLinarBitmap);
    public
      function CanLoad(const Ext: string): Boolean; override;
      function CanSave(const Ext: string): Boolean; override;
      function GetLoadFilter: string; override;
      function GetSaveFilter: string; override;
      procedure LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
      procedure LoadFromFile(const FileName,FileType: string; Bitmap: TLinarBitmap); override;
      function ReadImageTimeStamp(const FileName: string): TDateTime;
    end;

var
  Default : TCR2Loader;

implementation

uses
  JPEGLoader, BitmapRotate;

//==================================================================================================
// TCR2Loader
//==================================================================================================

function TCR2Loader.GetLoadFilter: string;
begin
  Result:=rsCR2ImageFile+' (*.cr2)|*.cr2|'+rsDNGImageFile+' (*.dng)|*.dng';
end;

function TCR2Loader.GetSaveFilter: string;
begin
  Result:='';
end;

function TCR2Loader.CanSave(const Ext: string): Boolean;
begin
  Result:=False;
end;

function TCR2Loader.CanLoad(const Ext: string): Boolean;
begin
  Result:=(Ext='CR2') or (Ext='DNG');
end;

procedure TCR2Loader.LoadFromFile(const FileName,FileType: string; Bitmap: TLinarBitmap);
var
  Stream : TFileStream;
begin
  Stream:=TFileStream.Create(FileName,[fsRead,fsShareRead]);
  try
  Load(Stream,FileName,Bitmap);
  finally
    Stream.Free;
  end;
end;

procedure TCR2Loader.LoadFromStream(Stream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
begin
  Load(Stream,'',Bitmap);
end;

type
  TTIFFHeader = packed record
                  II : array[0..1] of Char;
                  FortyTwo : Word;
                  IFDOffset : DWord;
                end;
  TIFDEntry = packed record
                Tag : Word;
                FieldType : Word;
                ValueCount : DWord;
                ValueOrOffset : DWord;
              end;

const // IFD entry field types
  ifdByte     = 1;
  ifdString   = 2;
  ifdShort    = 3;
  ifdLong     = 4;
  ifdRational = 5;

procedure TCR2Loader.Load(Stream: TSeekableStream; const FileName: string; Bitmap: TLinarBitmap);
var
  Header : TTIFFHeader;
  IFDEntry : TIFDEntry;
  I : Integer;
  IFDFieldCount : Word;
  Width, Height : DWord;
  ImageOffset, ImageSize : DWord;
  Compression, Orientation : Integer;
begin
  Stream.Read(Header,SizeOf(Header));   
  if (Header.II<>'II') or (Header.FortyTwo<>42) then
    raise ELinearBitmap.Create(rsUnsupportedFileFormat);

  while Header.IFDOffset>0 do
  begin
    ImageSize:=0;
    ImageOffset:=0;
    Width:=0;
    Height:=0;
    Compression:=-1;
    Orientation:=1;
    Stream.Seek(Header.IFDOffset);
    Stream.Read(IFDFieldCount,2);
    for I:=1 to IFDFieldCount do
    begin
      Stream.Read(IFDEntry,SizeOf(IFDEntry));
      case IFDEntry.Tag of
        $100 : Width:=IFDEntry.ValueOrOffset;
        $101 : Height:=IFDEntry.ValueOrOffset;
        $103 : Compression:=IFDEntry.ValueOrOffset;
        $111 : ImageOffset:=IFDEntry.ValueOrOffset;
        $112 : Orientation:=IFDEntry.ValueOrOffset;
        $117 : ImageSize:=IFDEntry.ValueOrOffset;
      end;
    end;
    Stream.Read(Header.IFDOffset,4); // Get next IFD offset

    if (ImageSize>0) and (ImageOffset>0) and (Width>0) and (Height>0) and (Compression>=0) then
    begin
      if Compression=6 then
      begin
        Stream.Seek(ImageOffset);
        Bitmap.LoadFromStream(Stream,'JPG');
      end
      else if FileName<>'' then
      begin
        BitmapLoaders.GetLoader('TIF').LoadFromFile(FileName,'TIF',Bitmap);
      end
      else
      begin
        Stream.Seek(0);
        Bitmap.LoadFromStream(Stream,'TIF');
      end;
      case Orientation of
        3 : Rotate180(Bitmap);
        6 : Rotate270(Bitmap);
        8 : Rotate90(Bitmap);
      end;
      Exit;
    end;
  end;

  // No preview image found
  raise ELinearBitmap.Create(rsUnsupportedFileFormat);
end;

function TCR2Loader.ReadImageTimeStamp(const FileName: string): TDateTime;
var
  Header : TTIFFHeader;
  IFDEntry : TIFDEntry;
  I : Integer;
  IFDFieldCount : Word;
  TimeStampOffset, TimeStampSize : DWord;
  Str : string;
  FormatSettings : TFormatSettings;
  Stream : TFileStream;
begin
  Result:=0;
  Stream:=TFileStream.Create(FileName,[fsRead,fsShareRead]);
  try
    Stream.Read(Header,SizeOf(Header));
    if (Header.II<>'II') or (Header.FortyTwo<>42) then
      raise ELinearBitmap.Create(rsUnsupportedFileFormat);

    while Header.IFDOffset>0 do
    begin
      TimeStampOffset:=0;
      TimeStampSize:=0;
      Stream.Seek(Header.IFDOffset);
      Stream.Read(IFDFieldCount,2);
      for I:=1 to IFDFieldCount do
      begin
        Stream.Read(IFDEntry,SizeOf(IFDEntry));
        case IFDEntry.Tag of
          $132 : if IFDEntry.FieldType=ifdString then // Time stamp string
                 begin
                   TimeStampOffset:=IFDEntry.ValueOrOffset;
                   TimeStampSize:=IFDEntry.ValueCount-1; // Skip terminating #0
                 end;
        end;
      end;
      Stream.Read(Header.IFDOffset,4); // Get next IFD offset

      if (TimeStampSize>0) and (TimeStampOffset>0) then
      begin
        SetLength(Str,TimeStampSize);
        Stream.Seek(TimeStampOffset);
        Stream.Read(Str[1],TimeStampSize);
        FormatSettings.DateSeparator:=':';
        FormatSettings.TimeSeparator:=':';
        FormatSettings.ShortDateFormat:='yyyy:mm:dd hh:nn:ss';
        FormatSettings.LongDateFormat:='yyyy:mm:dd hh:nn:ss';
        Result:=StrToDateTime(Str,FormatSettings);
        Exit;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

initialization
  Default:=TCR2Loader.Create;
  LinarBitmap.AddLoader(Default);
finalization
  Default.Free;
end.

