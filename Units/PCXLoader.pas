/////////////////////////////////////////////////////////////////////////////////////////////////////
//
// PCXLoader.pas - PCX bitmap coding/decoding
// ------------------------------------------
// Version:   2003-06-24
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//
unit PCXLoader;

interface

uses Classes, SysUtils, LinarBitmap, Streams, BufStream, Graphics, DelphiStream;

resourcestring
  rsPCXImageFile = 'Zsoft Paintbrush';

type
  PCXHeader = packed record
                Manuf        : Byte;
                Version      : Byte;
                Encode       : Byte;
                BitsPerPixel : Byte;
                X1,Y1,X2,Y2  : SmallInt;
                XRes,YRes    : SmallInt;
                Palette      : array [0..47] of Byte;
                VideoMode    : Byte;
                Planes       : Byte;
                BytesPerLine : SmallInt;
                Reserved     : array [0..59] of Byte;
              end; {128 bytes}

type
  TPCXGraphic = class(TLinarGraphic)
                  procedure LoadFromStream(Stream: TStream); override;
                  procedure SaveToStream(Stream: TStream); override;
                end;


implementation

uses FileMappedBitmap;

const
  FileExtension = 'PCX';

//==================================================================================================
// TPCXLoader
//==================================================================================================

type
  TPCXLoader = class(TFileMappedBitmapLoader)
              public
                function CanLoad(const Ext: string): Boolean; override;
                function CanSave(const Ext: string): Boolean; override;

                function GetLoadFilter: string; override;

                procedure LoadFromStream(InStream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
                procedure SaveToStream(OutStream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap); override;
              end;

function TPCXLoader.GetLoadFilter: string;
begin
  Result:=rsPCXImageFile+' (*.pcx)|*.pcx';
end;

function TPCXLoader.CanLoad(const Ext: string): Boolean;
begin
  Result:=Ext=FileExtension;
end;

function TPCXLoader.CanSave(const Ext: string): Boolean;
begin
  Result:=Ext=FileExtension;
end;

procedure TPCXLoader.LoadFromStream(InStream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
  Palette : array [0..767] of Byte;
  Header  : PCXHeader;
  Code, Color : Byte;
  X, LPos, Plan, Count : Integer;
  Stream : TBufferedStream;
begin
  Stream:=TBufferedStream.Create(-1,0,InStream);
  try
    Stream.Read(Header,SizeOf(Header));
    with Header do if (Manuf<>10) or (Version<>5) or (Encode<>1) or (BitsPerPixel<>8) or not (Planes in [1,3]) then
      raise ELinearBitmap.Create(rsUnsupportedFileFormat);

    with Header do
    if Planes=1 then  // 8 bit
    begin
      Bitmap.New(BytesPerLine,Y2-Y1+1,pf8bit);
      Plan:=0; X:=0;
    end
    else  // 24 bit
    begin
      Bitmap.New(BytesPerLine,Y2-Y1+1,pf24bit);
      Plan:=2; X:=2;
    end;

    LPos:=0;
    with Bitmap do
    while (X<Size) do
    begin
      Stream.Read(Code,1);
      if Code and $c0=$c0 then // Compressed block
      begin
        Count:=Code and 63;
        if Count=0 then Count:=1;
        Stream.Read(Color,1);
      end
      else // Single pixel
      begin
        Count:=1;
        Color:=Code;
      end;

      for Count:=1 to Count do
      begin
        if X>=Size then
          raise ELinearBitmap.Create(rsErrorInBitmapData);
        Map^[X]:=Color;
        Inc(X,Header.Planes); Inc(LPos);
        if (Header.Planes=3) and (LPos>=Width) then
        begin
          Dec(X,Plan);
          LPos:=0;
          if Plan=0 then
          begin
            if Assigned(ProgressUpdate) then ProgressUpdate(Int64(X)*100 div Size);
            Plan:=2
          end
          else
          begin
            Dec(X,BytesPerLine);
            Dec(Plan);
          end;
          Inc(X,Plan);
        end;
      end;
    end;

    Bitmap.ResizeCanvas(Header.X2-Header.X1+1,Bitmap.Height,0,0,0);

    if Header.Planes=1 then // 8 bit image, get palette
    begin
      repeat
        try
          Count:=Stream.Read(Code,1);

          if Code=12 then
          begin
            Stream.Read(Palette,768);
            Bitmap.Palette^:=GetFromRGBPalette(Palette);
            Count:=0;
          end;
        except
          Count:=0;
        end;
      until Count=0;

    end;
  finally
    Stream.Free;
  end;
end;

procedure TPCXLoader.SaveToStream(OutStream: TSeekableStream; const Ext: string; Bitmap: TLinarBitmap);
var
 Header : PCXHeader;
 Stream : TFilterStream;
 X, Y, P : Integer;
 Farv, Ant, Plan, Code : Byte;
begin
  if (Bitmap.Width>High(SmallInt)) or (Bitmap.Height>High(SmallInt)) then raise Exception.Create(rsInvalidBitmapSize);
  Stream:=TBufferedStream.Create(0,-1,OutStream);
  try
    with Bitmap do
    begin
      with Header do
      begin
        Manuf:=10;
        Version:=5;
        Encode:=1;
        BitsPerPixel:=8;
        if PixelFormat=pf8bit then Planes:=1
        else Planes:=3;
        BytesPerLine:=Width;
        X1:=0; Y1:=0;
        X2:=Width-1; Y2:=Height-1;
        XRes:=96; YRes:=96;
        VideoMode:=0;
      end;
      {$I-}
      Stream.Write(Header,SizeOf(Header));
      if PixelFormat=pf8bit then // Write 8 bit image data
      begin
        for Y:=0 to Height-1 do
        begin
          ScanLineSafe[Y];
          P:=Y*BytesPerLine;
          Ant:=0;
          Farv:=Map^[P];
          for X:=0 to Width-1 do
          begin
            if (Farv<>Map^[P]) or (Ant=63) then
            begin
              if (Ant<>1) or (Farv and $c0=$c0) then
              begin
                Code:=Ant or $c0;
                Stream.Write(Code,1);
              end;
              Stream.Write(Farv,1);
              Farv:=Map^[P];
              Ant:=1;
            end
            else Inc(Ant);
            Inc(P);
          end;
          // Write last block/pixel
          if (Ant<>1) or (Farv and $c0=$c0) then
          begin
            Code:=Ant or $c0;
            Stream.Write(Code,1);
          end;
          Stream.Write(Farv,1);
        end;
        // Write palette
        Code:=12;
        Stream.Write(Code,1);
        for Ant:=0 to 255 do
        begin
          Stream.Write(Palette^[Ant].R,1); Stream.Write(Palette^[Ant].G,1); Stream.Write(Palette^[Ant].B,1);
        end;
      end
      else // Write 24 bit image data
      begin
        for Y:=0 to Height-1 do
        begin
          ScanLineSafe[Y];
          for Plan:=2 downto 0 do
          begin
           P:=Y*BytesPerLine+Plan;
           Ant:=0;
           Farv:=Map^[P];
           for X:=0 to Width-1 do
           begin
            if (Farv<>Map^[P]) or (Ant=63) then
            begin
              if (Ant<>1) or (Farv and $c0=$c0) then
              begin
                Code:=Ant or $c0;
                Stream.Write(Code,1);
              end;
              Stream.Write(Farv,1);
              Farv:=Map^[P];
              Ant:=1;
            end
            else Inc(Ant);
            Inc(P,3);
           end;
           // Write last block/pixel
           if (Ant<>1) or (Farv and $c0=$c0) then
           begin
             Code:=Ant or $c0;
             Stream.Write(Code,1);
           end;
           Stream.Write(Farv,1);
          end;
          if Assigned(ProgressUpdate) then ProgressUpdate(Y*100 div (Height-1));
        end;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

//==================================================================================================
// TLinarGraphic
//==================================================================================================

procedure TPCXGraphic.LoadFromStream(Stream: TStream);
var
  Filter : TDelphiFilterStream;
begin
  Filter:=TDelphiFilterStream.Create(Stream);
  try
    FImage.LoadFromStream(Filter,FileExtension);
  finally
    Filter.Free;
  end;
end;

procedure TPCXGraphic.SaveToStream(Stream: TStream);
var
  Filter : TDelphiFilterStream;
begin
  Filter:=TDelphiFilterStream.Create(Stream);
  try
    FImage.SaveToStream(Filter,FileExtension);
  finally
    Filter.Free;
  end;
end;

var
  Loader : TPCXLoader;
initialization
  Loader:=TPCXLoader.Create;
  LinarBitmap.AddLoader(Loader);
  TPicture.RegisterFileFormat(FileExtension,rsPCXImageFile,TPCXGraphic);
finalization
  TPicture.UnregisterGraphicClass(TPCXGraphic);
  Loader.Free;
end.

