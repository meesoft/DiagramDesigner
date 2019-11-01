////////////////////////////////////////////////////////////////////////////////
//
// IconList.pas - Windows icon and cursor handling
// -----------------------------------------------
// Version:   2007-02-27
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
// Last changes:
//   AssignTo fixed
//   Made thread safe
//
unit IconList;

interface

uses Monitor, Windows, SysUtils, MemUtils, Graphics, Streams, LinarBitmap, MathUtils;

type
  TPalEntriesArray = array[0..255] of TPALETTEENTRY;
  PPalEntriesArray = ^TPalEntriesArray;

  TIconImage = class
    protected
      fWidth, fHeight  : Integer;
      fPixelFormat : TPixelFormat;
      fBitsPerPixel : Integer;
      fBytesPerLine : Integer;
      fMaskBytesPerLine : Integer;
      fImageSize : Integer;
      fMaskSize : Integer;
      fPaletteSize : Integer;
    public
      Mask : PByteArray;
      Image : PByteArray;
      Palette : PPalEntriesArray;

      HotSpot : TPoint;
      property Width: Integer read fWidth;
      property Height: Integer read fHeight;
      property BytesPerLine: Integer read fBytesPerLine;
      property MaskBytesPerLine: Integer read fMaskBytesPerLine;
      property ImageSize : Integer read fImageSize;
      property MaskSize : Integer read fMaskSize;
      property PixelFormat : TPixelFormat read fPixelFormat;
      property BitsPerPixel : Integer read fBitsPerPixel;
      property PaletteSize : Integer read fPaletteSize;

      constructor Create(Width,Height,BitsPerPixel: Integer);
      destructor Destroy; override;

      procedure AssignTo(Bitmap: TObject; Transparent: TColor=-1);
      procedure Draw(XPos,YPos: Integer; Canvas: TCanvas);
    end;

  TIconImageArray = array[0..0] of TIconImage;
  PIconImageArray = ^TIconImageArray;

  TIconListType = (ltUnknown,ltIcon,ltCursor);

  TIconList = class
    private
      fCount : Integer;
      fIcons : PIconImageArray;

      ListFree : Integer; // Number of unused places in fIcons

    public
      ListType : TIconListType;

      property Count: Integer read fCount;
      property Icons : PIconImageArray read fIcons;

      destructor Destroy; override;

      function AddIcon(Width,Height,BitsPerPixel: Integer): TIconImage;
      procedure AddList(List: TIconList);

      procedure LoadFromStream(Stream: TSeekableStream);
      procedure SaveToStream(Stream: TBaseStream);

      procedure LoadFromFile(Name: string);
      procedure SaveToFile(Name: string);

      procedure Clear;
    end;

implementation

uses PNGLoader;

//==================================================================================================
// TIconImage
//==================================================================================================

constructor TIconImage.Create(Width,Height,BitsPerPixel: Integer);
begin
  inherited Create;

  fWidth:=Width; fHeight:=Height;

  case BitsPerPixel of
    1  : fPixelFormat:=pf1bit;
    4  : fPixelFormat:=pf4bit;
    8  : fPixelFormat:=pf8bit;
    24 : fPixelFormat:=pf24bit;
    32 : fPixelFormat:=pf32bit;
    else raise Exception.Create(rsInvalidPixelFormat+' ('+IntToStr(BitsPerPixel)+')');
  end;
  fBitsPerPixel:=BitsPerPixel;

  if BitsPerPixel<=8 then
  begin
    fPaletteSize:=(1 shl BitsPerPixel)*SizeOf(TPALETTEENTRY);
    GetMem(Palette,PaletteSize);
  end
  else fPaletteSize:=0;

  fBytesPerLine:=Ceil4(Width*BitsPerPixel div 8);
  fMaskBytesPerLine:=Ceil4(Ceil8(Width) div 8);

  fImageSize:=fBytesPerLine*Height;
  fMaskSize:=fMaskBytesPerLine*Height;

  GetMem(Image,fImageSize+1);
  GetMem(Mask,fMaskSize);
end;

destructor TIconImage.Destroy;
begin
  if Assigned(Mask) then FreeMem(Mask);
  if Assigned(Image) then FreeMem(Image);
  if Assigned(Palette) then FreeMem(Palette);
end;

procedure TIconImage.AssignTo(Bitmap: TObject; Transparent: TColor);

  function MakeHPalette(var Pal: TPalEntriesArray; Count: Integer): HPALETTE;
  var LogPal: PLogPalette; // LogPalette
  begin
    GetMem(LogPal,SizeOf(TLogPalette)+SizeOf(TPaletteEntry)*(Count-1));
    try
      LogPal^.PalVersion:=$300;
      LogPal^.PalNumEntries:=Count;
      Move(Pal,LogPal^.palPalEntry,Count*SizeOf(TPaletteEntry));
      Result:=CreatePalette(LogPal^);
    finally
      FreeMem(LogPal);
    end;
  end;

  function FindColor(Color: TColor): Integer;
  var
    I : Integer;
  begin
    Result:=-1;
    for I:=0 to 1 shl BitsPerPixel-1 do if Integer(Palette^[I]) and $ffffff=Color and $ffffff then
    begin
      Result:=I;
      Break;
    end;
  end;

var
  Y, X, Bit : Integer;
  MaskPtr : ^Byte;
  NewPal : PPalEntriesArray;
  DelphiBitmap : TBitmap;
begin
  if Transparent=-1 then Transparent:=$ff00ff
  else Transparent:=ColorToRGB(Transparent);
  if Bitmap is TBitmap then // TBitmap
  begin
    TBitmap(Bitmap).PixelFormat:=PixelFormat;
    TBitmap(Bitmap).Width:=Width;
    TBitmap(Bitmap).Height:=Height;
    if BitsPerPixel<=8 then TBitmap(Bitmap).Palette:=MakeHPalette(Palette^,1 shl BitsPerPixel);
    if Transparent=-1 then Transparent:=TBitmap(Bitmap).TransparentColor
    else TBitmap(Bitmap).TransparentColor:=Transparent;
    if PixelFormat=pf32bit then Draw(0,0,TBitmap(Bitmap).Canvas)
    else
      for Y:=0 to Height-1 do
      begin
        Move(Image^[Y*BytesPerLine],TBitmap(Bitmap).ScanLine[Height-1-Y]^,BytesPerLine);
        Bit:=7;
        MaskPtr:=@Mask^[Y*MaskBytesPerLine];
        for X:=0 to Width-1 do
        begin
          if Boolean((MaskPtr^ shr Bit) and 1) then
            TBitmap(Bitmap).Canvas.Pixels[X,Height-1-Y]:=Transparent;
          if Bit=0 then
          begin
            Bit:=7;
            Inc(MaskPtr);
          end
          else Dec(Bit);
        end;
      end;
  end
  else if Bitmap is TLinearBitmap then
  begin
    DelphiBitmap:=TBitmap.Create;
    try
      if BitsPerPixel>8 then
      begin
        DelphiBitmap.Canvas.Lock;
        DelphiBitmap.Canvas.Brush.Color:=Transparent;
        AssignTo(DelphiBitmap,Transparent);
        DelphiBitmap.Canvas.UnLock;
      end
      else if (BitsPerPixel=8) and (FindColor(Transparent)=-1) then
      begin
        DelphiBitmap.PixelFormat:=pf24bit;
        DelphiBitmap.Width:=Width;
        DelphiBitmap.Height:=Height;
        with DelphiBitmap.Canvas do
        begin
          Lock;
          Brush.Color:=Transparent;
          FillRect(ClipRect);
        end;
        Draw(0,0,DelphiBitmap.Canvas);
        DelphiBitmap.Canvas.Unlock;
      end
      else
      begin
        DelphiBitmap.PixelFormat:=pf8bit;
        DelphiBitmap.Width:=Width;
        DelphiBitmap.Height:=Height;
        if FindColor(Transparent)=-1 then
        begin
          GetMem(NewPal,256*4);
          ZeroMem(NewPal^,256*4);
          Move(Palette^,NewPal^,4 shl BitsPerPixel);
          TColor(NewPal^[255]):=Transparent;
          DelphiBitmap.Palette:=MakeHPalette(NewPal^,256);
          FreeMem(NewPal);
        end
        else DelphiBitmap.Palette:=MakeHPalette(Palette^,1 shl BitsPerPixel);
        with DelphiBitmap.Canvas do
        begin
          Lock;
          Brush.Color:=Transparent;
          FillRect(ClipRect);
        end;
        Draw(0,0,DelphiBitmap.Canvas);
        DelphiBitmap.Canvas.Unlock;
      end;
      TLinearBitmap(Bitmap).Assign(DelphiBitmap);
    finally
      DelphiBitmap.Free;
    end;
  end
  else raise Exception.Create('Cannot assign '+ClassName+' to '+Bitmap.ClassName);
end;

procedure TIconImage.Draw(XPos,YPos: Integer; Canvas: TCanvas);
var
  Y, X, MaskBit, ImageBit : Integer;
  MaskPtr, ImagePtr : PByte;
  Pix24 : PRGBRec;
  Pix32 : PColor;
begin
  YPos:=YPos+Height-1;
  if BitsPerPixel<=8 then
  begin
    for Y:=0 to Height-1 do
    begin
      ImageBit:=8;
      ImagePtr:=@Image^[Y*BytesPerLine];
      MaskBit:=7;
      MaskPtr:=@Mask^[Y*MaskBytesPerLine];
      for X:=0 to Width-1 do
      begin
        if not Boolean((MaskPtr^ shr MaskBit) and 1) then
          Canvas.Pixels[XPos+X,YPos-Y]:=
            TColor(Palette^[(ImagePtr^ shr (ImageBit-BitsPerPixel)) and (1 shl BitsPerPixel-1)]) and $ffffff;

        if MaskBit=0 then
        begin
          MaskBit:=8;
          Inc(MaskPtr);
        end;
        Dec(MaskBit);

        Dec(ImageBit,BitsPerPixel);
        if ImageBit=0 then
        begin
          ImageBit:=8;
          Inc(ImagePtr);
        end;
      end;
    end;
  end
  else if BitsPerPixel=24 then
  begin
    for Y:=0 to Height-1 do
    begin
      Pix24:=@Image^[Y*BytesPerLine];
      MaskPtr:=@Mask^[Y*MaskBytesPerLine];
      MaskBit:=7;
      for X:=0 to Width-1 do
      begin
        if not Boolean((MaskPtr^ shr MaskBit) and 1) then
          Canvas.Pixels[XPos+X,YPos-Y]:=RGB2BGR(PColor(Pix24)^) and $ffffff;

        if MaskBit=0 then
        begin
          MaskBit:=8;
          Inc(MaskPtr);
        end;
        Dec(MaskBit);

        Inc(Pix24);
      end;
    end;
  end
  else if BitsPerPixel=32 then
  begin
    for Y:=0 to Height-1 do
    begin
      Pix32:=@Image^[Y*BytesPerLine];
      MaskPtr:=@Mask^[Y*MaskBytesPerLine];
      MaskBit:=7;
      for X:=0 to Width-1 do
      begin
        if not Boolean((MaskPtr^ shr MaskBit) and 1) then
          if TRGBQuad(Pix32^).rgbReserved=255 then
            Canvas.Pixels[XPos+X,YPos-Y]:=RGB2BGR(Pix32^) and $ffffff
          else
            Canvas.Pixels[XPos+X,YPos-Y]:=ColorTone(RGB2BGR(Pix32^) and $ffffff,Canvas.Pixels[XPos+X,YPos-Y],TRGBQuad(Pix32^).rgbReserved,255);
        if MaskBit=0 then
        begin
          MaskBit:=8;
          Inc(MaskPtr);
        end;
        Dec(MaskBit);

        Inc(Pix32);
      end;
    end;
  end
  else raise Exception.Create(rsInvalidPixelFormat+': '+IntToStr(BitsPerPixel)+' bits');
end;

//==================================================================================================
// TIconList
//==================================================================================================

type
  TIconDir = packed record
               idReserved : WORD;   // Reserved (must be 0)
               idType     : WORD;   // Resource Type (1 for icons)
               idCount    : WORD;   // How many images?
             end;

  TIconDirEntry = packed record
                    bWidth         : BYTE;    // Width, in pixels, of the image
                    bHeight        : BYTE;    // Height, in pixels, of the image
                    bColorCount    : BYTE;    // Number of colors in image (0 if >=8bpp)
                    bReserved      : BYTE;    // Reserved ( must be 0)
                    wHotSpotX      : WORD;    // Cursor Hot Spot X
                    wHotSpotY      : WORD;    // Cursor Hot Spot X
                    dwBytesInRes   : DWORD;   // How many bytes in this resource?
                    dwImageOffset  : DWORD;   // Where in the file is this image?
                  end;

//   typdef struct
//   {
//      BITMAPINFOHEADER   icHeader;      // DIB header
//      RGBQUAD            icColors[1];   // Color table
//      BYTE               icXOR[1];      // DIB bits for XOR mask
//      BYTE               icAND[1];      // DIB bits for AND mask
//   } ICONIMAGE, *LPICONIMAGE

  T4CharArray = array[0..3] of Char;
  P4CharArray = ^T4CharArray;

procedure TIconList.LoadFromStream(Stream: TSeekableStream);

  procedure MoveLine8to1(var Src,Dest; Count: Integer);
  var
    Bit : Integer;
    SourcePix, DestPix : PByte;
  begin
    SourcePix:=@Src;
    DestPix:=@Dest;
    Bit:=8;
    for Count:=1 to Count do
    begin
      Dec(Bit);
      if SourcePix^<=0 then DestPix^:=DestPix^ or (1 shl Bit);
      Inc(SourcePix);
      if Bit=0 then
      begin
        Inc(DestPix);
        Bit:=8;
      end;
    end;
  end;

  procedure SetLineAlpha(var Src,Dest; Count: Integer);
  var
    SourcePix : PByte;
    DestPix : PRGBQuad;
  begin
    SourcePix:=@Src;
    DestPix:=@Dest;
    for Count:=1 to Count do
    begin
      DestPix^.rgbReserved:=SourcePix^;
      Inc(SourcePix);
      Inc(DestPix);
    end;
  end;

var
  Dir : TIconDir;
  DirEntry : TIconDirEntry;
  I, C : Integer;
  DirEntryOffset : Integer;
  BitmapHeader : TBitmapInfoHeader;
  RGBPal : array[0..255] of RGBQUAD;
  PNGLoader : TPNGLoader;
  PNGImage : TLinearBitmap;
begin
  Clear;

  // Read header
  DirEntryOffset:=Stream.Read(Dir,SizeOf(Dir));
  case Dir.idType of
    1  : ListType:=ltIcon;
    2  : ListType:=ltCursor;
    else ListType:=ltUnknown;
  end;
  GetMem(fIcons,Dir.idCount*SizeOf(TIconImage));
  ListFree:=Dir.idCount;
  for I:=1 to Dir.idCount do
  begin
    // Read icon info
    Stream.Position:=DirEntryOffset;
    Inc(DirEntryOffset,Stream.Read(DirEntry,SizeOf(DirEntry)));

    Stream.Position:=DirEntry.dwImageOffset;
    Stream.Read(BitmapHeader,SizeOf(BitmapHeader));

    if P4CharArray(@BitmapHeader)^=#137'PNG' then // PNG encoded icon
    begin
      PNGLoader:=TPNGLoader.Create;
      PNGImage:=TLinearBitmap.Create;
      try
        Stream.Position:=DirEntry.dwImageOffset;
        PNGLoader.ExtraInfo:=True;
        PNGLoader.LoadFromStream(Stream,'PNG',PNGImage);
        if Assigned(PNGLoader.AlphaChannel) then PNGImage.PixelFormat:=pf32bit
        else PNGImage.PixelFormat:=pf24bit;
        with AddIcon(PNGImage.Width,PNGImage.Height,PNGImage.PixelSize*8) do // Create new icon
        begin
          for C:=0 to Height-1 do Move(PNGImage.ScanLine[Height-1-C]^,Image^[C*BytesPerLine],PNGImage.BytesPerLine);
          ZeroMem(Mask^,MaskSize);
          if Assigned(PNGLoader.AlphaChannel) then
          begin
            PNGLoader.AlphaChannel.PixelFormat:=pf8bit;
            for C:=0 to Height-1 do
            begin
              MoveLine8to1(PNGLoader.AlphaChannel.ScanLine[Height-1-C]^,Mask^[C*MaskBytesPerLine],PNGImage.Width);
              SetLineAlpha(PNGLoader.AlphaChannel.ScanLine[Height-1-C]^,Image^[C*BytesPerLine],PNGImage.Width);
            end;
          end;
        end;
      finally
        PNGImage.Free;
        PNGLoader.Free;
      end;
    end
    else // Uncompressed bitmap
    begin
      if (BitmapHeader.biWidth<>DirEntry.bWidth) or (BitmapHeader.biHeight div 2<>DirEntry.bHeight) then
        Continue;
      with AddIcon(DirEntry.bWidth,DirEntry.bHeight,BitmapHeader.biBitCount) do // Create new icon
      begin
        HotSpot.X:=DirEntry.wHotSpotX;
        HotSpot.Y:=DirEntry.wHotSpotY;
        // Load icon data
        if BitmapHeader.biBitCount<=8 then
        begin
          Stream.Read(RGBPal,PaletteSize);
          for C:=0 to 1 shl BitmapHeader.biBitCount-1 do
          begin
            Palette^[C].peRed:=RGBPal[C].rgbRed;
            Palette^[C].peGreen:=RGBPal[C].rgbGreen;
            Palette^[C].peBlue:=RGBPal[C].rgbBlue;
            Palette^[C].peFlags:=PC_RESERVED;
          end;{}
        end;
        Stream.Read(Image^,ImageSize);
        Stream.Read(Mask^,MaskSize);
      end;
    end;
  end;
end;

procedure TIconList.SaveToStream(Stream: TBaseStream);
var
  Dir : TIconDir;
  DirEntry : TIconDirEntry;
  I, C : Integer;
  Offset : Integer;
  BitmapHeader : TBitmapInfoHeader;
  RGBPal : array[0..255] of RGBQUAD;

begin
  // Write header
  Dir.idReserved:=0;
  case ListType of
    ltIcon   : Dir.idType:=1;
    ltCursor : Dir.idType:=2;
    else raise Exception.Create(rsUnsupportedFileFormat);
  end;
  Dir.idCount:=Count;
  Stream.Write(Dir,SizeOf(Dir));

  // Write all dir entries
  Offset:=SizeOf(Dir)+Count*SizeOf(DirEntry);
  DirEntry.bReserved:=0;
  for I:=0 to Count-1 do with Icons[I] do
  begin
    DirEntry.bWidth:=Width;
    DirEntry.bHeight:=Height;
    DirEntry.wHotSpotX:=HotSpot.X;
    DirEntry.wHotSpotY:=HotSpot.Y;
    DirEntry.bColorCount:=1 shl BitsPerPixel;
    DirEntry.dwBytesInRes:=SizeOf(BitmapHeader)+ImageSize+MaskSize+PaletteSize;
    DirEntry.dwImageOffset:=Offset;

    Inc(Offset,DirEntry.dwBytesInRes);

    Stream.Write(DirEntry,SizeOf(DirEntry));
  end;

  // Write all images
  ZeroMem(BitmapHeader,SizeOf(BitmapHeader));
  BitmapHeader.biSize:=SizeOf(BitmapHeader);
  BitmapHeader.biPlanes:=1;
  for I:=0 to Count-1 do with Icons[I] do
  begin
    BitmapHeader.biWidth:=Width;
    BitmapHeader.biHeight:=Height*2;
    BitmapHeader.biBitCount:=BitsPerPixel;
    BitmapHeader.biSizeImage:=ImageSize+MaskSize;

    Stream.Write(BitmapHeader,SizeOf(BitmapHeader));

    if BitmapHeader.biBitCount<=8 then
    begin
      for C:=0 to 1 shl BitmapHeader.biBitCount-1 do
      begin
        RGBPal[C].rgbRed:=Palette^[C].peRed;
        RGBPal[C].rgbGreen:=Palette^[C].peGreen;
        RGBPal[C].rgbBlue:=Palette^[C].peBlue;
        RGBPal[C].rgbReserved:=0;
      end;
      Stream.Write(RGBPal,PaletteSize);
    end;

    Stream.Write(Image^,ImageSize);
    Stream.Write(Mask^,MaskSize);
  end;
end;

procedure TIconList.Clear;
var
  I : Integer;
begin
  for I:=0 to Count-1 do Icons[I].Free;
  if Assigned(fIcons) then
  begin
    FreeMem(fIcons);
    fIcons:=nil;
  end;
  ListFree:=0;
  fCount:=0;
end;

function TIconList.AddIcon(Width,Height,BitsPerPixel: Integer): TIconImage;
var
  Old : PIconImageArray;
begin
  if ListFree=0 then
  begin
    Old:=fIcons;
    ListFree:=10;
    GetMem(fIcons,(Count+ListFree)*SizeOf(TIconImage));

    if Assigned(Old) then
    begin
      Move(Old^,fIcons^,Count*SizeOf(TIconImage));
      FreeMem(Old);
    end;
  end;

  Result:=TIconImage.Create(Width,Height,BitsPerPixel);
  Icons^[Count]:=Result;
  Inc(fCount);
  Dec(ListFree);
end;

procedure TIconList.AddList(List: TIconList);
var
  I : Integer;
begin
  for I:=0 to List.Count-1 do
  with AddIcon(List.Icons[I].Width,List.Icons[I].Height,List.Icons[I].BitsPerPixel) do
  begin
    Move(List.Icons[I].Image^,Image^,ImageSize);
    Move(List.Icons[I].Mask^,Mask^,MaskSize);
    if BitsPerPixel<=8 then Move(List.Icons[I].Palette^,Palette^,(1 shl BitsPerPixel)*SizeOf(TPALETTEENTRY));
  end;
end;

procedure TIconList.LoadFromFile(Name: string);
var
  Stream : TFileStream;
begin
  Stream:=TFileSTream.Create(Name,[fsRead]);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIconList.SaveToFile(Name: string);
var
  Stream : TFileStream;
begin
  Stream:=TFileSTream.Create(Name,fsRewrite);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

destructor TIconList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

