unit PluginLinearBitmap;

interface

uses Graphics, Windows, SysUtils, LinarBitmap, AnalyzerPlugins, MemUtils;

type
  TPluginLinearBitmap = class(TLinearBitmap)
    private
      OtherPalette, OtherMap : Pointer;
    protected
      procedure SetPixelFormat(PixFormat: TPixelFormat); override;
    public
      CopyOnTakeOver : Boolean;
      constructor Create(const Source: TImageContainer); overload;
      procedure New(const Source: TImageContainer); overload;
      procedure Dispose; override;
      procedure ForceLocal;
      // Make sure BytesPerLine=Width*PixelSize so no memory is wasted
      procedure OptimizeMem; override;
    end;

  TProgressUpdate = class
    public
      class procedure ProgressUpdate(Done: Integer);
    end;

procedure LinearBitmapFromImageContainer(Bitmap: TLinearBitmap; const Image: TImageContainer);
procedure LinearBitmapToImageContainer(Bitmap: TLinearBitmap; out Image: TImageContainer; TakeOver: Boolean=False);
procedure CreateAnalyzerChild(Image: TLinearBitmap; const Name: string);

implementation

uses
  BitmapConversion;

procedure LinearBitmapFromImageContainer(Bitmap: TLinearBitmap; const Image: TImageContainer);
var
  Planes, Y : Integer;
begin
  if Image.PixelFormat=$04 then
  begin
    Bitmap.New(Image.Width,Image.Height,pf32bit);
    for Y:=0 to Image.Height-1 do
      Move(Image.Map^[Y*Image.BytesPerLine],Bitmap.ScanLine[Y]^,Bitmap.BytesPerLine);
    Exit;
  end;
  if Image.PixelFormat=$64 then
  begin
    Bitmap.New(Image.Width,Image.Height,pf32bit);
    for Y:=0 to Image.Height-1 do
      Move(Image.Map^[Y*Image.BytesPerLine],Bitmap.ScanLine[Y]^,Bitmap.BytesPerLine);
    ConvertColorSpaceCMYK2RGB(Bitmap);
    Exit;
  end;

  Planes:=Image.PixelFormat and $0f;
  case Planes of
    1 : if Image.PixelFormat=$21 then Bitmap.New(Image.Width,Image.Height,pf16bit)
        else Bitmap.New(Image.Width,Image.Height,pf8bit);
    2 : begin
          Bitmap.New(Image.Width,Image.Height,pf24bit);
          Bitmap.Clear;
        end;
    3..15 : Bitmap.New(Image.Width,Image.Height,pf24bit);
  else raise ELinearBitmap.Create(rsUnsupportedBitmapFormat);
  end;

  if (Planes=1) or (Image.PixelFormat=$03) then
  begin
    for Y:=0 to Image.Height-1 do
      Move(Image.Map^[Y*Image.BytesPerLine],Bitmap.ScanLine[Y]^,Bitmap.BytesPerLine); // Direct data, no processing needed
  end
  else if Image.PixelFormat and $f0=$10 then DeInterleave(Bitmap,Image.Map,Planes) // Seperated planes
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
  if Bitmap.PixelFormat=pf8bit then
  begin
    if Assigned(Image.Palette) then Bitmap.Palette^:=TPalette(Image.Palette^)
    else Bitmap.Palette^:=GrayPal;
  end;
end;

procedure LinearBitmapToImageContainer(Bitmap: TLinearBitmap; out Image: TImageContainer; TakeOver: Boolean);
begin
  ZeroMem(Image,SizeOf(Image));
  if Assigned(Bitmap) then
  begin
    case Bitmap.PixelFormat of
      pf8bit  : Image.PixelFormat:=$01;
      pf24bit : Image.PixelFormat:=$03;
      pf32bit : Image.PixelFormat:=$04;
    else raise ELinearBitmap.Create(rsInvalidPixelFormat);
    end;
    Image.Width:=Bitmap.Width;
    Image.Height:=Bitmap.Height;
    Image.BytesPerLine:=Bitmap.BytesPerLine;
    Image.Palette:=Pointer(Bitmap.Palette);
    Image.Map:=Bitmap.Map;
    if TakeOver then
    begin
      Bitmap.Map:=nil;
      Bitmap.Palette:=nil;
      Bitmap.Dispose
    end;
  end;
end;

procedure CreateAnalyzerChild(Image: TLinearBitmap; const Name: string);
var
  CreateImageWindow : TCreateImageWindow;
begin
  LinearBitmapToImageContainer(Image,CreateImageWindow.Image);
  CreateImageWindow.Name:=PChar(Name);
  AnalyzerCallback(cmdCreateImageWindow,@CreateImageWindow);
end;

//==============================================================================================================================
// TProgressUpdate
//==============================================================================================================================
class procedure TProgressUpdate.ProgressUpdate(Done: Integer);
begin
  if not AnalyzerCallback(cmdUpdateProgress,@Done) then Abort;
end;

//==============================================================================================================================
// TPluginLinearBitmap
//==============================================================================================================================
constructor TPluginLinearBitmap.Create(const Source: TImageContainer);
begin
  inherited Create;
  New(Source);
end;

procedure TPluginLinearBitmap.New(const Source: TImageContainer);
begin
  Dispose;
  if (Source.Width<1) or (Source.Height<1) or (Source.Map=nil) then raise ELinearBitmap.Create(rsInvalidBitmapSize);
  OtherMap:=Source.Map;
  OtherPalette:=Source.Palette;
  case Source.PixelFormat of
    $1 : begin
           fPixelFormat:=pf8bit;
           if Assigned(Source.Palette) then Palette:=Pointer(Source.Palette)
           else
           begin
             GetMem(Palette,SizeOf(TPalette));
             Palette^:=GrayPal;
           end;
         end;
    $3 : fPixelFormat:=pf24bit;
    $4 : fPixelFormat:=pf32bit;
  else raise ELinearBitmap.Create(rsInvalidPixelFormat);
  end;
  fPixelSize:=Source.PixelFormat;
  Map:=Source.Map;
  fWidth:=Source.Width; fHeight:=Source.Height;
  fBytesPerLine:=Source.BytesPerLine;
  fSize:=BytesPerLine*Height;
  fPresent:=True;
end;

procedure TPluginLinearBitmap.Dispose;
begin
  if (Map=OtherMap) and (Map<>nil) then
    Map:=nil; // Owned by Analyzer, don't free
  if Palette=OtherPalette then Palette:=nil; // Owned by Analyzer, don't free
  OtherMap:=nil;
  OtherPalette:=nil;
  inherited;
end;

procedure TPluginLinearBitmap.ForceLocal;
var
  Y, NewBytesPerLine : Integer;
  NewMap : PByteArray;
begin
  if Present and (Map=OtherMap) then
  begin
    NewBytesPerLine:=Width*PixelSize;
    fSize:=Height*NewBytesPerLine;
    GetMem(NewMap,Size+1);
    for Y:=0 to Height-1 do Move(Map^[Y*BytesPerLine],NewMap^[Y*NewBytesPerLine],NewBytesPerLine);
    OtherMap:=nil;
    Map:=NewMap;
    fBytesPerLine:=NewBytesPerLine;
  end;
end;

procedure TPluginLinearBitmap.OptimizeMem;
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
    if Map<>OtherMap then FreeMem(Map);
    OtherMap:=nil;
    Map:=NewMap;
    fBytesPerLine:=NewBytesPerLine;
  end;
end;

procedure TPluginLinearBitmap.SetPixelFormat(PixFormat: TPixelFormat);
begin
  ForceLocal;
  inherited;
end;

end.

