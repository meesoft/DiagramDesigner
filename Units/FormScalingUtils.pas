unit FormScalingUtils;

// https://msdn.microsoft.com/en-us/library/windows/desktop/dn469266(v=vs.85).aspx

interface

uses Windows, Graphics, Controls;

procedure SetSystemFont(Font: TFont);
function ScaleToDPI(Width: Integer): Integer; overload;
function ScaleToDPI(Width: Integer; Canvas: TCanvas): Integer; overload;
procedure ScaleImageList(ImageList: TImageList; NewSize: Integer);

implementation

uses Forms, LinarBitmap, BitmapResize, Interpolation, ColorMapper, MultiCoreProcessing, StringUtils;

procedure ScaleImageList(ImageList: TImageList; NewSize: Integer);
var
  Images : array of TBitmap;
  Masks : array of TBitmap;
  Mask : TBitmap;

  procedure ScaleIcon(I: Integer);
  var
    Image : TLinearBitmap;
  begin
    Images[I].Canvas.Lock;
    Image:=TLinearBitmap.Create(Images[I]);
    ResizeImage(Image,NewSize,NewSize,Lanczos3ResampleLine8);
    Image.AssignTo(Images[I]);
    //Images[I].SaveToFile('x:\'+IntToStrLeadZero(I,2)+'i.bmp');
    Images[I].Canvas.Unlock;
    Masks[I].Canvas.Lock;
    Image.Assign(Masks[I]);
    ResizeImg(Image,NewSize,NewSize);
    ApplyThreshold(Image,128);
    Image.AssignTo(Masks[I]);
    Masks[I].Canvas.Unlock;
    //Masks[I].SaveToFile('x:\'+IntToStrLeadZero(I,2)+'m.bmp');
    Image.Free;
  end;

var
  I : Integer;
begin
  if ImageList.Width=NewSize then Exit;
  SetLength(Images,ImageList.Count);
  SetLength(Masks,ImageList.Count);
  Mask:=TBitmap.Create;
  Mask.Handle:=ImageList.GetMaskBitmap;
  Mask.PixelFormat:=pf24bit;
  for I:=0 to High(Images) do
  begin
    Images[I]:=TBitmap.Create;
    Images[I].Canvas.Brush.Color:=clBtnFace;
    ImageList.GetBitmap(I,Images[I]);
    Masks[I]:=TBitmap.Create;
    Masks[I].Width:=ImageList.Width;
    Masks[I].Height:=ImageList.Height;
    Masks[I].Canvas.Draw(0,-I*ImageList.Height,Mask);
  end;
  ParallelFor(0,High(Images),@ScaleIcon);
  Mask.Free;
  ImageList.Width:=NewSize;
  ImageList.Height:=NewSize;
  for I:=0 to High(Images) do
  begin
    ImageList.Add(Images[I],Masks[I]);
    Images[I].Free;
    Masks[I].Free;
  end;
end;

procedure SetSystemFont(Font: TFont);
var
  ICM : TIconMetrics;
begin
  ICM.cbSize:=SizeOf(ICM);
  if SystemParametersInfo(SPI_GETICONMETRICS, 0, @ICM, 0) then
    with ICM.lfFont do
      Font.Handle:=CreateFont(lfHeight,lfWidth,lfEscapement,lfOrientation,lfWeight,lfItalic,lfUnderline,lfStrikeOut,lfCharSet,
                              lfOutPrecision,lfClipPrecision,lfQuality,lfPitchAndFamily,lfFaceName);
end;

function ScaleToDPI(Width: Integer): Integer;
begin
  Result:=ScaleToDPI(Width,Application.MainForm.Canvas);
end;

function ScaleToDPI(Width: Integer; Canvas: TCanvas): Integer;
begin
  Result:=Width*GetDeviceCaps(Canvas.Handle,LOGPIXELSX) div 96;
end;

end.
