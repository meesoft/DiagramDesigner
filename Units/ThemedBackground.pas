////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// ThemedBackground.pas - Theme background picture for TStyleForm
// --------------------------------------------------------------
// Changed:   2005-04-22
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
unit ThemedBackground;

interface

procedure Enable;

implementation

uses Types, Graphics, Themes, StyleForm, LinarBitmap, BitmapResize, Math;

var
  BackgroundImage : TLinearBitmap = nil;

procedure TransitionBackgroundRenderer(Bitmap: TBitmap);
var
  W, H, X, Y, TriangleSize : Integer;

  procedure DrawTriangle(X,Y: Integer);
  var
    Points : array[1..4] of TPoint;
  begin
    Points[1]:=Point(X+TriangleSize,Y);
    Points[2]:=Point(X+TriangleSize,Y+TriangleSize);
    Points[3]:=Point(X,Y+TriangleSize);
    Points[4]:=Point(X+TriangleSize,Y);
    Bitmap.Canvas.Polygon(Points);
  end;

begin
  // Background
  W:=Bitmap.Width;
  H:=Bitmap.Height;
  BackgroundImage.PaintToCanvas(Bitmap.Canvas,Rect(0,0,W,H),True);
  // Triangles
  TriangleSize:=Min(H div 9,16);
  Bitmap.Canvas.Brush.Color:=ColorTone(ColorToRGB(clBtnShadow),ColorToRGB(clBtnFace),50,100);
  Bitmap.Canvas.Pen.Color:=ColorTone(ColorToRGB(clBtnShadow),ColorToRGB(clBtnFace),40,100);
  for Y:=1 to 4 do
    for X:=1 to (5-Y) do
      DrawTriangle(W-X*TriangleSize,H-Y*TriangleSize);
end;

procedure Enable;
begin
  if (BackgroundImage=nil) and (GetSystemColorBitDepth>8) then
  begin
    BackgroundImage:=TLinearBitmap.Create(3,3,pf24bit);
    //BackgroundImage.ClearColor(clRed);
    BackgroundImage.ClearColor(ColorToRGB(clBtnFace));
    BackgroundImage.PixelColor[0,0]:=ColorTone(ColorToRGB(clWhite),ColorToRGB(clBtnFace),70,100);
    BackgroundImage.PixelColor[2,2]:=ColorTone(ColorToRGB(clBtnShadow),ColorToRGB(clBtnFace),40,100);
    ResizeImg(BackgroundImage,256,256);
    BackgroundRenderer:=TransitionBackgroundRenderer;
  end;
end;

initialization
  if ThemeServices.ThemesEnabled then Enable;
finalization
  BackgroundImage.Free;
end.

