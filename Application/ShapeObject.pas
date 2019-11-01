unit ShapeObject;

interface

uses Windows, SysUtils, StreamUtils, MathUtils, Graphics, DiagramBase, TextObject,
  Types, Streams, Math, Classes;

type
  TBaseLineObject = class(TTextObject)
    protected
      FLineColor  : TColor;
      FLineWidth  : LongInt;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
    public
      constructor Create; override;
      function ValidProperties: TObjectProperties; override;
      function GetBounds: TRect; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TShapeObject = class(TBaseLineObject)
    protected
      FFillColor : TColor;
      FGradientColor : TColor;
      GradientBitmap : TBitmap;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
      procedure SetupGradientBrush(Canvas: TCanvas; const Rect: TRect);
      procedure DrawRoundRect(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; const DrawRect: TRect; DX,DY: Integer);
      procedure DrawPolygon(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; const Points: array of TPoint; const HiResPoints: TFloatPointArray=nil);
    public
      constructor Create; override;
      constructor CreateNew(PropertyObject: TBaseObject=nil); override;
      destructor Destroy; override;
      function ValidProperties: TObjectProperties; override;
      procedure DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      function Move(DX,DY: Integer; Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TRectangleObject = class(TShapeObject)
    protected
      FCornerRadius : Integer;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
    public
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      class function Identifier: Integer; override;
      function ValidProperties: TObjectProperties; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TEllipseObject = class(TShapeObject)
    public
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      function ValidProperties: TObjectProperties; override;
      class function Identifier: Integer; override;
    end;

implementation

uses StringUtils, LinarBitmap, StrUtils;

//==============================================================================================================================
// TLineObject
//==============================================================================================================================
constructor TBaseLineObject.Create;
begin
  if Links=nil then
  begin
    SetLength(Links,2);
    Links[0]:=FloatPoint(0,0);
    Links[1]:=FloatPoint(1,1);
  end;
  inherited;
end;

function TBaseLineObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opLineWidth,opLineColor];
end;

function TBaseLineObject.GetBounds: TRect;
var
  HalfLineWidth : Integer;
begin
  if FLineColor=clNone then HalfLineWidth:=0
  else HalfLineWidth:=(FLineWidth+1) div 2;
  Result:=FPosition;
  Dec(Result.Left,HalfLineWidth);
  Dec(Result.Top,HalfLineWidth);
  Inc(Result.Right,HalfLineWidth);
  Inc(Result.Bottom,HalfLineWidth);
end;

function TBaseLineObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opLineWidth : Result:=FLineWidth;
    opLineColor : Result:=FLineColor;
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TBaseLineObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opLineWidth  : FLineWidth:=Value;
    opLineColor  : FLineColor:=Value;
    else inherited SetProperty(Index,Value);
  end;
end;

procedure TBaseLineObject.SaveToStream(Stream: TBaseStream);
begin
  inherited;
  Stream.Write(FLineWidth,4);
  Stream.Write(FLineColor,4);
end;

procedure TBaseLineObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
begin
  inherited;
  Stream.Read(FLineWidth,4);
  Stream.Read(FLineColor,4);
end;

//==============================================================================================================================
// TShapeObject
//==============================================================================================================================

var StandardShapeObjectLinks : TFloatPointArray;

constructor TShapeObject.Create;
begin
  Links:=StandardShapeObjectLinks;
  inherited;
end;

constructor TShapeObject.CreateNew(PropertyObject: TBaseObject);
begin
  Create;
  FFillColor:=clWhite;
  FGradientColor:=clNone;
  FLineWidth:=DefaultLineWidth;
  Name:=ExtractObjectName(ClassName);
  FMargin:=DesignerDPpoint*2;
  if Assigned(PropertyObject) then Assign(PropertyObject);
end;

destructor TShapeObject.Destroy;
begin
  GradientBitmap.Free;
  inherited;
end;

function TShapeObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opFillColor,opTextXAlign,opTextYAlign,opGradientColor];
end;

procedure TShapeObject.DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
var
  Text : string;
  FillColor, LineColor : TColor;
begin
  if FFillColor<>clNone then
  begin
    Text:=FText;
    FillColor:=FFillColor;
    LineColor:=FLineColor;
    FText:='';
    FLineColor:=clNone;
    FFillColor:=ShadowColor;
    Draw(Canvas,CanvasInfo,0);
    FText:=Text;
    FFillColor:=FillColor;
    FLineColor:=LineColor;
  end;
end;

function TShapeObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opFillColor         : Result:=FFillColor;
    opGradientColor     : Result:=FGradientColor;
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TShapeObject.SetProperty(Index: TObjectProperty; Value: Integer);
var
  I : Integer;
begin
  case Index of
    opFillColor     : begin
                        FFillColor:=Value;
                        FreeAndNil(GradientBitmap);
                      end;
    opGradientColor : begin
                        FGradientColor:=Value;
                        FreeAndNil(GradientBitmap);
                      end;
    opCustomLinks :   begin
                        I:=High(Links);
                        if PFloatPointArray(Value)^=StandardShapeObjectLinks then Links:=StandardShapeObjectLinks
                        else Links:=Copy(PFloatPointArray(Value)^);
                        for I:=High(Links)+1 to I do NotifyLinkPointDeleted(I);
                      end;
    else inherited SetProperty(Index,Value);
  end;
end;

function TShapeObject.Move(DX,DY,Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint;

  function OffsetX(X: Integer): Integer;
  begin
    Result:=RoundInt(X+DX,Grid.X);
  end;

  function OffsetY(Y: Integer): Integer;
  begin
    Result:=RoundInt(Y+DY,Grid.Y);
  end;

begin
  //  1 5 2
  //  7 0 8
  //  3 6 4
  if (ssCtrl in Shift) and (Handle in [1..4]) then
  begin
    case Handle of
      1 : begin
            Result.X:=Min(OffsetX(FPosition.Left),FPosition.Right)-FPosition.Left;
            Inc(FPosition.Left,Result.X);
            Result.Y:=(FPosition.Bottom-Width)-FPosition.Top;
            Inc(FPosition.Top,Result.Y);
          end;
      2 : begin
            Result.X:=Max(OffsetX(FPosition.Right),FPosition.Left)-FPosition.Right;
            Inc(FPosition.Right,Result.X);
            Result.Y:=(FPosition.Bottom-Width)-FPosition.Top;
            Inc(FPosition.Top,Result.Y);
          end;
      3 : begin
            Result.X:=Min(OffsetX(FPosition.Left),FPosition.Right)-FPosition.Left;
            Inc(FPosition.Left,Result.X);
            Result.Y:=(FPosition.Top+Width)-FPosition.Bottom;
            Inc(FPosition.Bottom,Result.Y);
          end;
      4 : begin
            Result.X:=Max(OffsetX(FPosition.Right),FPosition.Left)-FPosition.Right;
            Inc(FPosition.Right,Result.X);
            Result.Y:=(FPosition.Top+Width)-FPosition.Bottom;
            Inc(FPosition.Bottom,Result.Y);
          end;
    end;
    NotifyMovement;
  end
  else Result:=inherited Move(DX,DY,Handle,Grid,Shift);
end;

procedure TShapeObject.SaveToStream(Stream: TBaseStream);
begin
  inherited;
  Stream.Write(FFillColor,4);
  Stream.Write(FGradientColor,4);
end;

procedure TShapeObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
begin
  inherited;
  Stream.Read(FFillColor,4);
  if FileVersion>=20 then Stream.Read(FGradientColor,4)
  else FGradientColor:=clNone;
end;

procedure TShapeObject.SetupGradientBrush(Canvas: TCanvas; const Rect: TRect);
var
  I, Length : Integer;
  Pix : PIntegerArray;
  Scale : Double;
begin
  if FGradientColor and $80000000=0 then Length:=Max(1,Abs(Rect.Right-Rect.Left))
  else Length:=Max(1,Abs(Rect.Bottom-Rect.Top));
  if (GradientBitmap=nil) or (GradientBitmap.Width*GradientBitmap.Height<>Length) then
  begin
    GradientBitmap.Free;
    GradientBitmap:=TBitmap.Create;
    GradientBitmap.PixelFormat:=pf32bit;
    if FGradientColor and $80000000=0 then // Vertical
    begin
      GradientBitmap.Width:=Length;
      GradientBitmap.Height:=1;
    end
    else // Horizontal
    begin
      GradientBitmap.Width:=1;
      GradientBitmap.Height:=Length;
    end;
    Scale:=1/Max(1,Length-1);
    Pix:=GradientBitmap.ScanLine[GradientBitmap.Height-1];
    for I:=0 to Length-1 do
      with TPaletteEntry(Pix^[I]) do
      begin
        peBlue:=TRGBQuad(FFillColor).rgbBlue+Round((TRGBQuad(FGradientColor).rgbBlue-TRGBQuad(FFillColor).rgbBlue)*I*Scale);
        peGreen:=TRGBQuad(FFillColor).rgbGreen+Round((TRGBQuad(FGradientColor).rgbGreen-TRGBQuad(FFillColor).rgbGreen)*I*Scale);
        peRed:=TRGBQuad(FFillColor).rgbRed+Round((TRGBQuad(FGradientColor).rgbRed-TRGBQuad(FFillColor).rgbRed)*I*Scale);
      end;
  end;
  Canvas.Brush.Bitmap:=GradientBitmap;
  SetBrushOrgEx(Canvas.Handle,Rect.Left,Rect.Top,nil);
end;

// Draw rectangle (DX=DY=0) /rounded rectangle / ellipse (DX>Width,DY>Height) / circle (DX=DY>Width=Height).
// To be used in descending classes
procedure TShapeObject.DrawRoundRect(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; const DrawRect: TRect; DX, DY: Integer);
var
  RX, RY : Integer;
begin
  with Canvas do
  begin
    if FLineColor=clNone then
    begin
      Pen.Style:=psClear;
      CanvasTextMargin:=0;
    end
    else
    begin
      Pen.Style:=psSolid;
      Pen.Color:=FLineColor;
      Pen.Width:=Round(FLineWidth*CanvasInfo.Scale.X);
      CanvasTextMargin:=Pen.Width*2;
    end;
    if FFillColor=clNone then Brush.Style:=bsClear
    else if (FGradientColor=clNone) or (CanvasInfo.DrawMode=dmShadow) then
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=FFillColor;
    end
    else SetupGradientBrush(Canvas,DrawRect);
  end;

  with DrawRect, Canvas do
  if CanvasInfo.DrawMode=dmPreAntialiasing then
  begin
    AADraw:=False;
    if FLineColor=clNone then
    begin
      if FFillColor<>clNone then // Fill color only
      begin
        AADraw:=(DX<>0) or (DY<>0);
        if AADraw then RoundRect(Left+1,Top+1,Right-1,Bottom-1,DX,DY)
        else Rectangle(DrawRect);
        if (CanvasInfo.Container<>nil) and CanvasInfo.Container.ObjectShadows then AADraw:=True;
      end;
    end
    else if (DX=0) and (DY=0) then // Rectangle
    begin
      AADraw:=Pen.Width=0;
      if AADraw then Pen.Style:=psClear;
      Rectangle(DrawRect);
      if (CanvasInfo.Container<>nil) and CanvasInfo.Container.ObjectShadows then AADraw:=True;
    end
    else // Rounded
    begin
      AADraw:=True;
      if FFillColor<>clNone then // Fill inside
      begin
        Pen.Style:=psClear;
        RoundRect(Left+1,Top+1,Right,Bottom,DX,DY);
        Pen.Style:=psSolid;
      end;
      if Pen.Width>0 then
      begin
        if DX<Right-Left then
        begin
          RX:=(DX+1) div 2;
          MoveTo(Left+RX,Top); LineTo(Right-RX,Top);
          MoveTo(Left+RX,Bottom-1); LineTo(Right-RX,Bottom-1);
        end;
        if DY<Bottom-Top then
        begin
          RY:=(DY+1) div 2;
          MoveTo(Left,Top+RY); LineTo(Left,Bottom-RY);
          MoveTo(Right-1,Top+RY); LineTo(Right-1,Bottom-RY);
        end;
      end;
    end;
  end
  else if DX=MaxInt then Ellipse(Left,Top,Right,Bottom)
  else RoundRect(Left,Top,Right,Bottom,DX,DY);
  SetBrushOrgEx(Canvas.Handle,0,0,nil);
end;

// Draw polygon with AA support.
// To be used in descending classes
procedure TShapeObject.DrawPolygon(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; const Points: array of TPoint; const HiResPoints: TFloatPointArray);

  procedure PolygonAxisLines;
  var
    I, IPrev : Integer;
  begin
    IPrev:=High(Points);
    for I:=0 to IPrev do
    begin
      if (Points[I].X=Points[IPrev].X) or (Points[I].Y=Points[IPrev].Y) then
      with Canvas do
      begin
        with Points[IPrev] do MoveTo(X,Y);
        with Points[I] do LineTo(X,Y);
        if Pen.Width<=1 then with Points[I] do Pixels[X,Y]:=Pen.Color;
      end;
      IPrev:=I;
    end;
  end;

  procedure PolygonAxisLinesHiRes;
  var
    I, IPrev : Integer;
  begin
    Assert(Length(Points)=Length(HiResPoints));
    IPrev:=High(Points);
    for I:=0 to IPrev do
    begin
      if (HiResPoints[I].X=HiResPoints[IPrev].X) or (HiResPoints[I].Y=HiResPoints[IPrev].Y) then
      with Canvas do
      begin
        with Points[IPrev] do MoveTo(X,Y);
        with Points[I] do LineTo(X,Y);
        if Pen.Width<=1 then with Points[I] do Pixels[X,Y]:=Pen.Color;
      end;
      IPrev:=I;
    end;
  end;

var
  Bounds : TRect;
  I : Integer;
begin
  with Canvas do
  begin
    if FLineColor=clNone then Pen.Style:=psClear
    else
    begin
      Pen.Style:=psSolid;
      Pen.Color:=FLineColor;
      Pen.Width:=Round(FLineWidth*CanvasInfo.Scale.X);
    end;
    if FFillColor=clNone then Brush.Style:=bsClear
    else if (FGradientColor=clNone) or (CanvasInfo.DrawMode=dmShadow) then
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=FFillColor;
    end
    else
    begin
      Bounds:=Rect(MaxInt,MaxInt,-MaxInt,-MaxInt);
      for I:=0 to High(Points) do
      begin
        Bounds.Left:=Min(Bounds.Left,Points[I].X);
        Bounds.Top:=Min(Bounds.Top,Points[I].Y);
        Bounds.Right:=Max(Bounds.Right,Points[I].X);
        Bounds.Bottom:=Max(Bounds.Bottom,Points[I].Y);
      end;
      SetupGradientBrush(Canvas,Bounds);
    end;

    if CanvasInfo.DrawMode=dmPreAntialiasing then
    begin
      AADraw:=True;
      if FLineColor=clNone then
      begin
        if FFillColor<>clNone then Polygon(Points); // Fill color only
      end
      else
      begin
        Pen.Style:=psClear;
        Polygon(Points);
        Pen.Style:=psSolid;
        if Pen.Width>0 then
          if HiResPoints=nil then PolygonAxisLines
          else PolygonAxisLinesHiRes;
      end;
    end
    else if (CanvasInfo.DrawMode=dmRender) and (FLineColor=clNone) and (FFillColor<>clNone) and (FGradientColor=clNone) then
    begin
      Pen.Color:=FFillColor;
      Pen.Width:=1;
      Pen.Style:=psSolid;
      Polygon(Points);
    end
    else Polygon(Points);
    SetBrushOrgEx(Canvas.Handle,0,0,nil);
  end;
end;

//==============================================================================================================================
// TRectangleObject
//==============================================================================================================================
function TRectangleObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opCornerRadius,opCustomLinks];
end;

function TRectangleObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opCornerRadius : Result:=FCornerRadius;
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TRectangleObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opCornerRadius : FCornerRadius:=Value;
    else inherited SetProperty(Index,Value);
  end;
end;

procedure TRectangleObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Count : SmallInt;
begin
  inherited;
  if FileVersion>=15 then Stream.Read(FCornerRadius,4);
  if FileVersion>=24 then
  begin
    Stream.Read(Count,2);
    if Count<0 then Links:=StandardShapeObjectLinks
    else
    begin
      SetLength(Links,Count);
      Stream.Read(Links[0],Count*SizeOf(TFloatPoint));
    end;
  end;
end;

procedure TRectangleObject.SaveToStream(Stream: TBaseStream);
var
  Count : SmallInt;
begin
  inherited;
  Stream.Write(FCornerRadius,4);
  if Links=StandardShapeObjectLinks then
  begin
    Count:=-1;
    Stream.Write(Count,2);
  end
  else
  begin
    Count:=Length(Links);
    Stream.Write(Count,2);
    Stream.Write(Links[0],Count*SizeOf(TFloatPoint));
  end;
end;

procedure TRectangleObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  DrawRect : TRect;
  Diameter : TPoint;
begin
  DrawRect:=CanvasInfo.CanvasRect1(Position);
  if FCornerRadius=0 then DrawRoundRect(Canvas,CanvasInfo,DrawRect,0,0)
  else
  begin
    Diameter:=RoundPoint(FCornerRadius*CanvasInfo.Scale.X*2,FCornerRadius*CanvasInfo.Scale.Y*2);
    DrawRoundRect(Canvas,CanvasInfo,DrawRect,Diameter.X,Diameter.Y)
  end;

  if Assigned(CanvasInfo.ZBuffer) then with CanvasInfo.ZBuffer do
  begin
    if FLineColor=clNone then Pen.Width:=0
    else Pen.Width:=Max(Canvas.Pen.Width,3);
    Pen.Style:=psSolid;
    Pen.Color:=Index;
    if FFillColor=clNone then Brush.Style:=bsClear
    else
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Index;
    end;
    if FCornerRadius=0 then Rectangle(DrawRect)
    else RoundRect(DrawRect.Left,DrawRect.Top,DrawRect.Right,DrawRect.Bottom,Diameter.X,Diameter.Y);
  end;
  
  inherited;
end;

class function TRectangleObject.Identifier: Integer;
begin
  Result:=otRectangleObject;
end;

//==============================================================================================================================
// TEllipseObject
//==============================================================================================================================
procedure TEllipseObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  DrawRect : TRect;
begin
  DrawRect:=CanvasInfo.CanvasRect1(Position);
  DrawRoundRect(Canvas,CanvasInfo,DrawRect,MaxInt,MaxInt);

  if Assigned(CanvasInfo.ZBuffer) then with CanvasInfo.ZBuffer do
  begin
    if FLineColor=clNone then Pen.Width:=0
    else Pen.Width:=Max(Canvas.Pen.Width,3);
    Pen.Style:=psSolid;
    Pen.Color:=Index;
    if FFillColor=clNone then Brush.Style:=bsClear
    else
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Index;
    end;
    Ellipse(DrawRect);
  end;

  inherited;
end;

class function TEllipseObject.Identifier: Integer;
begin
  Result:=otEllipseObject;
end;

function TEllipseObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)-[opTextYAlign,opMargin]+[opBlockAlignOnly];
end;

initialization
  SetLength(StandardShapeObjectLinks,5);
  StandardShapeObjectLinks[0]:=FloatPoint(0.5,0.5);
  StandardShapeObjectLinks[1]:=FloatPoint(0,0.5);
  StandardShapeObjectLinks[2]:=FloatPoint(1,0.5);
  StandardShapeObjectLinks[3]:=FloatPoint(0.5,0);
  StandardShapeObjectLinks[4]:=FloatPoint(0.5,1);
finalization
  Assert(Length(StandardShapeObjectLinks)=5);
end.

