unit FlowchartObject;

interface

uses Windows, SysUtils, Graphics, DiagramBase, Types, Streams, Math, TextObject, ShapeObject;

resourcestring
  rsSideBars = 'Side bars';
  rsRounded1 = 'Rounded 1';
  rsRounded2 = 'Rounded 2';
  rsRounded3 = 'Rounded 3';
  rsSlantRight = 'Slant right';
  rsSlantLeft = 'Slant left';
  rsOddRounded1 = 'Odd rounded 1';
  rsOddRounded2 = 'Odd rounded 2';

const
  foSideBars       = $11;
  foRounded1       = $21;
  foRounded2       = $22;
  foRounded3       = $23;
  foSlantRight     = $31;
  foSlantLeft      = $32;
  foOddRounded1    = $41;
  foOddRounded2    = $51;

  FlowchartObjectLayout : array[0..7] of Integer =
    (foSideBars,
     foRounded1,
     foRounded2,
     foRounded3,
     foSlantRight,
     foSlantLeft,
     foOddRounded1,
     foOddRounded2);

  FlowchartObjectLayoutNames : array[0..High(FlowchartObjectLayout)] of Pointer =
    (@rsSideBars,
     @rsRounded1,
     @rsRounded2,
     @rsRounded3,
     @rsSlantRight,
     @rsSlantLeft,
     @rsOddRounded1,
     @rsOddRounded2);

type
  TFlowchartObject = class(TShapeObject)
    protected                     
      FType : Integer;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
    public
      constructor CreateByKind(Kind: Integer);
      function ValidProperties: TObjectProperties; override;
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      function GetBounds: TRect; override;
      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

implementation

//==============================================================================================================================
// TFlowchartObject
//==============================================================================================================================
constructor TFlowchartObject.CreateByKind(Kind: Integer);
var
  I : Integer;
begin
  CreateNew;
  FFillColor:=clWhite;
  FLineWidth:=DefaultLineWidth;
  FMargin:=DesignerDPpoint*2;
  for I:=0 to High(FlowchartObjectLayout) do if FlowchartObjectLayout[I]=Kind then
    Name:=LoadResString(FlowchartObjectLayoutNames[I]);
  FType:=Kind;
end;

procedure TFlowchartObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  DrawRect : TRect;
  I, X, Y : Integer;
  Rad : Double;
  Points : array[0..31] of TPoint;
begin
  CanvasTextMargin:=0;
  DrawRect:=CanvasInfo.CanvasRect1(Position);
  with Canvas do
  begin
    case FType of
      foSideBars :
        with DrawRect do
        begin
          DrawRoundRect(Canvas,CanvasInfo,DrawRect,0,0);
          X:=(Right-Left+4) div 8;
          Y:=Bottom-Pen.Width div 2;
          MoveTo(Left+X,Top+1);
          LineTo(Left+X,Y);
          MoveTo(Right-X,Top+1);
          LineTo(Right-X,Y);
          Inc(CanvasTextMargin,X*2);
        end;
      foRounded1 :
        with DrawRect do
        begin
          I:=Bottom-Top;
          DrawRoundRect(Canvas,CanvasInfo,DrawRect,I,I);
        end;
      foRounded2 :
        with DrawRect do
        begin
          I:=(Bottom-Top+1) div 2;
          DrawRoundRect(Canvas,CanvasInfo,DrawRect,I,I);
        end;
      foRounded3 :
        with DrawRect do
        begin
          I:=(Bottom-Top+2) div 4;
          DrawRoundRect(Canvas,CanvasInfo,DrawRect,I,I);
        end;
      foSlantRight :
        with DrawRect do
        begin
          I:=(Bottom-Top+4) div 8;
          Points[0]:=Point(Left+I,Top);
          Points[1]:=Point(Right+I,Top);
          Points[2]:=Point(Right-I,Bottom);
          Points[3]:=Point(Left-I,Bottom);
          DrawPolygon(Canvas,CanvasInfo,Slice(Points,4));
          CanvasTextMargin:=I*2;
          if Pen.Style=psSolid then Inc(CanvasTextMargin,Pen.Width*2);
        end;
      foSlantLeft :
        with DrawRect do
        begin
          I:=(Bottom-Top+4) div 8;
          Points[0]:=Point(Left-I,Top);
          Points[1]:=Point(Right-I,Top);
          Points[2]:=Point(Right+I,Bottom);
          Points[3]:=Point(Left+I,Bottom);
          DrawPolygon(Canvas,CanvasInfo,Slice(Points,4));
          CanvasTextMargin:=I*2;
          if Pen.Style=psSolid then Inc(CanvasTextMargin,Pen.Width*2);
        end;
      foOddRounded1 :
        with DrawRect do
        begin
          Rad:=(Bottom-Top)/2;
          for I:=0 to 7 do             
          begin
            X:=Round(Rad*(1-Sin(I*Pi/15)));
            Y:=Round(Rad*(1-Cos(I*Pi/15)));
            Points[I   ]:=Point(Left+X,Top+Y);
            Points[15-I]:=Point(Left+X,Bottom-Y);
            X:=X div 2;
            Points[16+I]:=Point(Right-X,Bottom-Y);
            Points[31-I]:=Point(Right-X,Top+Y);
          end;
          DrawPolygon(Canvas,CanvasInfo,Points);
          CanvasTextMargin:=Round(Rad);
          if Pen.Style=psSolid then Inc(CanvasTextMargin,Pen.Width*2);
        end;
      foOddRounded2 :
        with DrawRect do
        begin
          Rad:=(Bottom-Top)/2;
          for I:=0 to 7 do
          begin
            X:=Round(Rad*(1-Sin(I*Pi/15)));
            Y:=Round(Rad*(1-Cos(I*Pi/15)));
            Points[I   ]:=Point(Left+X,Top+Y);
            Points[15-I]:=Point(Left+X,Bottom-Y);
            Points[16+I]:=Point(Right+X,Bottom-Y);
            Points[31-I]:=Point(Right+X,Top+Y);
          end;
          DrawPolygon(Canvas,CanvasInfo,Points);
          CanvasTextMargin:=Round(Rad);
          if Pen.Style=psSolid then Inc(CanvasTextMargin,Pen.Width*2);
        end;
      else
      begin
        Assert(False,'Unknow type');
        DrawRoundRect(Canvas,CanvasInfo,DrawRect,0,0);
      end;
    end;
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
    Rectangle(DrawRect);
  end;
  inherited;
end;

function TFlowchartObject.GetBounds: TRect;
var
  HalfLineWidth, I : Integer;
begin
  if FLineColor=clNone then HalfLineWidth:=0
  else HalfLineWidth:=(FLineWidth+1) div 2;
  Result:=FPosition;
  case FType of
    foSlantRight, foSlantLeft :
      with Result do
      begin
        I:=(Bottom-Top+4) div 8;
        Dec(Left,I);
        Inc(Right,I);
      end;
    foOddRounded2 :
      with Result do Inc(Right,(Bottom-Top+1) div 2);
  end;
  Dec(Result.Left,HalfLineWidth);
  Dec(Result.Top,HalfLineWidth);
  Inc(Result.Right,HalfLineWidth);
  Inc(Result.Bottom,HalfLineWidth);
end;

class function TFlowchartObject.Identifier: Integer;
begin
  Result:=otFlowchartObject;
end;

function TFlowchartObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opRectangleType,opBlockAlignOnly];
end;

function TFlowchartObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opRectangleType : Result:=FType;
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TFlowchartObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opRectangleType : FType:=Value;
    else inherited SetProperty(Index,Value);
  end;
end;

procedure TFlowchartObject.SaveToStream(Stream: TBaseStream);
begin
  inherited;
  Stream.Write(FType,4);
end;

procedure TFlowchartObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
begin
  inherited;
  Stream.Read(FType,4);
end;

end.

