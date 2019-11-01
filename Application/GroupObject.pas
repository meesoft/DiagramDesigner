
unit GroupObject;

interface

uses Windows, SysUtils, MathUtils, Graphics, DiagramBase, TextObject,
  Math, Types, Streams, ShapeObject, MemStream, Classes;

type
  TPolygonObject = class(TShapeObject)
    protected
      CurrentPoints : array of TPoint;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
      procedure RescaleLinks;
    public
      function ValidProperties: TObjectProperties; override;
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      procedure Rotate(const DAngle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint); override;
      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TGroupObject = class(TBaseObject)
    protected
      FList : TBaseObjectList; // All objects must always be selected
      UseInnerBounds : Boolean;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
      function GroupAnchors: TObjectAnchors;
    public
      constructor Create; override;
      constructor CreateNew(PropertyObject: TBaseObject); override;
      constructor CreateFromSelection(Objects: TDiagramLayer; CreateAnchors: Boolean);
      destructor Destroy; override;
      function CreateCopy: TBaseObject; override;
      function ValidProperties: TObjectProperties; override;
      function TextObject: TTextObject;
      property Group: TBaseObjectList read FList;

      procedure DissolveGroup(Target: TDiagramLayer);
      function CreatePolygon: TPolygonObject;

      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      procedure DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      procedure DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      procedure DrawSelected(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;

      function Move(DX,DY: Integer; Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint; override;
      procedure Scale(const ScaleX, ScaleY: Double; const Center: TPoint); override;
      procedure Rotate(const Angle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint); override;
      procedure UpdatePosition;

      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

resourcestring
  rsPolygonMustHaveAtLeastTwoPoints = 'Polygon must have at least two points';
  rsTooFewLinesToCreatePolygon = 'Too few lines to create polygon';
  rsAllObjectsInGroupMustBeSimpleLines = 'All objects in group must be simple lines';
  rsGroupMustConsistOfConnectedLines = 'Group must consist of connected lines';
  
implementation

uses LineObject;

//==============================================================================================================================
// TGroupObject
//==============================================================================================================================
constructor TGroupObject.Create;
begin
  inherited;
  FList:=TBaseObjectList.Create;
end;

constructor TGroupObject.CreateNew(PropertyObject: TBaseObject);
begin
  Assert(False);
  inherited;
end;

destructor TGroupObject.Destroy;
begin
  FList.Free;
  inherited;
end;

constructor TGroupObject.CreateFromSelection(Objects: TDiagramLayer; CreateAnchors: Boolean);
var
  I : Integer;
begin
  Create;
  Name:=ExtractObjectName(ClassName);
  Group.AddCopy(Objects,True);
  FPosition:=Group.GetBounds;
  // Set default anchors
  if CreateAnchors then
  begin
    for I:=0 to Group.Count-1 do
      if Group.Objects[I].Anchors*[oaLeft,oaRight,oaTop,oaBottom]<>[] then
      begin
        CreateAnchors:=False;
        Break
      end;
    if CreateAnchors then
      for I:=0 to Group.Count-1 do
        if Group.Objects[I].Anchors=[] then Group.Objects[I].Anchors:=[oaHorzScale,oaVertScale];
  end;
end;

function TGroupObject.CreateCopy: TBaseObject;
begin
  Result:=inherited CreateCopy;
  TGroupObject(Result).Group.AddCopy(Group);
  TGroupObject(Result).Group.SelectAll;
end;

procedure TGroupObject.DissolveGroup(Target: TDiagramLayer);
var
  I : Integer;
  Stream : TMemStream;
  TempLayer : TDiagramLayer;
begin
  TempLayer:=TDiagramLayer.Create;
  try
    for I:=0 to Group.Count-1 do
    begin
      TempLayer.Add(Group.Objects[I]);
      Group.Objects[I]:=nil;
    end;
    Stream:=TMemStream.Create;
    TempLayer.SaveSelected(Stream); // Use stream and SaveSelected to keep links
  finally
    TempLayer.Free;
  end;
  Stream.Position:=0;
  Target.LoadSelected(Stream);
  Stream.Free;
end;

function TGroupObject.CreatePolygon: TPolygonObject;

  function PointsEqual(const P1,P2: TPoint): Boolean;
  begin
    Result:=(P1.X=P2.X) and (P1.Y=P2.Y);
  end;

  function FindLine(const Point: TPoint; Exclude: TBaseObject): TBaseObject;
  var
    I : Integer;
  begin
    for I:=0 to Group.Count-1 do
      with Group.Objects[I].Position do
      if (Group.Objects[I]<>Exclude) and (PointsEqual(TopLeft,Point) or PointsEqual(BottomRight,Point)) then
      begin
        Result:=Group.Objects[I];
        Exit;
      end;
    Result:=nil;
  end;

var
  I : Integer;
  Line : TBaseObject;
  Min, Max, P : TPoint;
  NormalizeScale : TFloatPoint;
begin
  if Group.Count<3 then raise Exception.Create(rsTooFewLinesToCreatePolygon);
  Result:=TPolygonObject.CreateNew;
  with Result do
  try
    SetLength(Links,Group.Count);
    Line:=Group.Objects[0];
    Assign(Line);
    if Name=ExtractObjectName(Line.ClassName) then Name:=ExtractObjectName(ClassName); 
    P:=Line.Position.TopLeft;
    Max:=Point(Low(Integer),Low(Integer));
    Min:=Point(High(Integer),High(Integer));
    for I:=0 to Group.Count-1 do
    begin
      if not (Line is TStraightLineObject) then raise Exception.Create(rsAllObjectsInGroupMustBeSimpleLines);
      with P do
      begin
        Links[I]:=FloatPoint(X,Y);
        if X>Max.X then Max.X:=X;
        if X<Min.X then Min.X:=X;
        if Y>Max.Y then Max.Y:=Y;
        if Y<Min.Y then Min.Y:=Y;
      end;
      Line:=FindLine(P,Line);
      if Line=nil then raise Exception.Create(rsGroupMustConsistOfConnectedLines);
      if PointsEqual(P,Line.Position.TopLeft) then P:=Line.Position.BottomRight
      else P:=Line.Position.TopLeft;
    end;
    NormalizeScale:=FloatPoint(1/(Max.X-Min.X),1/(Max.Y-Min.Y));
    for I:=0 to High(Links) do with Links[I] do
    begin
      X:=(X-Min.X)*NormalizeScale.X;
      Y:=(Y-Min.Y)*NormalizeScale.Y;
    end;
    FPosition.TopLeft:=Min;
    FPosition.BottomRight:=Max;
  except
    Free;
    raise;
  end;
end;

procedure TGroupObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  I : Integer;
  LocalCanvasInfo : TCanvasInfo;
begin
  LocalCanvasInfo:=CanvasInfo;
  if LocalCanvasInfo.DrawMode in [dmEditing,dmEditDrag] then LocalCanvasInfo.DrawMode:=dmPreview;
  for I:=0 to Group.Count-1 do Group.Objects[I].Draw(Canvas,LocalCanvasInfo,Index);
  inherited;
end;

procedure TGroupObject.DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
begin
  Group.DrawAntialiasing(Canvas,CanvasInfo);
  inherited;
end;

procedure TGroupObject.DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
begin
  Group.DrawShadow(Canvas,CanvasInfo);
  inherited;
end;

function TGroupObject.GroupAnchors: TObjectAnchors;
var
  I : Integer;
begin
  Result:=[];
  for I:=0 to Group.Count-1 do
    Result:=Result+Group.Objects[I].Anchors;
end;

procedure TGroupObject.DrawSelected(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);

  procedure DrawFocusRect(X,Y,Marker: Integer);
  var
    BoxRect : TRect;
  begin
    BoxRect:=Rect(X-MarkerBoxSize,Y-MarkerBoxSize,X+(MarkerBoxSize+1),Y+(MarkerBoxSize+1));
    Canvas.FrameRect(BoxRect);
    with CanvasInfo, ZBuffer do
    begin
      Brush.Color:=Index or (Marker shl 16);
      FillRect(BoxRect);
    end;
  end;

//  1 5 2
//  7 0 8
//  3 6 4
var
  CanvasRect : TRect;
  Anchors : TObjectAnchors;
begin
  if Selected then
  begin
    Anchors:=GroupAnchors;
    PrepareSelectMarkerCanvas(Canvas);
    if Anchors=[] then Canvas.FrameRect(CanvasInfo.CanvasRect1(Position))
    else
    begin
      CanvasRect:=CanvasInfo.CanvasRect(Position);
      with CanvasRect do
      begin
        if [oaTop,oaVertScale]*Anchors<>[] then
          DrawFocusRect((Left+Right) div 2,Top,5);
        if [oaBottom,oaVertScale]*Anchors<>[] then
          DrawFocusRect((Left+Right) div 2,Bottom,6);
        if [oaLeft,oaHorzScale]*Anchors<>[] then
          DrawFocusRect(Left,(Top+Bottom) div 2,7);
        if [oaRight,oaHorzScale]*Anchors<>[] then
          DrawFocusRect(Right,(Top+Bottom) div 2,8);
        if ([oaLeft,oaTop]*Anchors=[oaLeft,oaTop]) or ([oaHorzScale,oaVertScale]*Anchors=[oaHorzScale,oaVertScale]) then
          DrawFocusRect(Left,Top,1);
        if ([oaRight,oaTop]*Anchors=[oaRight,oaTop]) or ([oaHorzScale,oaVertScale]*Anchors=[oaHorzScale,oaVertScale]) then
          DrawFocusRect(Right,Top,2);
        if ([oaLeft,oaBottom]*Anchors=[oaLeft,oaBottom]) or ([oaHorzScale,oaVertScale]*Anchors=[oaHorzScale,oaVertScale]) then
          DrawFocusRect(Left,Bottom,3);
        if ([oaRight,oaBottom]*Anchors=[oaRight,oaBottom]) or ([oaHorzScale,oaVertScale]*Anchors=[oaHorzScale,oaVertScale]) then
          DrawFocusRect(Right,Bottom,4);
      end;
    end;
  end;
end;

function TGroupObject.Move(DX,DY: Integer; Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint;
//  1 5 2
//  7 0 8
//  3 6 4
var
  I, S : Integer;
  Obj : TBaseObject;
  Anchors : TObjectAnchors;
begin
  if Handle>0 then
  begin
    Anchors:=GroupAnchors;
    if ((Handle in [1,7,3]) and ([oaLeft,oaHorzScale]*Anchors<>[])) or
       ((Handle in [2,8,4]) and ([oaRight,oaHorzScale]*Anchors<>[])) or
       ((Handle in [1,5,2]) and ([oaTop,oaVertScale]*Anchors<>[])) or
       ((Handle in [3,6,4]) and ([oaBottom,oaVertScale]*Anchors<>[])) then
    begin
      Result:=inherited Move(DX,DY,Handle,Grid,[]);
      if (Result.X<>0) or (Result.Y<>0) then
        for I:=0 to Group.Count-1 do
        begin
          Obj:=Group.Objects[I];
          if Handle in [1,7,3] then // Left edge
          begin
            if oaHorzScale in Obj.Anchors then
            begin
              S:=Position.Right-Position.Left+Result.X;
              if S<>0 then Obj.Scale((Position.Right-Position.Left)/S,1,Position.BottomRight)
            end
            else if (oaLeft in Obj.Anchors) and (oaRight in Obj.Anchors) then
              Obj.Move(Result.X,0,7,NoGrid,[])
            else if (oaLeft in Obj.Anchors) then
              Obj.Move(Result.X,0,0,NoGrid,[]);
          end;
          if Handle in [2,8,4] then // Right edge
          begin
            if oaHorzScale in Obj.Anchors then
            begin
              S:=Position.Right-Position.Left-Result.X;
              if S<>0 then Obj.Scale((Position.Right-Position.Left)/S,1,Position.TopLeft)
            end
            else if (oaLeft in Obj.Anchors) and (oaRight in Obj.Anchors) then
              Obj.Move(Result.X,0,8,NoGrid,[])
            else if (oaRight in Obj.Anchors) then
              Obj.Move(Result.X,0,0,NoGrid,[]);
          end;
          if Handle in [1,5,2] then // Top edge
          begin
            if oaVertScale in Obj.Anchors then
            begin
              S:=Position.Bottom-Position.Top+Result.Y;
              if S<>0 then Obj.Scale(1,(Position.Bottom-Position.Top)/S,Position.BottomRight)
            end
            else if (oaTop in Obj.Anchors) and (oaBottom in Obj.Anchors) then
              Obj.Move(0,Result.Y,5,NoGrid,[])
            else if (oaTop in Obj.Anchors) then
              Obj.Move(0,Result.Y,0,NoGrid,[]);
          end;
          if Handle in [3,6,4] then // Bottom edge
          begin
            if oaVertScale in Obj.Anchors then
            begin
              S:=Position.Bottom-Position.Top-Result.Y;
              if S<>0 then Obj.Scale(1,(Position.Bottom-Position.Top)/S,Position.TopLeft)
            end
            else if (oaTop in Obj.Anchors) and (oaBottom in Obj.Anchors) then
              Obj.Move(0,Result.Y,6,NoGrid,[])
            else if (oaBottom in Obj.Anchors) then
              Obj.Move(0,Result.Y,0,NoGrid,[]);
          end;
        end;
    end
    else Result:=Origo
  end
  else
  begin
    Result:=inherited Move(DX,DY,Handle,Grid,[]);
    Group.MoveSelected(Result.X,Result.Y,-1,NoGrid,[]);
  end;
end;

procedure TGroupObject.Scale(const ScaleX, ScaleY: Double; const Center: TPoint);
var
  I : Integer;
begin
  inherited;
  for I:=0 to Group.Count-1 do
    Group.Objects[I].Scale(ScaleX,ScaleY,Center);
end;

procedure TGroupObject.Rotate(const Angle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint);
var
  P : TFloatPoint;
  I : Integer;
begin
  if Angle<>0 then
    for I:=0 to High(Links) do
      with Links[I] do
      begin
        X:=X*Width+Position.Left;
        Y:=Y*Height+Position.Top;
      end;

  Group.RotateSelected(Angle,FlipLR,FlipUD,Center);
  inherited;
  UpdatePosition;

  if Angle<>0 then
  begin
    for I:=0 to High(Links) do
    begin
      P:=RotatePoint(FloatPoint(Links[I].X,Links[I].Y),FloatPoint(Center),Angle);
      Links[I].X:=(P.X-Position.Left)/Width;
      Links[I].Y:=(P.Y-Position.Top)/Height;
    end;
    NotifyMovement;
  end;
end;

procedure TGroupObject.UpdatePosition;
begin
  if UseInnerBounds then FPosition:=Group.GetInnerBounds
  else FPosition:=Group.GetBounds;
end;

class function TGroupObject.Identifier: Integer;
begin
  Result:=otGroupObject;
end;

function TGroupObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opCustomLinks,opBoundsOptions];
end;

function TGroupObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opBoundsOptions : Result:=Byte(UseInnerBounds);
    opText          : if TextObject<>nil then Result:=TextObject.Properties[opText]
                      else Result:=inherited GetProperty(Index);
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TGroupObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opBoundsOptions : begin
                        UseInnerBounds:=Value<>0;
                        UpdatePosition;
                        NotifyMovement;
                      end;
    else inherited SetProperty(Index,Value);
  end;
end;

function TGroupObject.TextObject: TTextObject;
var
  I : Integer;
begin
  for I:=Group.Count-1 downto 0 do
    if Group.Objects[I].ClassType=TTextObject then
    begin
      Result:=TTextObject(Group.Objects[I]);
      Exit;
    end;
  for I:=Group.Count-1 downto 0 do
    if Group.Objects[I] is TTextObject then
    begin
      Result:=TTextObject(Group.Objects[I]);
      Exit;
    end;
  Result:=nil;
end;

procedure TGroupObject.SaveToStream(Stream: TBaseStream);
var
  Count : Integer;
begin
  inherited;
  Group.SaveToStream(Stream);
  Count:=Length(Links);
  Stream.Write(Count,2);
  Stream.Write(Links[0],Count*SizeOf(TFloatPoint));
end;

procedure TGroupObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Count : Word;
  InnerBounds : TRect;
begin
  inherited;   
  Group.LoadFromStream(Stream,FileVersion);
  Group.SelectAll;
  if FileVersion>=2 then
  begin
    Stream.Read(Count,2);
    SetLength(Links,Count);
    Stream.Read(Links[0],Count*SizeOf(TFloatPoint));

    InnerBounds:=Group.GetInnerBounds;
    UseInnerBounds:=CompareMem(@FPosition,@InnerBounds,SizeOf(InnerBounds));
  end;
end;

//==============================================================================================================================
// TPolygonObject
//==============================================================================================================================
procedure TPolygonObject.Rotate(const DAngle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint);
var
  P : TFloatPoint;
  I : Integer;
begin
  if DAngle<>0 then
  begin
    Angle:=Angle+DAngle;
    // Rotate points
    for I:=0 to High(Links) do
    begin
      P.X:=(Links[I].X-0.5)*Width;
      P.Y:=(Links[I].Y-0.5)*Height;
      P:=RotatePoint(P,FloatOrigo,DAngle);
      Links[I].X:=P.X/Width+0.5;
      Links[I].Y:=P.Y/Height+0.5;
    end;
    // Rotate center
    with Position do P:=FloatPoint((Right+Left)/2,(Bottom+Top)/2);
    P:=VectorSubtract(RotatePoint(P,FloatPoint(Center),DAngle),P);
    OffsetRect(FPosition,Round(P.X),Round(P.Y));
    // Rescale
    RescaleLinks;
  end;
  inherited Rotate(0,FlipLR,FlipUD,Center);
end;

class function TPolygonObject.Identifier: Integer;
begin
  Result:=otPolygonObject;
end;

procedure TPolygonObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  DrawRect : TRect;
  I, ObjWidth, ObjHeight : Integer;
begin
  DrawRect:=CanvasInfo.CanvasRect(Position);
  SetLength(CurrentPoints,Length(Links));
  ObjWidth:=DrawRect.Right-DrawRect.Left;
  ObjHeight:=DrawRect.Bottom-DrawRect.Top;
  for I:=0 to High(Links) do with Links[I] do
    CurrentPoints[I]:=Point(DrawRect.Left+Round(X*ObjWidth),DrawRect.Top+Round(Y*ObjHeight));
  DrawPolygon(Canvas,CanvasInfo,CurrentPoints,Links);
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
    Polygon(CurrentPoints);
  end;
  inherited;
end;

procedure TPolygonObject.SaveToStream(Stream: TBaseStream);
var
  Count : Integer;
begin
  inherited;
  Count:=Length(Links);
  Stream.Write(Count,2);
  Stream.Write(Links[0],Count*SizeOf(TFloatPoint));
end;

procedure TPolygonObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Count : Word;
begin
  inherited;
  Stream.Read(Count,2);
  SetLength(Links,Count);
  Stream.Read(Links[0],Count*SizeOf(TFloatPoint));
end;

function TPolygonObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opCustomLinks]; 
end;

procedure TPolygonObject.RescaleLinks;
var
  I : Integer;
  Min, Max : Double;
begin
  Min:=Infinity; Max:=-Infinity;
  for I:=0 to High(Links) do
  begin
    if Links[I].X<Min then Min:=Links[I].X;
    if Links[I].X>Max then Max:=Links[I].X;
  end;
  if (Min>0) or (Max<1) then
  begin
    // Update position
    I:=Width;
    Inc(FPosition.Left,Round(Min*I));
    Dec(FPosition.Right,Round((1-Max)*I));
    // Streth to fill [0;1]
    if Min<>Max then Max:=1/(Max-Min);
    for I:=0 to High(Links) do Links[I].X:=(Links[I].X-Min)*Max;
  end;

  Min:=Infinity; Max:=-Infinity;
  for I:=0 to High(Links) do
  begin
    if Links[I].Y<Min then Min:=Links[I].Y;
    if Links[I].Y>Max then Max:=Links[I].Y;
  end;
  if (Min>0) or (Max<1) then
  begin
    // Update position
    I:=Height;
    Inc(FPosition.Top,Round(Min*I));
    Dec(FPosition.Bottom,Round((1-Max)*I));
    // Streth to fill [0;1]
    if Min<>Max then Max:=1/(Max-Min);
    for I:=0 to High(Links) do Links[I].Y:=(Links[I].Y-Min)*Max;
  end;
end;

procedure TPolygonObject.SetProperty(Index: TObjectProperty; Value: Integer);
var
  I : Integer;
begin
  case Index of
    opCustomLinks : if Length(PFloatPointArray(Value)^)<2 then raise Exception.Create(rsPolygonMustHaveAtLeastTwoPoints)
                    else
                    begin
                      I:=High(Links);
                      Links:=Copy(PFloatPointArray(Value)^);
                      for I:=I downto High(Links)+1 do NotifyLinkPointDeleted(I);
                      RescaleLinks;
                    end;
    else inherited SetProperty(Index,Value);
  end;
end;

end.

