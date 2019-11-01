unit PictureObject;

interface

uses Windows, SysUtils, MathUtils, Graphics, DiagramBase,
  Math, Types, Streams, LinarBitmap, Classes;

type
  TPictureObject = class(TBaseObject)
    protected
      HWR : Double;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
    public
      function Move(DX,DY: Integer; Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TBitmapObject = class(TPictureObject)
    protected
      Image, AlphaChannel : TLinearBitmap;
      AlphaBitmap : TBitmap; // Bitmap with premultiplied alpha channel
      FAlphaValue : Integer;
      FHalftoneStretch : Boolean;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
    public
      constructor Create; override;
      constructor CreateNew(NewImage: TLinearBitmap; NewAlphaChannel: TLinearBitmap=nil); reintroduce;
      destructor Destroy; override;
      function ValidProperties: TObjectProperties; override;
      procedure Rotate(const Angle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint); override;
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      procedure DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      procedure DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TMetafileObject = class(TPictureObject)
    protected
      Metafile : TMetafile;
      MemStream : TMemoryStream;
      Angle, OldAngle : Single;
      RotateHandlePos : TFloatPoint;
      function GetProperty(Index: TObjectProperty): Integer; override;
      procedure SetProperty(Index: TObjectProperty; Value: Integer); override;
      procedure CompleteRotation;
    public
      constructor Create; override;
      constructor CreateNew(NewImage: TMetafile); reintroduce;
      destructor Destroy; override;
      function ValidProperties: TObjectProperties; override;
      function Move(DX,DY: Integer; Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint; override;
      procedure Rotate(const DAngle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint); override;
      procedure NotifyMovementDone; override;

      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      procedure DrawSelected(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;

      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

  TInheritedLayerObject = class(TPictureObject)
    protected
      FRelativePageIndex, FLayerIndex : Integer;
      Drawing : Boolean;
    public
      constructor CreateNew(RelativePageIndex,LayerIndex: Integer; Page: TDiagramPage=nil); reintroduce;
      function CreateCopy: TBaseObject; override;
      function ValidProperties: TObjectProperties; override;
      procedure Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer); override;
      procedure DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      procedure DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo); override;
      class function Identifier: Integer; override;
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer); override;
    end;

implementation

uses BitmapRotate, MemUtils, Complex, FastBitmap, BitmapGammaInterpolation;

resourcestring
  rsPagePSLayerD = 'Page p%s layer %d';

//==============================================================================================================================
// TPictureObject
//==============================================================================================================================
function TPictureObject.Move(DX,DY: Integer; Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint;
begin
  //  1 5 2
  //  7 0 8
  //  3 6 4
  if (Handle in [1..4]) and not (ssCtrl in Shift) then with FPosition do
  begin
    if (Left<>Right) and (HWR=0) then HWR:=Height/Width;
    case Handle of
      1 : begin
            Result:=inherited Move(DX,DY,7,Grid,[]);
            if HWR<>0 then FPosition.Top:=Round(FPosition.Bottom-Width*HWR);
          end;
      2 : begin
            Result:=inherited Move(DX,DY,8,Grid,[]);
            if HWR<>0 then FPosition.Top:=Round(FPosition.Bottom-Width*HWR);
          end;
      3 : begin
            Result:=inherited Move(DX,DY,7,Grid,[]);
            if HWR<>0 then FPosition.Bottom:=Round(FPosition.Top+Width*HWR);
          end;
      4 : begin
            Result:=inherited Move(DX,DY,8,Grid,[]);
            if HWR<>0 then FPosition.Bottom:=Round(FPosition.Top+Width*HWR);
          end;
    end;
  end
  else
  begin
    Result:=inherited Move(DX,DY,Handle,Grid,Shift);
    HWR:=0;
  end;
end;

function TPictureObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opPosition    : Result:=Integer(@FPosition);
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TPictureObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opPosition    : begin
                      Position:=PRect(Value)^;
                      HWR:=0;
                      NotifyMovement;
                    end;
    else inherited SetProperty(Index,Value);
  end;
end;

procedure TPictureObject.SaveToStream(Stream: TBaseStream);
var
  Count : Integer;
begin
  inherited;
  Count:=Length(Links);
  Stream.Write(Count,2);
  Stream.Write(Links[0],Count*SizeOf(TFloatPoint));
end;

procedure TPictureObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Count : Word;
begin
  inherited;
  if FileVersion>=2 then
  begin
    Stream.Read(Count,2);
    SetLength(Links,Count);
    Stream.Read(Links[0],Integer(Count)*SizeOf(TFloatPoint));
  end;
end;

//==============================================================================================================================
// TBitmapObject
//==============================================================================================================================
constructor TBitmapObject.Create;
begin
  inherited;
  Image:=TLinearBitmap.Create;
  FAlphaValue:=255;
end;

constructor TBitmapObject.CreateNew(NewImage,NewAlphaChannel: TLinearBitmap);
begin
  {SetLength(Links,5);
  Links[0]:=FloatPoint(0.5,0.5);
  Links[1]:=FloatPoint(0,0.5);
  Links[2]:=FloatPoint(1,0.5);
  Links[3]:=FloatPoint(0.5,0);
  Links[4]:=FloatPoint(0.5,1);}
  Create;
  Name:=ExtractObjectName(ClassName);
  FHalftoneStretch:=True;
  Image.Assign(NewImage);
  if Image.PixelFormat=pf16bit then Image.PixelFormat:=pf8bit;
  if NewAlphaChannel<>nil then
  begin
    AlphaChannel:=TLinearBitmap.Create(NewAlphaChannel);
    AlphaChannel.PixelFormat:=pf8bit;
    AlphaChannel.Palette^:=GrayPal;
  end;
end;

destructor TBitmapObject.Destroy;
begin
  Image.Free;
  AlphaChannel.Free;
  AlphaBitmap.Free;
  inherited;
end;

function TBitmapObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opPosition,opBitmap,opAlphaBitmap,opAlphaValue,opHalftoneStretch,opCustomLinks];
end;

function TBitmapObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opBitmap          : Result:=Integer(Image);
    opAlphaBitmap     : Result:=Integer(AlphaChannel);
    opAlphaValue      : Result:=FAlphaValue;
    opHalftoneStretch : Result:=Integer(FHalftoneStretch);
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TBitmapObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opBitmap          : begin
                          FreeAndNil(AlphaBitmap);
                          Image.Assign(TObject(Value));
                          HWR:=0;
                        end;
    opAlphaBitmap     : begin
                          FreeAndNil(AlphaBitmap);
                          FreeAndNil(AlphaChannel);
                          if Value<>0 then
                          begin
                            AlphaChannel:=TLinearBitmap.Create(TObject(Value));
                            AlphaChannel.PixelFormat:=pf8bit;
                            AlphaChannel.Palette^:=GrayPal;
                          end
                        end;
    opAlphaValue      : begin
                          FAlphaValue:=Value;
                          FreeAndNil(AlphaBitmap);
                        end;
    opHalftoneStretch : FHalftoneStretch:=Value<>0;
    opFillColor       : begin
                         Image.New(1,1,pf24bit);
                         Image.ClearColor(Value);
                         FHalftoneStretch:=False;
                        end;
    else inherited SetProperty(Index,Value);
  end;
end;

procedure TBitmapObject.Rotate(const Angle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint);
var
  OrgSize : TPoint;
  P : TFloatPoint;
  I : Integer;
begin
  FreeAndNil(AlphaBitmap);

  OrgSize:=Point(Image.Width,Image.Height);
  BitmapRotate.Rotate(Image,-Angle,clWhite,DefaultMonitorGamma);
  if (AlphaChannel=nil) and InRange(FloatMod(Abs(Angle/Pi*180),90),1e-6,90-1e-6) then
  begin
    AlphaChannel:=TLinearBitmap.Create(OrgSize.X,OrgSize.Y,pf8bit);
    AlphaChannel.Palette^:=GrayPal;
    AlphaChannel.Clear(255);
  end;
  if AlphaChannel<>nil then BitmapRotate.Rotate(AlphaChannel,-Angle,0);
  OrgSize:=RoundPoint(Width*Image.Width/OrgSize.X,Height*Image.Height/OrgSize.Y);
  if opCustomLinks in ValidProperties then // Rotate link points
    for I:=0 to High(Links) do
    begin
      P.X:=(Links[I].X-0.5)*Width;
      P.Y:=(Links[I].Y-0.5)*Height;
      P:=RotatePoint(P,FloatOrigo,Angle);
      Links[I].X:=P.X/OrgSize.X+0.5;
      Links[I].Y:=P.Y/OrgSize.Y+0.5;
    end;

  if FlipLR then
  begin
    BitmapRotate.Mirror(Image);
    if AlphaChannel<>nil then BitmapRotate.Mirror(AlphaChannel);
  end;
  if FlipUD then
  begin
    BitmapRotate.Flip(Image);
    if AlphaChannel<>nil then BitmapRotate.Flip(AlphaChannel);
  end;

  inherited;

  with CenterPoint(Position) do Position:=Bounds(X-OrgSize.X div 2,Y-OrgSize.Y div 2,OrgSize.X,OrgSize.Y);
  NotifyMovement;
end;

procedure TBitmapObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  CanvasRect : TRect;
begin
  CanvasRect:=CanvasInfo.CanvasRect1(Position);
  if AlphaChannel=nil then
  begin
    if FAlphaValue=255 then Image.PaintToCanvas(Canvas,CanvasRect,FHalftoneStretch)
    else
    begin
      if AlphaBitmap=nil then
      begin
        AlphaBitmap:=TBitmap.Create;
        Image.AssignTo(AlphaBitmap);
      end;
      AlphaBlendDraw(AlphaBitmap,Canvas,CanvasRect,FAlphaValue);
    end;
  end
  else
  begin
    if AlphaBitmap=nil then AlphaBitmap:=CreatePremultipliedAlphaBitmap(Image,AlphaChannel);
    AlphaBlendDrawPremultipliedAlpha(AlphaBitmap,Canvas,CanvasRect,FAlphaValue);
  end;
  if Assigned(CanvasInfo.ZBuffer) then with CanvasInfo.ZBuffer do
  begin
    Brush.Style:=bsSolid;
    Brush.Color:=Index;
    FillRect(CanvasRect);
  end;
  inherited;
end;

procedure TBitmapObject.DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
begin
  inherited;
  if FAlphaValue<255 then Draw(Canvas,CanvasInfo,0);
end;

procedure TBitmapObject.DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
var
  CanvasRect : TRect;
  Shadow : TPoint;
begin
  if (AlphaChannel=nil) and (FAlphaValue=255) then
  begin
    Shadow:=RoundPoint(ShadowOffset*CanvasInfo.Scale.X,ShadowOffset*CanvasInfo.Scale.Y);
    CanvasRect:=CanvasInfo.CanvasRect1(Position);
    with Canvas do
    begin
      Pen.Style:=psClear;
      Brush.Style:=bsSolid;
      Brush.Color:=ShadowColor;
      FillRect(Rect(CanvasRect.Right-Shadow.X,CanvasRect.Top,CanvasRect.Right,CanvasRect.Bottom)); // Right strip
      FillRect(Rect(CanvasRect.Left,CanvasRect.Bottom-Shadow.Y,CanvasRect.Right-Shadow.X,CanvasRect.Bottom)); // Bottom strip
    end;
  end;
end;

class function TBitmapObject.Identifier: Integer;
begin
  Result:=otBitmapObject;
end;

procedure TBitmapObject.SaveToStream(Stream: TBaseStream);
var
  Format : Byte;
  I : Integer;
begin
  inherited;
  Stream.Write(FHalftoneStretch,1);
  Format:=0;
  if AlphaChannel<>nil then Format:=1;
  Stream.Write(Format,1);
  I:=Image.Width; Stream.Write(I,4);
  I:=Image.Height; Stream.Write(I,4);
  case Image.PixelFormat of
    pf8bit  : begin
                I:=8; Stream.Write(I,1);
                Stream.Write(Image.Palette^,SizeOf(TPalette));
              end;
    pf24bit : begin
                I:=24; Stream.Write(I,1);
              end;
  else
    raise Exception.Create(rsInvalidPixelFormat);
  end;
  Stream.Write(Image.Map^,Image.Size);
  if Format=1 then Stream.Write(AlphaChannel.Map^,AlphaChannel.Size);
  Stream.Write(FAlphaValue,1);
end;

procedure TBitmapObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Format, BitsPerPixel : Byte;
  W, H : Integer;
begin
  inherited;
  Stream.Read(FHalftoneStretch,1);
  Stream.Read(Format,1);
  Stream.Read(W,4);
  Stream.Read(H,4);
  Stream.Read(BitsPerPixel,1);
  case BitsPerPixel of
    8  : begin
           Image.New(W,H,pf8bit);
           Stream.Read(Image.Palette^,SizeOf(TPalette));
         end;
    24 : begin
           Image.New(W,H,pf24bit);
         end;
  else   begin // Try to recover a file saved with an invalid image
           Image.New(1,1,pf24bit);
           Exit;
           //raise Exception.Create(rsInvalidDiagram);
         end;
  end;
  Stream.Read(Image.Map^,Image.Size);
  if Format=1 then
  begin
    AlphaChannel.Free;
    AlphaChannel:=TLinearBitmap.Create(W,H,pf8bit);
    Stream.Read(AlphaChannel.Map^,AlphaChannel.Size);
    AlphaChannel.Palette^:=GrayPal;
  end;
  if FileVersion>=17 then Stream.Read(FAlphaValue,1);
  FreeAndNil(AlphaBitmap);
end;

//==============================================================================================================================
// TMetafileObject
//==============================================================================================================================
constructor TMetafileObject.Create;
begin
  inherited;
  RotateHandlePos.X:=Nan;
  OldAngle:=Nan;
  Metafile:=TMetafile.Create;
end;

constructor TMetafileObject.CreateNew(NewImage: TMetafile);
begin
  Create;
  Name:=ExtractObjectName(ClassName);
  Metafile.Assign(NewImage);
end;

destructor TMetafileObject.Destroy;
begin
  Metafile.Free;
  MemStream.Free;
  inherited;
end;

function TMetafileObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opPosition,opMetafile,opCustomLinks,opAngle];
end;

function TMetafileObject.GetProperty(Index: TObjectProperty): Integer;
begin
  case Index of
    opMetafile        : Result:=Integer(Metafile);
    opAngle           : Result:=Integer(@Angle);
    else Result:=inherited GetProperty(Index);
  end;
end;

procedure TMetafileObject.SetProperty(Index: TObjectProperty; Value: Integer);
begin
  case Index of
    opMetafile        : begin
                          Metafile.Assign(TPersistent(Value));
                          HWR:=0;
                        end;
    opAngle           : begin
                          Angle:=PSingle(Value)^;
                          RotateHandlePos.X:=Nan;
                        end;
    opPosition        : begin
                          inherited SetProperty(Index,Value);
                          RotateHandlePos.X:=Nan;
                        end;
    else inherited SetProperty(Index,Value);
  end;
end;

function TMetafileObject.Move(DX, DY, Handle: Integer; const Grid: TPoint; Shift: TShiftState): TPoint;
begin
  if Handle=9 then // Rotate
  begin
    if IsNan(OldAngle) then OldAngle:=Angle;
    Result:=Point(DX,DY);
    RotateHandlePos.X:=RotateHandlePos.X+DX;
    RotateHandlePos.Y:=RotateHandlePos.Y+DY;
    Angle:=ComplexVal(-RotateHandlePos.Y,RotateHandlePos.X).Angle;
  end
  else
  begin
    Result:=inherited Move(DX,DY,Handle,Grid,Shift);
    if Handle<>0 then RotateHandlePos.X:=Nan;
  end;
end;

procedure TMetafileObject.CompleteRotation;
var
  DAngle : Single;
begin
  if not IsNan(OldAngle) then
  begin
    DAngle:=Angle-OldAngle;
    Angle:=OldAngle;
    OldAngle:=Nan;
    Rotate(DAngle,False,False,CenterPoint(Position));
  end;
end;

procedure TMetafileObject.NotifyMovementDone;
begin
  inherited;
  CompleteRotation;
end;

procedure TMetafileObject.Rotate(const DAngle: Double; FlipLR,FlipUD: Boolean; const Center: TPoint);
var
  OrgSize : TPoint;
  NewMetafile : TMetafile;
  MetafileCanvas : TMetafileCanvas;
  P : TFloatPoint;
  I : Integer;
begin
  if opCustomLinks in ValidProperties then // Rotate link points
    for I:=0 to High(Links) do
    begin
      P.X:=(Links[I].X-0.5)*Width;
      P.Y:=(Links[I].Y-0.5)*Height;
      P:=RotatePoint(P,FloatOrigo,DAngle);
      Links[I].X:=P.X/Width+0.5;
      Links[I].Y:=P.Y/Height+0.5;
    end;
  Angle:=Angle+DAngle;

  if FlipLR then
  begin
    FreeAndNil(MemStream);
    NewMetafile:=TMetafile.Create;
    try
      NewMetafile.Width:=MetaFile.Width;
      NewMetafile.Height:=MetaFile.Height;
      MetafileCanvas:=TMetafileCanvas.Create(NewMetafile,0);
      try
        MetafileCanvas.StretchDraw(Rect(MetaFile.Width,0,0,MetaFile.Height),Metafile);
      finally
        MetafileCanvas.Free;
      end;
      SwapDWords(Metafile,NewMetafile);
    finally
      NewMetafile.Free;
    end;
  end;
  if FlipUD then
  begin
    FreeAndNil(MemStream);
    NewMetafile:=TMetafile.Create;
    try
      NewMetafile.Width:=MetaFile.Width;
      NewMetafile.Height:=MetaFile.Height;
      MetafileCanvas:=TMetafileCanvas.Create(NewMetafile,0);
      try
        MetafileCanvas.StretchDraw(Rect(0,MetaFile.Height,MetaFile.Width,0),Metafile);
      finally
        MetafileCanvas.Free;
      end;
      SwapDWords(Metafile,NewMetafile);
    finally
      NewMetafile.Free;
    end;
  end;
  OrgSize:=Point(Width,Height);
  inherited;
  with CenterPoint(Position) do Position:=Bounds(X-OrgSize.X div 2,Y-OrgSize.Y div 2,OrgSize.X,OrgSize.Y);
  RotateHandlePos.X:=Nan;
  NotifyMovement;
end;

procedure TMetafileObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);

  function Transform(const P: TPoint; const M: XFORM): TPoint;
  begin
    with M, P do
    begin
      Result.X:=Round(eM11*X+eM12*Y);
      Result.Y:=Round(eM21*X+eM22*Y);
    end;
  end;

var
  CanvasRect : TRect;
  Trans : XFORM;
  I : Integer;
begin
  CanvasRect:=CanvasInfo.CanvasRect1(Position);
  if Angle=0 then
  begin
    Canvas.StretchDraw(CanvasRect,Metafile)
  end
  else
  begin
    with Trans, CanvasRect do
    begin
      eM11:=Cos(Angle);
      eM12:=Sin(Angle);
      eM21:=-eM12;
      eM22:=eM11;
      eDx:=Left+Width*CanvasInfo.Scale.X*0.5;
      eDy:=Top+Height*CanvasInfo.Scale.Y*0.5;
    end;
    I:=SetGraphicsMode(Canvas.Handle,GM_ADVANCED);
    SetWorldTransform(Canvas.Handle,Trans);
    with CanvasRect do Canvas.StretchDraw(Bounds(-(Right-Left+1) div 2,-(Bottom-Top+1) div 2,Right-Left,Bottom-Top),Metafile);
    ModifyWorldTransform(Canvas.Handle,Trans,MWT_IDENTITY);
    SetGraphicsMode(Canvas.Handle,I);
  end;
  if Assigned(CanvasInfo.ZBuffer) then with CanvasInfo.ZBuffer do
  begin
    Brush.Style:=bsSolid;
    Brush.Color:=Index;
    FillRect(CanvasRect);
  end;
  inherited;
end;

procedure TMetafileObject.DrawSelected(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);

  procedure DrawFocusBall(X,Y,Marker: Integer);
  const
    MarkerBoxSize  = 3;
  var
    BoxRect : TRect;
    I : Integer;
  begin
    BoxRect:=Rect(X-MarkerBoxSize,Y-MarkerBoxSize,X+(MarkerBoxSize+1),Y+(MarkerBoxSize+1));
    Canvas.FrameRect(BoxRect);
    I:=SetGraphicsMode(Canvas.Handle,GM_ADVANCED);
    Dec(BoxRect.Left,2);
    Dec(BoxRect.Top,2);
    Inc(BoxRect.Right,1);
    Inc(BoxRect.Bottom,1);
    Canvas.Brush.Style:=bsClear;
    Canvas.Pen.Width:=1;
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Color:=$a0a0a0;
    Canvas.Ellipse(BoxRect);
    SetGraphicsMode(Canvas.Handle,I);
    if Index>=0 then
      with CanvasInfo, ZBuffer do
      begin
        Brush.Color:=Index or (Marker shl 16);
        Pen.Color:=Index or (Marker shl 16);
        Pen.Width:=1;
        Ellipse(BoxRect);
      end;
  end;

begin
  if Selected then
  begin
    inherited;
    // Compute rotation handle position
    if IsNan(RotateHandlePos.X) then
      with Position do
        RotateHandlePos:=RotatePoint(FloatPoint(0,Min((Bottom-Top),(Right-Left))*-0.4),FloatOrigo,Angle);
    // Draw handle
    with CenterPoint(CanvasInfo.CanvasRect(Position)) do
      DrawFocusBall(X+Round(RotateHandlePos.X*CanvasInfo.Scale.X),Y+Round(RotateHandlePos.Y*CanvasInfo.Scale.Y),9);
  end;
end;

class function TMetafileObject.Identifier: Integer;
begin
  Result:=otMetafileObject;
end;

procedure TMetafileObject.SaveToStream(Stream: TBaseStream);
var
  I : Integer;
begin
  inherited;
  if MemStream=nil then
  begin
    MemStream:=TMemoryStream.Create;
    Metafile.SaveToStream(MemStream);
  end;
  I:=MemStream.Size;
  Stream.Write(I,4);
  Stream.Write(MemStream.Memory^,I);
  Stream.Write(Angle,4);
end;

procedure TMetafileObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  I : Integer;
begin
  inherited;
  Stream.Read(I,4);
  MemStream:=TMemoryStream.Create;
  MemStream.Size:=I;
  Stream.Read(MemStream.Memory^,I);
  Metafile.LoadFromStream(MemStream);
  if FileVersion>=4 then Stream.Read(Angle,4);
end;

//==============================================================================================================================
// TInheritedLayerObject
//==============================================================================================================================
constructor TInheritedLayerObject.CreateNew(RelativePageIndex,LayerIndex: Integer; Page: TDiagramPage);
begin
  Create;
  if RelativePageIndex>=0 then Name:='+'+IntToStr(RelativePageIndex)
  else if RelativePageIndex<0 then Name:=IntToStr(RelativePageIndex);
  Name:=Format(rsPagePSLayerD,[Name,LayerIndex+1]);
  FRelativePageIndex:=RelativePageIndex;
  FLayerIndex:=LayerIndex;
  if Assigned(Page) then FPosition:=Rect(0,0,Page.Width,Page.Height);
end;

function TInheritedLayerObject.CreateCopy: TBaseObject;
begin
  Result:=TInheritedLayerObject.CreateNew(FRelativePageIndex,FLayerIndex);
  Result.Position:=Position;
end;

function TInheritedLayerObject.ValidProperties: TObjectProperties;
begin
  Result:=(inherited ValidProperties)+[opPosition];
end;

procedure TInheritedLayerObject.Draw(Canvas: TCanvas; const CanvasInfo: TCanvasInfo; Index: Integer);
var
  LocalCanvasInfo: TCanvasInfo;
  Page : TDiagramPage;
begin
  if not Drawing then // Avoid recursive calls
  begin
    LocalCanvasInfo:=CanvasInfo;
    Inc(LocalCanvasInfo.PageIndex,FRelativePageIndex);
    if Assigned(LocalCanvasInfo.Container) and
       InRange(LocalCanvasInfo.PageIndex,0,LocalCanvasInfo.Container.Count-1) then
    begin
      Page:=LocalCanvasInfo.Container.Pages[LocalCanvasInfo.PageIndex];
      if InRange(FLayerIndex,0,Page.Count-1) then
      try
        Drawing:=True;

        if LocalCanvasInfo.DrawMode in [dmEditing,dmEditDrag] then LocalCanvasInfo.DrawMode:=dmPreview;
        LocalCanvasInfo.ZBuffer:=nil;
        LocalCanvasInfo.Offset.X:=LocalCanvasInfo.Offset.X+Round(Position.Left*CanvasInfo.Scale.X);
        LocalCanvasInfo.Offset.Y:=LocalCanvasInfo.Offset.Y+Round(Position.Top*CanvasInfo.Scale.Y);
        LocalCanvasInfo.Scale.X:=LocalCanvasInfo.Scale.X*Width/Page.Width;
        LocalCanvasInfo.Scale.Y:=LocalCanvasInfo.Scale.Y*Height/Page.Height;

        Page.Layers[FLayerIndex].Draw(Canvas,LocalCanvasInfo);
      finally
        Drawing:=False;
      end;
    end;
    inherited;
  end;
end;

procedure TInheritedLayerObject.DrawAntialiasing(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
var
  LocalCanvasInfo: TCanvasInfo;
  Page : TDiagramPage;
begin
  if not Drawing then // Avoid recursive calls
  begin
    LocalCanvasInfo:=CanvasInfo;
    Inc(LocalCanvasInfo.PageIndex,FRelativePageIndex);
    if Assigned(LocalCanvasInfo.Container) and
       InRange(LocalCanvasInfo.PageIndex,0,LocalCanvasInfo.Container.Count-1) then
    begin
      Page:=LocalCanvasInfo.Container.Pages[LocalCanvasInfo.PageIndex];
      if InRange(FLayerIndex,0,Page.Count-1) then
      try
        Drawing:=True;

        Assert(LocalCanvasInfo.DrawMode=dmRender);
        LocalCanvasInfo.ZBuffer:=nil;
        LocalCanvasInfo.Offset.X:=LocalCanvasInfo.Offset.X+Round(Position.Left*CanvasInfo.Scale.X+0.5);
        LocalCanvasInfo.Offset.Y:=LocalCanvasInfo.Offset.Y+Round(Position.Top*CanvasInfo.Scale.Y+0.5);
        LocalCanvasInfo.Scale.X:=LocalCanvasInfo.Scale.X*Width/Page.Width;
        LocalCanvasInfo.Scale.Y:=LocalCanvasInfo.Scale.Y*Height/Page.Height;

        Page.Layers[FLayerIndex].DrawAntialiasing(Canvas,LocalCanvasInfo);
      finally
        Drawing:=False;
      end;
    end;
    inherited;
  end;
end;

procedure TInheritedLayerObject.DrawShadow(Canvas: TCanvas; const CanvasInfo: TCanvasInfo);
var
  LocalCanvasInfo: TCanvasInfo;
  Page : TDiagramPage;
begin
  if not Drawing then // Avoid recursive calls
  begin
    LocalCanvasInfo:=CanvasInfo;
    Inc(LocalCanvasInfo.PageIndex,FRelativePageIndex);
    if Assigned(LocalCanvasInfo.Container) and
       InRange(LocalCanvasInfo.PageIndex,0,LocalCanvasInfo.Container.Count-1) then
    begin
      Page:=LocalCanvasInfo.Container.Pages[LocalCanvasInfo.PageIndex];
      if InRange(FLayerIndex,0,Page.Count-1) then
      try
        Drawing:=True;

        LocalCanvasInfo.ZBuffer:=nil;
        LocalCanvasInfo.Offset.X:=LocalCanvasInfo.Offset.X+Round(Position.Left*CanvasInfo.Scale.X);
        LocalCanvasInfo.Offset.Y:=LocalCanvasInfo.Offset.Y+Round(Position.Top*CanvasInfo.Scale.Y);
        LocalCanvasInfo.Scale.X:=LocalCanvasInfo.Scale.X*Width/Page.Width;
        LocalCanvasInfo.Scale.Y:=LocalCanvasInfo.Scale.Y*Height/Page.Height;

        Page.Layers[FLayerIndex].DrawShadow(Canvas,LocalCanvasInfo);
      finally
        Drawing:=False;
      end;
    end;
  end;
end;

class function TInheritedLayerObject.Identifier: Integer;
begin
  Result:=otInheritedLayer;
end;

procedure TInheritedLayerObject.SaveToStream(Stream: TBaseStream);
begin
  inherited;
  Stream.Write(FRelativePageIndex,4);
  Stream.Write(FLayerIndex,4);
end;

procedure TInheritedLayerObject.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
begin
  inherited;
  Stream.Read(FRelativePageIndex,4);
  Stream.Read(FLayerIndex,4);
end;

end.

