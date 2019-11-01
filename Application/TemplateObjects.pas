unit TemplateObjects;

interface

uses Windows, SysUtils, MathUtils, DiagramBase, ShapeObject, Classes, Types, Streams;

type
  TTemplateSheet = class(TBaseObjectList)
    protected
      FWidth, FHeight : Integer;
    public
      property Width: Integer read FWidth write FWidth;
      property Height: Integer read FHeight write FHeight;
      procedure MakeDefault;
      procedure AddTemplate(Source: TDiagramLayer; Obj: TBaseObject);
      procedure UpdateHeight;
      procedure CopyToPage(Page: TDiagramPage);
      procedure SaveToStream(Stream: TBaseStream); override;
      procedure LoadFromStream(Stream: TBaseStream; FileVersion: Integer=0); override;
      procedure LoadFromFile(const FileName: string); override;
    end;

resourcestring
  rsInvalidTemplatePalette = 'Invalid template palette';

implementation

uses LineObject, GroupObject, Deflate, BufStream, FlowchartObject, TextObject;

//==============================================================================================================================
// TTemplateSheet
//==============================================================================================================================
const
  XSpace = 8*DesignerDPmm;
  YSpace = 8*DesignerDPmm;

const
  DefaultText = 'abc';

procedure TTemplateSheet.MakeDefault;
begin
  Clear;
  FHeight:=YSpace;
  FWidth:=2*31*DesignerDPmm+3*XSpace;

  with Objects[Add(TTextObject.CreateNew(DefaultText))] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,5*DesignerDPmm);
    Name:=ExtractObjectName(ClassName);
  end;
  with Objects[Add(TTextObject.CreateNew(DefaultText))] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,31*DesignerDPmm,5*DesignerDPmm);
    Properties[opTextXAlign]:=-1;
    FHeight:=Position.Bottom+YSpace;
    Name:=ExtractObjectName(ClassName);
  end;

  with Objects[Add(TRectangleObject.CreateNew)] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,19*DesignerDPmm);
  end;
  with Objects[Add(TPolygonObject.CreateNew)] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,19*DesignerDPmm,19*DesignerDPmm);
    SetLength(Links,4);
    Links[0]:=FloatPoint(0.5,0);
    Links[1]:=FloatPoint(1,0.5);
    Links[2]:=FloatPoint(0.5,1);
    Links[3]:=FloatPoint(0,0.5);
    FHeight:=Position.Bottom+YSpace;
  end;

  with Objects[Add(TEllipseObject.CreateNew)] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,19*DesignerDPmm);
  end;
  with Objects[Add(TPolygonObject.CreateNew)] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,31*DesignerDPmm,19*DesignerDPmm);
    SetLength(Links,4);
    Links[0]:=FloatPoint(0,0);
    Links[1]:=FloatPoint(1,0);
    Links[2]:=FloatPoint(1,1);
    Links[3]:=FloatPoint(0,1);
    FHeight:=Position.Bottom+YSpace;
  end;

  with Objects[Add(TFlowchartObject.CreateByKind(foRounded2))] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,15*DesignerDPmm);
  end;
  with Objects[Add(TFlowchartObject.CreateByKind(foSideBars))] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,31*DesignerDPmm,15*DesignerDPmm);
    FHeight:=Position.Bottom+YSpace;
  end;

  with Objects[Add(TFlowchartObject.CreateByKind(foSlantLeft))] do
  begin
    Position:=Bounds(XSpace,FHeight,29*DesignerDPmm,15*DesignerDPmm);
  end;
  with Objects[Add(TFlowchartObject.CreateByKind(foSlantRight))] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,29*DesignerDPmm,15*DesignerDPmm);
    FHeight:=Position.Bottom+YSpace;
  end;

  with Objects[Add(TFlowchartObject.CreateByKind(foOddRounded1))] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,15*DesignerDPmm);
  end;
  with Objects[Add(TFlowchartObject.CreateByKind(foOddRounded2))] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,26*DesignerDPmm,15*DesignerDPmm);
    FHeight:=Position.Bottom+YSpace;
  end;

  with Objects[Add(TStraightLineObject.CreateNew)] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,19*DesignerDPmm);
  end;
  with Objects[Add(TArrowObject.CreateNew)] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,31*DesignerDPmm,19*DesignerDPmm);
    FHeight:=Position.Bottom+YSpace;
  end;

  with Objects[Add(TAxisLineObject.CreateNew)] do
  begin
    Position:=Bounds(XSpace,FHeight,31*DesignerDPmm,19*DesignerDPmm);
  end;
  with Objects[Add(TConnectorObject.CreateNew)] do
  begin
    Position:=Bounds(2*XSpace+31*DesignerDPmm,FHeight,31*DesignerDPmm,19*DesignerDPmm);
    FHeight:=Position.Bottom+YSpace;
  end;
end;

procedure TTemplateSheet.AddTemplate(Source: TDiagramLayer; Obj: TBaseObject);
var
  NewObject : TBaseObject;
begin
  Source.DeselectAll;
  Obj.Selected:=True;
  AddCopy(Source,True);
  NewObject:=LastSelected;
  NewObject.Move(XSpace-NewObject.Left,Height-NewObject.Top,-1,NoGrid,[]);
  UpdateHeight;
end;

procedure TTemplateSheet.UpdateHeight;
begin
  FHeight:=GetBounds.Bottom+YSpace;
end;

procedure TTemplateSheet.CopyToPage(Page: TDiagramPage);
begin
  Page.Width:=Width;
  Page.Height:=Height;
  Page.Layers[0].AddCopy(Self);
end;

procedure TTemplateSheet.SaveToStream(Stream: TBaseStream);
var
  Header : TDiagramHeader;
  Deflate : TDeflateStream;
begin
  Header.DDd:='DDt';
  Header.FileVersion:=CurrentFileVersion;
  Stream.Write(Header,SizeOf(Header));
  Deflate:=TDeflateStream.Create(Stream,fmWrite);
  try
    Deflate.Write(FWidth,4);
    Deflate.Write(FHeight,4);
    inherited SaveToStream(Deflate);
  finally
    Deflate.Free;
  end;
end;

// Note: FileVersion is unused but necessary to override base method
procedure TTemplateSheet.LoadFromStream(Stream: TBaseStream; FileVersion: Integer);
var
  Header : TDiagramHeader;
  Deflate : TDeflateStream;
begin
  Clear;
  Stream.Read(Header,SizeOf(Header));
  if Header.DDd<>'DDt' then raise Exception.Create(rsInvalidTemplatePalette);
  if Header.FileVersion>CurrentFileVersion then raise Exception.Create(rsFileFromLaterProgramVersion);
  Deflate:=TDeflateStream.Create(Stream,fmRead);
  try
    Deflate.Read(FWidth,4);
    Deflate.Read(FHeight,4);
    inherited LoadFromStream(Deflate,Header.FileVersion);
  finally
    Deflate.Free;
  end;
end;

procedure TTemplateSheet.LoadFromFile(const FileName: string);
var
  Stream : TFilterStream;
begin
  Stream:=OpenBufferedFile(FileName,fmRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.FreeAll;
  end;
end;

end.

