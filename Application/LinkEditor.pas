unit LinkEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StyleForm, MathUtils, StdCtrls, ExtCtrls, ValueEdits, Math,
  Buttons, PanelFrame, DiagramBase, Clipbrd;

resourcestring
  rsEditLinkPoint = 'Edit link point';
  rsHorizontal = 'Horizontal';
  rsVertical = 'Vertical';
  
type
  TLinkEditorForm = class(TStyleForm)
    Button1: TButton;
    Button2: TButton;
    ListBox: TListBox;
    AddButton: TButton;
    EditButton: TButton;
    DeleteButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MoveUpButton: TBitBtn;
    MoveDownButton: TBitBtn;
    SampleFrame: TDoubleBufferedPanel;
    PasteButton: TBitBtn;
    CopyButton: TBitBtn;
    procedure EditButtonClick(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure SampleFramePaint(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure SampleFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CopyButtonClick(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    List : TFloatPointArray;
    Obj : TBaseObject;
  end;

implementation

uses SysConst, MemUtils, DesignerSetup;

{$R *.dfm}

procedure TLinkEditorForm.FormShow(Sender: TObject);
var
  I, Index : Integer;
begin
  UseBackgroundTheme:=True;
  Index:=ListBox.ItemIndex;
  ListBox.Items.BeginUpdate;
  ListBox.Clear;
  for I:=0 to High(List) do
    ListBox.Items.AddObject(FormatFloat('0.0000',List[I].X)+'  '+FormatFloat('0.0000',List[I].Y),TObject(I));
  ListBox.Items.EndUpdate;
  if Index=-1 then ListBox.ItemIndex:=ListBox.Items.Count-1
  else ListBox.ItemIndex:=Min(Index,ListBox.Items.Count-1);
  if Sender=nil then SampleFrame.Invalidate;
end;

procedure TLinkEditorForm.ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_DELETE then DeleteButtonClick(nil);
end;

procedure TLinkEditorForm.EditButtonClick(Sender: TObject);
var
  X, Y : Extended;
  I : Integer;
begin
  {if IntegerQuery('','Number of sides',I,3,100) then
  begin
    SetLength(List,I);
    for I:=0 to I-1 do List[I]:=FloatPoint((1+Cos(I/Length(List)*2*pi))/2,(1+Sin(I/Length(List)*2*pi))/2);
    FormShow(nil);
    Exit;
  end;{}

  I:=ListBox.ItemIndex;
  if I=-1 then
  begin
    X:=0.5;
    Y:=0.5;
  end
  else
  begin
    X:=List[I].X;
    Y:=List[I].Y;
  end;
  if FloatQuery(rsEditLinkPoint,rsHorizontal+' (0-1):',X,0,1,120,0.1) and
     FloatQuery(rsEditLinkPoint,rsVertical+' (0-1):',Y,0,1,120,0.1) then
  begin
    if I=-1 then
    begin
      SetLength(List,Length(List)+1);
      List[High(List)]:=FloatPoint(X,Y);
    end
    else List[I]:=FloatPoint(X,Y);
    FormShow(nil);
  end;
end;

procedure TLinkEditorForm.DeleteButtonClick(Sender: TObject);
var
  I : Integer;
begin
  I:=ListBox.ItemIndex;
  if I>=0 then
  begin
    Move(List[I+1],List[I],(High(List)-I)*SizeOf(TFloatPoint));
    SetLength(List,Length(List)-1);
    FormShow(nil);
  end;
end;

procedure TLinkEditorForm.AddButtonClick(Sender: TObject);
begin
  if Length(List)>32000 then raise EOutOfMemory.Create(SOutOfMemory);
  ListBox.ItemIndex:=-1;
  EditButtonClick(nil);
end;

procedure TLinkEditorForm.MoveUpButtonClick(Sender: TObject);
var
  I : Integer;
  Point : TFloatPoint;
begin
  I:=ListBox.ItemIndex;
  if I>0 then
  begin
    Point:=List[I];
    List[I]:=List[I-1];
    List[I-1]:=Point;
    FormShow(nil);
    ListBox.ItemIndex:=I-1;
  end;
end;

procedure TLinkEditorForm.MoveDownButtonClick(Sender: TObject);
var
  I : Integer;
  Point : TFloatPoint;
begin
  I:=ListBox.ItemIndex;
  if (I>=0) and (I<High(List)) then
  begin
    Point:=List[I];
    List[I]:=List[I+1];
    List[I+1]:=Point;
    FormShow(nil);
    ListBox.ItemIndex:=I+1;
  end;
end;

procedure TLinkEditorForm.ListBoxClick(Sender: TObject);
begin
  SampleFrame.Invalidate;
end;

procedure TLinkEditorForm.SampleFramePaint(Sender: TObject);
var
  CanvasInfo : TCanvasInfo;
begin
  with SampleFrame.BitmapCanvas do
  begin
    Brush.Style:=bsSolid;
    SampleFrame.Clear(clBtnFace,False);
    Brush.Color:=clBtnShadow;
    FrameRect(Rect(1,1,SampleFrame.Width-1,SampleFrame.Height-1));
  end;
  if Obj<>nil then
  begin
    ZeroMem(CanvasInfo,SizeOf(CanvasInfo));
    CanvasInfo.Scale.X:=(SampleFrame.Width-3)/(Obj.Width-1);
    CanvasInfo.Scale.Y:=(SampleFrame.Height-3)/(Obj.Height-1);
    CanvasInfo.Offset:=RoundPoint(1-Obj.Position.Left*CanvasInfo.Scale.X,1-Obj.Position.Top*CanvasInfo.Scale.Y);
    CanvasInfo.DrawMode:=dmPreview;
    CanvasInfo.DefaultFont:=SampleFrame.Canvas.Font;
    CanvasInfo.DefaultFont.Name:='Arial';
    CanvasInfo.DefaultFont.Size:=10;
    Obj.Draw(SampleFrame.BitmapCanvas,CanvasInfo,0);
  end;
  if ListBox.ItemIndex>=0 then
    with SampleFrame.BitmapCanvas do
    begin
      Brush.Color:=clRed;
      Brush.Style:=bsSolid;
      FrameRect(Bounds(1+Round(List[ListBox.ItemIndex].X*(SampleFrame.Width-3))-1,
                       1+Round(List[ListBox.ItemIndex].Y*(SampleFrame.Height-3))-1,3,3));
    end;
end;

procedure TLinkEditorForm.SampleFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (ListBox.ItemIndex>=0) then
  begin
    List[ListBox.ItemIndex].X:=EnsureRange((X-1)/(SampleFrame.Width-3),0,1);
    List[ListBox.ItemIndex].Y:=EnsureRange((Y-1)/(SampleFrame.Height-3),0,1);
    FormShow(nil);
  end
end;

procedure TLinkEditorForm.SampleFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then SampleFrameMouseDown(Sender,mbLeft,[],X,Y);
end;

procedure TLinkEditorForm.CopyButtonClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    Clipboard.Clear;
    Clipboard.AsText:=LinksToString(List);
  finally
    Clipboard.Close;
  end;
end;

procedure TLinkEditorForm.PasteButtonClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    List:=LinksFromString(Clipboard.AsText+#13);
  finally
    Clipboard.Close;
    FormShow(nil);
  end;
end;

end.

