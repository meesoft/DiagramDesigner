unit RearrangePages; 

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DiagramBase, StdCtrls, Math, StyleForm, Menus;

type
  TRearrangePagesForm = class(TStyleForm)
    OKButton: TButton;
    UpButton: TButton;
    DownButton: TButton;
    DeleteButton: TButton;
    RenameButton: TButton;
    PageListBox: TListBox;
    procedure UpdateControlStates(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure MoveButtonClick(Sender: TObject);
    procedure RenameButtonClick(Sender: TObject);
    procedure PageListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PageListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
    Diagram: TDiagramContainer;
    Modified : Boolean;
  public
    { Public declarations }
    class function Execute(ADiagram: TDiagramContainer; ActivePage: Integer; out DiagramModified: Boolean): Integer;
  end;

implementation

{$R *.dfm}

resourcestring
  rsDeleteDiagramPage = 'Delete diagram page?';
  rsPageTitleEmptyForAuto = 'Page title (empty for auto):';

class function TRearrangePagesForm.Execute(ADiagram: TDiagramContainer; ActivePage: Integer; out DiagramModified: Boolean): Integer;
var
  I : Integer;
begin
  with Create(nil,GetActiveFormHandle) do
  try
    UseBackgroundTheme:=True;
    ResetControlAnchors(TControl(PageListBox.Owner));
    Diagram:=ADiagram;
    for I:=0 to Diagram.Count-1 do PageListBox.Items.AddObject(Diagram.Pages[I].GetName(I),TObject(I));
    PageListBox.ItemIndex:=ActivePage;
    ShowModal;
    DiagramModified:=Modified;
    Result:=PageListBox.ItemIndex;
    Assert(Result>=0);
  finally
    Free;
  end;
end;

procedure TRearrangePagesForm.UpdateControlStates(Sender: TObject);
begin
  UpButton.Enabled:=PageListBox.ItemIndex>0;
  DownButton.Enabled:=PageListBox.ItemIndex<PageListBox.Count-1;
  DeleteButton.Enabled:=PageListBox.Count>1;
end;

procedure TRearrangePagesForm.MoveButtonClick(Sender: TObject);
var
  Delta, I : Integer;
begin
  if Sender=DownButton then Delta:=1 else Delta:=-1;
  I:=PageListBox.ItemIndex;
  Modified:=True;
  PageListBox.Items.Move(I,I+Delta);
  Diagram.Move(I,I+Delta);
  PageListBox.ItemIndex:=I+Delta;
  UpdateControlStates(nil);
end;

procedure TRearrangePagesForm.DeleteButtonClick(Sender: TObject);
var
  I : Integer;
begin
  if (Diagram.Count>1) and (MessageDlg(rsDeleteDiagramPage,mtConfirmation,mbYesNo)=mrYes) then
  begin
    I:=PageListBox.ItemIndex;
    Modified:=True;
    PageListBox.Items.Delete(I);
    Diagram.Delete(I);
    PageListBox.ItemIndex:=Max(0,I-1);
  end;
  UpdateControlStates(nil);
end;

procedure TRearrangePagesForm.RenameButtonClick(Sender: TObject);
var
  Str : string;
begin
  Str:=Diagram.Pages[PageListBox.ItemIndex].RawName;
  if InputQuery(StripHotKey(RenameButton.Caption),rsPageTitleEmptyForAuto,Str) then
  begin
    Modified:=True;
    Diagram.Pages[PageListBox.ItemIndex].RawName:=Str;
    PageListBox.Items[PageListBox.ItemIndex]:=
      Diagram.Pages[PageListBox.ItemIndex].GetName(Integer(PageListBox.Items.Objects[PageListBox.ItemIndex]));
  end;
  UpdateControlStates(nil);
end;

procedure TRearrangePagesForm.PageListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F2     : RenameButtonClick(nil);   
    VK_DELETE : DeleteButtonClick(nil);
    VK_DOWN   : if (ssCtrl in Shift) and DownButton.Enabled then
                begin
                  MoveButtonClick(DownButton);
                  Key:=0;
                end;
    VK_UP     : if (ssCtrl in Shift) and UpButton.Enabled then
                begin
                  MoveButtonClick(UpButton);
                  Key:=0;
                end;
  end;
end;

procedure TRearrangePagesForm.PageListBoxDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

end.

