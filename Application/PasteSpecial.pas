unit PasteSpecial;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, StyleForm;

type
  TPasteSpecialForm = class(TStyleForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ListBox: TListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TPasteSpecialForm.FormCreate(Sender: TObject);
begin
  UseBackgroundTheme:=True;
end;

procedure TPasteSpecialForm.ListBoxDblClick(Sender: TObject);
begin
  if ListBox.ItemIndex>=0 then ModalResult:=mrOK;
end;

end.
