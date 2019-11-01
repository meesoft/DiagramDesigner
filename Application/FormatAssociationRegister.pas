unit FormatAssociationRegister;

interface     

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormatAssociation, StdCtrls, ExtCtrls;

type
  TFormatAssociateRegisterForm = class(TFormatAssociateForm)
    procedure AssociateButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses FileUtils;

{$R *.dfm}

procedure TFormatAssociateRegisterForm.AssociateButtonClick(Sender: TObject);
var
  I : Integer;
  Param : string;
begin
  inherited;
  for I:=0 to ScrollBox.ControlCount-1 do
    if not TCheckBox(ScrollBox.Controls[I]).Checked then
    begin
      Param:='/u ';
      Break;
    end;
  Param:=Param+'/s "'+ProgramPath+'DDThumb.dll"';
  ExecuteFile('regsvr32.exe',Param);
  ModalResult:=mrOk;
end;

end.
 
