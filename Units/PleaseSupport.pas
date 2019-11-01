unit PleaseSupport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Settings, StyleForm;

type
  TPleaseSupportForm = class(TStyleForm)
    Label1: TLabel;
    CloseButton: TButton;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowPleaseSupportForm(const DonationLink: string; ForceShow: Boolean=False);

implementation

{$R *.DFM}

procedure ShowPleaseSupportForm(const DonationLink: string; ForceShow: Boolean=False);
var
  ShowPleaseSupport : Boolean;
begin
  try
    ShowPleaseSupport:=(Setup=nil) or Setup.GetBoolean('ShowPleaseSupport',True);
    if ShowPleaseSupport or ForceShow then
    with TPleaseSupportForm.Create(nil,GetActiveFormHandle) do
    try
      Caption:=Format(Caption,[Application.Title]);
      Label4.Hint:=DonationLink;
      MakeLinkLabel(Label4);

      CheckBox1.Checked:=ShowPleaseSupport;
      ShowModal;
      if Assigned(Setup) then Setup.WriteBoolean('ShowPleaseSupport',CheckBox1.Checked);
    finally
      Free;
    end;
  except
  end;
end;

procedure TPleaseSupportForm.FormCreate(Sender: TObject);
begin
  Font.Color:=clWindowText;
end;

end.

