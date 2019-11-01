unit FormatAssociation;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, ExtCtrls, StyleForm, Types, Math;

type
  TFormatAssociateForm = class(TStyleForm)
    AssociateButton: TButton;
    CancelButton: TButton;
    NoneButton: TButton;
    Panel: TPanel;
    ScrollBox: TScrollBox;
    DescriptionBox: TCheckBox;
    AllButton: TButton;
    NoteLabel: TLabel;
    OpenActionButton: TRadioButton;
    EditActionButton: TRadioButton;
    procedure SelectButtonClick(Sender: TObject);
    procedure AssociateButtonClick(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute(Formats: string; const IconIndex: array of Integer; SelectAll: Boolean=False; SkipButton: Boolean=False; EditAction: Boolean=False);
  end;

implementation

uses FileUtils;

{$R *.DFM}

resourcestring
  rsSkip = 'Skip';

class procedure TFormatAssociateForm.Execute(Formats: string; const IconIndex: array of Integer; SelectAll,SkipButton,EditAction: Boolean);
var
  Y, P, ExtensionCount, FormatIndex : Integer;
  Str, Description, Ext : string;
  Form : TFormatAssociateForm;
begin
  Form:=Create(nil,GetActiveFormHandle);
  with Form do
  try
    Caption:=Caption+' '+Application.Title;
    if SkipButton then CancelButton.Caption:=rsSkip;
    if (Application.MainForm=nil) or (Application.MainForm.Width=0) then Position:=poScreenCenter;
    ResetControlAnchors(Form);
    if EditAction then EditActionButton.Checked:=True
    else
    begin
      OpenActionButton.Visible:=False;
      EditActionButton.Visible:=False;
    end;
    Y:=4;
    ExtensionCount:=0;
    FormatIndex:=0;
    while Formats<>'' do
    begin
      P:=Pos('|',Formats);
      if P=0 then P:=Length(Formats)+1;
      Str:=Copy(Formats,1,P-1);
      Delete(Formats,1,P);
      P:=Pos('*.',Str);
      Description:=Copy(Str,1,P-2);
      while P<>0 do
      begin
        Delete(Str,1,P);
        P:=2;
        while (P<Length(Str)) and not (Str[P] in [',',';',' ',')']) do Inc(P);
        with TCheckBox.Create(ScrollBox) do
        begin
          Ext:=Copy(Str,1,P-1);
          Tag:=IconIndex[Min(High(IconIndex),FormatIndex)];
          Caption:=Description+'(*'+Ext+')';
          {$IFOPT D+}
          Caption:=Caption+' ['+IntToStr(Tag)+']';
          {$ENDIF}
          Left:=6;
          Top:=Y;
          Width:=ScrollBox.ClientWidth;
          Checked:=SelectAll or IsFormatRegistered(Ext,Application.Title);
          Inc(Y,Height+6);
          Parent:=ScrollBox;
          Inc(ExtensionCount);
        end;
        P:=Pos('*.',Str);
      end;
      Delete(Formats,1,Pos('|',Formats));
      Inc(FormatIndex);
    end;
    if ExtensionCount=1 then
    begin
      AllButton.Visible:=False;
      NoneButton.Visible:=False;
    end;
    if ExtensionCount>0 then ShowModal;
  finally
    Free;
  end;
end;

procedure TFormatAssociateForm.FormCreate(Sender: TObject);
begin
  Font.Color:=clWindowText;
  NoteLabel.Font.Style:=[fsBold];
end;

procedure TFormatAssociateForm.SelectButtonClick(Sender: TObject);
var
  I : Integer;
begin
  for I:=0 to ScrollBox.ControlCount-1 do TCheckBox(ScrollBox.Controls[I]).Checked:=(Sender=AllButton);
end;

procedure TFormatAssociateForm.AssociateButtonClick(Sender: TObject);
var
  I, P : Integer;
  Str, Description : string;
begin
  Screen.Cursor:=crHourGlass;
  try
    Description:='';
    for I:=0 to ScrollBox.ControlCount-1 do
    begin
      with TCheckBox(ScrollBox.Controls[I]) do if Checked then
      begin
        Str:=Caption;
        P:=Pos('*.',Str);
        if DescriptionBox.Checked then Description:=Copy(Str,1,P-3);
        Delete(Str,1,P);
        P:=2;
        while (P<Length(Str)) and not (Str[P] in [',',';',' ',')']) do Inc(P);
        RegisterFileFormat(Copy(Str,1,P-1),Application.Title,Description,'',Tag,EditActionButton.Checked);
      end
      else // Unassociate
      begin
        Str:=Caption;
        P:=Pos('*.',Str);
        Delete(Str,1,P);
        P:=2;
        while (P<Length(Str)) and not (Str[P] in [',',';',' ',')']) do Inc(P);
        Str:=Copy(Str,1,P-1);
        if IsFormatRegistered(Str,Application.Title) then
          RegisterFileFormat(Str,'');
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
  ModalResult:=mrOk;
end;

procedure TFormatAssociateForm.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox.VertScrollBar.Position:=ScrollBox.VertScrollBar.Position-WheelDelta div 2;
  Handled:=True;
end;

end.

