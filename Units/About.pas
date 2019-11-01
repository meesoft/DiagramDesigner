unit About;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, 
  SysUtils, VersionInfo, StyleForm, PanelFrame;
                     
type
  TAboutBox = class(TStyleForm)
    Panel1: TPanelFrame;
    OKButton: TButton;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    WWWLabel: TLabel;
    EMailLabel: TLabel;
    MeeSoftImage: TImage;
    InfoLabel: TLabel;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAboutBox;

var
  InfoLabelText : string = '';

implementation

{$R *.DFM}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  Font.Color:=clWindowText;
end;

procedure ShowAboutBox;
begin
  {$IFDEF UseStyleForm}
  with TAboutBox.Create(Application,GetActiveFormHandle) do
  {$ELSE}
  with TAboutBox.Create(Application) do
  {$ENDIF}
  try
    {$IFOPT D+}
    with TLabel.Create(Panel1) do
    begin
      Top:=Version.Top;
      Width:=Panel1.ClientWidth-16;
      Alignment:=taRightJustify;
      Caption:='DEBUG BUILD';
      Parent:=Panel1;
      Font.Color:=clRed;
      Font.Style:=[fsBold];
      Transparent:=True;
    end;
    {$ENDIF}

    MakeLinkLabel(EMailLabel);
    MakeLinkLabel(WWWLabel);

    MeeSoftImage.Hint:=WWWLabel.Hint;
    MeeSoftImage.OnClick:=WWWLabel.OnClick;

    ShowModal;
  finally
    Free;
  end;
end;

procedure TAboutBox.FormShow(Sender: TObject);

  function LeadingZero(I: Integer): string;
  begin
    if I<10 then Result:='0'+IntToStr(I)
    else Result:=IntToStr(I);
  end;

var
  MinorVer : string;

begin
  {$IFDEF UseStyleForm}
  UseBackgroundTheme:=True;
  {$ENDIF}
  if VersionStr='' then Version.Caption:=''
  else
  begin
    if ThisApp.ProductVersion.Build=0 then MinorVer:=LeadingZero(ThisApp.ProductVersion.Minor)
    else MinorVer:=LeadingZero(ThisApp.ProductVersion.Minor)+'.'+IntToStr(ThisApp.ProductVersion.Build);
    Version.Caption:=Version.Caption+Format(' %d.%s     %d',[ThisApp.ProductVersion.Major,MinorVer,ThisApp.ProductVersion.Rel]);

    //Version.Caption:=Version.Caption+'     BETA'; 
  end;

  ProductName.Caption:=Application.Title;
  ProgramIcon.Picture.Assign(Application.Icon);

  InfoLabel.Width:=Panel1.Width-2*InfoLabel.Left;
  InfoLabel.Caption:=InfoLabelText;
  if InfoLabelText='' then InfoLabel.Height:=0;
  Panel1.Height:=InfoLabel.BoundsRect.Bottom+MeeSoftImage.Top+1;
  OKButton.Top:=Panel1.BoundsRect.Bottom+MeeSoftImage.Top;
  ClientHeight:=OKButton.BoundsRect.Bottom+Panel1.Top;

  //MeeSoftImage.Picture.Bitmap.PixelFormat:=pf24bit;
  MeeSoftImage.Transparent:=True;
end;

end.

