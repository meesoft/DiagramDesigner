unit PageProperties;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, DiagramBase, ValueEdits, DesignerSetup, StyleForm,
  Dialogs;

type
  TPagePropertiesForm = class(TStyleForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    WidthEdit: TFloatEdit;
    HeightEdit: TFloatEdit;
    Label1: TLabel;
    Label2: TLabel;
    UnitLabel1: TLabel;
    UnitLabel2: TLabel;
    PrinterButton: TButton;
    FlipButton: TButton;
    PrinterSetupDialog: TPrinterSetupDialog;
    ApplyToAllBox: TCheckBox;
    Label3: TLabel;
    NameEdit: TEdit;
    procedure FlipButtonClick(Sender: TObject);
    procedure PrinterButtonClick(Sender: TObject);
    procedure ChangeValue(Sender: TObject);
  private
    { Private declarations }
    FOptions : ^TDesignerSetup;
    Modified : Boolean;
  public
    { Public declarations }
    class function Execute(Page: TDiagramPage; Diagram: TDiagramContainer; const Options: TDesignerSetup; EnableCancel: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses StringUtils, Math, Printers, TranslationTools;

class function TPagePropertiesForm.Execute(Page: TDiagramPage; Diagram: TDiagramContainer; const Options: TDesignerSetup; EnableCancel: Boolean): Boolean;
var
  I : Integer;
begin
  with Create(nil,GetActiveFormHandle) do
  try
    CancelBtn.Visible:=EnableCancel;
    ApplyToAllBox.Enabled:=Diagram.Count>1;
    with Options, Page do
    begin
      NameEdit.Text:=RawName;
      WidthEdit.FormatString:=DisplayUnitFormat[DisplayUnits];
      HeightEdit.FormatString:=DisplayUnitFormat[DisplayUnits];
      UnitLabel1.Caption:=TranslationManager.TranslateString(DisplayUnitName[DisplayUnits]);
      UnitLabel2.Caption:=TranslationManager.TranslateString(DisplayUnitName[DisplayUnits]);
      WidthEdit.Value:=Width/DisplayUnitSize[DisplayUnits];
      HeightEdit.Value:=Height/DisplayUnitSize[DisplayUnits];
    end;
    FOptions:=@Options;
    Modified:=False;
    Result:=(ShowModal=mrOk) and (Modified or (ApplyToAllBox.Enabled and ApplyToAllBox.Checked));
    if Result then
    begin
      with Options, Page do
      begin
        RawName:=NameEdit.Text;
        Width:=Max(1,Round(WidthEdit.Value*DisplayUnitSize[DisplayUnits]));
        Height:=Max(1,Round(HeightEdit.Value*DisplayUnitSize[DisplayUnits]));
      end;                    
      if ApplyToAllBox.Checked then
        for I:=0 to Diagram.Count-1 do with Diagram.Pages[I] do
        begin
          Width:=Page.Width;
          Height:=Page.Height;
        end;
    end;
  finally
    Free;
  end;
end;

procedure TPagePropertiesForm.FlipButtonClick(Sender: TObject);
var
  Size : Extended;
begin
  Size:=WidthEdit.Value;
  WidthEdit.Value:=HeightEdit.Value;
  HeightEdit.Value:=Size;
  ChangeValue(nil);
end;

procedure TPagePropertiesForm.PrinterButtonClick(Sender: TObject);
var
  DC : THandle;
begin
  if PrinterSetupDialog.Execute then with FOptions^ do
  begin
    DC:=Printer.Handle;
    WidthEdit.Value:=GetDeviceCaps(DC,PHYSICALWIDTH)/GetDeviceCaps(DC,LOGPIXELSX)*DesignerDPI/DisplayUnitSize[DisplayUnits];
    HeightEdit.Value:=GetDeviceCaps(DC,PHYSICALHEIGHT)/GetDeviceCaps(DC,LOGPIXELSY)*DesignerDPI/DisplayUnitSize[DisplayUnits];
    ChangeValue(nil);
  end;
end;

procedure TPagePropertiesForm.ChangeValue(Sender: TObject);
begin
  OKBtn.Enabled:=WidthEdit.Valid and HeightEdit.Valid;
  Modified:=True;
end;

end.

