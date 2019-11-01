unit ColorDialog;

interface

uses Windows, Dialogs, Graphics, ExtCtrls, SysUtils;

resourcestring
  rsNone = 'None';

function ShowColorDialog(var Color: TColor; Options: TColorDialogOptions=[cdFullOpen,cdAnyColor]): Boolean;

procedure SetColorPanelColor(Panel: TPanel; Color: TColor);

var CustomColors : string = '';

implementation

uses Forms, StyleForm, LinarBitmap, EventUtils;

function ShowColorDialog(var Color: TColor; Options: TColorDialogOptions): Boolean;
var
  ColorDialog : TColorDialog;
begin
  ColorDialog:=TColorDialog.Create(nil);
  try
    ColorDialog.Color:=Color;
    ColorDialog.Options:=Options;
    ColorDialog.CustomColors.Text:=CustomColors;
    SetApplicationHandleForDialog(0,ColorDialog);
    try
      Result:=ColorDialog.Execute;
    finally
      RestoreApplicationHandle;
    end;
    if Result then
    begin
      CustomColors:=ColorDialog.CustomColors.Text;
      Color:=ColorDialog.Color;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure SetColorPanelColor(Panel: TPanel; Color: TColor);
begin
  if Color=clNone then
  begin
    Panel.Color:=clBtnFace;
    Panel.Caption:=rsNone;
    Panel.Font.Color:=clWindowText;
  end
  else
  begin
    Panel.Color:=Color;
    Panel.ParentBackground:=False;
    Panel.Caption:=IntToHex(RGB2BGR(Color),6);
    if ColorBrightness(Color)<100*6 then Panel.Font.Color:=clWhite
    else Panel.Font.Color:=clBlack;
  end;
end;

end.

