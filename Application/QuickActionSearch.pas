unit QuickActionSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, QuickMenuSearch, Menus, StdCtrls, ExtCtrls, ComCtrls, ActnList, ActnMan;

type
  TQuickActionSearchForm = class(TQuickMenuSearchForm)
  private
    { Private declarations }
  public
    { Public declarations }
    class function CheckKey(ActionManager: TActionManager; var Key: Char; Shift: TShiftState=[]): Boolean;
    class procedure Execute(ActionManager: TActionManager);
  end;

implementation

{$R *.dfm}

{ TQuickActionSearchForm }

class function TQuickActionSearchForm.CheckKey(ActionManager: TActionManager; var Key: Char; Shift: TShiftState): Boolean;
var
  Str : string;
  I : Integer;
begin
  Result:=False;
  if (QuickMenuSearchForm=nil) and (Shift*[ssCtrl,ssAlt]=[]) and ((Key=#0) or (Key>#32)) then
  begin
    Str:=Key;
    if (Key in [#0,'0'..'9']) or (AnsiLowerCase(Str)<>AnsiUpperCase(Str)) then
    begin
      QuickMenuSearchForm:=Create(ActionManager);
      QuickMenuSearchForm.ListView.SmallImages:=ActionManager.Images;
      for I:=0 to ActionManager.ActionCount-1 do
        TQuickActionSearchForm(QuickMenuSearchForm).AddAction(ActionManager.Actions[I] as TAction);
      TQuickActionSearchForm(QuickMenuSearchForm).MenuItemList.Sort;
      QuickMenuSearchForm.SearchEdit.Text:=Str;
      QuickMenuSearchForm.SearchEdit.SelStart:=2;
      QuickMenuSearchForm.ListView.Columns.Delete(1);
      QuickMenuSearchForm.Show;
      Key:=#0;
      Result:=True;
    end;
  end;
end;

class procedure TQuickActionSearchForm.Execute(ActionManager: TActionManager);
var
  Key : Char;
begin
  Key:=#0;
  CheckKey(ActionManager,Key);
end;

end.

