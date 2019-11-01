unit QuickMenuSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StyleForm, ComCtrls, ExtCtrls, StdCtrls, EventUtils, StringLists, Math,
  ActnList;

type
  TQuickMenuSearchForm = class(TStyleForm)
    StatusBar: TStatusBar;
    ListView: TListView;
    Panel: TPanel;
    SearchEdit: TEdit;
    DummyMenu: TPopupMenu;
    CheckMarkImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SearchEditChange(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char);
  protected
    MenuItemList : TOpenStringList;
    procedure AddMenuItems(Menu: TMenuItem);
    procedure AddAction(Action: TAction);
    class procedure ClickEvent(Sender: TObject);
  public
    class function CheckKey(MainForm: TForm; var Key: Char; Shift: TShiftState=[]): Boolean;
    class procedure Execute(MainForm: TForm);
  end;

var
  QuickMenuSearchForm : TQuickMenuSearchForm = nil; // Must be public

implementation

uses StringUtils, TypInfo, ImgList;

{$R *.dfm}

var
  SelectedItem : TObject = nil;
  CheckMarkImageIndex : Integer = -1;

class function TQuickMenuSearchForm.CheckKey(MainForm: TForm; var Key: Char; Shift: TShiftState): Boolean;
var
  Str : string;
  Menu : TMainMenu;
begin
  Result:=False;
  if (QuickMenuSearchForm=nil) and (Shift*[ssCtrl,ssAlt]=[]) and ((Key=#0) or (Key>#32)) then
  begin
    Str:=Key;
    if (Key in [#0,'0'..'9']) or (AnsiLowerCase(Str)<>AnsiUpperCase(Str)) then
    begin
      QuickMenuSearchForm:=Create(MainForm);

      Menu:=MainForm.Menu;
      if Menu=nil then Menu:=GetObjectProp(MainForm,'SearchMenu',TMainMenu) as TMainMenu;

      if (CheckMarkImageIndex<0) and (Menu.Images<>nil) then
      begin
        QuickMenuSearchForm.CheckMarkImage.Picture.Bitmap.Width:=Menu.Images.Width;
        QuickMenuSearchForm.CheckMarkImage.Picture.Bitmap.Height:=Menu.Images.Height;
        CheckMarkImageIndex:=Menu.Images.AddMasked(QuickMenuSearchForm.CheckMarkImage.Picture.Bitmap,clWhite);
      end;
      QuickMenuSearchForm.ListView.SmallImages:=Menu.Images;
      QuickMenuSearchForm.AddMenuItems(Menu.Items);
      QuickMenuSearchForm.MenuItemList.Sort;
      QuickMenuSearchForm.SearchEdit.Text:=Str;
      QuickMenuSearchForm.SearchEdit.SelStart:=2;
      QuickMenuSearchForm.Show;
      Key:=#0;
      Result:=True;
    end;
  end;
end;

class procedure TQuickMenuSearchForm.Execute(MainForm: TForm);
var
  Key : Char;
begin
  Key:=#0;
  CheckKey(MainForm,Key);
end;

class procedure TQuickMenuSearchForm.ClickEvent(Sender: TObject);
begin
  if SelectedItem<>nil then
  begin
    if SelectedItem is TMenuItem then TMenuItem(SelectedItem).Click
    else if SelectedItem is TAction then TAction(SelectedItem).Execute
    else Assert(False);
    SelectedItem:=nil;
  end;
end;

procedure TQuickMenuSearchForm.FormCreate(Sender: TObject);
begin
  Font.Color:=clWindowText;
  MenuItemList:=TOpenStringList.Create;
  ClientWidth:=460*GetDeviceCaps(Canvas.Handle,LOGPIXELSX) div 96;
  ClientHeight:=250*GetDeviceCaps(Canvas.Handle,LOGPIXELSX) div 96;
end;

procedure TQuickMenuSearchForm.FormDestroy(Sender: TObject);
begin
  QuickMenuSearchForm:=nil;
  MenuItemList.Free;
end;

procedure TQuickMenuSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TQuickMenuSearchForm.FormResize(Sender: TObject);
begin
  SearchEdit.Width:=ClientWidth;
end;

procedure TQuickMenuSearchForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TQuickMenuSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure ActivateItem(Index: Integer);
  begin
    if ListView.Items.Count>0 then
    begin
      ListView.ItemIndex:=EnsureRange(Index,0,ListView.Items.Count-1);
      ListView.Selected.MakeVisible(False);
    end;
    Key:=0;
  end;

begin
  case Key of
    VK_ESCAPE : begin
                  Close;
                  Key:=0;
                end;
    VK_DOWN   : ActivateItem(ListView.ItemIndex+1);
    VK_UP     : ActivateItem(ListView.ItemIndex-1);
    VK_NEXT   : ActivateItem(ListView.ItemIndex+(ListView.ClientHeight div 17-1));
    VK_PRIOR  : ActivateItem(ListView.ItemIndex-(ListView.ClientHeight div 17-1));
    VK_END    : if ssCtrl in Shift then ActivateItem(ListView.Items.Count-1);
    VK_HOME   : if ssCtrl in Shift then ActivateItem(0);
    VK_RETURN : begin
                  ListViewDblClick(nil);
                  Key:=0;
                end;
    VK_F4     : begin
                  if ssAlt in Shift then Application.MainForm.Close;
                  Key:=0;
                end;
  end;
end;

procedure TQuickMenuSearchForm.SearchEditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and (ListView.Selected<>nil) then Key:=#0; // To avoid sound when selecting
end;

procedure TQuickMenuSearchForm.ListViewDblClick(Sender: TObject);
begin
  if ListView.Selected<>nil then
  begin
    SelectedItem:=ListView.Selected.Data;
    Close;
    NotifyOnIdle(ClickEvent,Owner);
  end;
end;

procedure TQuickMenuSearchForm.AddMenuItems(Menu: TMenuItem);
var
  I : Integer;
begin
  for I:=0 to Menu.Count-1 do
    if not Menu[I].IsLine and Menu[I].Visible and Menu[I].Enabled then
      if Menu[I].Count>0 then AddMenuItems(Menu[I])
      else MenuItemList.AddObject(AnsiLowerCase(StripHotkey(Menu[I].Caption)),Menu[I]);
end;

procedure TQuickMenuSearchForm.SearchEditChange(Sender: TObject);

  procedure AddItem(I: Integer);
  var
    Item : TMenuItem;
    Action : TAction;
    MenuName : string;
  begin
    with ListView.Items.Add do
    begin
      Data:=MenuItemList.Objects[I];
      if TObject(Data) is TMenuItem then
      begin
        Item:=TMenuItem(Data);
        ImageIndex:=Item.ImageIndex;
        if Item.Checked and (ImageIndex<0) then ImageIndex:=CheckMarkImageIndex;
        Caption:=StripHotkey(Item.Caption);
        MenuName:='';
        while (Item<>nil) and (Item.Caption<>'') do
        begin
          if MenuName<>'' then MenuName:=' | '+MenuName;
          MenuName:=StripHotkey(Item.Caption)+MenuName;
          Item:=Item.Parent;
        end;
        SubItems.Add(MenuName);
      end
      else if TObject(Data) is TAction then
      begin
        Action:=TAction(Data);
        ImageIndex:=Action.ImageIndex;
        Caption:=StripHotkey(Action.Caption);
      end;
    end;
  end;

var
  Mask : string;
  I : Integer;
  Added : array of Boolean;
begin
  Mask:=Trim(AnsiLowerCase(SearchEdit.Text));
  if not EndsWith('*',Mask) and not EndsWith('?',Mask) then Mask:=Mask+'*';
  ListView.Items.BeginUpdate;
  try
    ListView.Clear;
    SetLength(Added,MenuItemList.Count);
    for I:=0 to MenuItemList.Count-1 do
      if MaskCompare(MenuItemList[I],Mask) then
      begin
        AddItem(I);
        Added[I]:=True;
      end;
    if not StartsWith('*',Mask) and not StartsWith('?',Mask) and not EndsWith('?',Mask) then
    begin
      Mask:='*'+Mask;
      for I:=0 to MenuItemList.Count-1 do
        if not Added[I] and MaskCompare(MenuItemList[I],Mask) then AddItem(I);
    end;
    if ListView.Items.Count>0 then ListView.ItemIndex:=0;
  finally
    ListView.Items.EndUpdate;
  end;
  UpdateListViewColumnSizes(ListView);
end;

procedure TQuickMenuSearchForm.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Str : string;
begin
  if Selected then
  begin
    if TObject(Item.Data) is TMenuItem then Str:=TMenuItem(Item.Data).Hint
    else if TObject(Item.Data) is TAction then Str:=TAction(Item.Data).Hint;
    StatusBar.SimpleText:=Copy(Str,Pos('|',Str)+1,MaxInt);
  end;
end;

procedure TQuickMenuSearchForm.AddAction(Action: TAction);
begin
  if Action.Visible and Action.Enabled and Assigned(Action.OnExecute) and (Action.Caption<>'') then
    MenuItemList.AddObject(AnsiLowerCase(StripHotkey(Action.Caption)),Action);
end;

end.

