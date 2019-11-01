unit ExpressionHelp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ActnList, ExpressionEval, Menus,
  EventUtils, Math
{$IFDEF UseStyleForm} ,StyleForm; {$ELSE} ; type TStyleForm = TForm; {$ENDIF}

var
  DefaultExpressions : string = '';

type
  TExpressionEvalForm = class(TStyleForm)
    TopPanel: TPanel;
    Splitter1: TSplitter;
    BottomPanel: TPanel;
    ListView: TListView;
    ExpressionMemo: TMemo;
    RunButton: TButton;
    ActionList: TActionList;
    ActionRun: TAction;
    HelpLabel: TLabel;
    CloseButton: TButton;
    ActionClose: TAction;
    PopupMenu: TPopupMenu;
    N1: TMenuItem;
    ActionResetSymbols: TAction;
    Resetsymboltable1: TMenuItem;
    DeleteSymbolItem: TMenuItem;
    ResultLabel: TEdit;
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ActionRunExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DeleteSymbolItemClick(Sender: TObject);
    procedure ActionResetSymbolsExecute(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure FormCloseFree(Sender: TObject; var Action: TCloseAction);
  protected
    procedure OnGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    { Public declarations }
    Defines : TSymbolTable;
    Value : EvalFloat;
  end;

// Show as modal window. Last evaluation result is returned
function ShowExpressionEvaluator(ADefines: TSymbolTable=nil; const ACaption: string=''): EvalFloat;
// Show window and return form
function OpenExpressionEvaluator(OpenAsMDIChild: Boolean=False; ADefines: TSymbolTable=nil; const ACaption: string=''): TExpressionEvalForm;

function FloatToStr(const Value: Extended; ThousandSeparators: Boolean=False): string;

implementation

uses WinAPIUtils, MathUtils;

{$R *.DFM}

function FloatToStr(const Value: Extended; ThousandSeparators: Boolean): string;
begin
  if (Value<>0) and ((Abs(Value)<1e-4) or (Abs(Value)>1e10)) then Result:=FloatToStrF(Value,ffExponent,15,0)
  else if ThousandSeparators then Result:=FormatFloat(',0.############',Value)
  else Result:=FormatFloat('0.############',Value);
end;

function IntToBin(A: Byte): string;
var
  I : Integer;
begin
  Result:='';
  for I:=7 downto 0 do Result:=Result+Char(Byte('0')+(A shr I) and 1);
end;

function ShowExpressionEvaluator(ADefines: TSymbolTable; const ACaption: string): EvalFloat;
begin
  with TExpressionEvalForm.Create(Application) do
  try
    if ACaption<>'' then Caption:=ACaption;
    if Assigned(ADefines) then Defines:=ADefines
    else Defines:=DefaultDefines;
    SetPrecisionMode(pmExtended);
    try
      ActionRunExecute(nil);
      ExpressionMemo.Text:=DefaultExpressions;
      ExpressionMemo.SelectAll;
      UpdateListViewColumnSizes(ListView);
      ShowModal;
      DefaultExpressions:=ExpressionMemo.Text;
      Result:=Value;
    finally
      Set8087CW(DefaultFPUControlWord);
    end;
  finally
    Free;
  end;
end;

function OpenExpressionEvaluator(OpenAsMDIChild: Boolean; ADefines: TSymbolTable; const ACaption: string): TExpressionEvalForm;
begin
  if OpenAsMDIChild then Result:=TExpressionEvalForm.Create(Application.MainForm)
  else Result:=TExpressionEvalForm.Create(Application);
  with Result do
  begin
    if ACaption<>'' then Caption:=ACaption;
    if Assigned(ADefines) then Defines:=ADefines
    else Defines:=DefaultDefines;
    ActionRunExecute(nil);
    ExpressionMemo.Text:=DefaultExpressions;
    ExpressionMemo.SelectAll;                         
    OnClose:=FormCloseFree;
    if OpenAsMDIChild then              
    begin
      Position:=poDefaultPosOnly;
      FormStyle:=fsMDIChild;
    end;
    Show;
    UpdateListViewColumnSizes(ListView);
    ExpressionMemo.SetFocus;
  end;
end;

//=======================================================================================================
// TExpressionEvalForm
//=======================================================================================================
resourcestring
  rsChangeValue = 'Change value';
  rsStandardFunction = 'Standard function';
  rsExpressionHelp = 'Operators:'#10+
                     '&'#9+'Logical AND'#10+
                     '|'#9+'Logical OR'#10+
                     '='#9+'Equal'#10+
                     '#'#9+'Not equal'#10+
                     '>'#9+'Greater'#10+
                     '<'#9+'Less'#10+
                     '+'#9+'Add'#10+
                     '-'#9+'Subtract'#10+
                     '*'#9+'Multiply'#10+
                     '/'#9+'Divide'#10+
                     '%'#9+'Modulus'#10+
                     '^'#9+'Power'#10+
                     #10'Defining symbols:'#10+
                     ':Name = Value;'#10+
                     ':Func(x) = Expression;';

procedure TExpressionEvalForm.FormCreate(Sender: TObject);
begin
  Font.Color:=clWindowText;
  UseBackgroundTheme:=False;
  HelpLabel.Caption:=rsExpressionHelp;

  CloseButton.Left:=BottomPanel.Width-CloseButton.Width-RunButton.Left;
  CloseButton.Top:=BottomPanel.Height-CloseButton.Height-RunButton.Left;
  HelpLabel.Left:=BottomPanel.Width-HelpLabel.Width-RunButton.Left*2;
  ListView.Width:=HelpLabel.Left-RunButton.Left*2;
  ClientWidth:=600*GetDeviceCaps(Canvas.Handle,LOGPIXELSX) div 96;
  ClientHeight:=480*GetDeviceCaps(Canvas.Handle,LOGPIXELSX) div 96;
end;

procedure TExpressionEvalForm.FormShow(Sender: TObject);
begin
  ListView.Columns[1].AutoSize:=True;
  EnableVistaViewStyle(ListView);
end;

procedure TExpressionEvalForm.ListViewData(Sender: TObject; Item: TListItem);
var
  Symbol : TSymbol;
begin
  Item.Caption:=Defines[Item.Index];
  Symbol:=TSymbol(Defines.Objects[Item.Index]);
  if Assigned(Symbol) then
  case Symbol.SymbolType of
    stValue      : Item.SubItems.Add(FloatToStr(TSymbolValue(Defines.Objects[Item.Index]).Value,True));
    stExpression : Item.SubItems.Add(TSymbolExpression(Defines.Objects[Item.Index]).Expression);
    stFunction   : if Symbol is TSymbolFunctionWithDescription then with TSymbolFunctionWithDescription(Symbol) do
                   begin
                     Item.Caption:=Item.Caption+'(x)';
                     if Description='' then Item.SubItems.Add(rsStandardFunction)
                     else Item.SubItems.Add(Description);
                   end
                   else if Symbol is TExpressionFunction then with TExpressionFunction(Symbol) do
                   begin
                     Item.Caption:=Item.Caption+'('+VarName+')';
                     Item.SubItems.Add(Expression);
                   end
                   else
                   begin
                     Item.Caption:=Item.Caption+'(x)';
                     Item.SubItems.Add(TSymbolFunction(Defines.Objects[Item.Index]).ClassName);
                   end;
    stSymbolicFunction : with TSymbolicFunction(Symbol) do
                   begin
                     Item.Caption:=Item.Caption+'('+VarName+')';
                     Item.SubItems.Add(Help);
                   end;
  end;
end;

procedure TExpressionEvalForm.ActionRunExecute(Sender: TObject);
var
  Str : string;
  I : Integer;
begin
  if Sender=nil then ResultLabel.Text:=''
  else
  try
    Str:=ExpressionMemo.Text;
    Value:=EvaluateExpression(Str,Defines);
    Str:=FloatToStr(Value,True);
    if (Value<=High(Integer)) and (Value>=Low(Integer)) and (Abs(Value-Round(Value))<0.0000001) and (Abs(Value)>0.5) then
    begin
      I:=Round(Value);
      Str:=Str+'     Hex: '+IntToHex(I,0);
      if (I>=0) and (I<256) then Str:=Str+'     Bin: '+IntToBin(I);
    end;
    ResultLabel.Text:=Str;
  except
    on Error: EExpressionError do
    begin
      if Error.ErrorPosition<>0 then
      begin
        ExpressionMemo.SelStart:=Error.ErrorPosition-1;
        ResultLabel.Text:=Error.Message;
      end
      else ResultLabel.Text:=Error.Message;
      ExpressionMemo.SetFocus;
    end;
  end;
  ListView.Items.Count:=Defines.Count;
  ListView.Invalidate;
end;                                                                                

procedure TExpressionEvalForm.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TExpressionEvalForm.ListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var I : Integer;
begin
  case Key of
    Word('A')..Word('Z') :
                begin
                  if ssShift in Shift then Defines.Find(Char(Key),I)
                  else Defines.Find(LowerCase(Char(Key)),I);
                  if (I>=0) and (I<Defines.Count) then
                  begin
                    ListView.ItemFocused:=ListView.Items[I];
                    ListView.Selected:=ListView.ItemFocused;
                    ListView.ItemFocused.MakeVisible(False);
                  end;
                end;
    VK_DELETE : DeleteSymbolItemClick(Self);
    VK_RETURN : ListViewDblClick(Self);
  end;
end;

procedure TExpressionEvalForm.DeleteSymbolItemClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
  begin
    Defines.Delete(ListView.Selected.Index);
    ListView.Items.Count:=Defines.Count;
    ListView.Invalidate;
  end;
end;

procedure TExpressionEvalForm.ActionResetSymbolsExecute(Sender: TObject);
begin
  Defines.Clear;
  AddStandardSymbols(Defines);
  ListView.Items.Count:=Defines.Count;
  ListView.Invalidate;
end;

procedure TExpressionEvalForm.PopupMenuPopup(Sender: TObject);
begin
  DeleteSymbolItem.Enabled:=Assigned(ListView.Selected);
end;

procedure TExpressionEvalForm.ListViewDblClick(Sender: TObject);
var
  Str : string;
  Symbol : TSymbol;
begin
  if Assigned(ListView.ItemFocused) then with ListView.ItemFocused do
  begin
    Symbol:=TSymbol(Defines.Objects[Index]);
    if Symbol is TParameterValue then
    begin
      Str:=FloatToStr(TParameterValue(Symbol).Value);
      if InputQuery(rsChangeValue,Defines[Index]+' =',Str) then
      begin
        TParameterValue(Symbol).Value:=EvaluateExpression(Str,Defines);
        ListView.Invalidate;
      end;
    end
    else
    case Symbol.SymbolType of
      stValue, stExpression         : ExpressionMemo.SelText:=Defines[Index];
      stFunction,stSymbolicFunction : ExpressionMemo.SelText:=Defines[Index]+'(';
    end;
  end;
end;

procedure TExpressionEvalForm.FormCloseFree(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

// Make sure that child window maximized correctly in Vista
procedure TExpressionEvalForm.OnGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  if Msg.MinMaxInfo.ptMaxTrackSize.X<Msg.MinMaxInfo.ptMaxSize.X then
    Msg.MinMaxInfo.ptMaxTrackSize.X:= Msg.MinMaxInfo.ptMaxSize.X;
  if Msg.MinMaxInfo.ptMaxTrackSize.Y<Msg.MinMaxInfo.ptMaxSize.Y then
    Msg.MinMaxInfo.ptMaxTrackSize.Y:= Msg.MinMaxInfo.ptMaxSize.Y;
end;

end.

