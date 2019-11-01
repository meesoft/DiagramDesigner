///////////////////////////////////////////////////////////////////////////////////////////////
//
// ValueEdit.pas - Value editor components
// ---------------------------------------
// Version:   2007-01-06
// Maintain:  Michael Vinther  |  mv@logicnet·dk
//
// Contains:
//   TValueEdit
//     TIntegerEdit
//     TFloatEdit
//
unit ValueEdits;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExpressionEval, Monitor, Consts, Spin, Buttons
{$IFDEF UseStyleForm} ,StyleForm; {$ELSE} ; type TStyleForm = TForm; {$ENDIF}

resourcestring
  rsValueMustBeInteger = 'Value must be integer';
  rsValueNotInAllowedRangeSS = 'Value not in allowed range: [%s - %s]';

const
  MaxIntegerDifference = 0.0001;

type
  TClientProc = function(Handle:HWND; MSG:UINT; wParam:WPARAM; lParam:LPARAM): LRESULT; stdcall;

  TValueEdit = class(TCustomEdit)
    protected
      ParentForm : TCustomForm;
      RestoreRightAlign : Integer;
      FFocusOnMouseWheel : Boolean;
      FAlignment : TAlignment;
      FOnChangeValue : TNotifyEvent;
      FValid : Boolean;
      FSimpleValue : Boolean;
      FDefines : TSymbolTable;
      FInvalidTextColor : TColor;
      FSpinButton : TSpinButton;
      FUpButton, FDownButton : TSpeedButton;
      RestoreParentFont : Boolean;
      procedure CreateWnd; override;
      procedure SetAlignment(const Value: TAlignment);
      procedure SetEnabled(Value: Boolean); override;
      procedure SetFocusOnMouseWheel(const Value: Boolean);
      procedure CreateSpinButton;
      procedure SetEditRect;
      procedure UpClick(Sender: TObject); virtual; abstract;
      procedure DownClick(Sender: TObject); virtual; abstract;
      procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure KeyPress(var Key: Char); override;
      procedure WMSizeSpinButton(var Msg: TMessage); message WM_USER;
      procedure WMSize(var Msg: TWMSize); message WM_SIZE;
      procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure Change; override;
    public
      procedure SetFocus; override;
      procedure CreateParams(var Params: TCreateParams); override;
      constructor Create(AOwner: TComponent); override;
      // Defines for expression, set to DefaultDefines on create
      property Defines: TSymbolTable read FDefines write FDefines;
      // True when value is valid
      property Valid: Boolean read FValid;
      function ValidOrDisabled: Boolean;
      // Form mouse wheel handler for changing focus
      class procedure FormMouseWheelSetFocusControl(Sender: TObject; Shift: TShiftState;
        WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    published
      property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
      // Occures when the value changes or the text entered becomes invalid
      property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
      // If SimpleValue is True, math expressions are not allowed
      property SimpleValue: Boolean read FSimpleValue write FSimpleValue default False;
      // Text color when value is invalid
      property InvalidTextColor: TColor read FInvalidTextColor write FInvalidTextColor default clMaroon;
      property FocusOnMouseWheel: Boolean read FFocusOnMouseWheel write SetFocusOnMouseWheel default True;
      property AutoSelect;
      property Anchors;
      property Constraints;
      property DragKind;
      property DragMode;
      property OnEndDock;
      property OnStartDock;
      property AutoSize;
      property BorderStyle;
      property Color;
      property Ctl3D;
      property DragCursor;
      property Enabled;
      property Font;
      property HideSelection;
      property MaxLength;
      property ParentColor;
      property ParentCtl3D;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Visible;
      property OnClick;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDrag;
    end;

  TIntegerEdit = class(TValueEdit)
    protected
      FValue, FMin, FMax : Integer;
      FSpinIncrement : Integer;
      procedure OnChangeText(Sender: TObject);
      procedure SetMax(Max: Integer);
      procedure SetMin(Min: Integer);
      procedure SetValue(Value: Integer);
      procedure SetText;
      procedure SetSpinIncrement(const Value: Integer);
      procedure UpClick(Sender: TObject); override;
      procedure DownClick(Sender: TObject); override;
    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
    published
      { Published declarations }
      property Max: Integer read FMax write SetMax default 100;
      property Min: Integer read FMin write SetMin default 0;
      property SpinIncrement: Integer read FSpinIncrement write SetSpinIncrement default 0;
      property Value: Integer read FValue write SetValue default 0;
    end;

  TFloatEdit = class(TValueEdit)
    protected
      FValue, FMin, FMax : EvalFloat;
      FFormatString : string;
      FSpinIncrement : EvalFloat;
      procedure OnChangeText(Sender: TObject);
      procedure SetMax(Max: EvalFloat);
      procedure SetMin(Min: EvalFloat);
      procedure SetValue(Value: EvalFloat);
      procedure SetText;
      procedure SetFormatString(const Value: string);
      function GetIntValue: Int64;
      procedure SetSpinIncrement(const Value: EvalFloat);
      procedure UpClick(Sender: TObject); override;
      procedure DownClick(Sender: TObject); override;
    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
      function Evaluate: EvalFloat;
      property IntValue: Int64 read GetIntValue;
      procedure SetValueCheck(Value: EvalFloat; NotifyChangeValue: Boolean);
    published
      { Published declarations }
      property Max: EvalFloat read FMax write SetMax;
      property Min: EvalFloat read FMin write SetMin;
      property SpinIncrement: EvalFloat read FSpinIncrement write SetSpinIncrement;
      property Value: EvalFloat read FValue write SetValue;
      property FormatString: string read FFormatString write SetFormatString;
    end;

{$IFDEF UseStyleForm}
function IntegerQuery(const ACaption, APrompt: string; var Value: Integer; Min, Max: Integer; DialogWidth: Integer=120; SpinIncrement: Integer=0; OnChangeValue: TNotifyEvent=nil): Boolean;
function FloatQuery(const ACaption, APrompt: string; var Value: EvalFloat; Min, Max: EvalFloat; DialogWidth: Integer=120; SpinIncrement: EvalFloat=0): Boolean;
{$ENDIF}

procedure InstallMouseWheelFocusEvent(Form: TForm);

procedure Register;

implementation

{$R ValueEdit.res}

uses Types, ComCtrls, EventUtils;

procedure InstallMouseWheelFocusEvent(Form: TForm);
begin
  Assert(not Assigned(Form.OnMouseWheel),'OnMouseWheel already set on form');
  Form.OnMouseWheel:=TValueEdit.FormMouseWheelSetFocusControl;
end;

// Add value edit control to global list and set parent form client proc
procedure InstallFormClientProc(Control: TValueEdit);
var
  Form : TWinControl;
begin
  // Find parent form
  Form:=Control.Parent;
  while (Form<>nil) and not (Form is TForm) do Form:=Form.Parent;
  if Form=nil then Exit;

  // Set form mouse wheel event if it is not already set
  if not Assigned(TForm(Form).OnMouseWheel) then
    TForm(Form).OnMouseWheel:=TValueEdit.FormMouseWheelSetFocusControl;

  Control.ParentForm:=TForm(Form);
end;

//==============================================================================================================================
// TValueEdit
//==============================================================================================================================
constructor TValueEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Assert(MonitorComponent(Self));
  FInvalidTextColor:=clMaroon;
  FFocusOnMouseWheel:=True;
  if not (csDesigning in ComponentState) then FDefines:=DefaultDefines;
end;

procedure TValueEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Word = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style:=Params.Style or Alignments[Alignment];
  if Assigned(FSpinButton) then Params.Style:=Params.Style or WS_CLIPCHILDREN or ES_MULTILINE;
end;

function TValueEdit.ValidOrDisabled: Boolean;
begin
  Result:=Valid or not Enabled;
end;

procedure TValueEdit.SetAlignment(const Value: TAlignment);
begin
  if (Value<>FAlignment) and not ((Win32MajorVersion=5) and (Win32MinorVersion=1)) then
  begin
    FAlignment:=Value;
    RecreateWnd;
    RestoreRightAlign:=0;
  end;
end;

procedure TValueEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  if Assigned(FSpinButton) then
  begin
    FUpButton.Enabled:=Value;
    FDownButton.Enabled:=Value;
  end;
end;

procedure TValueEdit.SetFocusOnMouseWheel(const Value: Boolean);
begin
  if FFocusOnMouseWheel<>Value then
  begin
    FFocusOnMouseWheel:=Value;
    if Value then InstallFormClientProc(Self)
    //else RemoveFormClientProc(Self);
  end;
end;

class procedure TValueEdit.FormMouseWheelSetFocusControl(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Control : TWinControl;
  Child : TControl;
begin
  Control:=Sender as TCustomForm;
  repeat
    Child:=Control.ControlAtPos(Control.ScreenToClient(MousePos),False,True);
    if not (Child is TWinControl) then
    begin
      if not Control.Focused and (TCustomForm(Sender).ActiveControl<>Control) and Control.CanFocus then
      begin
        if Control is TCustomListControl then
        begin
          Control.SetFocus;
          Handled:=True;
        end
        else if Control is TTrackBar then
        begin
          Control.SetFocus;
          if WheelDelta>0 then TTrackBar(Control).Position:=TTrackBar(Control).Position-TTrackBar(Control).LineSize
          else if WheelDelta<0 then TTrackBar(Control).Position:=TTrackBar(Control).Position+TTrackBar(Control).LineSize;
          Handled:=True;
        end
        else if (Control is TValueEdit) and Assigned(TValueEdit(Control).FSpinButton) then
        begin
          Control.SetFocus;
          if WheelDelta>0 then TValueEdit(Control).DoMouseWheelUp(Shift,MousePos)
          else if WheelDelta<0 then TValueEdit(Control).DoMouseWheelDown(Shift,MousePos);
          Handled:=True;
        end;
      end;
      Break;
    end;
    Control:=TWinControl(Child);
  until False;
end;

procedure TValueEdit.Change;
var
  P : Integer;
begin
  if Assigned(FSpinButton) then
  begin
    if Alignment=taRightJustify then // Change from right to left aligned when the box is full
    begin
      if SendMessage(Handle,EM_GETLINECOUNT,0,0)>1 then
      begin
        if SelLength=0 then P:=SelStart
        else P:=-1;
        Alignment:=taLeftJustify;
        if P<>-1 then SelStart:=P;
        RestoreRightAlign:=Length(Text);
      end;
    end
    else if (RestoreRightAlign<>0) and (SendMessage(Handle,EM_GETLINECOUNT,0,0)<=1) and (Length(Text)<RestoreRightAlign) then
    begin
      if SelLength=0 then P:=SelStart
      else P:=-1;
      Alignment:=taRightJustify;
      if P<>-1 then SelStart:=P;
    end;
  end;
  inherited;
end;

procedure TValueEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
  if AutoSelect and (SelLength=0) then SelectAll;
  if (FSpinButton<>nil) and FocusOnMouseWheel then
    InstallFormClientProc(Self);
end;

procedure TValueEdit.SetFocus;
begin
  inherited;
  if AutoSelect and (SelLength=0) then SelectAll;
end;

procedure TValueEdit.CreateSpinButton;

  procedure PrepareGlyph(Bitmap: TBitmap);
  var
    X, Y, W : Integer;
    Pix : PColor;
  begin
    // Delete top line
    Bitmap.Canvas.Draw(0,-1,Bitmap);
    Bitmap.Height:=Bitmap.Height-1;
    // Make copy of the arrow
    W:=Bitmap.Width;
    Bitmap.Width:=W*2;
    Bitmap.Canvas.Draw(W,0,Bitmap);
    // Color the copy gray (for display when disabled) 
    Bitmap.PixelFormat:=pf32bit;
    for Y:=0 to Bitmap.Height-1 do
    begin
      Pix:=Pointer(Integer(Bitmap.ScanLine[Y])+W*4);
      for X:=1 to W-1 do
      begin
        if Pix^=clBlack then
        begin
          Pix^:=clDkGray;
          Inc(Pix);
          if Pix^<>clBlack then Pix^:=clWhite; // Make white "shadow" 
        end
        else Inc(Pix);
      end;
    end;
  end;

begin
  if FSpinButton=nil then
  begin
    FSpinButton := TSpinButton.Create(Self);
    FSpinButton.Width := 15;
    FSpinButton.Height := 17;
    FSpinButton.Visible := True;
    FSpinButton.Parent := Self;
    FSpinButton.FocusControl := Self;
    FSpinButton.OnDownClick:=DownClick;
    FSpinButton.OnUpClick:=UpClick;

    FUpButton:=FSpinButton.Components[0] as TSpeedButton;
    FDownButton:=FSpinButton.Components[1] as TSpeedButton;
    PrepareGlyph(FUpButton.Glyph);
    FUpButton.NumGlyphs:=2;
    PrepareGlyph(FDownButton.Glyph);
    FDownButton.NumGlyphs:=2;

    if not Enabled then
    begin
      FUpButton.Enabled:=False;
      FDownButton.Enabled:=False;
    end;
    if Alignment=taRightJustify then
    begin
      // This is sometimes necessary to avoid that the text gets behind the spin buttons
      RestoreParentFont:=ParentFont;
      Font.OnChange(Self);
    end;
  end;
end;

procedure TValueEdit.WMSizeSpinButton(var Msg: TMessage);
var
  W, H : Integer;
begin
  W:=FSpinButton.Width;
  H:=FSpinButton.Height;
  FUpButton.SetBounds(0,0,W,(H+1) div 2);
  FDownButton.SetBounds(0,FUpButton.Height-1,W,H-FUpButton.Height+1);
  if RestoreParentFont and (Application.MainForm<>nil) then Font.Handle:=Application.MainForm.Font.Handle;
end;

procedure TValueEdit.SetEditRect;
var
  Loc: TRect;
begin
  if FSpinButton<>nil then
  begin
    Loc.Bottom:=ClientHeight+1;  {+1 is a workaround for windows paint bug}
    Loc.Right:=ClientWidth-FSpinButton.Width-1;
    Loc.Top:=0;
    Loc.Left:=0;
    SendMessage(Handle,EM_SETRECTNP,0,LongInt(@Loc));
  end;
end;

procedure TValueEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  if Assigned(FSpinButton) then
  begin
    if NewStyleControls and Ctl3D then
      FSpinButton.SetBounds(Width - FSpinButton.Width - 4, 0, FSpinButton.Width, Height - 4)
    else FSpinButton.SetBounds (Width - FSpinButton.Width-1, 1, FSpinButton.Width, Height - 3);
    SetEditRect;
    PostMessage(Handle,WM_USER,0,0); // Size spin buttons
  end;
end;

procedure TValueEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TValueEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FSpinButton) then
  case Key of
    VK_UP     : UpClick(Self);
    VK_DOWN   : DownClick (Self);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TValueEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key=Chr(VK_RETURN) then Key:=#0;
end;

procedure TValueEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result:=Msg.Result and not DLGC_WANTALLKEYS;
end;

{procedure TValueEdit.WMMouseWheel(var Msg: TMessage);
begin
  if ParentForm<>nil then
    Msg.Result:=FormClientProc(ParentFormHandle,WM_MOUSEWHEEL,Msg.WParam,Msg.LParam);
  if Msg.Result=0 then
    inherited;
end;}

function TValueEdit.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:=False;
  if ParentForm<>nil then FormMouseWheelSetFocusControl(ParentForm,Shift,-1,MousePos,Result);
  if not Result then
  begin
    Result:=Assigned(FSpinButton);
    if Result then DownClick(Self);
  end;
end;

function TValueEdit.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:=False;
  if ParentForm<>nil then FormMouseWheelSetFocusControl(ParentForm,Shift,+1,MousePos,Result);
  if not Result then
  begin
    Result:=Assigned(FSpinButton);
    if Result then UpClick(Self);
  end;
end;

//==============================================================================================================================
// TIntegerEdit
//==============================================================================================================================

constructor TIntegerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax:=100;
  SetText;
end;

procedure TIntegerEdit.SetValue(Value: Integer);
begin
  if Value>FMax then FValue:=FMax
  else if Value<FMin then FValue:=FMin
  else FValue:=Value;
  SetText;
end;

procedure TIntegerEdit.SetMax(Max: Integer);
begin
  FMax:=Max;
  if Value>FMax then
  begin
    FValue:=FMax;
    SetText;
  end;
end;

procedure TIntegerEdit.SetMin(Min: Integer);
begin
  FMin:=Min;
  if Value<FMin then
  begin
    FValue:=FMin;
    SetText;
  end;
end;

procedure TIntegerEdit.SetText;
begin
  OnChange:=nil;
  if not FValid then
  begin
    Font.Color:=clWindowText;
    SetEditRect;
    FValid:=True;
  end;
  Text:=IntToStr(FValue);
  OnChange:=OnChangeText;
end;

procedure TIntegerEdit.DownClick(Sender: TObject);
begin
  if SpinIncrement>=0 then Value:=Value-SpinIncrement
  else Value:=-Value div SpinIncrement;
  SelStart:=Length(Text);
  if Assigned(OnChangeValue) then OnChangeValue(Self);
end;

procedure TIntegerEdit.UpClick(Sender: TObject);
begin
  if SpinIncrement>=0 then Value:=Value+SpinIncrement
  else Value:=-Value*SpinIncrement;
  SelStart:=Length(Text);
  if Assigned(OnChangeValue) then OnChangeValue(Self);
end;

procedure TIntegerEdit.SetSpinIncrement(const Value: Integer);
begin
  FSpinIncrement:=Value;
  if Value=0 then FreeAndNil(FSpinButton)
  else CreateSpinButton;
  RecreateWnd;
end;

procedure TIntegerEdit.OnChangeText(Sender: TObject);
var
  V, T : Integer;
  F : EvalFloat;
begin
  if SimpleValue then Val(Text,V,T)
  else
  try // Evaluate expression
    F:=EvaluateExpression(Text,Defines);
    V:=Round(F);
    if Abs(V-F)<=MaxIntegerDifference then T:=0
    else T:=1;
  except
    T:=1; V:=0;
  end;

  if T<>0 then
  begin
    if FValid then // Change from valid to invalid
    begin
      Font.Color:=InvalidTextColor;
      SetEditRect;
      FValid:=False;
      SendMessage(Handle,EM_SCROLLCARET,0,0);
      if Assigned(OnChangeValue) then OnChangeValue(Self);
    end;
  end
  else if (V<Min) or (V>Max) then // Out of range
  begin
    if FValid then
    begin
      Font.Color:=InvalidTextColor;
      SetEditRect;
      FValid:=False;
      SendMessage(Handle,EM_SCROLLCARET,0,0);
    end;
    if V<Min then V:=Min
    else V:=Max;
    if V<>FValue then
    begin
      FValue:=V;
      if Assigned(OnChangeValue) then OnChangeValue(Self);
    end;
  end
  else // Ok
  begin
    if not FValid then
    begin
      Font.Color:=clWindowText;
      SetEditRect;
      FValid:=True;
      SendMessage(Handle,EM_SCROLLCARET,0,0);
    end;
    FValue:=V;
    if Assigned(OnChangeValue) then OnChangeValue(Self);
  end;
end;

//==============================================================================================================================
//                TFloatEdit
//==============================================================================================================================

constructor TFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax:=100;
  FFormatString:='0.##';
  SetText;
end;

procedure TFloatEdit.SetValue(Value: EvalFloat);
begin
  if (Value<>FValue) or not Valid then
  begin
    if Value>FMax then FValue:=FMax
    else if Value<FMin then FValue:=FMin
    else FValue:=Value;
    SetText;
  end;
end;

procedure TFloatEdit.SetValueCheck(Value: EvalFloat; NotifyChangeValue: Boolean);
begin
  if (Value<>FValue) or not Valid then
  begin
    if (Value<FMin) or (Value>FMax) then
      raise EExpressionError.CreateFmt(rsValueNotInAllowedRangeSS,[FormatFloat(FormatString,Min),FormatFloat(FormatString,Max)]);
    FValue:=Value;
    SetText;
    if NotifyChangeValue and Assigned(OnChangeValue) then OnChangeValue(Self);
  end;
end;

procedure TFloatEdit.SetMax(Max: EvalFloat);
begin
  FMax:=Max;
  if Value>FMax then
  begin
    FValue:=FMax;
    SetText;
  end;
end;

procedure TFloatEdit.SetMin(Min: EvalFloat);
begin
  FMin:=Min;
  if Value<FMin then
  begin
    FValue:=FMin;
    SetText;
  end;
end;

procedure TFloatEdit.SetText;
begin
  OnChange:=nil;
  if not FValid then
  begin
    Font.Color:=clWindowText;
    SetEditRect;
    FValid:=True;
  end;
  Text:=FormatFloat(FormatString,FValue);
  OnChange:=OnChangeText;
end;

procedure TFloatEdit.SetFormatString(const Value: string);
begin
  FFormatString:=Value;
  SetText;
end;

procedure TFloatEdit.DownClick(Sender: TObject);
begin
  if SpinIncrement<0 then Value:=-Value/SpinIncrement
  else if GetKeyState(VK_SHIFT)<0 then Value:=Value-SpinIncrement/10
  else Value:=Value-SpinIncrement;
  SelStart:=Length(Text);
  if Assigned(OnChangeValue) then OnChangeValue(Self);
end;

procedure TFloatEdit.UpClick(Sender: TObject);
begin
  if SpinIncrement<0 then Value:=-Value*SpinIncrement
  else if GetKeyState(VK_SHIFT)<0 then Value:=Value+SpinIncrement/10
  else Value:=Value+SpinIncrement;
  SelStart:=Length(Text);
  if Assigned(OnChangeValue) then OnChangeValue(Self);
end;

procedure TFloatEdit.SetSpinIncrement(const Value: EvalFloat);
begin
  FSpinIncrement:=Value;
  if Value=0 then FreeAndNil(FSpinButton)
  else CreateSpinButton;
  RecreateWnd;
end;

procedure TFloatEdit.OnChangeText(Sender: TObject);
var
  V : EvalFloat;
  T : Integer;
begin
  if SimpleValue then Val(ValStr(Text),V,T)
  else
  try // Evaluate expression
    V:=EvaluateExpression(Text,Defines);
    T:=0;
  except
    T:=1; V:=0;
  end;

  if T<>0 then
  begin
    if FValid then // Change from valid to invalid
    begin
      Font.Color:=InvalidTextColor;
      SetEditRect;
      FValid:=False;
      SendMessage(Handle,EM_SCROLLCARET,0,0);
      if Assigned(OnChangeValue) then OnChangeValue(Self);
    end;
  end
  else if (V<Min) or (V>Max) then // Out of range
  begin
    if FValid then
    begin
      Font.Color:=InvalidTextColor;
      SetEditRect;
      FValid:=False;
      SendMessage(Handle,EM_SCROLLCARET,0,0);
    end;
    if V<Min then V:=Min
    else V:=Max;
    if V<>FValue then
    begin
      FValue:=V;
      if Assigned(OnChangeValue) then OnChangeValue(Self);
    end;
  end
  else // Ok
  begin
    if not FValid then
    begin
      Font.Color:=clWindowText;
      SetEditRect;
      FValid:=True;
      SendMessage(Handle,EM_SCROLLCARET,0,0);
    end;
    FValue:=V;
    if Assigned(OnChangeValue) then OnChangeValue(Self);
  end;
end;

function TFloatEdit.GetIntValue: Int64;
begin
  Result:=Round(Value);
end;

function TFloatEdit.Evaluate: EvalFloat;
begin
  Result:=EvaluateExpression(Text,Defines);
  if Result<Min then Result:=Min
  else if Result>Max then Result:=Max;
end;

//==============================================================================================================================

{$IFDEF UseStyleForm}
function IntegerQuery(const ACaption, APrompt: string; var Value: Integer; Min, Max,DialogWidth,SpinIncrement: Integer;
  OnChangeValue: TNotifyEvent): Boolean;
var
  Form: TStyleForm;
  Prompt: TLabel;
  Edit: TIntegerEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  TestVal : EvalFloat;
  NonClientMetrics: TNonClientMetrics;
begin
  Form:=TStyleForm.CreateNewWithParent(Application,GetActiveFormHandle);
  with Form do
    try
      UseBackgroundTheme:=True;
      NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
      if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
        Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(DialogWidth, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Transparent:=True;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;
      Edit := TIntegerEdit.Create(Form);
      Edit.Min:=Min;
      Edit.Max:=Max;
      Edit.Value:=Value;
      Edit.SpinIncrement:=SpinIncrement;
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top+Prompt.Height+MulDiv(3,DialogUnits.Y,8);
        Width := Form.ClientWidth-2*Left;
        SelectAll;
      end;
      Edit.OnChangeValue:=OnChangeValue;
      ButtonTop := Edit.Top+Edit.Height+MulDiv(9,DialogUnits.Y,8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth div 2-MulDiv(2,DialogUnits.X,4)-ButtonWidth, ButtonTop, ButtonWidth,ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth div 2+MulDiv(2,DialogUnits.X,4),ButtonTop,ButtonWidth,ButtonHeight);
      end;
      ClientHeight:=ButtonTop+ButtonHeight+MulDiv(8,DialogUnits.Y,8);
      repeat
        Result:=ShowModal=mrOk;
        if Result and not Edit.Valid then
        try
          try
            TestVal:=EvaluateExpression(Edit.Text,Edit.Defines);
            if Abs(TestVal-Round(TestVal))>MaxIntegerDifference then raise EExpressionError.Create(rsValueMustBeInteger)
            else raise EExpressionError.CreateFmt(rsValueNotInAllowedRangeSS,[IntToStr(Min),IntToStr(Max)]);
          except
            on E: EMathError do raise EExpressionError.Create(E.Message);
          end;
        except
          Application.HandleException(nil);
        end;
      until Edit.Valid or not Result;
      if Result then Value:=Edit.Value;
    finally
      Form.Free;
    end;
end;

function FloatQuery(const ACaption, APrompt: string; var Value: EvalFloat; Min, Max: EvalFloat; DialogWidth: Integer; SpinIncrement: EvalFloat): Boolean;
var
  Form: TStyleForm;
  Prompt: TLabel;
  Edit: TFloatEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  NonClientMetrics: TNonClientMetrics;
begin
  Form:=TStyleForm.CreateNewWithParent(Application,GetActiveFormHandle);
  with Form do
    try
      UseBackgroundTheme:=True;
      NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
      if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
        Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(DialogWidth, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Transparent:=True;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;
      Edit := TFloatEdit.Create(Form);
      Edit.Min:=Min;
      Edit.Max:=Max;
      Edit.Value:=Value;
      Edit.SpinIncrement:=SpinIncrement;
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top+Prompt.Height+MulDiv(3,DialogUnits.Y,8);
        Width := Form.ClientWidth-2*Left;
        SelectAll;
      end;
      ButtonTop := Edit.Top+Edit.Height+MulDiv(9,DialogUnits.Y,8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth div 2-MulDiv(2,DialogUnits.X,4)-ButtonWidth, ButtonTop, ButtonWidth,ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth div 2+MulDiv(2,DialogUnits.X,4),ButtonTop,ButtonWidth,ButtonHeight);
      end;
      ClientHeight:=ButtonTop+ButtonHeight+MulDiv(8,DialogUnits.Y,8);
      repeat
        Result:=ShowModal=mrOk;
        if Result and not Edit.Valid then
        try
          EvaluateExpression(Edit.Text,Edit.Defines);
          raise EExpressionError.CreateFmt(rsValueNotInAllowedRangeSS,[
            FormatFloat(Edit.FormatString,Min),
            FormatFloat(Edit.FormatString,Max)]);
        except
          Application.HandleException(nil);
        end;
      until Edit.Valid or not Result;
      if Result then Value:=Edit.Value;
    finally
      Form.Free;
    end;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Samples',[TIntegerEdit,TFloatEdit]);
end;

end.

