////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// StyleForm.pas - Form and dialogs with alternate caption bar
// -----------------------------------------------------------
// Changed:   2008-03-15
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// TStyleForm is based on TdfsGradientForm v2.03 copyright 2000-2001, Brad Stowers.
//
// Last changes:
//   PreventFormScrollbars added
//   Exception dialog
//   Menu bar updated properly OnActivate
//   DisableProcessWindowsGhosting;
//   Theme background
//   XP theme support
//   Vista Alt key bugfix
//   Conditional define VistaTaskbarButton
unit StyleForm;

interface

uses
  {$IFDEF VER140} RTLConsts, {$ENDIF} Consts, Windows, Messages, SysUtils,
  Forms, Classes, Graphics, Controls, Dialogs, Math, ExtCtrls, StdCtrls, MemUtils,
  {$IFDEF UseStyleForm} {$ENDIF}
  Menus, ShellAPI, Comctrls, FileUtils, SysConst, Themes, ImgList, Buttons;

{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

//{$D-}

const
  VK_OEM_PLUS  = $BB;
  VK_OEM_MINUS = $BD;

  SC_TITLEDBLCLICK = 61490;

  CSIDL_MYDOCUMENTS = $000C;

  WM_XBUTTONDOWN  = $020B;
  WM_XBUTTONUP    = $020C;

  APPCOMMAND_BROWSER_BACKWARD = $8001;
  APPCOMMAND_BROWSER_FORWARD  = $8002;

{$IFNDEF UseStyleForm}

type TStyleForm = TForm;

{$ELSE}

resourcestring
  rsTerminateApplication = 'Terminate application';
  rsSendBugReport = 'Send bug report';

type
  TBackgroundRenderer = procedure(Bitmap: TBitmap);

  TStyleForm = class(TForm)
  private
    // Internal variables
    FCaptionFont: TFont;
    FCreating : Boolean;
    FUseBackgroundTheme : Boolean;
    C1Active, C2Active, C1Inactive, C2Inactive, C1Bright, C2Bright : TColor;
    FShowTaskBarButton : Boolean;
    FOnHelpButtonClick : TNotifyEvent;
    CaptionMouseDown : Boolean;
    CaptionMouseDownPos : TPoint;

    // Window message handlers
    procedure WMNCActivate(var Msg: TWMNCActivate); message WM_NCACTIVATE;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMSysColorChange(var Msg: TWMSysColorChange); message WM_SYSCOLORCHANGE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMSettingChange(var Msg: TMessage); message WM_SETTINGCHANGE;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Msg: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCMouseMove(var Msg: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMEnterIdle(var Msg: TWMEnterIdle); message WM_ENTERIDLE;
    procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    //procedure WMEndSession(var Msg: TWMEndSession); message WM_EndSession; // No longer necessary in Delphi 7 sp1
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMUpdateUIState(var Msg: TWMUpdateUIState); message WM_UPDATEUISTATE;
    // Misc
    function GetSysCaptionLogFont: TLogFont;
    // Utility methods
    procedure UpdateCaption(Active: Boolean);
    procedure UpdateCaptionFont;
    procedure UpdateColors;
    procedure InvalidateCaption;
    function GetCaptionRect: TRect;
    function DrawCaption(FormDC: HDC; Active: Boolean): TRect;
    function DrawThemeCaption(FormDC: HDC; Active: Boolean): TRect;
    procedure PaintMenuIcon(DC: HDC; var R: TRect; Active: Boolean);
    procedure PaintCaptionText(DC: HDC; R: TRect; Active: Boolean; const Text: string);
    procedure PaintCaptionButtons(DC: HDC; var Rect: TRect);
    function GetUseBackgroundTheme: Boolean;
    procedure SetShowTaskBarButton(const Value: Boolean);
    procedure GetWindowFrameSizeChange(out DeltaFrameX, DeltaFrameYTop, DeltaFrameYBottom: Integer);
    function GetFrameSize: TPoint;
    function IsActiveWindow: boolean;
  protected
    FBackgroundBitmap : TBitmap;
    FParentHandle : THandle;
    procedure DoCreate; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    class procedure StyleFormExceptionEvent(Sender: TObject; E: Exception);
    procedure ChangeWindowSizeIgnoringAnchors(DeltaX, DeltaY: Integer);
  public
    // Event for the ? taskbar button
    property OnHelpButtonClick: TNotifyEvent read FOnHelpButtonClick write FOnHelpButtonClick;
    // Show window button in Windows task bar (only valid for forms other than the main form)
    property ShowTaskBarButton: Boolean read FShowTaskBarButton write SetShowTaskBarButton;
    // Call from OnCreate to prevent display of scrollbars when MDI children move outside the borders
    procedure PreventFormScrollbars;
    procedure MoveWindowTo(Pos: TPoint);
    property ParentHandle: THandle read FParentHandle;
    property UseBackgroundTheme: Boolean read GetUseBackgroundTheme write FUseBackgroundTheme;
    constructor Create(AOwner: TComponent; ParentHandle: THandle); reintroduce; overload;
    constructor Create(AOwner: TComponent); overload; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    constructor CreateNewWithParent(AOwner: TComponent; ParentHandle: THandle);
    destructor Destroy; override;
  end;

var
  BackgroundRenderer : TBackgroundRenderer = nil;

function StyleFormActive: Boolean;
function GetSystemColorBitDepth: integer;
// Return the handle of the active window or nil if it cannot be determined
function GetActiveFormHandle: THandle;

// Look up code address in MAP file to determine source code location
function LookupCodeLocation(Address: Pointer; MapFileName: string=''): string;
// Trace call stack. Last entry marked with 0.
procedure GetStackTrace(SkipLevels,MaxLevels: Integer; DestArray: PInteger);
// Comvert stack trace to text line using LookupCodeLocation
function StackTraceToString(CallStackArray: PInteger; const Separator: string): string;

{$ENDIF}

type
  TMenuItemEx = class(TMenuItem)
    public
      // Fixes bug in menu item rendering under WinXP
      class procedure AdvancedDrawTopItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    end;

  TSpeedButtonEx = class(TSpeedButton)
    protected
      // Fixes bug when paiting glyph on Vista
      procedure Paint; override;
    end;

// Turn a label into a link. The Hint property must be the link target.
procedure MakeLinkLabel(LinkLabel: TLabel);
// Set the width of a check box after the length of it's text
procedure AutoSizeCheckBox(CheckBox: TCheckBox);
// Set control position by right edge
procedure RightAlignControl(Control: TWinControl; Right: Integer);
procedure BottomAlignControl(Control: TWinControl; Bottom: Integer);
// Clear label captions if Label.Caption=Label.Name
procedure ClearDelphiLabelCaptions(Control: TWinControl);
// Reset anchors in all child controls to [akLeft,akTop]
procedure ResetControlAnchors(Control: TControl);
// Change visibility of a separator (or button) in a tool bar
procedure SetSeparatorVisible(var Separator: TToolButton; AVisible: Boolean; ALeft: Integer=Low(Integer));
// Enable or disable control and all children
procedure RecursiveSetEnabled(Control: TControl; Enabled: Boolean);

type
  TMsgDlgBtn = (mbYes,mbNo,mbYesToAll,mbNoToAll,mbAll,mbOK,mbRetry,mbIgnore,mbAbort,mbCancel,mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  mbYesNo = [mbYes,mbNo];
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  mbAbortIgnore = [mbAbort, mbIgnore];

function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TStyleForm;
function CreateMessageDialogStr(const Msg: string; DlgType: TMsgDlgType; const Buttons: array of const; CancelButton: Integer=10000): TStyleForm;
function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer;
function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons=[mbOk]; HelpCtx: Longint=0): Integer;
// Result is button number (1,2,3...) or 0 if window closed (Alt-F4). Default is escape will press last button.
function MessageDlgStr(const Msg: string; DlgType: TMsgDlgType; const Buttons: array of const; CancelButton: Integer=10000): Integer;

procedure ShowMessage(Value: Integer); overload;
procedure ShowMessage(const Msg: string); overload;

type
  TInputQueryForm = class(TStyleForm)
  public
    Prompt : TLabel;
    Edit : TEdit;
    OKButton : TButton;
  end;

function InputQuery(const ACaption, APrompt: string; var Value: string; DialogWidth: Integer=180; PasswordQuery: Boolean=False; OnChangeText: TNotifyEvent=nil): Boolean;
function GetAveCharSize(Canvas: TCanvas): TPoint;

// Specify either default file or folder in FileName or leave it empty
function SaveFileDialog(var FileName: string; const Filter: string;
  Options: TOpenOptions=[ofHideReadOnly,ofPathMustExist,ofEnableSizing,ofOverwritePrompt,ofNoReadOnlyReturn]): Boolean;
function OpenFileDialog(var FileName: string; const Filter: string;
  Options: TOpenOptions=[ofHideReadOnly,ofFileMustExist,ofEnableSizing]): Boolean;
// Set FilterIndex based on DefaultExt in save dialog
procedure SetupSaveDialogFilter(Dialog: TSaveDialog; const FileName: string);

function ColorTone(Col1,Col2: TColor; Tone,Max: Integer): TColor;
function GrayColor(Shade: Byte): TColor;
// 6*brightness
function ColorBrightness(Col: TColor): Integer;

// Get TRect with Windows taskbar
function GetTaskBarRect: TRect;

type
  // For write access to application handle
  TOpenApplication = class(TComponent)
  public
    FHandle: HWnd;
  end;

// Temporarely change Application.Handle to give Delphi's default dialogs another parent (0 for main form)
// (only works when VistaTaskbarButton is defined)
// If this is not done, dialogs may end up behind the main form if clicking on the main form or the task bar button.
// FocusDialog - dialog to give keyboard focus so that e.g. Esc works
procedure SetApplicationHandleForDialog(TempHandle: THandle=0; FocusDialog: TCommonDialog=nil);
procedure RestoreApplicationHandle;

implementation

uses Types, Registry, StrUtils, TranslationTools, StringUtils, EMailUtils, VersionInfo,
  StringLists, EventUtils, WinAPIUtils;

function ColorTone(Col1,Col2: TColor; Tone,Max: Integer): TColor;
var
  Scale : Single;
begin
  with TPaletteEntry(Result) do
  begin
    Scale:=1/Max;
    peRed:=Round((TPaletteEntry(Col1).peRed*Tone+TPaletteEntry(Col2).peRed*(Max-Tone))*Scale);
    peGreen:=Round((TPaletteEntry(Col1).peGreen*Tone+TPaletteEntry(Col2).peGreen*(Max-Tone))*Scale);
    peBlue:=Round((TPaletteEntry(Col1).peBlue*Tone+TPaletteEntry(Col2).peBlue*(Max-Tone))*Scale);
    peFlags:=0;
  end;
end;

function GrayColor(Shade: Byte): TColor;
begin
  Result:=Shade or (Shade shl 8) or (Shade shl 16);
end;

// 6*brightness
function ColorBrightness(Col: TColor): Integer;
begin
  with TPaletteEntry(Col) do Result:=peRed*2+peGreen*3+peBlue;
end;

procedure RightAlignControl(Control: TWinControl; Right: Integer);
begin
  Control.Left:=Right-Control.Width;
end;

procedure BottomAlignControl(Control: TWinControl; Bottom: Integer);
begin
  Control.Top:=Bottom-Control.Height;
end;

type
  TCommonDialogHelper = class(TComponent)
  private
    Dialog : TCommonDialog;
    procedure Activate(Sender: TObject);
  public
    procedure DialogShown(Sender: TObject);
  end;

procedure TCommonDialogHelper.DialogShown(Sender: TObject);
begin
  Dialog:=Sender as TCommonDialog;
  with TTimer.Create(Dialog) do
  begin
    Interval:=10;
    OnTimer:=Activate;
    Enabled:=True;
  end;
end;

procedure TCommonDialogHelper.Activate(Sender: TObject);
begin
  SetFocus(Dialog.Handle);
  (Sender as TTimer).Enabled:=False;    
end;

var
  ApplicationHandle : THandle = 0;

procedure SetApplicationHandleForDialog(TempHandle: THandle; FocusDialog: TCommonDialog);
begin
  {$IFDEF VistaTaskbarButton}
  if TempHandle=0 then TempHandle:=Application.MainForm.Handle;
  if ApplicationHandle=0 then ApplicationHandle:=Application.Handle;
  Assert(TOpenApplication(Application).FHandle=ApplicationHandle);
  TOpenApplication(Application).FHandle:=TempHandle;
  if FocusDialog<>nil then FocusDialog.OnShow:=TCommonDialogHelper.Create(FocusDialog).DialogShown;
  {$ENDIF}
end;

procedure RestoreApplicationHandle;
begin
  {$IFDEF VistaTaskbarButton}
  Assert(ApplicationHandle<>0);
  TOpenApplication(Application).FHandle:=ApplicationHandle;
  {$ENDIF}
end;

{$IFDEF UseStyleForm}
procedure CaptionBox(Canvas: TCanvas; Rect: TRect; TopColor,BottomColor,FillColor: TColor);
var
  TopRight, BottomLeft: TPoint;
begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  with Canvas, Rect do
  begin
    TopRight.X := Right;
    TopRight.Y := Top;
    BottomLeft.X := Left;
    BottomLeft.Y := Bottom;
    Pen.Color := TopColor;
    PolyLine([BottomLeft, TopLeft, TopRight]);
    Pen.Color := BottomColor;
    Dec(BottomLeft.X);
    PolyLine([TopRight, BottomRight, BottomLeft]);
  end;
  Inc(Rect.Top); Inc(Rect.Left);
  Canvas.Brush.Color:=FillColor;
  Canvas.FillRect(Rect);
end;

//==============================================================================================================================
// TStyleForm
//==============================================================================================================================

var
  UseStyleCaption : Boolean = True;
  DisableDrawing : Boolean = False;

function StyleFormActive: Boolean;
begin
  Result:=UseStyleCaption;
end;

function GetSystemColorBitDepth: Integer;
var
  DC : HDC;
begin
  DC:=GetDC(0);
  try
    Result:=(GetDeviceCaps(DC,PLANES)*GetDeviceCaps(DC,BITSPIXEL));
  finally
    ReleaseDC(0,DC);
  end;
end;

procedure UpdateSystemInfo;
begin
  UseStyleCaption:=UseStyleCaption and (GetSystemColorBitDepth>8) and (Win32MajorVersion<=5);
  if UseStyleCaption then
  begin
    with TRegistry.Create(KEY_READ) do
    try
      RootKey:=HKEY_CURRENT_USER;
      if OpenKey('Software\MeeSoft\',False) and ValueExists('UseStyleForm') and not ReadBool('UseStyleForm') then
        UseStyleCaption:=False;
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('Software\MeeSoft\',False) and ValueExists('UseStyleForm') and not ReadBool('UseStyleForm') then
        UseStyleCaption:=False;
    finally
      Destroy;
    end;
  end;
end;

constructor TStyleForm.Create(AOwner: TComponent);
var
  DeltaFrameX, DeltaFrameYTop, DeltaFrameYBottom : Integer;
begin
  Application.OnException:=StyleFormExceptionEvent;
  {$IFDEF VistaTaskbarButton}
  if not IsLibrary and WindowsIsVistaOrLater then
  begin
    if Application.MainForm=nil then
    begin
      // We are creating the main form under Vista, fix task bar button handling
      SetWindowLong(Application.Handle, GWL_EXSTYLE,GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW);
      FShowTaskBarButton:=True;
    end
    else if FParentHandle=0 then FParentHandle:=GetActiveFormHandle;
  end;
  {$ENDIF}
  inherited;
  Assert(not OldCreateOrder,ClassName+': Don''t use OldCreateOrder!');
  if Assigned(TranslationManager) then TranslationManager.TranslateForm(Self);
  Assert(Position<>poDefaultSizeOnly,'This is a stupid value for Posision!');
  if (Position<>poDefault) and (WindowState<>wsMaximized) then // Handle changed frame size
  begin
    GetWindowFrameSizeChange(DeltaFrameX,DeltaFrameYTop,DeltaFrameYBottom);
    ChangeWindowSizeIgnoringAnchors(DeltaFrameX*2,DeltaFrameYTop+DeltaFrameYBottom);
  end;{}
end;

constructor TStyleForm.Create(AOwner: TComponent; ParentHandle: THandle);
begin
  FParentHandle:=ParentHandle;
  Create(AOwner);
end;

constructor TStyleForm.CreateNew(AOwner: TComponent; Dummy: Integer=0);
begin
  if UseStyleCaption then
  begin
    FCaptionFont:=TFont.Create;
    UpdateColors;
    UpdateCaptionFont;
  end;
  inherited CreateNew(AOwner);
end;

constructor TStyleForm.CreateNewWithParent(AOwner: TComponent; ParentHandle: THandle);
begin
  FParentHandle:=ParentHandle;
  ParentFont:=True;
  CreateNew(AOwner);
end;

{: Destroy destroys an instance of TStyleForm. Do not call Destroy
   directly in an application. Instead, call Free. Free verifies that the
   instance is not already freed, and only then calls Destroy.<BR>
   Destroy is used to free resources allocated in the
   <See Method=TStyleForm.Create Text=Create> constructor. }
destructor TStyleForm.Destroy;
begin
  inherited Destroy;
  FCaptionFont.Free;
  FBackgroundBitmap.Free;
end;

procedure TStyleForm.DoCreate;
var
  I : Integer;
begin
  Assert((Application.MainForm=nil) or not Application.MainForm.ParentFont,'MainForm cannot have ParentFont set');

  if ParentFont and Assigned(Application.MainForm) then Font:=Application.MainForm.Font;
  UseBackgroundTheme:=(Color=clBtnFace) and ThemeServices.ThemesEnabled;

  // Fixes bug in menu item rendering under WinXP
  if Assigned(Menu) and (Win32MajorVersion>=5) and (Win32MinorVersion>=1) then
    for I:=0 to Menu.Items.Count-1 do
      if not Assigned(Menu.Items[I].OnAdvancedDrawItem) then
        Menu.Items[I].OnAdvancedDrawItem:=TMenuItemEx.AdvancedDrawTopItem;

  inherited;
end;

// Get change from default window frame size under Windows XP
procedure TStyleForm.GetWindowFrameSizeChange(out DeltaFrameX, DeltaFrameYTop, DeltaFrameYBottom : Integer);
begin
  if BorderStyle in [bsToolWindow,bsSingle,bsDialog] then // Fixed window
  begin
    DeltaFrameX:=GetSystemMetrics(SM_CXFIXEDFRAME)-3;
    DeltaFrameYBottom:=GetSystemMetrics(SM_CYFIXEDFRAME)-3;
  end
  else if BorderStyle in [bsSizeable, bsSizeToolWin] then // Sizable window
  begin
    DeltaFrameX:=GetSystemMetrics(SM_CXSIZEFRAME)-4;
    DeltaFrameYBottom:=GetSystemMetrics(SM_CYSIZEFRAME)-4;
  end
  else
  begin
    DeltaFrameX:=0;
    DeltaFrameYBottom:=0;    
  end;
  DeltaFrameYTop:=DeltaFrameYBottom;
  if BorderStyle=bsSizeToolWin then
    DeltaFrameYTop:=DeltaFrameYBottom+GetSystemMetrics(SM_CYSMCAPTION)-18
  else if BorderStyle=bsSizeable then
    DeltaFrameYTop:=DeltaFrameYBottom+GetSystemMetrics(SM_CYCAPTION)-26;
end;

procedure TStyleForm.ChangeWindowSizeIgnoringAnchors(DeltaX,DeltaY: Integer);
var
  OldAnchors : array of TAnchors;
  Anchor : Integer;

  procedure ResetAnchors(ParentControl: TWinControl);
  var
    I : Integer;
    Control : TControl;
  begin
    for I:=0 to ParentControl.ControlCount-1 do
    begin
      Control:=ParentControl.Controls[I];
      if Anchor>=Length(OldAnchors) then SetLength(OldAnchors,Length(OldAnchors)*2);
      OldAnchors[Anchor]:=Control.Anchors;
      Inc(Anchor);
      Control.Anchors:=[akLeft,akTop];
      if Control is TWinControl then
        ResetAnchors(TWinControl(Control));
    end
  end;

  procedure RestoreAnchors(ParentControl: TWinControl);
  var
    I : Integer;
    Control : TControl;
  begin
    for I:=0 to ParentControl.ControlCount-1 do
    begin
      Control:=ParentControl.Controls[I];
      Control.Anchors:=OldAnchors[Anchor];
      Inc(Anchor);
      if Control is TWinControl then
        RestoreAnchors(TWinControl(Control));
    end
  end;

var
  OldVertScrollBarVisible, OldHorzScrollBarVisible : Boolean;
  OldOnResize : TNotifyEvent;
begin
  if (DeltaX<>0) or (DeltaY<>0) then
  begin
    OldOnResize:=OnResize;
    OnResize:=nil;
    // Store old anchors and reset
    SetLength(OldAnchors,ControlCount);
    Anchor:=0;
    ResetAnchors(Self);
    // Change window size
    OldVertScrollBarVisible:=VertScrollBar.Visible;
    OldHorzScrollBarVisible:=HorzScrollBar.Visible;
    VertScrollBar.Visible:=False;
    HorzScrollBar.Visible:=False;
    SetBounds(Left,Top,Width+DeltaX,Height+DeltaY);
    VertScrollBar.Visible:=OldVertScrollBarVisible;
    HorzScrollBar.Visible:=OldHorzScrollBarVisible;
    // Restore anchors
    Anchor:=0;
    RestoreAnchors(Self);
    OnResize:=OldOnResize;
    //if Assigned(OldOnResize) then OldOnResize(nil);
  end;
end;

function TStyleForm.GetUseBackgroundTheme: Boolean;
begin
  Result := FUseBackgroundTheme and Assigned(BackgroundRenderer);
end;

procedure TStyleForm.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if UseBackgroundTheme then Msg.Result:=LRESULT(False)
  else inherited;
end;

procedure TStyleForm.WMUpdateUIState(var Msg: TWMUpdateUIState);
begin
  if WindowsIsVistaOrLater then RepaintOnIdle(Self);
  inherited;
end;

procedure TStyleForm.SetShowTaskBarButton(const Value: Boolean);
begin
  if FShowTaskBarButton<>Value then
  begin
    FShowTaskBarButton:=Value;
    RecreateWnd;
  end;
end;

procedure TStyleForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if ShowTaskBarButton then Params.ExStyle:=Params.ExStyle or WS_EX_APPWINDOW;
  if FParentHandle<>0 then
  begin
    if FormStyle in [fsNormal,fsStayOnTop] then Params.WndParent:=FParentHandle
    else FParentHandle:=0;
  end;
end;

procedure TStyleForm.Paint;
begin
  if UseBackgroundTheme then
  begin
    if FBackgroundBitmap=nil then
    begin
      FBackgroundBitmap:=TBitmap.Create;
      FBackgroundBitmap.Width:=ClientWidth;
      FBackgroundBitmap.Height:=ClientHeight;
      BackgroundRenderer(FBackgroundBitmap);
    end;
    Canvas.Draw(0,0,FBackgroundBitmap);
  end;
  inherited Paint;
end;

procedure TStyleForm.Resize;
begin
  if FBackgroundBitmap<>nil then
  begin
    FBackgroundBitmap.Destroy;
    FBackgroundBitmap:=nil;
    Invalidate;
  end;
  inherited Resize;
end;

{procedure TStyleForm.WMEndSession(var Msg: TWMEndSession);
begin
  // Process this message since the VCL does not
  if Msg.EndSession then Halt;
end;}

procedure TStyleForm.UpdateColors;
begin
  C1Inactive:=ColorToRGB(clGradientInactiveCaption);
  C2Inactive:=ColorToRGB(clInactiveCaption);
  if ColorBrightness(C1Inactive)<ColorBrightness(C2Inactive) then SwapDWords(C1Inactive,C2Inactive);

  C1Active:=ColorToRGB(clGradientActiveCaption);
  C2Active:=ColorToRGB(clActiveCaption);
  if ColorBrightness(C1Active)<ColorBrightness(C2Active) then SwapDWords(C1Active,C2Active);

  C1Bright:=ColorToRGB(clBtnFace);
  C2Bright:=ColorTone(C1Bright,clBlack,1,2);
end;

function TStyleForm.IsActiveWindow: boolean;
begin
  if FormStyle = fsMDIChild then
    if assigned(Application.MainForm) then
      Result:=(GetActiveWindow=Application.MainForm.Handle) and
              (TForm(Application.MainForm).ActiveMDIChild=Self)
    else
      Result:=FALSE
  else
    Result := GetActiveWindow=Handle;
end;

// The caption rect is the rectangle we are interested in painting. This will
// be the area that contains the caption icon, text and buttons.
function TStyleForm.GetCaptionRect: TRect;
begin
  // if we have no border style, then just set the rectange empty.
  if BorderStyle = bsNone then
    SetRectEmpty(Result)
  else
  begin
    GetWindowRect(Handle, Result);
    // Convert rect from screen (absolute) to client (0 based) coordinates.
    OffsetRect(Result, -Result.Left, -Result.Top);
    // Shrink rectangle to allow for window border.  We let Windows paint it.
    if (WindowState = wsMinimized) or (BorderStyle in [bsToolWindow, bsSingle, bsDialog]) then
      InflateRect(Result, -GetSystemMetrics(SM_CXFIXEDFRAME), -GetSystemMetrics(SM_CYFIXEDFRAME))
    else if BorderStyle in [bsSizeable, bsSizeToolWin] then
      InflateRect(Result, -GetSystemMetrics(SM_CXSIZEFRAME), -GetSystemMetrics(SM_CYSIZEFRAME));

    // Set the appropriate height of caption bar.
    if BorderStyle in [bsToolWindow, bsSizeToolWin] then
      Result.Bottom := Result.Top + GetSystemMetrics(SM_CYSMCAPTION) - 1
    else
      Result.Bottom := Result.Top + GetSystemMetrics(SM_CYCAPTION) - 1;
  end;
end;

function TStyleForm.GetFrameSize: TPoint;
begin
  if BorderStyle in [bsToolWindow,bsSingle,bsDialog] then // Fixed window
  begin
    Result.X:=GetSystemMetrics(SM_CXFIXEDFRAME);
    Result.Y:=GetSystemMetrics(SM_CYFIXEDFRAME);
  end
  else if BorderStyle in [bsSizeable, bsSizeToolWin] then // Sizable window
  begin
    Result.X:=GetSystemMetrics(SM_CXSIZEFRAME);
    Result.Y:=GetSystemMetrics(SM_CYSIZEFRAME);
  end
  else // BorderStyle=bsNone
  begin
    Result.X:=0;
    Result.Y:=0;
  end;
end;

// Paint the icon for the system menu.
procedure TStyleForm.PaintMenuIcon(DC: HDC; var R: TRect; Active: boolean);
var
  SmallCopy,
  IconHandle: HIcon;
  Size: integer;
begin
  // Does the form have an icon assigned to it?
  if Icon.Handle <> 0 then IconHandle := Icon.Handle
  // If not, does the application have an icon?
  else if Application.Icon.Handle <> 0 then IconHandle := Application.Icon.Handle
  // If not, then just use the system defined application icon.
  else IconHandle := LoadIcon(0, IDI_APPLICATION);

  with R do
  begin
    //Size:=GetSystemMetrics(SM_CXSMICON);
    Size:=R.Bottom-R.Top-2;

    // Let CopyImage() make get us a nice small version of the icon and we'll paint it.
    SmallCopy:=CopyImage(IconHandle,IMAGE_ICON,Size,Size,LR_COPYFROMRESOURCE);
    DrawIconEx(HDC(DC),Left+1,Top+1,SmallCopy,0,0,0,0,DI_NORMAL);
  end;
  DestroyIcon(SmallCopy);
  Inc(R.Left,Size+3);
end;

procedure TStyleForm.PaintCaptionText(DC: HDC; R: TRect; Active: Boolean; const Text: string);
var
  OldColor: TColorRef;
  OldMode: integer;
  OldFont: HFont;
begin
  Inc(R.Left, 2);

  // Set the color to paint the text with.
  if Active then OldColor:=SetTextColor(HDC(DC), ColorToRGB(clCaptionText))
  else OldColor:=SetTextColor(HDC(DC), ColorToRGB(clInactiveCaptionText));
  // Set the background text painting mode to transparent so that drawing text
  // doesn't distrub the gradient we just painted.  If you didn't do this, then
  // drawing text would also fill the text rectangle with a solid background
  // color, screwing up our gradient.
  OldMode:=SetBkMode(HDC(DC), TRANSPARENT);
  // Select in the system defined caption font (see Create constructor).
  if FCaptionFont.Handle <> 0 then
    OldFont:=SelectObject(HDC(DC), FCaptionFont.Handle)
  else
    OldFont:=0;
  try
    // Draw the text making it left aligned, centered vertically, allowing no line breaks.
    DrawText(HDC(DC),PChar(Text),-1,R,DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX);
  finally
    // Clean up all the drawing objects.
    if OldFont<>0 then SelectObject(HDC(DC), OldFont);
    SetBkMode(HDC(DC), OldMode);
    SetTextColor(HDC(DC), OldColor);
  end;
end;

// Paint the min/max/help/close buttons.
procedure TStyleForm.PaintCaptionButtons(DC: HDC; var Rect: TRect);
var
  BtnWidth: integer;
  Flag: UINT;
  SrcRect: TRect;
  ABorderStyle: TFormBorderStyle;
  ABorderIcons: TBorderIcons;
begin
  SrcRect := Rect;
  InflateRect(SrcRect, -2, -2);
  ABorderStyle := BorderStyle;
  ABorderIcons := BorderIcons;

  if ABorderStyle in [bsToolWindow, bsSizeToolWin] then
  begin
    // Tool windows only have the close button, nothing else.
    with SrcRect do
      Left := Right - (GetSystemMetrics(SM_CXSMSIZE)) + 2;
    Flag := DFCS_CAPTIONCLOSE;
    if (GetClassLong(Handle, GCL_STYLE) and CS_NOCLOSE) <> 0 then
      Flag := Flag or DFCS_INACTIVE;
    DrawFrameControl(HDC(DC), SrcRect, DFC_CAPTION, Flag);
    Rect.Right := SrcRect.Left-5;
  end
  else
  begin
    BtnWidth := GetSystemMetrics(SM_CXSMICON)-2;
    // Windows is loopy.  It always returns an even number, no matter what
    if (Odd(BtnWidth) XOR Odd(Rect.Bottom-Rect.Top)) then
      inc(BtnWidth);
    SrcRect.Left := SrcRect.Right - BtnWidth - 2;
    // if it has system menu, it has a close button.
    if biSystemMenu in ABorderIcons then
    begin
      Flag := DFCS_CAPTIONCLOSE;
      if (GetClassLong(Handle, GCL_STYLE) and CS_NOCLOSE) <> 0 then
        Flag := Flag or DFCS_INACTIVE;
      DrawFrameControl(HDC(DC), SrcRect, DFC_CAPTION, Flag);
      OffsetRect(SrcRect, -BtnWidth-4, 0);
      Dec(Rect.Right,BtnWidth+4);
    end;
    // Minimize and Maximized don't show up at all if BorderStyle is bsDialog or
    // if neither of them are enabled.
    if (ABorderStyle in [bsSizeable, bsSingle]) and
       (ABorderIcons * [biMinimize, biMaximize] <> []) then
    begin
      if WindowState = wsMaximized then
        Flag := DFCS_CAPTIONRESTORE
      else
        Flag := DFCS_CAPTIONMAX;
      // if it doesn't have max in style, then it shows up disabled.
      if not (biMaximize in ABorderIcons) then
        Flag := Flag or DFCS_INACTIVE;

      DrawFrameControl(HDC(DC), SrcRect, DFC_CAPTION, Flag);
      OffsetRect(SrcRect, -BtnWidth-2, 0);
      Dec(Rect.Right,BtnWidth+2);

      if WindowState = wsMinimized then
        Flag := DFCS_CAPTIONRESTORE
      else
        Flag := DFCS_CAPTIONMIN;
      // if it doesn't have min in style, then it shows up disabled.
      if not (biMinimize in ABorderIcons) then
        Flag := Flag or DFCS_INACTIVE;

      DrawFrameControl(HDC(DC), SrcRect, DFC_CAPTION, Flag);
      OffsetRect(SrcRect, -BtnWidth-2, 0);
      Dec(Rect.Right,BtnWidth+2);
    end;

    // Help only shows up in bsDialog style, and bsSizeable, bsSingle when there
    // is no min or max button.
    if biHelp in ABorderIcons then
    begin
      if ((ABorderStyle in [bsSizeable, bsSingle]) and
         (ABorderIcons * [biMinimize, biMaximize] = [])) or
         (ABorderStyle = bsDialog) then
      begin
        DrawFrameControl(HDC(DC), SrcRect, DFC_CAPTION, DFCS_CAPTIONHELP);
        Dec(Rect.Right,BtnWidth+2);
      end;
    end;

    Dec(Rect.Right, 3);
  end;
end;

function TStyleForm.DrawCaption(FormDC: HDC; Active: boolean): TRect;
var
  CaptionBoxSize : Integer;
  R, Box : TRect;
  OldBmp, Bmp : HBitmap;
  BmpDC : HDC;
  Canvas : TCanvas;
  W, H, Y, X, StairX, BarY, I : Integer;
  CaptionText : string;
  PaintSystemMenu : Boolean;
  C1A, C2A : TColor;
begin
  R := GetCaptionRect; // Get only the portion we need to draw.
  Result:=R;
  OffsetRect(R,-R.Left,-R.Top); // Convert to logical (0-based) coordinates
  W:=R.Right-R.Left;
  H:=R.Bottom-R.Top;

  CaptionText:=Caption;         
  PaintSystemMenu:=((biSystemMenu in BorderIcons) and
                   (BorderStyle in [bsSingle, bsSizeable]));

  // Create a temporary device context to draw on.  Drawing on a temporary DC
  // and copying it to the real DC accomplishes two things:
  // 1) It is faster because Windows doesn't have to draw anything in the
  //    temporary DC on the screen, it only draws when you paint something on a
  //    real DC.  Then it just draws everything at once when we copy it, instead
  //    of drawing a little, do some calculations, draw a little, etc.
  // 2) It looks much better because it is drawn faster.  It reduces the
  //    "flicker" that you would see from each individual part being drawn,
  //    especially the gradient bands.
  BmpDC := CreateCompatibleDC(HDC(FormDC));
  Bmp := CreateCompatibleBitmap(HDC(FormDC), W, H);
  OldBmp := SelectObject(BmpDC, Bmp);
  try
    Canvas := TCanvas.Create;
    with Canvas do
    try
      Handle:=BmpDC;

      Font:=FCaptionFont;
      StairX:=TextWidth(CaptionText)+21;
      if PaintSystemMenu then Inc(StairX,19);
      if StairX>R.Right-60 then StairX:=Width
      else if StairX<R.Right div 3 then StairX:=R.Right div 3;

      if Active then
      begin
        C1A:=C1Active; C2A:=C2Active;
      end
      else
      begin
        C1A:=C1Inactive; C2A:=C2Inactive;
      end;

      X:=StairX+R.Bottom;

      // Draw something to initialize canvas properly (?)
      Pen.Color:=345; MoveTo(0,R.Bottom); LineTo(X,R.Bottom);

      CaptionBoxSize:=R.Bottom div 4+1;
      for I:=1 to R.Bottom div CaptionBoxSize+1 do
      begin
        BarY:=R.Bottom-I*CaptionBoxSize;
                                                            
        for Y:=Max(0,BarY) to BarY+CaptionBoxSize-1 do
        begin
          Pen.Color:=ColorTone(C2A,C1A,Y,R.Bottom-1);
          MoveTo(0,Y);
          LineTo(X,Y);
          Pen.Color:=ColorTone(C2Bright,C1Bright,Y,R.Bottom-1);
          LineTo(R.Right,Y);
        end;

        Pen.Color:=C1A;
        MoveTo(X-CaptionBoxSize-1,BarY);
        LineTo(X-1,BarY);
        Pen.Color:=C2A;
        LineTo(X-1,BarY+CaptionBoxSize);

        if BarY>0 then
        begin
          Box:=Bounds(X+CaptionBoxSize,BarY,CaptionBoxSize,CaptionBoxSize);
          CaptionBox(Canvas,Box,C1A,C2A,ColorTone(C2A,C1A,BarY+CaptionBoxSize div 2,R.Bottom-1));
          if I=1 then
          begin
            OffsetRect(Box,CaptionBoxSize*2,0);
            CaptionBox(Canvas,Box,C1A,C2A,ColorTone(C2A,C1A,BarY+CaptionBoxSize div 2,R.Bottom-1));
          end;
        end;

        Dec(X,CaptionBoxSize);
      end;
    finally
      Free;
    end;
    Inc(R.Left, 1);
    if not ((FormStyle = fsMDIChild) and (WindowState = wsMaximized)) then
    begin
      // PaintMenuIcon will adjust the rect so that future drawing operations happen in the right spot.
      if PaintSystemMenu then PaintMenuIcon(HDC(BmpDC), R, Active);
      // Paint the min/max/help/close buttons.
      PaintCaptionButtons(HDC(BmpDC), R);
    end;

    PaintCaptionText(HDC(BmpDC),R,Active,CaptionText); // Paint the caption text.
    // Copy the gradient caption bar to the real DC.
    BitBlt(HDC(FormDC), Result.Left, Result.Top, W, H, BmpDC, 0, 0, SRCCOPY);
  finally
    // Clean up all the temporary drawing objects.
    SelectObject(BmpDC, OldBmp);
    DeleteObject(Bmp);
    DeleteDC(BmpDC);
  end;
end;

var
  GrayThemeBitmap : TBitmap = nil;
  DisableThemeIfInactive : Boolean = False;

function TStyleForm.DrawThemeCaption(FormDC: HDC; Active: Boolean): TRect;

var
  Height : Integer;
  CaptionBoxSize, ButtonSize, CaptionButtonX : Integer;
  ThemeBitmap : TBitmap;
  StairsBitmap : TBitmap;
  C1A, C2A : TColor;
  FrameSize : TPoint;

  procedure SetBox(X,Y: Integer; TopRight,Left: Boolean);
  var
    BoxRect : TRect;
  begin
    X:=(X-1)*CaptionBoxSize+1;
    Y:=Height-Y*CaptionBoxSize;
    BoxRect:=Bounds(X,Y,CaptionBoxSize,CaptionBoxSize);
    with StairsBitmap.Canvas do
    begin
      // Copy box contents
      CopyRect(BoxRect,ThemeBitmap.Canvas,BoxRect);
      // Draw box edges
      if TopRight then
      begin
        Pen.Color:=C1A;
        MoveTo(BoxRect.Left,BoxRect.Top);
        LineTo(BoxRect.Right-1,BoxRect.Top);
        Pen.Color:=C2A;
        LineTo(BoxRect.Right-1,BoxRect.Bottom);
        if Left then
        begin
          Pen.Color:=C1A;
          MoveTo(BoxRect.Left,BoxRect.Top);
          LineTo(BoxRect.Left,BoxRect.Bottom);
        end;
      end;
    end;
  end;

  procedure DrawCaptionButton(Button: TThemedWindow; Enabled: Boolean);
  var
    Details : TThemedElementDetails;
  begin
    Details:=ThemeServices.GetElementDetails(Button);
    if not Active then Details.State:=5;
    if not Enabled then Inc(Details.State,3);
    ThemeServices.DrawElement(StairsBitmap.Canvas.Handle,Details,Bounds(CaptionButtonX,FrameSize.Y+2,ButtonSize,ButtonSize));
    Dec(CaptionButtonX,ButtonSize+2);
  end;

var
  Y, X, StairX, ButtonBitmapWidth, GrayTone : Integer;
  MaximizedCorrection : Integer;
  Details : TThemedElementDetails;
  Pix : PColor;
begin
  if not ((BorderStyle in [bsNone,bsToolWindow,bsSizeToolWin]) or
          ((FormStyle=fsMDIChild) and (WindowState=wsMaximized)) or
          (DisableThemeIfInactive and not Active)) then
  begin
    // Determine caption bar size
    Result:=GetCaptionRect;
    FrameSize:=GetFrameSize;
    Height:=Result.Bottom-Result.Top+FrameSize.Y+1; 

    // Generate gray theme bitmap
    if GrayThemeBitmap=nil then
    begin
      GrayThemeBitmap:=TBitmap.Create;
      GrayThemeBitmap.PixelFormat:=pf32bit;
      GrayThemeBitmap.Width:=Screen.Width;
      GrayThemeBitmap.Height:=Height;
      GrayThemeBitmap.Canvas.Font:=FCaptionFont;
      Details:=ThemeServices.GetElementDetails(twCaptionInactive);
      ThemeServices.DrawElement(GrayThemeBitmap.Canvas.Handle,Details,Rect(0,0,GrayThemeBitmap.Width,Height));
      // Determine brightness
      GrayTone:=0;
      for Y:=0 to GrayThemeBitmap.Height-1 do
      begin
        Pix:=GrayThemeBitmap.ScanLine[Y];
        for X:=1 to GrayThemeBitmap.Width do
        begin
          Inc(GrayTone,ColorBrightness(Pix^));
          Inc(Pix);
        end;
      end;
      GrayTone:=GrayTone div (GrayThemeBitmap.Height*GrayThemeBitmap.Width*6);
      DisableThemeIfInactive:=GrayTone>230;
      if GrayTone>200 then GrayTone:=6
      else GrayTone:=5;
      // Convert to grayscale
      for Y:=0 to GrayThemeBitmap.Height-1 do
      begin
        Pix:=GrayThemeBitmap.ScanLine[Y];
        for X:=1 to GrayThemeBitmap.Width do
        begin
          Pix^:=GrayColor(Min(255,(ColorBrightness(Pix^)+3) div GrayTone));
          Inc(Pix);
        end;
      end;
    end;

    // Determine box size
    if WindowState=wsMaximized then MaximizedCorrection:=4
    else MaximizedCorrection:=0;
    CaptionBoxSize:=(Height+3-MaximizedCorrection div 2) div 4;

    // Determine size of system buttons
    ButtonSize:=Result.Bottom-Result.Top-4; //TODO: This is not correct! 
    ButtonBitmapWidth:=4;
    if biSystemMenu in BorderIcons then
    begin
      Inc(ButtonBitmapWidth,ButtonSize+2);
      if (BorderStyle<>bsDialog) and ([biMinimize,biMaximize]*BorderIcons<>[]) then Inc(ButtonBitmapWidth,2*(ButtonSize+2))
      else if biHelp in BorderIcons then Inc(ButtonBitmapWidth,ButtonSize+2);
    end;
    Inc(Result.Right,FrameSize.X);

    // Determine size of caption text
    StairX:=GrayThemeBitmap.Canvas.TextWidth(Caption)+21;
    if (biSystemMenu in BorderIcons) and (BorderStyle in [bsSingle, bsSizeable]) then Inc(StairX,20);
    if StairX>Result.Right-Height*2-ButtonBitmapWidth then
    begin
      Result:=Rect(0,0,0,0); // No room for the stairs
      Exit;
    end
    else if StairX<Result.Right div 3 then StairX:=Result.Right div 3;
    Result.Left:=StairX;

    // Make standard caption bitmap
    ThemeBitmap:=TBitmap.Create;
    ThemeBitmap.Width:=Height*2;
    ThemeBitmap.Height:=Height;
    if Active then
    begin
      C1A:=C1Active; C2A:=C2Active;
      if WindowState=wsMaximized then Details:=ThemeServices.GetElementDetails(twMaxCaptionActive)
      else Details:=ThemeServices.GetElementDetails(twCaptionActive);
    end
    else
    begin
      C1A:=C1Inactive; C2A:=C2Inactive;
      if WindowState=wsMaximized then Details:=ThemeServices.GetElementDetails(twMaxCaptionInactive)
      else Details:=ThemeServices.GetElementDetails(twCaptionInactive);
    end;
    ThemeServices.DrawElement(ThemeBitmap.Canvas.Handle,Details,Rect(-64,MaximizedCorrection,ThemeBitmap.Width+96,Height));
    // Make style caption bitmap
    StairsBitmap:=TBitmap.Create;
    StairsBitmap.Width:=Result.Right-Result.Left;
    StairsBitmap.Height:=Height;
    StairsBitmap.Canvas.Draw(StairsBitmap.Width-GrayThemeBitmap.Width,0,GrayThemeBitmap);
    // Draw boxes
    for Y:=1 to 4 do
    begin
      for X:=0 to 4-Y do SetBox(X,Y,X=4-Y,False);
      if Y<>4 then SetBox(6-Y,Y,True,True);
    end;
    SetBox(7,1,True,True);
    // Buttons
    CaptionButtonX:=Result.Right-Result.Left-ButtonSize-FrameSize.X-2;
    if biSystemMenu in BorderIcons then
    begin
      DrawCaptionButton(twCloseButtonNormal,True);
      if (BorderStyle<>bsDialog) and ([biMinimize,biMaximize]*BorderIcons<>[]) then
      begin
        Assert((FParentHandle=0) or (BorderStyle=bsDialog) or not (biMinimize in BorderIcons),'ParentHandle cannot be set for form with a minimize button');
        if WindowState=wsMaximized then DrawCaptionButton(twRestoreButtonNormal,biMaximize in BorderIcons)
        else DrawCaptionButton(twMaxButtonNormal,biMaximize in BorderIcons);
        DrawCaptionButton(twMinButtonNormal,biMinimize in BorderIcons);
      end
      else if biHelp in BorderIcons then DrawCaptionButton(twHelpButtonNormal,True);
    end;

    // Paint bitmaps to the screen
    BitBlt(FormDC,Result.Left,0,StairsBitmap.Width,Height,StairsBitmap.Canvas.Handle,0,0,SRCCOPY);

    ThemeBitmap.Free;
    StairsBitmap.Free;

    Result.Top:=0;
    Result.Bottom:=Height;
  end
  else Result:=Rect(0,0,0,0);
end;

// Windows sends this message when the window has been activated or deactivated.
procedure TStyleForm.WMNCActivate(var Msg: TWMNCActivate);
begin
  inherited;
  if UseStyleCaption and Visible then UpdateCaption(Msg.Active);
  Monitor.ClassType; // Hopefully reinitialize the monitor handler to avoid rare AVs
end;

// Windows sends this message whenever any part of the non-client area
// (caption, window border) needs repainting.
procedure TStyleForm.WMNCPaint(var Msg: TMessage);
var
  SaveWR, CR, WR, R: TRect;
  DC : HDC;
  MyRgn : HRGN;
  DeleteRgn : Boolean;
begin
  if UseStyleCaption then
  begin
    DeleteRgn := FALSE;
    // The region that needs painting is passed in WParam.  A region is a Windows
    // object used to describe the non-rectangular area used by a combination of
    // rectangles.  We have to typecast it because in Delphi 4 wParam is signed
    // and HRGN in unsigned.  It worked prior to D4 because they were both
    // signed.
    MyRgn := HRGN(Msg.wParam);
    DC := GetWindowDC(Handle);
    try
      GetWindowRect(Handle, WR);
      // Select the update region as the clipping region.  Clipping regions
      // guarantee that any painting done outside of the selected region is not
      // shown (thrown away).
      if SelectClipRgn(DC, MyRgn) = ERROR then
      begin
        // We got passed an invalid region.  Generally, this happens when the
        // window is first created or a MDI is minimized.  We'll create our own
        // region (the rectangle that makes up the entire window) and use that
        // instead.
        with WR do
          MyRgn := CreateRectRgn(Left, Top, Right, Bottom);
        SelectClipRgn(DC, MyRgn);
        DeleteRgn := TRUE;
      end;
      // Convert the clipping region coordinates from screen to client.
      OffsetClipRgn(DC,-WR.Left,-WR.Top);
      // Draw our caption.
      if ThemeServices.ThemesEnabled then R:=DrawThemeCaption(DC,IsActiveWindow)
      else R:=DrawCaption(DC,IsActiveWindow);
      // Here's the trick.  DrawCaption returns the rectangle that we painted.
      // We now exclude that rectangle from the clipping region.  This guarantees
      // that any further painting that occurs will not happen in this rectangle.
      // That means that when we let the default painting for WM_NCPAINT occur,
      // it will not paint over our gradient. It only paints the stuff that we
      // didn't, like the window borders.
      ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);

      // Draw border if needed
      if BorderWidth > 0 then
      begin
        Windows.GetClientRect(Handle, CR);
        SaveWR := WR;
        MapWindowPoints(0, Handle, WR, 2);
        OffsetRect(CR, -WR.Left, -WR.Top);
        // Draw borders in non-client area
        InflateRect(CR, BorderWidth, BorderWidth);
        WR := SaveWR;
        OffsetRect(WR, -WR.Left, -WR.Top);
        Windows.FillRect(DC, WR, Brush.Handle);
        WR := SaveWR;
      end;

      // Convert coordinates back into screen-based.
      OffsetClipRgn(DC, WR.Left, WR.Top);
      // Get the region that is now described by the clipping region.
      GetClipRgn(DC, MyRgn);
      // Pass that region on to the default WM_NCPAINT handler.  Remember, we
      // excluded the rectangle that we painted, so Windows will not be able to
      // paint over what we did. Most gradient captions components just let
      // windows draw its stuff first, and then paint the gradient.  This results
      // in an irritating "flicker", caused by the area being painted normally,
      // and then painted over a second time by the gradient. We have to
      // typecast the wParam parameter because in Delphi 4 wParam is signed and
      // HRGN in unsigned.  It worked prior to D4 because they were both signed.
      Msg.Result := DefWindowProc(Handle, Msg.Msg, WPARAM(MyRgn), Msg.lParam);
    finally
      // If we had to create our own region, we have to clean it up.
      if DeleteRgn then
        DeleteObject(MyRgn);
      ReleaseDC(Handle, DC); // NEVER leave this hanging.
    end;
  end
  else inherited;
end;

// Windows sends this message if the user changes any of the system colors.
procedure TStyleForm.WMSysColorChange(var Msg: TWMSysColorChange);
var
  X : integer;
begin
  // Did they change to 16-color mode?
  UpdateSystemInfo;
  if UseStyleCaption then
  begin
    UpdateColors;
    FreeAndNil(GrayThemeBitmap);
  end;

  // This only goes to top-level windows so we have to feed it to MDI children
  if FormStyle=fsMDIForm then
    for X:=0 to MDIChildCount-1 do
      if MDIChildren[x] is TStyleForm then
        TStyleForm(MDIChildren[x]).WMSysColorChange(Msg);
  inherited;
end;

// The window has been resized.
procedure TStyleForm.WMSize(var Msg: TWMSize);
begin
  inherited;
  if UseStyleCaption and Visible then
  begin
    // If the window was maximized or restored, we need to redraw so the right
    // caption button is painted.
    if (Msg.SizeType = SIZE_MAXIMIZED) or (Msg.SizeType = SIZE_RESTORED) then
      UpdateCaption(IsActiveWindow);
  end;
end;

// Windows would like to have a cursor displayed.  I know, you're wondering
// why the hell I care about this, aren't you?  Well, in the inherited handling
// (default Windows processing) of this message, if the mouse is over a
// resizeable border section, Windows repaints the caption buttons.  Why?  I
// have absolutely no idea.  However, that's not the important part.  When it
// repaints those buttons, it also repaints the background around them in the
// last color it painted the caption in.  Now, usually this would just result
// in losing a few bands of the caption gradient, which 99.44% of all users
// would never notice.  However, because we don't always allow default
// processing of WM_NCACTIVATE, sometimes Windows doesn't have the right idea
// about which color is currently the background.  This cause the background to
// get painted in the wrong color sometimes, which 99.44% of all users *will*
// notice.  We fix it by setting the appropriate cursor and not allowing the
// default processing to occur.
procedure TStyleForm.WMSetCursor(var Msg: TWMSetCursor);
begin
  if UseStyleCaption then
  begin
    // Tell Windows we handled the message
    Msg.Result := 1;
    // Load and display the correct cursor for the border area being hit
    case Msg.HitTest of
      HTTOP,
      HTBOTTOM:      SetCursor(Screen.Cursors[crSizeNS]);
      HTLEFT,
      HTRIGHT:       SetCursor(Screen.Cursors[crSizeWE]);
      HTTOPRIGHT,
      HTBOTTOMLEFT:  SetCursor(Screen.Cursors[crSizeNESW]);
      HTTOPLEFT,
      HTBOTTOMRIGHT: SetCursor(Screen.Cursors[crSizeNWSE]);
    else
      // Wasn't anything we cared about, so tell Windows we didn't handle it.
      Msg.Result := 0;
      inherited;
    end;
  end
  else inherited;
  if (Screen.Cursor<>crDefault) and
     (Smallint(Msg.HitTest)=HTERROR) or // Non-active form hit
     (Msg.HitTest in [HTCAPTION,HTSYSMENU,HTMINBUTTON,HTMAXBUTTON,HTCLOSE]) then // Caption bar hit
    Windows.SetCursor(Screen.Cursors[Screen.Cursor]);
end;

procedure TStyleForm.WMSetText(var Msg: TWMSetText);
var
  Wnd : HWND;
  Rgn : THandle;
  MaximizedChild : Boolean;
begin
  if UseStyleCaption then
  begin
    Wnd:=0;
    MaximizedChild:=(FormStyle=fsMDIChild) and (WindowState=wsMaximized);
    if MaximizedChild then
    begin
      // Need to cause main form's caption to be redrawn, not the MDI child.
      if Application.MainForm.HandleAllocated then Wnd:=Application.MainForm.Handle;
    end
    else if HandleAllocated then Wnd:=Handle;

    Rgn:=0;
    if (Wnd<>0) and IsWindowVisible(Wnd) then // No update region for the window.  changes won't be painted.
    begin
      Rgn:=CreateRectRgn(0,0,0,0);
      SetWindowRgn(Wnd,Rgn,FALSE);
    end;

    // Normally, processing WM_SETTEXT would cause all sorts of flicker as it
    // changed the caption text of the window.  But, we've told it that the
    // update region for the window (the portion it is allowed to paint in) is
    // a NULL region (a rectangle equal to 0, 0, 0, 0).  So, the changes don't
    // have anywhere to paint now, so it is safe to call inherited at this
    // point.  After that, we'll restore the window region so that painting
    // can happen again.
    inherited;

    if Rgn<>0 then
    begin
      SetWindowRgn(Wnd,0,FALSE); // Reset region to normal.
      DeleteObject(Rgn);
      if MaximizedChild then TStyleForm(Application.MainForm).UpdateCaption(IsActiveWindow)
      else UpdateCaption(IsActiveWindow);
    end;
  end
  else inherited;
end;

procedure TStyleForm.WMSettingChange(var Msg: TMessage);
begin
  if UseStyleCaption then
  begin
    // User might have changed NC font.
    if Msg.wParam=SPI_SETNONCLIENTMETRICS then UpdateCaptionFont;
  end;
  inherited;
end;

{: This procedure is used to paint the caption gradient.  It is normally
   called internally, but it can be used any time a repaint of the caption
   is needed. The <B>Active</B> parameter is used to indicate whether the
   caption should be painted as the active window or an inactive window. }
procedure TStyleForm.UpdateCaption(Active: Boolean);
var
  DC: HDC;
begin
  if csDestroying in ComponentState then Exit;
  // Get the DC we need to paint in.  GetDC would only get the DC for the
  // client area, we need it for non-client area, too, so we use GetWindowDC.
  DC:=GetWindowDC(Handle);
  try
    if ThemeServices.ThemesEnabled then DrawThemeCaption(DC,Active)
    else DrawCaption(DC,Active);
  finally
    ReleaseDC(Handle,DC); // NEVER leave this hanging.
  end;
end;

procedure TStyleForm.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
begin
  inherited;
  if UseStyleCaption and not ThemeServices.ThemesEnabled then
    UpdateCaption(IsActiveWindow);
  if (WindowState=wsMaximized) and ((Win32MajorVersion<6) or (Win32MinorVersion<1)) then
  begin
    CaptionMouseDown:=True;
    CaptionMouseDownPos:=Point(Msg.XCursor,Msg.YCursor);
  end;
end;

procedure TStyleForm.WMNCLButtonUp(var Msg: TWMNCLButtonUp);
begin
  inherited;
  CaptionMouseDown:=False;
end;

procedure TStyleForm.WMNCMouseMove(var Msg: TWMNCMouseMove);
var
  X : Integer;
begin
  inherited;
  if CaptionMouseDown then
  begin
    if GetKeyState(VK_LBUTTON)<0 then
    begin
      if Abs(Msg.XCursor-CaptionMouseDownPos.X)>Width div 4 then
      begin
        X:=ScreenToClient(Point(Msg.XCursor,Msg.YCursor)).X;
        if X>Width-Width div 10 then // Move window right
        begin
          X:=Left+Width+10;
          if X<=Screen.DesktopRect.Right then
          begin
            CaptionMouseDown:=False;
            WindowState:=wsNormal;
            Left:=Min(X,Screen.DesktopRect.Right-10);
            WindowState:=wsMaximized;
          end;
        end
        else if X<Width div 10 then // Move window left
        begin
          X:=Left-10;
          if X>=Screen.DesktopRect.Left then
          begin
            CaptionMouseDown:=False;
            WindowState:=wsNormal;
            Left:=Max(Screen.DesktopRect.Left,X-Width);
            WindowState:=wsMaximized;
          end;
        end;
      end;
    end
    else CaptionMouseDown:=False;
  end;
end;

procedure TStyleForm.WMSysCommand(var Msg: TWMSysCommand);
begin
  if Msg.CmdType=SC_MINIMIZE then
  begin
    if Self=Application.MainForm then
    begin
      {$IFDEF VistaTaskbarButton}
      if WindowsIsVistaOrLater then
      begin
        ShowWindow(Handle,SW_MINIMIZE);
        Msg.Result:=0;
      end
      else
      {$ENDIF}
      inherited;
    end
    else if Assigned(Application.MainForm) and (FormStyle<>fsMDIChild) then
    begin
      {$IFDEF VistaTaskbarButton}
      if (Application.MainForm is TStyleForm) and TStyleForm(Application.MainForm).ShowTaskBarButton then
      begin
        Assert(FParentHandle<>0);
        ShowWindow(Application.MainForm.Handle,SW_MINIMIZE);
      end
      else
      {$ENDIF}
      begin
        Assert(FParentHandle=0);
        Application.Minimize;
      end;
      Msg.Result:=0;
    end
    else inherited;
  end
  else if UseStyleCaption then
  begin
    // Help button pressed, do't call Draw() because it will draw it in the up state.
    if Msg.CmdType=SC_CONTEXTHELP then
    begin
      if Assigned(OnHelpButtonClick) then OnHelpButtonClick(Self)
      else inherited
    end
    else
    begin
      UpdateCaption(IsActiveWindow);
      inherited;
      UpdateCaption(IsActiveWindow);
    end;
  end
  else if (Msg.CmdType=SC_CONTEXTHELP) and Assigned(OnHelpButtonClick) then OnHelpButtonClick(Self)
  else inherited;
end;

procedure TStyleForm.WMEnterIdle(var Msg: TWMEnterIdle);
begin
  if UseStyleCaption then UpdateCaption(IsActiveWindow);
  inherited;
end;

procedure TStyleForm.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
  with Msg.WindowPos^ do
    if FCreating and ((Flags and SWP_HIDEWINDOW) <> 0) then
      Flags := Flags or SWP_NOREDRAW;
  inherited;
end;

function TStyleForm.GetSysCaptionLogFont: TLogFont;
var
  NCM: TNonClientMetrics;
begin
  ZeroMem(Result, SizeOf(Result));
  NCM.cbSize := SizeOf(NCM);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0) then
  begin
    if BorderStyle in [bsToolWindow, bsSizeToolWin] then
      Result := NCM.lfSmCaptionFont
    else
      Result := NCM.lfCaptionFont;
  end;
end;

procedure TStyleForm.UpdateCaptionFont;
var
  CF: TLogFont;
  FS: TFontStyles;
begin
  CF := GetSysCaptionLogFont;
  FCaptionFont.Charset := TFontCharset(CF.lfCharSet);
  FCaptionFont.Name := CF.lfFaceName;
  FCaptionFont.Height := CF.lfHeight;
  case CF.lfPitchAndFamily and $F of
    VARIABLE_PITCH: FCaptionFont.Pitch := fpVariable;
    FIXED_PITCH: FCaptionFont.Pitch := fpFixed;
  else
    FCaptionFont.Pitch := fpDefault;
  end;
  FS := [];
  if CF.lfWeight >= FW_BOLD then
    Include(FS, fsBold);
  if CF.lfItalic = 1 then
    Include(FS, fsItalic);
  if CF.lfUnderline = 1 then
    Include(FS, fsUnderline);
  if CF.lfStrikeOut = 1 then
    Include(FS, fsStrikeOut);
  FCaptionFont.Style := FS;

  InvalidateCaption;
end;

procedure TStyleForm.InvalidateCaption;
begin 
  if HandleAllocated and UseStyleCaption then
  begin
    // Make the non client area repaint.
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_DRAWFRAME or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

procedure TStyleForm.MoveWindowTo(Pos: TPoint);
var
  Desktop : TRect;
begin
  if Pos.X=Low(Integer) then Exit;
  Position:=poDesigned;
  if Screen.MonitorCount>1 then Desktop:=Screen.DesktopRect
  else Desktop:=Screen.WorkAreaRect;
  Left:=EnsureRange(Pos.X,Desktop.Left,Desktop.Right-Width);
  Top:=EnsureRange(Pos.Y,Desktop.Top,Desktop.Bottom-Height);
end;

function ClientWindowProc(Wnd: HWND; Msg: Cardinal; WParam,LParam: Integer): Integer; stdcall;
var
  F : Pointer;
begin
  F:=Pointer(GetWindowLong(Wnd,GWL_USERDATA));
  case Msg of
    WM_CHAR :
      if Assigned(Application.MainForm.OnKeyPress) and (Application.MainForm.ClientHandle=Wnd) then
      begin
        Application.MainForm.OnKeyPress(Application.MainForm, PChar(@WParam)^); // KeyDataToShiftState(LParam)
        if PChar(@WParam)^=#0 then
        begin
          Result:=1;
          Exit;
        end;
      end;
    WM_KEYDOWN :
      if Assigned(Application.MainForm.OnKeyDown) and (Application.MainForm.ClientHandle=Wnd) then
      begin
        Application.MainForm.OnKeyDown(Application.MainForm, PWord(@WParam)^, KeyDataToShiftState(LParam));
        if PWord(@WParam)^=0 then
        begin
          Result:=1;
          Exit;
        end;
      end;
    WM_NCCALCSIZE :
      if (GetWindowLong(Wnd,GWL_STYLE) and (WS_HSCROLL or WS_VSCROLL))<>0 then
        SetWindowLong(Wnd,GWL_STYLE,GetWindowLong(Wnd,GWL_STYLE) and not (WS_HSCROLL or WS_VSCROLL));
  end;
  Result:=CallWindowProc(F,Wnd,Msg,WParam,LParam);
end;

procedure TStyleForm.PreventFormScrollbars;
begin
  HorzScrollBar.Visible:=False;
  VertScrollBar.Visible:=False;
  if (ClientHandle<>0) and (GetWindowLong(ClientHandle,GWL_USERDATA)=0) then
    SetWindowLong(ClientHandle,GWL_USERDATA,SetWindowLong(ClientHandle,GWL_WNDPROC,Integer(@ClientWindowProc)));
end;

// Procedure used to determine code segment offset in LookupCodeLocation
procedure __CodeSegmentTestDummy__;
begin
end;

function LookupCodeLocation(Address: Pointer; MapFileName: string=''): string;
var
  MapFile : TextFile;
  Line, BestMethod, ModuleName, BestModuleName : string;
  CodeOffset, CodeAddress, AddressDist, BestAddressDist, BestLineNumber : DWord;
  UnitAddress : TStringIntegerList;
  I : Integer;
begin
  Result:=IntToHex(DWord(Address),8);
  try
    CodeOffset:=$401000;
    if MapFileName='' then // Lookup in the running instance
    begin
      MapFileName:=RemoveFileExt(GetModuleName(HInstance))+'.map';
      if FileExists(MapFileName) then CodeOffset:=High(CodeOffset)
      else
      begin
        if IsLibrary then Result:=IntToHex(DWord(Address)-(HInstance+$1000)+CodeOffset,8);
        Exit;
      end;
    end;
    AssignFile(MapFile,MapFileName);
    Reset(MapFile);
    try
      // Skip first part
      repeat
        ReadLn(MapFile,Line);
      until StartsWith('Detailed map of segments',Line);
      ReadLn(MapFile);

      UnitAddress:=TStringIntegerList.Create;
      try
        // Read unit code address table
        repeat
          ReadLn(MapFile,Line);
          if not StartsWith(' 0001:',Line) then Break;
          I:=PosEx(' ',Line,60);
          UnitAddress.AddValue(Copy(Line,60,I-60),StrToInt('$'+Copy(Line,7,8)));
        until Line='';

        // Skip until proc entry point list
        repeat
          ReadLn(MapFile,Line);
        until StartsWith('  Address',Line);
        ReadLn(MapFile);

        // Determine code segment start address
        if CodeOffset=High(CodeOffset) then
        begin
          repeat
            ReadLn(MapFile,Line);
            if StartsWith(' 0001:',Line) and EndsWith('__CodeSegmentTestDummy__',Line) then
            begin
              CodeOffset:=DWord(@__CodeSegmentTestDummy__)-DWord(StrToInt('$'+Copy(Line,7,8)));
              Break;
            end;
          until Line='';
        end;
        if CodeOffset=High(CodeOffset) then 
        begin
          Assert(False,'__CodeSegmentTestDummy__ not found in MAP file');
          Exit;
        end;
        CodeAddress:=DWord(Address)-CodeOffset;

        // Find unit name
        for I:=1 to UnitAddress.Count-1 do
          if DWord(UnitAddress.Value[I])>CodeAddress then
          begin
            Result:=Result+', '+UnitAddress[I-1];
            Break;
          end;
      finally
        UnitAddress.Free;
      end;      

      // Skip the rest of the section
      repeat
        ReadLn(MapFile,Line);
      until Line='';
      repeat
        ReadLn(MapFile,Line);
      until StartsWith('  Address',Line);
      ReadLn(MapFile);

      // Scan method entry table to find best match for exception adress
      BestAddressDist:=MaxInt;
      repeat
        ReadLn(MapFile,Line);
        if StartsWith(' 0001:',Line) then
        begin
          AddressDist:=CodeAddress-DWord(StrToInt('$'+Copy(Line,7,8)));
          if AddressDist<BestAddressDist then
          begin
            BestMethod:=Line;
            BestAddressDist:=AddressDist;
          end;
        end;
      until Line='';
      if BestAddressDist<$1000000 then
        Result:=Result+', '+Copy(BestMethod,22,MaxInt);

      // Scan line number table
      BestAddressDist:=1024;
      BestLineNumber:=0;
      repeat
        ReadLn(MapFile,Line);
        if StartsWith('Line numbers for',Line) then
          ModuleName:=Copy(Line,18,Pos('(',Line)-18)
        else if Line='' then
          Continue
        else if Line[1]=' ' then
        begin
          for I:=0 to Length(Line) div 20-1 do
          begin
            AddressDist:=CodeAddress-DWord(StrToInt('$'+Copy(Line,13+I*20,8)));
            if AddressDist<BestAddressDist then
            begin
              BestModuleName:=ModuleName;
              BestLineNumber:=StrToInt(Copy(Line,2+I*20,5));
              BestAddressDist:=AddressDist;
            end;
          end;
        end
        else Break;
      until EOF(MapFile);
      if BestLineNumber<>0 then
        Result:=Result+', '+BestModuleName+' line '+IntToStr(BestLineNumber);
    finally
      CloseFile(MapFile);
    end;
  except
  end;
end;

type
  TExceptObjProc = function(P: PExceptionRecord): TObject;
  TErrorProc = procedure(ErrorCode: Byte; ErrorAddr: Pointer);

function ExceptObjProcDummy(P: PExceptionRecord): TObject;
begin
  Result:=TObject.Create;
end;

var
  OldExceptObjProc : TExceptObjProc;
  OldErrorProc : TErrorProc;
  ExceptionStackTrace : array[0..20] of Pointer;

function ExceptObjProcStackTrace(P: PExceptionRecord): TObject;
begin
  ExceptionStackTrace[0]:=nil;
  case P.ExceptionCode of
    STATUS_STACK_OVERFLOW :
      begin
        if GetCurrentThreadId=MainThreadID then
        begin
          DisableDrawing:=True; // Disable drawing to prevent AVs in paint events while showing message box
          MessageBox(Application.Handle,'Stack overflow.',nil,MB_ICONERROR);
        end;
        GetStackTrace(3,High(ExceptionStackTrace),@ExceptionStackTrace[0]);
      end;
    STATUS_INTEGER_DIVIDE_BY_ZERO,
    STATUS_ACCESS_VIOLATION,
    STATUS_PRIVILEGED_INSTRUCTION :
      GetStackTrace(3,High(ExceptionStackTrace),@ExceptionStackTrace[0]);
  end;
  Result:=OldExceptObjProc(P);
end;

procedure ErrorProcStackTrace(ErrorCode: Byte; ErrorAddr: Pointer);
begin
  if ErrorCode=Byte(reInvalidPtr) then
    GetStackTrace(2,High(ExceptionStackTrace),@ExceptionStackTrace[0]);
  OldErrorProc(ErrorCode,ErrorAddr);
end;

procedure GetStackTrace(SkipLevels,MaxLevels: Integer; DestArray: PInteger);
var
  Addr, I : Integer;
begin
  asm
    mov Addr,ebp
  end;
  ExceptObjProc:=@ExceptObjProcDummy;
  try
    for I:=1 to MaxLevels do
    begin
      if (Abs(Addr)<$10000) or (Abs(PInteger(Addr+4)^)<$10000) then Break;
      if I>SkipLevels then
      begin
        DestArray^:=PInteger(Addr+4)^;
        Inc(DestArray);
      end;
      Addr:=PInteger(Addr)^; // Next level
    end;
  except
  end;
  ExceptObjProc:=@ExceptObjProcStackTrace;
  DestArray^:=0;
end;

function StackTraceToString(CallStackArray: PInteger; const Separator: string): string;
begin
  if CallStackArray=nil then CallStackArray:=@ExceptionStackTrace[0];
  while CallStackArray^<>0 do
  begin
    Result:=Result+Separator+LookupCodeLocation(Pointer(CallStackArray^));
    Inc(CallStackArray);
  end;
end;

class procedure TStyleForm.StyleFormExceptionEvent(Sender: TObject; E: Exception);
const
  NewLine = #10#13;
var
  Msg, Module, Access : string;
  P : Integer;
  BugReport : TStringList;
begin
  try
    Msg:=E.Message;
    if E is EAccessViolation then
    begin
      P:=Pos('''',Msg);
      if P=0 then Module:=''
      else
      begin
        Module:=Copy(Msg,P+1,MaxInt);
        SetLength(Module,Pos('''',Module));
        Module:=' in module '''+Module;
      end;
      P:=StrToInt('$'+Copy(Msg,29,8)); // Code address
      if Abs(P)<1024 then Msg:='Invalid access to memory location'+Module+'. NIL function or method call'
      else
      begin                                               
        Access:=Copy(Msg,LastDelimiter('.',Msg)+2,MaxInt);
        SetLength(Access,Pos(' ',Access)-1);
        if Access=SReadAccess then Access:='Read'
        else Access:='Write';
        P:=StrToInt('$'+Copy(Msg,Length(Msg)-7,8)); // Data address
        if Abs(P)<1024 then Msg:='Invalid access to memory location'+Module+'. '+Access+' of NIL pointer'
        else Msg:='Invalid access to memory location'+Module+': '+Access;
      end;
    end;
    if (Msg<>'') and (AnsiLastChar(Msg)>'.') then Msg:=Msg+'.';

    if Assigned(Application) and
       ((E is EAccessViolation) or
        (E is EPrivilege) or
        (E is EInvalidPointer) or
        (E is EListError) or
        (E is EIntError) or
        (E is EMathError)) then
    begin
      case MessageDlgStr(Msg,mtError,[SMsgDlgIgnore,rsSendBugReport,rsTerminateApplication],1) of
        2 : try
              Screen.Cursor:=crHourGlass;
              if IsLibrary then
                with GetVersInfo(GetModuleName(HInstance)) do
                  Msg:='Module: '+ExtractFileName(GetModuleName(HInstance))+' '+Format('%d.%d.%d %d',[productversion.major,productversion.minor,productversion.build,productversion.rel])+NewLine+Msg;
              Msg:='Application: '+ExtractFileName(ParamStr(0))+' '+VersionStr+NewLine+Msg;
              Msg:=Msg+NewLine+NewLine+'Please describe what you did do before the error occurred:';
              Msg:=Msg+NewLine+NewLine+NewLine+'(You cannot expect a direct reply to this bug report, but it will be used to make the next version better!)'+NewLine+NewLine;
              Msg:=Msg+NewLine+'Exception address: '+LookupCodeLocation(ExceptAddr);
              if ExceptionStackTrace[0]<>nil then
                Msg:=Msg+NewLine+'Stack trace: '+StackTraceToString(@ExceptionStackTrace[0],NewLine);
              if Assigned(Sender) then Msg:=Msg+NewLine+'Caught by: '+Sender.ClassName;
              if Assigned(Screen.ActiveForm) then Msg:=Msg+NewLine+'Active form: '+Screen.ActiveForm.ClassName;
              Msg:=Msg+NewLine+Format('Windows version: %d.%d',[Win32MajorVersion,Win32MinorVersion]);

              BugReport := TStringList.Create;
              try
                BugReport.values['to']:='bugreports@logicnet.dk';
                BugReport.values['subject'] := 'MeeSoft Bug report';
                BugReport.values['body'] := Msg;
                if not SendEMailMailto(Application.Handle, BugReport) then
                  SendEMailMapi(Application.Handle, BugReport);
              finally
                BugReport.Free;
              end;
            finally
              Screen.Cursor:=crDefault;
            end;
        3 : Application.Terminate;
      end;
    end
    else if E is EStackOverflow then
    begin       
      Msg:=E.Message+NewLine+NewLine+'The application will now terminate.';
      if ExceptionStackTrace[0]<>nil then
        Msg:=Msg+NewLine+NewLine+'Stack trace: '+StackTraceToString(@ExceptionStackTrace[0],NewLine);
      MessageBox(Application.Handle,PChar(Msg),nil,MB_ICONERROR);
      Application.Terminate;
    end
    else MessageDlg(Msg,mtError);
  except
    MessageBox(0,'Exception',nil,MB_ICONERROR);
  end;
end;

{$ENDIF}

//==============================================================================================================================
// TMenuItemEx
//==============================================================================================================================

class procedure TMenuItemEx.AdvancedDrawTopItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  EdgeStyle: array[Boolean] of Longint = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  TopLevel = True;
var
  ImageList: TCustomImageList;
  ParentMenu: TMenu;
  Alignment: TPopupAlignment;
  DrawImage, DrawGlyph: Boolean;
  GlyphRect, SaveRect: TRect;
  DrawStyle: Longint;
  Glyph: TBitmap;
  OldBrushColor: TColor;
  Selected: Boolean;
  Win98Plus: Boolean;
  Win2K: Boolean;
  WinXP: Boolean;

  procedure NormalDraw;
  begin
    with ACanvas, TMenuItemEx(Sender) do
    begin
      if WinXP then
      begin
        if (odSelected in State) or (odHotLight in State) then
        begin
          if ThemeServices.ThemesEnabled then
            Brush.Color := clMenuHighlight
          else
            Brush.Color := clHighlight;
          Font.Color := clHighlightText;
        end
        else if TopLevel then
          Brush.Color := clBtnFace
      end;
      //ImageList := GetImageList;
      { With XP, we need to always fill in the rect, even when selected }
      if not Selected or WinXP then
        FillRect(ARect);
      if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).Alignment
      else
        Alignment := paLeft;
      GlyphRect.Left := ARect.Left + 1;
      GlyphRect.Top := ARect.Top + 1;
      if Caption = cLineCaption then
      begin
        FillRect(ARect);
        GlyphRect.Left := 0;
        GlyphRect.Right := -4;
        DrawGlyph := False;
      end
      else
      begin
        DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
          (ImageIndex < ImageList.Count) or Checked and ((Bitmap = nil) or
          Bitmap.Empty));
        if DrawImage or Assigned(Bitmap) and not Bitmap.Empty then
        begin
          DrawGlyph := True;

          if DrawImage then
          begin
            GlyphRect.Right := GlyphRect.Left + ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            { Need to add BitmapWidth/Height properties for TMenuItem if we're to
              support them.  Right now let's hardcode them to 16x16. }
            GlyphRect.Right := GlyphRect.Left + 16;
            GlyphRect.Bottom := GlyphRect.Top + 16;
          end;

          { Draw background pattern brush if selected }
          if Checked and not WinXP then
          begin
            Inc(GlyphRect.Right);
            Inc(GlyphRect.Bottom);
            OldBrushColor := Brush.Color;
            if not (odSelected in State) then
            begin
              OldBrushColor := Brush.Color;
              Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
              FillRect(GlyphRect);
            end
            else
            begin
              Brush.Color := clBtnFace;
              FillRect(GlyphRect);
            end;
            Brush.Color := OldBrushColor;
            Inc(GlyphRect.Left);
            Inc(GlyphRect.Top);
          end;

          if DrawImage then
          begin
            if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                Enabled)
            else
            begin
              { Draw a menu check }
              Glyph := TBitmap.Create;
              try
                Glyph.Transparent := True;
                Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
                OldBrushColor := Font.Color;
                Font.Color := clBtnText;
                Draw(GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
                  GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);
                Font.Color := OldBrushColor;
              finally
                Glyph.Free;
              end;
            end;
          end
          else
          begin
            SaveRect := GlyphRect;
            { Make sure image is within glyph bounds }
            if Bitmap.Width < GlyphRect.Right - GlyphRect.Left then
              with GlyphRect do
              begin
                Left := Left + ((Right - Left) - Bitmap.Width) div 2 + 1;
                Right := Left + Bitmap.Width;
              end;
            if Bitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
              with GlyphRect do
              begin
                Top := Top + ((Bottom - Top) - Bitmap.Height) div 2 + 1;
                Bottom := Top + Bitmap.Height;
              end;
            StretchDraw(GlyphRect, Bitmap);
            GlyphRect := SaveRect;
          end;

          if Checked then
          begin
            Dec(GlyphRect.Right);
            Dec(GlyphRect.Bottom);
          end;
        end
        else
        begin
          if (ImageList <> nil) and not TopLevel then
          begin
            GlyphRect.Right := GlyphRect.Left + ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            GlyphRect.Right := GlyphRect.Left;
            GlyphRect.Bottom := GlyphRect.Top;
          end;
          DrawGlyph := False;
        end;
      end;
      with GlyphRect do
      begin
        Dec(Left);
        Dec(Top);
        Inc(Right, 2);
        Inc(Bottom, 2);
      end;

      if Checked or Selected and DrawGlyph then
        if not WinXP then
          DrawEdge(Handle, GlyphRect, EdgeStyle[Checked], BF_RECT);

      if Selected then
      begin
        if DrawGlyph then ARect.Left := GlyphRect.Right + 1;
        if not (Win98Plus and TopLevel) then
          Brush.Color := clHighlight;
        FillRect(ARect);
      end;
      if TopLevel and Win98Plus and not WinXP then
      begin
        if Selected then
          DrawEdge(Handle, ARect, BDR_SUNKENOUTER, BF_RECT)
        else if odHotLight in State then
          DrawEdge(Handle, ARect, BDR_RAISEDINNER, BF_RECT);
        if not Selected then
          OffsetRect(ARect, 0, -1);
      end;

      if not (Selected and DrawGlyph) then
        ARect.Left := GlyphRect.Right + 1;
      Inc(ARect.Left, 2);
      Dec(ARect.Right, 1);

      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;
      if odDefault in State then
        Font.Style := [fsBold];
      DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
      if TopLevel and Selected and Win98Plus and not WinXP then
        OffsetRect(ARect, 1, 0);

      DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle);
      if (ShortCut <> 0) and not TopLevel then
      begin
        ARect.Left := ARect.Right;
        ARect.Right := SaveRect.Right - 10;
        DoDrawText(ACanvas, ShortCutToText(ShortCut), ARect, Selected, DT_RIGHT);
      end;

    end;
  end;

  procedure BiDiDraw;
  var
    S: string;
  begin
    with ACanvas, TMenuItemEx(Sender) do
    begin
      if WinXP then
      begin
        if (odSelected in State) or (odHotLight in State) then
        begin
          if ThemeServices.ThemesEnabled then
            Brush.Color := clMenuHighlight
          else
            Brush.Color := clHighlight;
          Font.Color := clHighlightText;
        end
        else if TopLevel then
          Brush.Color := clBtnFace
      end;
      //ImageList := GetImageList;
      { With XP, we need to always fill in the rect, even when selected }
      if not Selected or (WinXP and not Checked) then
        FillRect(ARect);
      if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).Alignment
      else
        Alignment := paLeft;
      GlyphRect.Right := ARect.Right - 1;
      GlyphRect.Top := ARect.Top + 1;
      if Caption = cLineCaption then
      begin
        FillRect(ARect);
        GlyphRect.Left := GlyphRect.Right + 2;
        GlyphRect.Right := 0;
        DrawGlyph := False;
      end
      else
      begin
        DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
          (ImageIndex < ImageList.    Count) or Checked and ((Bitmap = nil) or
          Bitmap.Empty));    
        if DrawImage or Assigned(Bitmap) and not Bitmap.Empty then
        begin
          DrawGlyph := True;    
    
          if DrawImage then
          begin
            GlyphRect.Left := GlyphRect.Right - ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            { Need to add BitmapWidth/Height properties for TMenuItem if we're to
              support them.  Right now let's hardcode them to 16x16. }
            GlyphRect.Left := GlyphRect.Right - 16;
            GlyphRect.Bottom := GlyphRect.Top + 16;
          end;    
    
          { Draw background pattern brush if selected }
          if Checked then
          begin
            Dec(GlyphRect.Left);
            Inc(GlyphRect.Bottom);
            OldBrushColor := Brush.Color;
            if not (odSelected in State) then
            begin
              OldBrushColor := Brush.Color;
              Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
              FillRect(GlyphRect);
            end
            else
            begin
              Brush.Color := clBtnFace;
              FillRect(GlyphRect);
            end;
            Brush.Color := OldBrushColor;
            Dec(GlyphRect.Right);
            Inc(GlyphRect.Top);
          end;
    
          if DrawImage then
          begin
            if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                Enabled)
            else
            begin
              { Draw a menu check }
              Glyph := TBitmap.Create;
              try
                Glyph.Transparent := True;
                Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
                OldBrushColor := Font.Color;
                Font.Color := clBtnText;
                Draw(GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
                  GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);
                Font.Color := OldBrushColor;
              finally
                Glyph.Free;
              end;
            end;
          end
          else
          begin
            SaveRect := GlyphRect;
            { Make sure image is within glyph bounds }
            if Bitmap.Width < GlyphRect.Right - GlyphRect.Left then
              with GlyphRect do
              begin
                Right := Right - ((Right - Left) - Bitmap.Width) div 2 + 1;
                Left := Right - Bitmap.Width;
              end;
            if Bitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
              with GlyphRect do
              begin
                Top := Top + ((Bottom - Top) - Bitmap.Height) div 2 + 1;
                Bottom := Top + Bitmap.Height;
              end;
            StretchDraw(GlyphRect, Bitmap);
            GlyphRect := SaveRect;
          end;

          if Checked then
          begin
            Dec(GlyphRect.Right);    
            Dec(GlyphRect.Bottom);    
          end;
        end
        else
        begin
          if (ImageList <> nil) and not TopLevel then
          begin
            GlyphRect.Left := GlyphRect.Right - ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            GlyphRect.Left := GlyphRect.Right;
            GlyphRect.Bottom := GlyphRect.Top;
          end;
          DrawGlyph := False;
        end;
      end;    
      with GlyphRect do
      begin
        Dec(Left);
        Dec(Top);
        Inc(Right, 2);    
        Inc(Bottom, 2);    
      end;    
    
      if Checked or Selected and DrawGlyph and not WinXP then
        DrawEdge(Handle, GlyphRect, EdgeStyle[Checked], BF_RECT);

      if Selected then
      begin
        if DrawGlyph then ARect.Right := GlyphRect.Left - 1;
        if not (Win98Plus and TopLevel) then
          Brush.Color := clHighlight;
        FillRect(ARect);
      end;    
      if TopLevel and Win98Plus and not WinXP then
      begin
        if Selected then
          DrawEdge(Handle, ARect, BDR_SUNKENOUTER, BF_RECT)
        else if odHotLight in State then
          DrawEdge(Handle, ARect, BDR_RAISEDINNER, BF_RECT);
        if not Selected then
          OffsetRect(ARect, 0, -1);
      end;
      if not (Selected and DrawGlyph) then
        ARect.Right := GlyphRect.Left - 1;
      Inc(ARect.Left, 2);    
      Dec(ARect.Right, 1);
      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];    
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;    
      if odDefault in State then
        Font.Style := [fsBold];
      DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);    
      { the DT_CALCRECT does not take into account alignment }
      ARect.Left := SaveRect.Left;
      ARect.Right := SaveRect.Right;
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);    
      if TopLevel and Selected and Win98Plus then
        OffsetRect(ARect, 1, 0);
      DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle);    
      if (ShortCut <> 0) and not TopLevel then
      begin
        S := ShortCutToText(ShortCut);
        ARect.Left := 10;
        ARect.Right := ARect.Left + ACanvas.TextWidth(S);
        DoDrawText(ACanvas, S, ARect, Selected, DT_RIGHT);
      end;
    end;    
  end;

begin
  if DisableDrawing then Exit;
  ParentMenu := TMenuItemEx(Sender).GetParentMenu;
  ImageList := TMenuItemEx(Sender).GetImageList;
  Selected := odSelected in State;
  Win98Plus := (Win32MajorVersion > 4) or
    ((Win32MajorVersion = 4) and (Win32MinorVersion > 0));
  Win2K := (Win32MajorVersion > 4) and (Win32Platform = VER_PLATFORM_WIN32_NT);
  WinXP := (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1);
  if (ParentMenu <> nil) and (not ParentMenu.IsRightToLeft) then
    NormalDraw
  else
    BiDiDraw;
end;

//==============================================================================================================================
// TSpeedButtonEx
//==============================================================================================================================

procedure TSpeedButtonEx.Paint;
var
  Bitmap : TBitmap;
  P : TPoint;
begin
  inherited Paint;
  if (Win32MajorVersion>=6) and Enabled and (Glyph.Width>0) then
  begin
    Bitmap := TBitmap.Create;
    Bitmap.Assign(Glyph);
    Bitmap.PixelFormat:=pf32bit;
    Bitmap.PixelFormat:=pf24bit;
    if NumGlyphs>1 then Bitmap.Width:=Bitmap.Width div NumGlyphs;
    Bitmap.Transparent:=True;
    P:=Point((Width-Bitmap.Width+1) div 2,(Height-Bitmap.Height+1) div 2);
    if FState=bsDown then
    begin
      Inc(P.X);
      if not ThemeServices.ThemesEnabled then Inc(P.Y);
    end;
    Canvas.Draw(P.X,P.Y,Bitmap);
    Bitmap.Free;
  end;
end;

//==============================================================================================================================

procedure SetSeparatorVisible(var Separator: TToolButton; AVisible: Boolean; ALeft: Integer);
var
  New : TToolButton;
begin
  with Separator do
  if AVisible<>Visible then
  begin
    New:=TToolButton.Create(Owner);
    if Action=nil then
    begin
      New.Hint:=Hint;
      New.ImageIndex:=ImageIndex;
      New.OnClick:=OnClick;
    end
    else New.Action:=Action;
    New.AutoSize:=False;
    New.Width:=Width;
    New.Height:=Height;
    if ALeft=Low(Integer) then New.Left:=Left
    else New.Left:=ALeft;
    New.Visible:=AVisible;
    New.Style:=Style;
    New.Parent:=Parent;
    Parent:=nil;
    Free;
    Separator:=New;
  end;
end;

procedure ClearDelphiLabelCaptions(Control: TWinControl);
var
  I : Integer;
begin
  with Control do
  for I:=0 to ControlCount-1 do
    if Controls[I] is TLabel then
    begin
      if Controls[I].Name=TLabel(Controls[I]).Caption then TLabel(Controls[I]).Caption:='';
    end
    else if Controls[I] is TWinControl then ClearDelphiLabelCaptions(TWinControl(Controls[I]));
end;

procedure ResetControlAnchors(Control: TControl);
var
  I : Integer;
begin
  Control.Anchors:=[akLeft,akTop];
  if Control is TWinControl then
    with TWinControl(Control) do
      for I:=0 to ControlCount-1 do ResetControlAnchors(Controls[I]);
end;

procedure AutoSizeCheckBox(CheckBox: TCheckBox);
var
  Canvas : TCanvas;
  Control : TWinControl;
begin
  if Assigned(Application.MainForm) then Canvas:=Application.MainForm.Canvas
  else
  begin
    Control:=CheckBox.Parent;
    while Assigned(Control) and not (Control is TCustomForm) do Control:=Control.Parent;
    Assert(Assigned(Control));
    Canvas:=TCustomForm(Control).Canvas;
  end;
  CheckBox.Width:=19+Canvas.TextWidth(StripHotkey(CheckBox.Caption));
end;

procedure RecursiveSetEnabled(Control: TControl; Enabled: Boolean);
var
  I : Integer;
begin
  Control.Enabled:=Enabled;
  if Control is TWinControl then
    for I:=0 to TWinControl(Control).ControlCount-1 do
      RecursiveSetEnabled(TWinControl(Control).Controls[I],Enabled);
end;

//==============================================================================================================================
// MakeLinkLabel
//==============================================================================================================================

type
  TLinkExecute = class
                   class procedure OnClick(Sender: TObject);
                 end;

class procedure TLinkExecute.OnClick(Sender: TObject);
begin
  if ExecuteFile(TControl(Sender).Hint,'','')<=32 then RaiseLastOSError;
end;

// Turn a label into a link. The Hint property must be the link target.
procedure MakeLinkLabel(LinkLabel: TLabel);
begin
  Assert(LinkLabel.Hint<>'');
  LinkLabel.Font.Color:=clBlue;
  LinkLabel.Font.Style:=LinkLabel.Font.Style+[fsUnderline];
  LinkLabel.Cursor:=crHandPoint;
  LinkLabel.OnClick:=TLinkExecute.OnClick;
end;

//==============================================================================================================================
// TMessageForm
//==============================================================================================================================

type
  TMessageForm = class(TStyleForm)
  private
    procedure HelpButtonClick(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent); reintroduce;
  end;

constructor TMessageForm.CreateNew(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  {$IFDEF UseStyleForm}
  FParentHandle:=GetActiveFormHandle;
  {$ENDIF}
  inherited CreateNew(AOwner);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

procedure TMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

var
  Captions: array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError,
    @SMsgDlgInformation, @SMsgDlgConfirm, nil);
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonCaptions: array[TMsgDlgBtn] of Pointer = (
    @SMsgDlgYes, @SMsgDlgNo, @SMsgDlgYesToAll, @SMsgDlgNoToAll, @SMsgDlgAll, @SMsgDlgOK,
    @SMsgDlgRetry, @SMsgDlgIgnore, @SMsgDlgAbort, @SMsgDlgCancel, @SMsgDlgHelp);
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes,mrNo,mrYesToAll,mrNoToAll,mrAll,mrOk,mrRetry,mrIgnore,mrAbort,mrCancel,0);

var
  ButtonWidths : array[TMsgDlgBtn] of Integer;  // Initialized to zero

function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TStyleForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  B, DefaultButton, CancelButton: TMsgDlgBtn;
  IconID: PChar;
  TextRect: TRect;
begin
  Result:=TMessageForm.CreateNew(Application);
  with Result do
  begin
    {$IFDEF UseStyleForm}
    UseBackgroundTheme:=True;
    {$ENDIF}
    BiDiMode := Application.BiDiMode;
    BorderStyle := bsDialog;
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
        if ButtonWidths[B] = 0 then
        begin
          TextRect := Rect(0,0,0,0);
          Windows.DrawText( canvas.handle,
            PChar(LoadResString(ButtonCaptions[B])), -1,
            TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
            DrawTextBiDiModeFlagsReadingOnly);
          with TextRect do ButtonWidths[B] := Right - Left + 8;
        end;
        if ButtonWidths[B] > ButtonWidth then
          ButtonWidth := ButtonWidths[B];
      end;
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg)+1, TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);
    IconID := IconIDs[DlgType];
    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if IconID <> nil then
    begin
      Inc(IconTextWidth, 32 + HorzSpacing);
      if IconTextHeight < 32 then IconTextHeight := 32;
    end;
    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      VertMargin * 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mtCustom then
      Caption := LoadResString(Captions[DlgType]) else
      Caption := Application.Title;
    if IconID <> nil then
      with TImage.Create(Result) do
      begin
        Parent := Result;
        Picture.Icon.Handle := LoadIcon(0, IconID);
        SetBounds(HorzMargin, VertMargin, 32, 32);
        AutoSize:=True;
      end;
    with TLabel.Create(Result) do
    begin
      Parent := Result;
      Transparent:=True;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
      BiDiMode := Result.BiDiMode;
      ShowAccelChar:=False;
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        TextRect.Right, TextRect.Bottom);
    end;
    if mbOk in Buttons then DefaultButton := mbOk else
      if mbYes in Buttons then DefaultButton := mbYes else
        DefaultButton := mbRetry;
    if mbCancel in Buttons then CancelButton := mbCancel else
      if mbNo in Buttons then CancelButton := mbNo else
        CancelButton := mbOk;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
        with TButton.Create(Result) do
        begin
          Parent := Result;
          Caption := LoadResString(ButtonCaptions[B]);
          ModalResult := ModalResults[B];
          if B = DefaultButton then Default := True;
          if B = CancelButton then Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then
            OnClick := TMessageForm(Result).HelpButtonClick;
        end;
  end;
end;

function CreateMessageDialogStr(const Msg: string; DlgType: TMsgDlgType; const Buttons: array of const; CancelButton: Integer): TStyleForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft: Integer;
  B : Integer;
  IconID: PChar;
  TextRect: TRect;
begin
  Result:=TMessageForm.CreateNew(Application);
  with Result do
  begin
    {$IFDEF UseStyleForm}
    UseBackgroundTheme:=True;
    {$ENDIF}
    BiDiMode := Application.BiDiMode;
    BorderStyle := bsDialog;
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    for B:=Low(Buttons) to High(Buttons) do
    begin
      TextRect := Rect(0,0,0,0);
      Windows.DrawText( canvas.handle,
        PChar(Buttons[B].VAnsiString), -1,
        TextRect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
        DrawTextBiDiModeFlagsReadingOnly);
      with TextRect do ButtonWidth:=Max(ButtonWidth,Right-Left+8);
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg)+1, TextRect,
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);
    IconID := IconIDs[DlgType];
    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if IconID <> nil then
    begin
      Inc(IconTextWidth, 32 + HorzSpacing);
      if IconTextHeight < 32 then IconTextHeight := 32;
    end;
    ButtonCount:=Length(Buttons);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      VertMargin * 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
    if DlgType <> mtCustom then
      Caption := LoadResString(Captions[DlgType]) else
      Caption := Application.Title;
    if IconID <> nil then
      with TImage.Create(Result) do
      begin
        Parent := Result;
        Picture.Icon.Handle := LoadIcon(0, IconID);
        SetBounds(HorzMargin, VertMargin, 32, 32);
        AutoSize:=True;
      end;
    with TLabel.Create(Result) do
    begin
      Parent := Result;
      Transparent:=True;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
      BiDiMode := Result.BiDiMode;
      ShowAccelChar:=False;
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
      SetBounds(ALeft, VertMargin,
        TextRect.Right, TextRect.Bottom);
    end;
    X := (ClientWidth - ButtonGroupWidth) div 2;
    Dec(CancelButton);
    if CancelButton>High(Buttons) then CancelButton:=High(Buttons)
    else if CancelButton<Low(Buttons) then CancelButton:=Low(Buttons);
    for B:=Low(Buttons) to High(Buttons) do
    with TButton.Create(Result) do
    begin
      Parent := Result;
      Caption := string(Buttons[B].VAnsiString);
      ModalResult := -1-B;
      if B=Low(Buttons) then Default:=True;
      if B=CancelButton then Cancel:=True;
      SetBounds(X, IconTextHeight + VertMargin + VertSpacing, ButtonWidth, ButtonHeight);
      Inc(X, ButtonWidth + ButtonSpacing);
    end;
  end;
end;

function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint; X, Y: Integer; const HelpFileName: string): Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      HelpContext := HelpCtx;
      HelpFile := HelpFileName;
      if X >= 0 then Left := X;
      if Y >= 0 then Top := Y;
      if (Y < 0) and (X < 0) then Position := poScreenCenter;
      Result := ShowModal;
    finally
      Free;
    end;
end;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := MessageDlgPosHelp(Msg, DlgType, Buttons, HelpCtx, -1, -1, '');
end;

// Result is button number (1,2,3...) or 0 if window closed (Alt-F4). Default is escape will press last button.
function MessageDlgStr(const Msg: string; DlgType: TMsgDlgType; const Buttons: array of const; CancelButton: Integer): Integer;
begin
  with CreateMessageDialogStr(Msg,DlgType,Buttons,CancelButton) do
  try
    Position:=poScreenCenter;
    Result:=ShowModal;
    if Result>0 then Result:=0
    else Result:=-Result;
  finally
    Free;
  end;
end;

procedure ShowMessage(Value: Integer);
begin
  ShowMessage(IntToStr(Value));
end;

procedure ShowMessage(const Msg: string);
begin
  MessageDlg(Msg,mtCustom,[mbOk]);
end;

function InputQuery(const ACaption, APrompt: string; var Value: string; DialogWidth: Integer; PasswordQuery: Boolean; OnChangeText: TNotifyEvent): Boolean;
var
  Form : TInputQueryForm;
  DialogUnits : TPoint;
  ButtonTop, ButtonWidth, ButtonHeight : Integer;
  NonClientMetrics : TNonClientMetrics;
begin
  Result:=False;
  {$IFDEF UseStyleForm}
  Form:=TInputQueryForm.CreateNewWithParent(Application,GetActiveFormHandle);
  {$ELSE}
  Form:=TInputQueryForm.Create(Application);
  {$ENDIF}
  with Form do
    try
      {$IFDEF UseStyleForm}
      UseBackgroundTheme:=True;
      {$ENDIF}
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
        ShowAccelChar:=False;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top+Prompt.Height+MulDiv(3,DialogUnits.Y,8);
        Width := Form.ClientWidth-2*Left;
        if PasswordQuery then PasswordChar:='*';
        OnChange := OnChangeText;
      end;
      ButtonTop := Edit.Top+Edit.Height+MulDiv(9,DialogUnits.Y,8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      OKButton:=TButton.Create(Form);
      with OKButton do
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
      Edit.Text := Value;
      Edit.SelectAll;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

function OpenFileDialog(var FileName: string; const Filter: string; Options: TOpenOptions): Boolean;
var
  Dialog : TOpenDialog;
begin
  SetApplicationHandleForDialog;
  Dialog:=TOpenDialog.Create(nil);
  try
    Dialog.Options:=Options;
    Dialog.Filter:=Filter;

    if FileName<>'' then
    begin
      if FileName[Length(FileName)]='\' then Dialog.InitialDir:=FileName
      else
      begin
        Dialog.InitialDir:=ExtractFilePath(FileName);
        if ((Dialog.InitialDir='') or DirectoryExists(Dialog.InitialDir)) and
           (LastDelimiter('|<>"',FileName)=0) then Dialog.FileName:=FileName;
      end;
    end;
    Result:=Dialog.Execute;
    if Result then
    begin
      if ofAllowMultiSelect	in Options then FileName:=Dialog.Files.Text
      else FileName:=Dialog.FileName;
    end;
  finally
    RestoreApplicationHandle;
    Dialog.Free;
  end;
end;

function SaveFileDialog(var FileName: string; const Filter: string; Options: TOpenOptions): Boolean;
var
  Dialog : TSaveDialog;
begin
  SetApplicationHandleForDialog;
  Dialog:=TSaveDialog.Create(nil);
  try
    Dialog.Options:=Options;
    Dialog.Filter:=Filter;
    Dialog.DefaultExt:='A'; // Just put anything
    if FileName<>'' then
    begin
      if FileName[Length(FileName)]='\' then Dialog.InitialDir:=FileName
      else
      begin
        Dialog.InitialDir:=ExtractFilePath(FileName);
        if ((Dialog.InitialDir='') or DirectoryExists(Dialog.InitialDir)) and
           (LastDelimiter('|<>"',FileName)=0) then SetupSaveDialogFilter(Dialog,FileName);
      end;
    end;
    Result:=Dialog.Execute;
    if Result then FileName:=Dialog.FileName;
  finally
    RestoreApplicationHandle;
    Dialog.Free;
  end;
end;

procedure SetupSaveDialogFilter(Dialog: TSaveDialog; const FileName: string);
var
  Name, Ext, Filter : string;
  P, Index : Integer;
begin
  Name:=RemoveFileExt(FileName);                                              
  if (Name<>'') and (Name[Length(Name)]<>'\') then Dialog.FileName:=Name;     
  Ext:=ExtractFileExtNoDot(FileName);
  if Ext='' then Ext:=Dialog.DefaultExt            
  else Dialog.DefaultExt:=Ext;
  Ext:='*.'+UpperCase(Ext);
  Filter:=UpperCase(Dialog.Filter);
  P:=Pos(Ext,Filter);
  Index:=0;
  for P:=P downto 1 do if Filter[P]='|' then Inc(Index);
  Dialog.FilterIndex:=Index div 2+1;
end;

function GetTaskBarRect: TRect;
var
  BarData: TAppBarData;
begin
  ZeroMem(Result,SizeOf(Result));
  BarData.cbSize:=SizeOf(barData);
  BarData.hWnd:=FindWindow('Shell_TrayWnd',nil);
  SHAppBarMessage(ABM_GETTASKBARPOS,BarData);
  Result:=BarData.rc;
end;

procedure DisableProcessWindowsGhosting;
var
  DisableProcessWindowsGhostingImp : procedure;
begin
  DisableProcessWindowsGhostingImp:=GetProcAddress(GetModuleHandle('user32.dll'),'DisableProcessWindowsGhosting');
  if Assigned(DisableProcessWindowsGhostingImp) then DisableProcessWindowsGhostingImp;
end;

// Return the handle of the active window or nil if it cannot be determined
function GetActiveFormHandle: THandle;
begin
  Result:=0;
  if Screen.ActiveForm<>nil then
  begin
    if (Screen.ActiveForm=Application.MainForm) or (fsModal in Screen.ActiveForm.FormState) then
      Result:=Screen.ActiveForm.Handle
    else if (Screen.ActiveForm.FormStyle=fsMDIChild) and (Screen.ActiveForm.Owner=Application.MainForm) then
      Result:=Application.MainForm.Handle;
  end;
end;

{$IFDEF UseStyleForm}

//var MethodOffset : Integer;

initialization
  OldExceptObjProc:=ExceptObjProc;
  ExceptObjProc:=@ExceptObjProcStackTrace;
  OldErrorProc:=ErrorProc;
  ErrorProc:=@ErrorProcStackTrace;
  Application.OnException:=TStyleForm.StyleFormExceptionEvent;

  {if Win32MajorVersion>=6 then
  begin
    asm
      mov MethodOffset, VMTOFFSET TSpeedButtonEx.Paint
    end;
    FastCodeReplaceVirtualMethod(TSpeedButton,TSpeedButtonEx,MethodOffset);
  end;}

  DisableProcessWindowsGhosting;
  UpdateSystemInfo;
finalization
  GrayThemeBitmap.Free;
{$ENDIF}
end.

