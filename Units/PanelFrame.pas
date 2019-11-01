///////////////////////////////////////////////////////////////////////////////////////////
//
// PanelFrame.pas - TPanelFrame component
// --------------------------------------
// Version:   2005-08-30
// Maintain:  Michael Vinther   |   mv@logicnet·dk
//
// TPanelFrame is a panel which does not paint it's background, but instead has
// a Canvas property and an OnPaint event.
//
// Contains:
//   TPanelFrame
//   TDoubleBufferedPanel
//   TFormFrame
//
unit PanelFrame;

interface

uses
  Classes, ExtCtrls, Messages, Windows, Graphics, Forms, Controls
{$IFDEF UseStyleForm} ,StyleForm; {$ELSE} ; type TStyleForm = TForm; {$ENDIF}

type
  TPanelFrame = class(TCustomControl)
    private
      FRenderThemeBackground : Boolean;
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure SetRenderThemeBackground(const Value: Boolean);
    protected
      FOnPaint : TNotifyEvent;
      procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Clear(Color: TColor=clBtnFace);
      property DockManager;
      property Canvas;
      property WindowHandle;
    published
      property RenderThemeBackground: Boolean read FRenderThemeBackground write SetRenderThemeBackground default False; 
      property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
      property Align;
      property Anchors;
      property AutoSize;
      property BevelInner default bvNone;
      property BevelOuter default bvNone;
      property BevelWidth;
      property BorderWidth;
      property Constraints;
      property Ctl3D;
      property UseDockManager default True;
      property DockSite;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      //property Font;
      //property ParentFont;
      property ParentBiDiMode;
      property ParentCtl3D;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Visible;
      property OnCanResize;
      property OnClick;
      property OnConstrainedResize;
      property OnContextPopup;
      property OnDockDrop;
      property OnDockOver;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnGetSiteInfo;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnResize;
      property OnStartDock;
      property OnStartDrag;
      property OnUnDock;
    end;

  TDoubleBufferedPanel = class(TPanelFrame)
    protected
      FBackBuffer : TBitmap;
      FBitmapCanvas : TCanvas;
      FTotalBorder : Integer;
      procedure Paint; override;
      procedure Resize; override;
    public
      property BackBuffer: TBitmap read FBackBuffer;
      property BitmapCanvas: TCanvas read FBitmapCanvas;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure DrawBuffer;
      procedure Clear(Color: TColor=clBtnFace; Invalidate: Boolean=True);
    end;

  TFormFrame = class(TStyleForm)
    private
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    protected
      procedure DoCreate; override;
    end;

procedure DisableWindowUpdating(Control: TControl; FullScreen: Boolean=False);
procedure EnableWindowUpdating(Control: TWinControl);

procedure Register;

implementation

uses Types;

{$R PanelFrame.res}

procedure Register;
begin
  RegisterComponents('Samples',[TPanelFrame]);
  RegisterComponents('Samples',[TDoubleBufferedPanel]);
end;

procedure DesignClearBackground(Canvas: TCanvas; const Rect: TRect);
begin
  Canvas.Brush.Bitmap:=TBitmap.Create;
  Canvas.Brush.Bitmap.Width:=8;
  Canvas.Brush.Bitmap.Height:=8;
  with Canvas.Brush.Bitmap.Canvas do
  begin
    Brush.Color:=$707070;//clBtnShadow;
    FillRect(ClipRect);
    Pen.Color:=$505050;
    Pen.Width:=0;
    MoveTo(0,0);
    LineTo(8,8);
    MoveTo(0,7);
    LineTo(8,-1);
  end;
  Canvas.FillRect(Rect);
  Canvas.Brush.Bitmap.Free;
  Canvas.Brush.Bitmap:=nil;{}
end;

//==============================================================================================================================
// TPanelFrame
//==============================================================================================================================

constructor TPanelFrame.Create(AOwner: TComponent);
begin
  inherited;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
end;

// Prevent panel from clearing background
procedure TPanelFrame.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result:=LRESULT(False);
end;

type
  TOpenStyleForm = class(TStyleForm)
    end;

procedure TPanelFrame.Paint;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  {$IFDEF UseStyleForm}
  if RenderThemeBackground then
  begin
    if Assigned(TOpenStyleForm(Parent).FBackgroundBitmap) then
      Canvas.Draw(-Left,-Top,TOpenStyleForm(Parent).FBackgroundBitmap)
    else
    begin
      Canvas.Brush.Color:=TOpenStyleForm(Parent).Color;
      Canvas.FillRect(Canvas.ClipRect);
    end;
  end;
  {$ENDIF}
  if Assigned(FOnPaint) then FOnPaint(Self)
  else if csDesigning in ComponentState then DesignClearBackground(Canvas,Rect);
end;

procedure TPanelFrame.SetRenderThemeBackground(const Value: Boolean);
begin
  FRenderThemeBackground := Value and (Parent is TStyleForm);
end;

procedure TPanelFrame.Clear(Color: TColor);
begin
  with Canvas do
  begin
    Brush.Color:=Color;
    FillRect(ClipRect);
  end;
end;

//==============================================================================================================================
// TDoubleBufferedPanel
//==============================================================================================================================

constructor TDoubleBufferedPanel.Create;
begin
  inherited;
  FBackBuffer:=TBitmap.Create;
end;

destructor TDoubleBufferedPanel.Destroy;
begin
  inherited;
  FBackBuffer.Free;
end;

procedure TDoubleBufferedPanel.Resize;
begin
  FTotalBorder:=2*(2*BorderWidth+BevelWidth*(Integer(BevelInner<>bvNone)+Integer(BevelOuter<>bvNone)));
  FBackBuffer.Width:=Width-FTotalBorder;
  FBackBuffer.Height:=Height-FTotalBorder;
  FBitmapCanvas:=FBackBuffer.Canvas;
  inherited;
end;

procedure TDoubleBufferedPanel.Paint;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter<>bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner<>bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  if Assigned(FOnPaint) then FOnPaint(Self);

  if csDesigning in ComponentState then DesignClearBackground(Canvas,Rect)
  else Canvas.Draw(Rect.Left,Rect.Top,FBackBuffer);
end;

procedure TDoubleBufferedPanel.DrawBuffer;
begin
  Canvas.Draw(FTotalBorder shr 1,FTotalBorder shr 1,FBackBuffer);
end;

procedure TDoubleBufferedPanel.Clear(Color: TColor; Invalidate: Boolean);
begin
  if Assigned(BitmapCanvas) then with BitmapCanvas do
  begin
    {$IFDEF UseStyleForm}
    if RenderThemeBackground and Assigned(TOpenStyleForm(Parent).FBackgroundBitmap) then
      Draw(-Left,-Top,TOpenStyleForm(Parent).FBackgroundBitmap)
    else
    {$ENDIF}
    begin
      Brush.Color:=Color;
      FillRect(ClipRect);
    end;
  end;
  if Invalidate then Self.Invalidate;
end;

//==============================================================================================================================
// TFormFrame
//==============================================================================================================================

procedure TFormFrame.DoCreate;
begin
  inherited;
  {$IFDEF UseStyleForm}
  UseBackgroundTheme:=False;
  {$ENDIF}
end;

procedure TFormFrame.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result:=LRESULT(False);
end;

//==============================================================================================================================

type
  TGhostWindow = class(TForm)
    private
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    end;

procedure TGhostWindow.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result:=LRESULT(False);
end;

procedure DisableWindowUpdating(Control: TControl; FullScreen: Boolean);
var
  Rect : TRect;
begin
  if FullScreen then Rect:=Screen.DesktopRect
  else
  begin
    Rect.TopLeft:=Control.ClientOrigin;
    Rect.Right:=Rect.Left+Control.Width;
    Rect.Bottom:=Rect.Top+Control.Height;
  end;
  with TGhostWindow.CreateNew(Control) do
  begin             
    BorderStyle:=bsNone;
    BoundsRect:=Rect;
    FormStyle:=fsStayOnTop;
    Show;
    //ShowWindow(Handle,SW_SHOWNOACTIVATE);
  end;
end;

procedure EnableWindowUpdating(Control: TWinControl);
var
  I : Integer;
begin
  for I:=Control.ComponentCount-1 downto 0 do
    if Control.Components[I] is TGhostWindow then
    begin
      Control.Components[I].Free;
      Break;
    end;
end;

end.

