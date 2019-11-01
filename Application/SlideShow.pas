unit SlideShow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StyleForm,
  Dialogs, PanelFrame, DiagramBase, Menus, ExtCtrls, Math, ValueEdits, FastBitmap;

type
  TSlideShowForm = class(TStyleForm)
    SlidePanel: TPanelFrame;
    PopupMenu: TPopupMenu;
    NextSlideItem: TMenuItem;
    PreviousSlideItem: TMenuItem;
    CloseItem: TMenuItem;
    N1: TMenuItem;
    RenderTimer: TTimer;
    FirstSlideItem: TMenuItem;
    LastSlideItem: TMenuItem;
    GoToPageItem: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure NextSlideItemClick(Sender: TObject);
    procedure PreviousSlideItemClick(Sender: TObject);
    procedure SlidePanelPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FirstSlideItemClick(Sender: TObject);
    procedure LastSlideItemClick(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GoToPageItemClick(Sender: TObject);
  private
    { Private declarations }
    PrevSlide, CurSlide, NextSlide : TFastBitmap;
    procedure RenderSlide(Index: Integer; var Bitmap: TFastBitmap);
    function GetObjectAt(X,Y: Integer): TBaseObject;
    procedure GoToPage(NewPage: Integer);
    procedure WMSysCommand(var Msg: TMessage); message WM_SYSCOMMAND;
  public
    { Public declarations }
    CanvasInfo : TCanvasInfo;
    Slides : TDiagramContainer;
    Page : Integer;
    Antialiasing : Boolean;
  end;

resourcestring
  rsPageNumber = 'Page number:';

implementation

uses
  StrUtils, DiagramAntialiasingDrawing, Types, ShapeObject, TextObject, FileUtils;

{$R *.dfm}

procedure TSlideShowForm.FormShow(Sender: TObject);
begin
  CanvasInfo.DrawMode:=dmRender;
  CanvasInfo.Container:=Slides;
  //CanvasInfo.DisableFontSmoothing:=Antialiasing=2;
  SlidePanel.Height:=1;
  SlidePanel.Width:=1;
end;

procedure TSlideShowForm.FormDestroy(Sender: TObject);
begin
  Application.OnMessage:=nil;
  PrevSlide.Free;
  CurSlide.Free;
  NextSlide.Free;
end;

procedure TSlideShowForm.WMSysCommand(var Msg: TMessage);
begin
  if (Msg.wParam=SC_SCREENSAVE) or (Msg.wParam=SC_MONITORPOWER) then
    Msg.Result:=1
  else inherited;
end;

procedure TSlideShowForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TSlideShowForm.NextSlideItemClick(Sender: TObject);
begin
  if Page<Slides.Count-1 then
  begin
    PrevSlide.Free;
    PrevSlide:=CurSlide;
    CurSlide:=NextSlide;
    NextSlide:=nil;
    Inc(Page);
    SlidePanel.Repaint;
    RenderTimer.Enabled:=False;
    RenderTimer.Enabled:=True;
  end;
end;

procedure TSlideShowForm.PreviousSlideItemClick(Sender: TObject);
begin
  if Page>0 then
  begin
    NextSlide.Free;
    NextSlide:=CurSlide;
    CurSlide:=PrevSlide;
    PrevSlide:=nil;
    Dec(Page);
    SlidePanel.Repaint;
    RenderTimer.Enabled:=False;
    RenderTimer.Enabled:=True;
  end;
end;

procedure TSlideShowForm.FirstSlideItemClick(Sender: TObject);
begin
  if Page=1 then PreviousSlideItemClick(nil)
  else if Page>1 then
  begin
    Page:=0;
    FreeAndNil(CurSlide);
    SlidePanel.Repaint;
    RenderTimer.Enabled:=False;
    FreeAndNil(PrevSlide);
    FreeAndNil(NextSlide);
    RenderTimer.Enabled:=True;
  end;
end;

procedure TSlideShowForm.LastSlideItemClick(Sender: TObject);
begin
  if Page=Slides.Count-2 then NextSlideItemClick(nil)
  else if Page<Slides.Count-2 then
  begin
    Page:=Slides.Count-1;
    FreeAndNil(CurSlide);
    SlidePanel.Repaint;
    RenderTimer.Enabled:=False;
    FreeAndNil(PrevSlide);
    FreeAndNil(NextSlide);
    RenderTimer.Enabled:=True;
  end;
end;

procedure TSlideShowForm.RenderSlide(Index: Integer; var Bitmap: TFastBitmap);
begin
  CanvasInfo.PageIndex:=Index;
  with Slides.Pages[Index] do
  begin
    CanvasInfo.Scale.X:=Min(Self.Width/Width,Self.Height/Height);
    CanvasInfo.Scale.Y:=CanvasInfo.Scale.X;
    Bitmap:=TFastBitmap.Create(Round(Width*CanvasInfo.Scale.X),
                               Round(Height*CanvasInfo.Scale.Y),
                               pf32bit);
  end;
  DrawToBitmap(Slides.Pages[Index],Bitmap,CanvasInfo,Antialiasing);
end;

function TSlideShowForm.GetObjectAt(X, Y: Integer): TBaseObject;
var
  ZCanvasInfo : TCanvasInfo;
  ZBuffer, Bitmap : TFastBitmap;
  Layer, I : Integer;
begin
  Result:=nil;
  ZCanvasInfo:=CanvasInfo;
  ZCanvasInfo.PageIndex:=Page;
  ZCanvasInfo.DrawMode:=dmEditing;
  with Slides.Pages[Page] do
  begin
    ZCanvasInfo.Scale.X:=Min(Self.Width/Width,Self.Height/Height);
    ZCanvasInfo.Scale.Y:=ZCanvasInfo.Scale.X;
    Bitmap:=TFastBitmap.Create(Round(Width*ZCanvasInfo.Scale.X),
                               Round(Height*ZCanvasInfo.Scale.Y),
                               pf32bit);
    ZBuffer:=TFastBitmap.Create(Round(Width*ZCanvasInfo.Scale.X),
                               Round(Height*ZCanvasInfo.Scale.Y),
                               pf32bit);
  end;
  try
    ZCanvasInfo.ZBuffer:=ZBuffer.Canvas;
    Layer:=Slides.Pages[Page].Count-1;
    Slides.Pages[Page].Layers[Layer].Draw(Bitmap.Canvas,ZCanvasInfo);
    I:=ZBuffer.Canvas.Pixels[X,Y];
    if I<>$ffffff then
    begin
      I:=I and $ffff;
      if I<Slides.Pages[Page].Layers[Layer].Count then
        Result:=Slides.Pages[Page].Layers[Layer].Objects[I];
    end;
  finally
    ZBuffer.Free;
    Bitmap.Free;
  end;
end;

procedure TSlideShowForm.SlidePanelPaint(Sender: TObject);
begin
  if CurSlide=nil then RenderSlide(Page,CurSlide);
  SlidePanel.Width:=CurSlide.Width;
  SlidePanel.Height:=CurSlide.Height;
  SlidePanel.Left:=(Width-SlidePanel.Width) div 2;
  SlidePanel.Top:=(Height-SlidePanel.Height) div 2;
  SlidePanel.Canvas.Draw(0,0,CurSlide);
end;

procedure TSlideShowForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN,
    VK_SPACE,
    VK_RETURN : NextSlideItemClick(nil);
    VK_UP,
    VK_BACK   : PreviousSlideItemClick(nil);
    VK_END    : if ssCtrl in Shift then LastSlideItemClick(nil);
    VK_HOME   : if ssCtrl in Shift then FirstSlideItemClick(nil);
  end;
end;

procedure TSlideShowForm.PopupMenuPopup(Sender: TObject);
begin
  RenderTimer.Enabled:=False;
  NextSlideItem.Enabled:=Page<Slides.Count-1;
  LastSlideItem.Enabled:=Page<Slides.Count-1;
  PreviousSlideItem.Enabled:=Page>0;
  FirstSlideItem.Enabled:=Page>0;
end;

procedure TSlideShowForm.RenderTimerTimer(Sender: TObject);
begin
  RenderTimer.Enabled:=False;
  SetCursorPos(0,Height-1);
  if (NextSlide=nil) and (Page<Slides.Count-1) then RenderSlide(Page+1,NextSlide);
end;

procedure TSlideShowForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  PreviousSlideItemClick(nil);
end;

procedure TSlideShowForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  NextSlideItemClick(nil);
end;

procedure TSlideShowForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ObjectHit : TBaseObject;
  Text : string;
  P, Code : Integer;
begin
  if Button=mbLeft then
  begin
    // See if link object hit
    if Sender=SlidePanel then
    begin
      ObjectHit:=GetObjectAt(X,Y);
      if ObjectHit is TTextObject then
      begin
        Text:=PString(TTextObject(ObjectHit).Properties[opText])^;
        P:=Pos('\A',Text);
        while P<>0 do
        begin
          if (P=1) or (Text[P-1]<>'\') then // Execute link
          begin
            Text:=Copy(Text,P+2,MaxInt);
            Val(Text,P,Code); // Link to page number
            if Code=0 then
            begin
              GoToPage(EnsureRange(P-1,0,Slides.Count-1));
              Exit;
            end;

            if ExecuteFile(Text)<=32 then RaiseLastOSError;
            Exit;
          end;
          P:=PosEx('\A',Text,P+1)
        end;
      end;
    end;

    // Just go to next opage
    NextSlideItemClick(nil);
  end;
end;

procedure TSlideShowForm.GoToPage(NewPage: Integer);
begin
  if NewPage=Page-1 then PreviousSlideItemClick(nil)
  else if NewPage=Page+1 then NextSlideItemClick(nil)
  else if NewPage<>Page then
  begin
    Page:=NewPage;
    FreeAndNil(CurSlide);
    SlidePanel.Repaint;
    RenderTimer.Enabled:=False;
    FreeAndNil(PrevSlide);
    FreeAndNil(NextSlide);
    RenderTimer.Enabled:=True;
  end;
end;

procedure TSlideShowForm.GoToPageItemClick(Sender: TObject);
var
  NewPage : Integer;
begin
  NewPage:=Page+1;
  if IntegerQuery(StripHotkey(GoToPageItem.Caption),rsPageNumber,NewPage,1,Slides.Count,120,1) then
    GoToPage(NewPage-1);
end;

end.

