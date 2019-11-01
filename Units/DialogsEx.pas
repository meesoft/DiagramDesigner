/////////////////////////////////////////////////////////////////////////////////////////////////////
//
// DialogsEx.pas - Dialog extensions
// ---------------------------------
// Version:   2003-07-31
// Maintain:  Michael Vinther    |     mv@logicnet·dk
//
// Last changes:
//   Resampling of preview image
//   OnPreviewLoaded event
//
unit DialogsEx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, stdctrls,
  ExtCtrls, LinarBitmap, SyncObjs, Math, PanelFrame, ImageDLLLoader
{$IFDEF UseStyleForm} ,StyleForm; {$ELSE} ; type TStyleForm = TForm; {$ENDIF}

type
  TImageLoader = class;
  TImageLoadedEvent = procedure(Sender: TImageLoader; Image: TLinearBitmap; const Msg: string) of object;
  TImageLoader = class(TThread)
  protected
    WantedFileName, LoadedFileName : string;
    Sync : TCriticalSection;
    SyncEvent : TEvent;
    Image : TLinarBitmap;
    Msg : string;
    DLLOptions : string;
    procedure ReturnResult;
    procedure Execute; override;
  public
    OnLoaded: TImageLoadedEvent;
    constructor Create(ImageLoadedEvent: TImageLoadedEvent=nil);
    destructor Destroy; override;
    procedure LoadImage(const FileName: string);
    procedure Cancel;
    property ImageFileName: string read WantedFileName;
  end;

  TSaveDialogButtons = class(TSaveDialog)
    protected
      FPanel: TPanel;
      FOnButtonClick : TNotifyEvent;
      FButtonCaptions : string;
      ExtraControls : array of TControl;
      {$IFDEF VistaTaskbarButton}
      ApplicationHandle : THandle;
      {$ENDIF}
      procedure DoShow; override;
      procedure DoClose; override;
      procedure AddControls(var NextTop: Integer); virtual;
     public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Execute: Boolean; override;
    published
      property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
      // Button captions separated by |
      property ButtonCaptions: string read FButtonCaptions write FButtonCaptions;
    end;

  TPreviewLoadedEvent = procedure(Sender: TObject; Image: TLinearBitmap; var Text: string) of object;
  TOpenPictureDialogThread = class(TOpenDialog)
  protected
    FPicture: TPicture;
    FPicturePanel: TPanel;
    FPictureLabel: TLabel;
    FPaintBox: TPaintBox;
    FPreview : TLinearBitmap;
    FOnPreviewLoaded : TPreviewLoadedEvent;
    LoaderThread : TImageLoader;
    FPreviewFileName : string;
    {$IFDEF VistaTaskbarButton}
    ApplicationHandle : THandle;
    {$ENDIF}
    procedure PaintBoxPaint(Sender: TObject);
    procedure PreviewClick(Sender: TObject);
    procedure PreviewKeyPress(Sender: TObject; var Key: Char);
    procedure PreviewPaint(Sender: TObject);
    procedure PreviewCloseClick(Sender: TObject);
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    procedure PreviewLoaded(Sender: TImageLoader; Image: TLinearBitmap; const Msg: string);
  public
    ImageGamma : Single;
    property PreviewFileName: string read FPreviewFileName;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
  published
    property OnPreviewLoaded: TPreviewLoadedEvent read FOnPreviewLoaded write FOnPreviewLoaded;
  end;

function OpenPictureDialog(var FileName: string; const Filter: string;
  Options: TOpenOptions=[ofHideReadOnly,ofFileMustExist,ofEnableSizing]): Boolean;

procedure Register;

implementation

uses
  BitmapGammaInterpolation, BitmapResize, 
  BitmapConversion; // To get DLGTEMPLATE resource
//{$R DialogsEx.res}

resourcestring
  rsLoading = 'Loading...';
  rsDFilesSelected = '%d files selected';
  rsPreviewNotAvailable = 'Preview not available';

procedure Register;
begin
  RegisterComponents('Dialogs', [TSaveDialogButtons,TOpenPictureDialogThread]);
end;

//==============================================================================================================================
// TSaveDialogButtons
//==============================================================================================================================
constructor TSaveDialogButtons.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanel := TPanel.Create(Self);
  with FPanel do
  begin
    Name:='Panel';
    Caption:='';
    BevelOuter:=bvNone;
    TabOrder:=1;
  end;
end;

destructor TSaveDialogButtons.Destroy;
begin
  FPanel.Free;
  inherited Destroy;
end;

procedure TSaveDialogButtons.DoClose;
var
  I : Integer;
begin
  inherited DoClose;
  for I:=0 to High(ExtraControls) do ExtraControls[I].Free;
  ExtraControls:=nil;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TSaveDialogButtons.DoShow;
var
  PanelRect, StaticRect: TRect;
  NextTop : Integer;
begin
  {$IFDEF VistaTaskbarButton}
  TOpenApplication(Application).FHandle:=ApplicationHandle;
  {$ENDIF}
  { Set preview area to entire dialog }
  GetClientRect(Handle,PanelRect);
  StaticRect := GetStaticRect;
  { Move preview area to right of static area }
  PanelRect.Left := StaticRect.Left + (StaticRect.Right - StaticRect.Left);
  Inc(PanelRect.Top, 4);
  FPanel.BoundsRect := PanelRect;
  FPanel.ParentWindow := Handle;
  NextTop:=30;
  AddControls(NextTop);
  inherited DoShow;
end;

procedure TSaveDialogButtons.AddControls(var NextTop: Integer);
var
  I, P, LastP : Integer;
begin
  I:=0;
  LastP:=1;
  P:=Pos('|',ButtonCaptions);
  if P=0 then P:=256;
  if ButtonCaptions<>'' then while P<>LastP do
  begin
    SetLength(ExtraControls,I+1);
    if ButtonCaptions[LastP]='@' then
    begin
      ExtraControls[I]:=TLabel.Create(FPanel);
      with TLabel(ExtraControls[I]) do
      begin
        Parent:=FPanel;
        Name:='Label'+IntToStr(I);
        Left:=4;
        Top:=NextTop;
        Width:=FPanel.Width-10;
        Caption := Copy(ButtonCaptions,LastP+1,P-LastP-1);
        Tag:=I+1;
        Inc(NextTop,Height+4);
      end;
    end
    else
    begin
      ExtraControls[I]:=TButton.Create(FPanel);
      with TButton(ExtraControls[I]) do
      begin
        Parent:=FPanel;
        Left:=4;
        Top:=NextTop;
        Width:=FPanel.Width-10;
        Name:='Button'+IntToStr(I);
        Caption := Copy(ButtonCaptions,LastP,P-LastP);
        OnClick:=OnButtonClick;
        Tag:=I+1;
        Inc(NextTop,Height+4);
      end;
    end;
    Inc(P); Inc(I);
    LastP:=P;
    while (P<Length(ButtonCaptions)) and (ButtonCaptions[P]<>'|') do Inc(P);
  end;
end;

function TSaveDialogButtons.Execute: Boolean;
begin
  if Application.MainForm<>nil then FPanel.Font:=Application.MainForm.Font;
  if (ButtonCaptions<>'') and NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE'
  else
    Template := nil;
  {$IFDEF VistaTaskbarButton}
  ApplicationHandle:=Application.Handle;
  Assert(TOpenApplication(Application).FHandle=ApplicationHandle);
  TOpenApplication(Application).FHandle:=Application.MainForm.Handle;
  try
  {$ENDIF}
  Result := inherited Execute;
  {$IFDEF VistaTaskbarButton}
  finally
    TOpenApplication(Application).FHandle:=ApplicationHandle;
  end;
  {$ENDIF}
end;

//==============================================================================================================================
// TImageLoader
//==============================================================================================================================
constructor TImageLoader.Create(ImageLoadedEvent: TImageLoadedEvent);
begin
  OnLoaded:=ImageLoadedEvent;
  Sync:=TCriticalSection.Create;
  SyncEvent:=TEvent.Create(nil,False,False,'');
  DLLOptions:=ImageDLLLoader.Default.CompOptions;
  inherited Create(True);
end;

destructor TImageLoader.Destroy;
begin
  Sync.Enter;
  OnLoaded:=nil;
  WantedFileName:='';
  Sync.Leave;
  Terminate;
  SyncEvent.SetEvent;
  inherited Destroy;
  Sync.Free;
  SyncEvent.Free;
end;

procedure TImageLoader.LoadImage(const FileName: string);
begin
  Sync.Enter;
  WantedFileName:=FileName;
  Sync.Leave;
  if Suspended then Resume // First time only
  else SyncEvent.SetEvent;
end;

procedure TImageLoader.ReturnResult;
begin
  if Assigned(OnLoaded) and (LoadedFileName=WantedFileName) then
  try
    OnLoaded(Self,Image,Msg);
  except
    {$IFOPT D+}
    Application.HandleException(Self);
    {$ENDIF}
  end;
end;

procedure TImageLoader.Execute;

  function ReturnImage: Boolean;
  begin
    if Image.Present then
    begin
      Sync.Enter;
      Result:=LoadedFileName=WantedFileName;
      Sync.Leave;
      if not Result then Image.Dispose;
    end
    else Result:=False;
  end;

begin
  Image:=TLinarBitmap.Create;
  try
    repeat
      Sync.Enter;
      LoadedFileName:=WantedFileName;
      Sync.Leave;
      Msg:='';
      if LoadedFileName<>'' then
      begin
        try
          ImageDLLLoader.Default.CompOptions:=DLLOptions;
          Image.LoadFromFile(LoadedFileName);
          if not Image.Present then raise ELinearBitmap.Create(rsErrorInBitmapData);
        except
          on Error: Exception do
          begin
            Msg:=Error.Message;
            if Assigned(OnLoaded) then Synchronize(ReturnResult);
            Image.Dispose;
          end;
        end;
        if ReturnImage then
        begin
          if Assigned(OnLoaded) then Synchronize(ReturnResult);
          Image.Dispose;
        end;
      end;
      SyncEvent.WaitFor(INFINITE);
    until Terminated;
  finally
    FreeAndNil(Image);
  end;
end;

procedure TImageLoader.Cancel;
begin
  Sync.Enter;
  WantedFileName:='';
  Sync.Leave;
end;

//==============================================================================================================================
// TOpenPictureDialogEx
//==============================================================================================================================

constructor TOpenPictureDialogThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageGamma:=DefaultMonitorGamma;
  FPicture := TPicture.Create;
  FPicturePanel := TPanel.Create(Self);
  with FPicturePanel do
  begin
    Caption := '';
    SetBounds(204, 5, 169, 200);
    BevelOuter := bvNone;
    BorderWidth := 6;
    TabOrder := 1;
    FPictureLabel := TLabel.Create(Self);
    with FPictureLabel do
    begin
      SetBounds(6, 6, 157, 23);
      Align := alTop;
      AutoSize := False;
      Parent := FPicturePanel;
    end;
    FPaintBox := TPaintBox.Create(Self);
    with FPaintBox do
    begin
      SetBounds(6, 29, 157, 145);
      Align:=alClient;
      OnClick:=PreviewClick;
      OnPaint:=PaintBoxPaint;
      Parent := FPicturePanel;
    end;
  end;
end;

destructor TOpenPictureDialogThread.Destroy;
begin
  FPaintBox.Free;
  FPreview.Free;
  FPictureLabel.Free;
  FPicturePanel.Free;
  FPicture.Free;
  LoaderThread.Free;
  inherited Destroy;
end;

procedure TOpenPictureDialogThread.DoSelectionChange;

  function ValidFile(const FileName: string): Boolean;
  begin
    Result:=GetFileAttributes(PChar(FileName)) <> $FFFFFFFF;
  end;

var
  FullName : string;
  P, FileCount : Integer;
begin
  FullName:=FileName;

  if FileExists(FullName) and ValidFile(FullName) then
  begin
    LoaderThread.LoadImage(FullName);
    FPictureLabel.Caption:=rsLoading;
  end
  else
  begin
    LoaderThread.Cancel;
    FileCount:=0;
    for P:=1 to Length(FullName) do if FullName[P]='"' then Inc(FileCount);
    FileCount:=FileCount div 2;
    if FileCount>1 then FPictureLabel.Caption:=Format(rsDFilesSelected,[FileCount])
    else FPictureLabel.Caption:='';
    FPicture.Assign(nil);
    FPaintBox.Invalidate;
  end;
  inherited DoSelectionChange;
end;

procedure TOpenPictureDialogThread.PreviewLoaded(Sender: TImageLoader; Image: TLinarBitmap; const Msg: string);
var
  Scale : Double;
  Str : string;
begin
  FreeAndNil(FPreview);
  if Image.Present then
  begin
    Str:=IntToStr(Image.Width)+' x '+IntToStr(Image.Height)+'  ('+FormatFloat('0.0#',Image.Width*Image.Height/1000000)+' Mp)';
    FPreviewFileName:=Sender.ImageFileName;
    if Assigned(OnPreviewLoaded) then OnPreviewLoaded(Self,Image,Str);
    FPictureLabel.Caption:=Str;
    if Image.PixelFormat=pf16bit then Image.PixelFormat:=pf8bit
    else if Image.PixelFormat=pf32bit then Image.PixelFormat:=pf24bit;
    if (Image.Width>FPaintBox.Width) or (Image.Height>FPaintBox.Height) then
    begin
      Scale:=Min(FPaintBox.Width/Image.Width,FPaintBox.Height/Image.Height);
      FPreview:=TLinearBitmap.Create;
      FPreview.TakeOver(Image);
      CopyResizeImg(FPreview,Image,Max(1,Round(Image.Width*Scale)),Max(1,Round(Image.Height*Scale)),ImageGamma);
    end;
    Image.AssignTo(FPicture.Bitmap);
    FPaintBox.Cursor:=crHandPoint;
  end
  else
  begin
    FPictureLabel.Caption:=rsPreviewNotAvailable;
    {$IFOPT D+}
    FPictureLabel.Caption:=FPictureLabel.Caption+' ('+Msg+')';
    {$ENDIF}
    FPicture.Assign(nil);
    FPaintBox.Cursor:=crDefault;                             
  end;
  FPaintBox.Invalidate;
end;

procedure TOpenPictureDialogThread.DoShow;
var
  PreviewRect, StaticRect: TRect;
begin
  {$IFDEF VistaTaskbarButton}
  TOpenApplication(Application).FHandle:=ApplicationHandle;
  {$ENDIF}
  { Set preview area to entire dialog }
  GetClientRect(Handle, PreviewRect);
  StaticRect := GetStaticRect;
  { Move preview area to right of static area }
  PreviewRect.Left := StaticRect.Left + (StaticRect.Right - StaticRect.Left);
  Inc(PreviewRect.Top, 4);
  FPicturePanel.BoundsRect:=PreviewRect;
  FPicture.Assign(nil);
  FPicturePanel.ParentWindow := Handle;
  FPictureLabel.Caption:='';
  if LoaderThread=nil then LoaderThread:=TImageLoader.Create(PreviewLoaded);
  inherited DoShow;
end;

procedure TOpenPictureDialogThread.DoClose;
begin
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
  FreeAndNil(LoaderThread);
end;

function TOpenPictureDialogThread.Execute: Boolean;
var
  OldProgressUpdate : TProgressUpdateProc;
begin
  if Application.MainForm<>nil then FPicturePanel.Font:=Application.MainForm.Font;
  {$IFDEF VistaTaskbarButton}
  ApplicationHandle:=Application.Handle;
  Assert(TOpenApplication(Application).FHandle=ApplicationHandle);
  TOpenApplication(Application).FHandle:=Application.MainForm.Handle;
  try
  {$ENDIF}
  OldProgressUpdate:=ProgressUpdate;
  try
    ProgressUpdate:=nil;
    if NewStyleControls and not (ofOldStyleDialog in Options) then
      Template := 'DLGTEMPLATE' else
      Template := nil;
    Result := inherited Execute;
  finally
    ProgressUpdate:=OldProgressUpdate;
  end;
  {$IFDEF VistaTaskbarButton}
  finally    
    TOpenApplication(Application).FHandle:=ApplicationHandle;
  end;
  {$ENDIF}
end;

procedure TOpenPictureDialogThread.PaintBoxPaint(Sender: TObject);
begin
  TPaintBox(Sender).Canvas.Draw(0,0,FPicture.Graphic);
end;

procedure TOpenPictureDialogThread.PreviewClick(Sender: TObject);
var
  PreviewForm : TForm;
  Size : TPoint;
begin
  if (FPicture.Graphic=nil) or (FPicture.Width=0) or (FPicture.Height=0) then Exit;
  PreviewForm:=TFormFrame.CreateNew(Self);
  with PreviewForm do
  try
    Caption:=PreviewFileName;
    BorderStyle:=bsSizeToolWin;
    KeyPreview:=True;
    Position:=poMainFormCenter;
    OnKeyPress:=PreviewKeyPress;
    OnPaint:=PreviewPaint;
    OnResize:=PreviewPaint;
    OnClick:=PreviewCloseClick;
    if Assigned(FPreview) then Size:=Point(FPreview.Width,FPreview.Height)
    else Size:=Point(FPicture.Width,FPicture.Height);
    with Size do
    begin
      ClientWidth:=X;
      ClientHeight:=Y;
      if ClientWidth/ClientHeight>X/Y then
        ClientWidth:=Round(ClientHeight/Y*X)
      else
        ClientHeight:=Round(ClientWidth/X*Y);
    end;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TOpenPictureDialogThread.PreviewPaint(Sender: TObject);
var
  Dest : TRect;
  HWR : Double;
  Size : TPoint;
begin
  if Assigned(FPreview) then with FPreview do HWR:=Height/Width
  else with FPicture do HWR:=Height/Width;
  with TForm(Sender) do
  begin
    Size:=Point(ClientWidth,Round(Width*HWR));
    if Size.Y>ClientHeight then with Size do Size:=Point(Round(ClientHeight/HWR),ClientHeight);
    with Size do Dest:=Bounds((ClientWidth-X+1) div 2,(ClientHeight-Y+1) div 2,X,Y);

    if Assigned(FPreview) then FPreview.PaintToCanvas(Canvas,Dest,True)
    else Canvas.StretchDraw(Dest,FPicture.Graphic);
    with Dest do ExcludeClipRect(Canvas.Handle,Left,Top,Right,Bottom);
    Canvas.FillRect(Rect(0,0,ClientWidth,ClientHeight));
  end;
end;

procedure TOpenPictureDialogThread.PreviewCloseClick(Sender: TObject);
begin
  (Sender as TForm).Close;
end;

procedure TOpenPictureDialogThread.PreviewKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then (Sender as TForm).Close;
end;

function OpenPictureDialog(var FileName: string; const Filter: string;
  Options: TOpenOptions=[ofHideReadOnly,ofFileMustExist,ofEnableSizing]): Boolean;
var
  Dialog : TOpenDialog;
begin
  Dialog:=TOpenPictureDialogThread.Create(nil);
  try
    Dialog.Options:=Options;
    Dialog.Filter:=Filter;

    if FileName<>'' then
    begin
      if FileName[Length(FileName)]='\' then Dialog.InitialDir:=FileName
      else
      begin
        Dialog.InitialDir:=ExtractFilePath(FileName);
        Dialog.FileName:=FileName;
      end;
    end;
    Result:=Dialog.Execute;
    if Result then
    begin
      if ofAllowMultiSelect	in Options then FileName:=Dialog.Files.Text
      else FileName:=Dialog.FileName;
    end;
  finally
    Dialog.Free;
  end;
end;

end.

