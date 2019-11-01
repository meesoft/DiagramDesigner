///////////////////////////////////////////////////////////////////////////////////////////////
//
// EventUtils.pas
// --------------------------
// Changed:   2004-12-02
// Maintain:  Michael Vinter   |   mv@logicnet·dk
//
// Last change:
//   IdleEventMulticast added
//   SetPriority moved to WinAPIUtils
//
unit EventUtils;

interface

uses Windows, Forms, StdCtrls, Graphics, Classes, Controls, FileUtils, Monitor,
  MemUtils, SysUtils, Messages;

type
  TProcess = procedure(Thread: TThread) of object;
  TProcessThread = class(TThread)
    protected
      FProcess: TProcess;
      procedure Execute; override;
    public
      constructor Create(Process: TProcess);
    end;

// Frees UnwantedObject when application is idle. Remember NEVER to destroy the object yourself.
procedure FreeOnIdle(UnwantedObject: TObject; Owner: TComponent);
procedure CloseOnIdle(UnwantedForm: TCustomForm);
procedure NotifyOnIdle(Event: TNotifyEvent; Owner: TComponent);
procedure RepaintOnIdle(Control: TControl);
// Perform WM_WINDOWPOSCHANGED to force autosizing
procedure UpdateListViewColumnSizes(ListView: TControl);
// Perform a kind of Application.ProcessMessages to prevent Windows from displaying a ghost window
procedure ProcessPaintMessages;
procedure ProcessIdleEvents;

// OnIdle event handlers should only change Done if they are not done! The default value is True.
procedure AddIdleEvent(OwnerObject: TObject; const EventHandler: TIdleEvent);
procedure RemoveIdleEvent(Owner: TObject);

implementation

uses ComCtrls;

constructor TProcessThread.Create(Process: TProcess);
begin
  FProcess:=Process;
  inherited Create(False);
end;

procedure TProcessThread.Execute;
begin
  FProcess(Self);
end;

// Perform a kind of Application.ProcessMessages to prevent Windows from displaying a ghost window
procedure ProcessPaintMessages;
var
  Msg : TMsg;
begin
  while PeekMessage(Msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
    DispatchMessage(Msg);
end;

//==============================================================================================================================
// TActionOnIdle
//==============================================================================================================================

type
  TActionOnIdle = class(TComponent)
                    private
                      Obj : TObject;
                      Event : TNotifyEvent;
                      procedure DoFree(Sender: TObject; var Done: Boolean);
                      procedure DoClose(Sender: TObject; var Done: Boolean);
                      procedure Notify(Sender: TObject; var Done: Boolean);
                      procedure PerformWindowPosChanged(Sender: TObject; var Done: Boolean);
                      procedure DoRepaint(Sender: TObject; var Done: Boolean);
                    public
                      destructor Destroy; override;
                    end;

destructor TActionOnIdle.Destroy;
begin
  RemoveIdleEvent(Self);
  inherited;
end;

procedure TActionOnIdle.DoFree(Sender: TObject; var Done: Boolean);
begin
  try
    if Obj<>Self then Obj.Free;
  finally
    Destroy;
  end;
end;

procedure TActionOnIdle.DoClose(Sender: TObject; var Done: Boolean);
var
  Form : TCustomForm;
begin
  try
    Form:=Owner as TCustomForm;
  finally
    Destroy;
  end;
  Form.Close;
end;

procedure TActionOnIdle.Notify(Sender: TObject; var Done: Boolean);
var
  EventHandler : TNotifyEvent;
begin
  EventHandler:=Event;
  Destroy;
  EventHandler(nil);
end;

procedure TActionOnIdle.PerformWindowPosChanged(Sender: TObject; var Done: Boolean);
var
  Pos : TWindowPos;
begin
  try
    ZeroMem(Pos,SizeOf(Pos));
    TControl(Owner).Perform(WM_WINDOWPOSCHANGED,0,Integer(@Pos));
  finally
    Destroy;
  end;
end;

procedure TActionOnIdle.DoRepaint(Sender: TObject; var Done: Boolean);

  procedure RepaintBtnControls(Control: TControl);
  var
    I : integer;
  begin
    if (Control is TWinControl) and Control.Visible then
    begin
      if (Control is TButtonControl) or (Control is TStaticText) then // Repaint controls of affected types
        TWinControl(Control).Repaint
      else
      for I:=0 to TWinControl(Control).ControlCount-1 do // Repaint child controls
        // Only paint controls on active tabsheet of page control
        if not ((Control is TTabSheet) and (TTabSheet(Control).PageIndex<>TTabSheet(Control).PageControl.ActivePageIndex)) then
          RepaintBtnControls(TWinControl(Control).Controls[i]);
    end;
  end;

begin
  try
    RepaintBtnControls(TControl(Owner));
  finally
    Destroy;
  end;
end;


procedure FreeOnIdle(UnwantedObject: TObject; Owner: TComponent);
var ActionObject : TActionOnIdle;
begin
  ActionObject:=TActionOnIdle.Create(Owner);
  ActionObject.Obj:=UnwantedObject;
  AddIdleEvent(ActionObject,ActionObject.DoFree);
  Assert(MonitorComponent(ActionObject));
end;

procedure CloseOnIdle(UnwantedForm: TCustomForm);
var ActionObject : TActionOnIdle;
begin
  ActionObject:=TActionOnIdle.Create(UnwantedForm);
  AddIdleEvent(ActionObject,ActionObject.DoClose);
  Assert(MonitorComponent(ActionObject));
end;

procedure RepaintOnIdle(Control: TControl);
var ActionObject : TActionOnIdle;
begin
  ActionObject:=TActionOnIdle.Create(Control);
  AddIdleEvent(ActionObject,ActionObject.DoRepaint);
  Assert(MonitorComponent(ActionObject));
end;

procedure NotifyOnIdle(Event: TNotifyEvent; Owner: TComponent);
var ActionObject : TActionOnIdle;
begin
  ActionObject:=TActionOnIdle.Create(Owner);
  ActionObject.Event:=Event;
  AddIdleEvent(ActionObject,ActionObject.Notify);
  Assert(MonitorComponent(ActionObject));
end;

procedure UpdateListViewColumnSizes(ListView: TControl);
var ActionObject : TActionOnIdle;
begin
  ActionObject:=TActionOnIdle.Create(ListView);
  AddIdleEvent(ActionObject,ActionObject.PerformWindowPosChanged);
  Assert(MonitorComponent(ActionObject));
end;

//==============================================================================================================================
// TIdleEventMulticast
//==============================================================================================================================

type
  TIdleEventMulticast = class(TMonitorObject)
    private
      HandlerList : array of record
                               Handler : TIdleEvent;
                               Owner   : TObject;
                             end;
      ID : Integer;
      procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
      destructor Destroy; override;
    end;

var
  IdleEventMulticast : TIdleEventMulticast = nil;
  ActiveIdleEventMulticastID : Integer = 0;

destructor TIdleEventMulticast.Destroy;
begin
  Application.OnIdle:=nil;
  Inc(ActiveIdleEventMulticastID);
  inherited;
end;

procedure TIdleEventMulticast.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  I, InstanceID : Integer;
begin
  InstanceID:=ID;
  for I:=High(HandlerList) downto 0 do
    if (InstanceID=ActiveIdleEventMulticastID) and (I<Length(HandlerList)) then // Make sure that events have not already been deleted
      HandlerList[I].Handler(Sender,Done);
end;

procedure ProcessIdleEvents;
var
  Done : Boolean;
begin
  Done:=False;
  if IdleEventMulticast<>nil then IdleEventMulticast.ApplicationIdle(nil,Done);
end;

procedure AddIdleEvent(OwnerObject: TObject; const EventHandler: TIdleEvent);
begin
  if IdleEventMulticast=nil then
  begin
    Assert(not Assigned(Application.OnIdle));
    IdleEventMulticast:=TIdleEventMulticast.Create;
    IdleEventMulticast.ID:=ActiveIdleEventMulticastID;
    Application.OnIdle:=IdleEventMulticast.ApplicationIdle;
  end;
  with IdleEventMulticast do
  begin
    SetLength(HandlerList,Length(HandlerList)+1);
    with HandlerList[High(HandlerList)] do
    begin
      Handler:=EventHandler;
      Owner:=OwnerObject;
    end;
  end;
end;

procedure RemoveIdleEvent(Owner: TObject);
var
  I : Integer;
begin
  if Assigned(IdleEventMulticast) then with IdleEventMulticast do
  begin
    for I:=High(HandlerList) downto 0 do if HandlerList[I].Owner=Owner then
    begin
      HandlerList[I]:=HandlerList[High(HandlerList)];
      SetLength(HandlerList,Length(HandlerList)-1);
      if Length(HandlerList)=0 then FreeAndNil(IdleEventMulticast);
      Exit;
    end;
  end;
  Assert(False,'Event owner not found');
end;

initialization
finalization
  IdleEventMulticast.Free;
end.

