unit TsLib.NotificationManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Connection, TsLib.Types, syncobjs, Logger;

type

  { TNotificationManager }

  TNotificationManager = class
  private
    FAutoSendNotifications: boolean;
    FEventThread: TThread;
    FThreadedNotifications: boolean;
    FConnection: TTsConnection;
    Events: TEventLists;
    QueuedNotifications: TList;
    FLocked: TRTLCriticalSection;
    FActive: boolean;
    function GetNotificationsAvailable: Boolean;
    procedure SetActive(AValue: boolean);
  protected
    function NotificationRecieved(Sender: TObject; Data: string;
      var RemoveFromList: boolean): boolean;
  public
    constructor Create(Connection: TTsConnection);
    destructor Destroy; override;
    procedure Start;
    procedure SendNotifications;
    procedure Stop;
    procedure ClearNotifications;
    procedure ClearRegisteredEvents;

    procedure RegisterText(Event: TTextNotificationEvent);
    procedure RegisterServerEdit(Event: TServerEditNotificationEvent);
    procedure RegisterClientMove(Event: TClientMoveNotificationEvent);
    procedure RegisterDisconnect(Event: TClientDCNotificationEvent);
    procedure RegisterConnect(Event: TClientConnectNotificationEvent);
    procedure RegisterChannelEdit(Event: TChannelEditedNotificationEvent);
    procedure RegisterChannelDescription(Event: TChannelDescriptionChangedEvent);

    procedure UnregisterNotification(EventType: TNotificationType; Event: TMethod);
    property Active: boolean read FActive write SetActive;
    property EventThread: TThread read FEventThread write FEventThread;
    property AutoSendNotifications: boolean
      read FAutoSendNotifications write FAutoSendNotifications;
    property ThreadedNotifications: boolean
      read FThreadedNotifications write FThreadedNotifications;
    property NotificationsAvailable: Boolean read GetNotificationsAvailable;
  end;

implementation

uses strutils, TsLib.ValueRead;

{ TNotificationManager }

procedure TNotificationManager.SetActive(AValue: boolean);
begin
  if FActive = AValue then
    Exit;
  if AValue then
    Start
  else
    Stop;
end;

function TNotificationManager.GetNotificationsAvailable: Boolean;
begin
  EnterCriticalsection(FLocked);
  try
    Result:=QueuedNotifications.Count>0;
  finally
    LeaveCriticalsection(FLocked);
  end;
end;

function TNotificationManager.NotificationRecieved(Sender: TObject;
  Data: string; var RemoveFromList: boolean): boolean;
var
  n: PNotificationData;
  i: Integer;
begin
  RemoveFromList := False;
  Result := AnsiStartsStr('notify', Data);
  if not Result then
    exit;
  WriteStatus('Notification recieved');
  new(n);
  n^.NType := GetNotificationType(Copy(Data, 1, Pos(' ', Data) - 1));
  Delete(Data, 1, Pos(' ', Data));
  n^.Data := StringReplace(Data, #10#13,' ', [rfReplaceAll]);
  // Check if already listed (no doubled notifications)
  EnterCriticalsection(FLocked);
  try
    for i:=0 to QueuedNotifications.Count-1 do
      with PNotificationData(QueuedNotifications[i])^ do
        if (NType = n^.NType) and (Data = n^.Data) then
        begin
          Dispose(n);
          Exit;
        end;
  QueuedNotifications.Add(n);
  finally
    LeaveCriticalsection(FLocked);
  end;
  if FAutoSendNotifications then
    if FConnection.RecieversThreaded and Assigned(FEventThread) and
      not FThreadedNotifications then
      TThread.Synchronize(FEventThread, @SendNotifications)
    else
      SendNotifications;
end;

constructor TNotificationManager.Create(Connection: TTsConnection);
begin
  FConnection := Connection;
  QueuedNotifications := TList.Create;
  InitCriticalSection(FLocked);
  with Events do
  begin
    Texts := TTextEventList.Create;
    ServerEdits := TServerEditEventList.Create;
    ClientMoves := TClientMoveEventList.Create;
    Disconnects := TDisconnectedEventList.Create;
    Connects := TConnectedEventList.Create;
    ChannelEdits := TChannelEditedEventList.Create;
    DescriptionChanges := TDescriptionEventList.Create;
  end;
  FEventThread := TThread.CurrentThread;
  FAutoSendNotifications := False;
  FThreadedNotifications := True;
  FActive := False;
end;

procedure TNotificationManager.Start;
var
  i: integer;
begin
  if FActive then
    Exit;
  WriteStatus('Activating notifications');
  // Add reciever
  FConnection.AddReciever(@NotificationRecieved);
  // register events
  FConnection.ExecCommand('servernotifyregister event=textchannel');
  FConnection.ExecCommand('servernotifyregister event=textprivate');
  FConnection.ExecCommand('servernotifyregister event=server');
  FConnection.ExecCommand('servernotifyregister event=textserver');
  FConnection.ExecCommand('servernotifyregister event=channel id=0');
  FActive := True;
end;

procedure TNotificationManager.SendNotifications;

{$Include include/SendNotifications.inc}

var
  i: integer;
begin
  EnterCriticalsection(FLocked);
  try
    for i := 0 to QueuedNotifications.Count - 1 do
    begin
      case PNotificationData(QueuedNotifications[i])^.NType of
        ntTextMessage: SendTM(PNotificationData(QueuedNotifications[i])^);
        ntServerEdited: SendSE(PNotificationData(QueuedNotifications[i])^);
        ntClientMoved: SendCM(PNotificationData(QueuedNotifications[i])^);
        ntClientDisconnected: SendCD(PNotificationData(QueuedNotifications[i])^);
        ntClientConnected: SendCC(PNotificationData(QueuedNotifications[i])^);
        ntChannelEdited: SendCE(PNotificationData(QueuedNotifications[i])^);
        ntChanneldescriptionChanged: SendDC(PNotificationData(QueuedNotifications[i])^);
      end;
    end;
    QueuedNotifications.Clear;
  finally
    LeaveCriticalsection(FLocked);
  end;
end;

procedure TNotificationManager.Stop;
var
  cnt: integer;
begin
  // Send out chached Notifications
  EnterCriticalsection(FLocked);
  try
    cnt := QueuedNotifications.Count;
  finally
    LeaveCriticalsection(FLocked);
  end;
  if cnt > 0 then
    SendNotifications;
  if not Active then
    Exit;
  FConnection.ExecCommand('servernotifyunregister');
  FConnection.DeleteReciever(@NotificationRecieved);
  FThreadedNotifications := True;
  FActive := False;
end;

destructor TNotificationManager.Destroy;
begin
  Stop;

  ClearNotifications;
  QueuedNotifications.Free;

  with Events do
  begin
    Texts.Free;
    ServerEdits.Free;
    ClientMoves.Free;
    Disconnects.Free;
    Connects.Free;
    ChannelEdits.Free;
    DescriptionChanges.Free;
  end;

  inherited Destroy;
end;

procedure TNotificationManager.ClearNotifications;
var
  i: integer;
begin
  EnterCriticalsection(FLocked);
  try
    for i := 0 to QueuedNotifications.Count - 1 do
      Dispose(PNotificationData(QueuedNotifications[i]));
  QueuedNotifications.Clear;
  finally
    LeaveCriticalsection(FLocked);
  end;
end;

procedure TNotificationManager.ClearRegisteredEvents;
begin
  Stop;
  with Events do
  begin
    Texts.Clear;
    ServerEdits.Clear;
    ClientMoves.Clear;
    Disconnects.Clear;
    Connects.Clear;
    ChannelEdits.Clear;
    DescriptionChanges.Clear;
  end;
end;

procedure TNotificationManager.RegisterText(Event: TTextNotificationEvent);
begin
  Events.Texts.Add(Event);
end;

procedure TNotificationManager.RegisterServerEdit(Event: TServerEditNotificationEvent);
begin
  Events.ServerEdits.Add(Event);
end;

procedure TNotificationManager.RegisterClientMove(
  Event: TClientMoveNotificationEvent);
begin
  Events.ClientMoves.Add(Event);
end;

procedure TNotificationManager.RegisterDisconnect(Event: TClientDCNotificationEvent);
begin
  Events.Disconnects.Add(Event);
end;

procedure TNotificationManager.RegisterConnect(Event: TClientConnectNotificationEvent);
begin
  Events.Connects.Add(Event);
end;

procedure TNotificationManager.RegisterChannelEdit(
  Event: TChannelEditedNotificationEvent);
begin
  Events.ChannelEdits.Add(Event);
end;

procedure TNotificationManager.RegisterChannelDescription(
  Event: TChannelDescriptionChangedEvent);
begin
  Events.DescriptionChanges.Add(Event);
end;

procedure TNotificationManager.UnregisterNotification(EventType: TNotificationType;
  Event: TMethod);
begin
  case EventType of
    ntTextMessage: Events.Texts.Remove(TTextNotificationEvent(Event));
    ntServerEdited: Events.ServerEdits.Remove(TServerEditNotificationEvent(Event));
    ntClientMoved: Events.ClientMoves.Remove(TClientMoveNotificationEvent(Event));
    ntClientDisconnected: Events.Disconnects.Remove(TClientDCNotificationEvent(Event));
    ntClientConnected: Events.Connects.Remove(TClientConnectNotificationEvent(Event));
    ntChannelEdited: Events.ChannelEdits.Remove(TChannelEditedNotificationEvent(Event));
    ntChanneldescriptionChanged: Events.DescriptionChanges.Remove(
        TChannelDescriptionChangedEvent(Event));
  end;
end;

end.
