unit TsLib.NotificationManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Connection, TsLib.Types, Logger, strutils;

type

  { TNotificationManager }

  TNotificationManager = class
  private
    FAutoSendNotifications: boolean;
    FEventThread: TThread;
    FThreadedNotifications: boolean;
    FConnection: TTsConnection;
    Events: TEventLists;
    QueuedNotifications: TThreadList;
    FActive: boolean;
    ListeningChannels: TIntegerList;
    procedure SetActive(AValue: boolean);
    procedure AddListeningChannel(ChannelID: integer);
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
    procedure RegisterClientMove(Event: TClientMoveNotificationEvent;
      ChannelID: integer);
    procedure RegisterDisconnect(Event: TClientDCNotificationEvent);
    procedure RegisterConnect(Event: TClientConnectNotificationEvent);
    procedure RegisterChannelEdit(Event: TChannelEditedNotificationEvent;
      ChannelID: integer);
    procedure RegisterChannelDescription(Event: TChannelDescriptionChangedEvent;
      ChannelID: integer);

    procedure UnregisterNotification(EventType: TNotificationType; Event: TMethod);
    property Active: boolean read FActive write SetActive;
    property EventThread: TThread read FEventThread write FEventThread;
    property AutoSendNotifications: boolean
      read FAutoSendNotifications write FAutoSendNotifications;
    property ThreadedNotifications: boolean
      read FThreadedNotifications write FThreadedNotifications;
  end;

implementation

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

procedure TNotificationManager.AddListeningChannel(ChannelID: integer);
begin
  if ListeningChannels.IndexOf(ChannelID) = -1 then
  begin
    ListeningChannels.Add(ChannelID);
    if Active then
      FConnection.ExecCommand('servernotifyregister event=channel id=' +
        IntToStr(ChannelID));
  end;
end;

function TNotificationManager.NotificationRecieved(Sender: TObject;
  Data: string; var RemoveFromList: boolean): boolean;
var
  n: PNotificationData;
begin
  RemoveFromList := False;
  Result := AnsiStartsStr('notify', Data);
  if not Result then
    exit;
  new(n);
  n^.NType := GetNotificationType(Copy(Data, 1, Pos(' ', Data) - 1));
  Delete(Data, 1, Pos(' ', Data));
  n^.Data := Data;
  QueuedNotifications.Add(n);
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
  ListeningChannels := TIntegerList.Create;
  QueuedNotifications := TThreadList.Create;
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
  // Register Channel events
  for i := 0 to ListeningChannels.Count - 1 do
    FConnection.ExecCommand('servernotifyregister event=channel id=' +
      IntToStr(ListeningChannels[i]));
  FActive := True;
end;

procedure TNotificationManager.SendNotifications;

{$Include include/SendNotifications.inc}

var
  lst: TList;
  i: integer;
begin
  lst := QueuedNotifications.LockList;
  try
    for i := 0 to lst.Count - 1 do
    begin
      case PNotificationData(lst[i])^.NType of
        ntTextMessage: SendTM(PNotificationData(lst[i])^);
        ntServerEdited: SendSE(PNotificationData(lst[i])^);
        ntClientMoved: SendCM(PNotificationData(lst[i])^);
        ntClientDisconnected: SendCD(PNotificationData(lst[i])^);
        ntClientConnected: SendCC(PNotificationData(lst[i])^);
        ntChannelEdited: SendCE(PNotificationData(lst[i])^);
        ntChanneldescriptionChanged: SendDC(PNotificationData(lst[i])^);
      end;
    end;
  finally
    QueuedNotifications.UnlockList;
  end;
end;

procedure TNotificationManager.Stop;
var
  cnt: integer;
begin
  // Send out chached Notifications
  try
    cnt := QueuedNotifications.LockList.Count;
  finally
    QueuedNotifications.UnlockList;
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

  ListeningChannels.Free;
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
  lst: TList;
begin
  lst := QueuedNotifications.LockList;
  try
    for i := 0 to lst.Count - 1 do
      Dispose(PNotificationData(lst[i]));
  finally
    QueuedNotifications.UnlockList;
  end;
  QueuedNotifications.Clear;
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
  ListeningChannels.Clear;
end;

procedure TNotificationManager.RegisterText(Event: TTextNotificationEvent);
begin
  Events.Texts.Add(Event);
end;

procedure TNotificationManager.RegisterServerEdit(Event: TServerEditNotificationEvent);
begin
  Events.ServerEdits.Add(Event);
end;

procedure TNotificationManager.RegisterClientMove(Event: TClientMoveNotificationEvent;
  ChannelID: integer);
begin
  Events.ClientMoves.Add(Event);
  AddListeningChannel(ChannelID);
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
  Event: TChannelEditedNotificationEvent; ChannelID: integer);
begin
  Events.ChannelEdits.Add(Event);
  AddListeningChannel(ChannelID);
end;

procedure TNotificationManager.RegisterChannelDescription(
  Event: TChannelDescriptionChangedEvent; ChannelID: integer);
begin
  Events.DescriptionChanges.Add(Event);
  AddListeningChannel(ChannelID);
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
