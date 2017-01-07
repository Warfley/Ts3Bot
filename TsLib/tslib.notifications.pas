unit TsLib.notifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Connection, TsLib.Types, Logger, strutils;

type

  { TNotificationManager }

  TNotificationManager = class
  private
    FConnection: TTsConnection;
    NotifyReciever: TNotifyEventMap;
    Notifications: TNotificationList;
    FActive: boolean;
    RegisterChannel: TIntegerList;
    procedure SetActive(AValue: boolean);
  protected
    function NotificationRecieved(Sender: TObject; Data: string;
      var RemoveFromList: boolean): boolean;
  public
    constructor Create(Connection: TTsConnection);
    procedure Start;
    procedure SendNotifications;
    procedure Stop;
    destructor Destroy; override;
    procedure ClearNotifications;
    procedure ClearCache;
    procedure RegisterNotification(NType: TNotificationEventType;
      Event: TNotificationEvent; cid: integer = 0);
    procedure UnregisterNotification(NType: TNotificationEventType;
      Event: TNotificationEvent; cid: integer = 0);

    property Active: boolean read FActive write SetActive;
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

function TNotificationManager.NotificationRecieved(Sender: TObject;
  Data: string; var RemoveFromList: boolean): boolean;
var
  n: TNotificationData;
begin
  RemoveFromList := False;
  Result := AnsiStartsStr('notify', Data);
  if not Result then
    exit;
  n.NType := GetNotificationType(Copy(Data, 1, Pos(' ', Data) - 1));
end;

constructor TNotificationManager.Create(Connection: TTsConnection);
var
  i: integer;
begin
  FConnection := Connection;
  for i := Low(NotificationLists) to High(NotificationLists) do
    NotificationLists[i] := TNotifyEventList.Create;
  RegisterChannel := TIntegerList.Create;
  Notifications := TNotificationList.Create;
  FActive := False;
end;

procedure TNotificationManager.Start;

  function GetEventName(ntype: TNotificationEventType): string;
  begin
    case ntype of
      netChannelMessage: Result := 'textchannel';
      netChannel: Result := 'channel';
      netPersonalMessage: Result := 'textprivate';
      netServer: Result := 'server';
      netServerMessage: Result := 'textserver';
    end;
  end;

var
  i: TNotificationType;
begin
  if FActive then
    Exit;
  WriteStatus('Activating notifications');
  // Add reciever
  FConnection.AddReciever(@NotificationRecieved);

  // Activate for each channel
  for i := 0 to RegisterChannel.Count - 1 do
    FConnection.ExecCommand(Format('servernotifyregister event=channel id=%d',
      [RegisterChannel[i]]));
  // For each other Notification Type active Notifications
  for i := Low(NotificationLists) + 1 to High(NotificationLists) do
    FConnection.ExecCommand('servernotifyregister event=' + GetEventName(
      TNotificationType(i)));

  FActive := True;
end;

procedure TNotificationManager.SendNotifications;
var
  sl: TStringList;
  n: TNotificationData;
  i, x: integer;
begin
  sl := TStringList.Create;
  try
    for i := 0 to Notifications.Size - 1 do
    begin
      sl.Clear;
      sl.Delimiter := ' ';
      sl.StrictDelimiter := True;
      n := Notifications[i];
      sl.DelimitedText := n.Data;
      for x := 0 to NotifyReciever[Ord(n.NType)].Count - 1 do
        NotifyReciever[Ord(n.NType)][x](Self, n.NType, sl);
    end;
    Notifications.Clear;
  finally
    sl.Free;
  end;
end;

procedure TNotificationManager.Stop;
begin
  FConnection.ExecCommand('servernotifyunregister');
  FConnection.DeleteReciever(@NotificationRecieved);
  FActive := False;
end;

destructor TNotificationManager.Destroy;
var
  i: integer;
begin
  if Active then Stop;

  RegisterChannel.Free;
  Notifications.Free;

  for i := Low(NotificationLists) to High(NotificationLists) do
    NotifyReciever[i].Free;
  inherited Destroy;
end;

procedure TNotificationManager.ClearNotifications;
var
  i: integer;
begin
  if Active then
    Stop;
  RegisterChannel.Clear;
  for i := Low(NotificationLists) to High(NotificationLists) do
    NotifyReciever[i].Clear;
end;

procedure TNotificationManager.ClearCache;
begin
  Notifications.Clear;
end;

procedure TNotificationManager.RegisterNotification(
  NType: TNotificationEventType; Event: TNotificationEvent; cid: integer);
begin
  if Active then
    Stop;

end;

procedure TNotificationManager.UnregisterNotification(
  NType: TNotificationEventType; Event: TNotificationEvent; cid: integer);
begin

end;

end.
