unit TsLib.Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Types, TsLib.connection, TsLib.ValueRead,
  TsLib.NotificationManager, fgl;

type
  { Forward declarations }
  TTsClient = class;
  TTsChannel = class;
  TTsServer = class;

  { Lists }

  TTsClientList = specialize TFPGObjectList<TTsClient>;
  TTsChannelList = specialize TFPGObjectList<TTsChannel>;

  { Exceptions }

  EServerDataException = class(Exception);
  EServerException = class(Exception);

  EChannelDataException = class(Exception);
  EChannelException = class(Exception);

  { TTsClient }

  TTsClient = class

  end;

  { TTsChannel }

  TTsChannel = class
  private
    FChannelData: TChannelData;
    FClients: TTsChannelList;
    FChannels: TTsChannelList;
    FNotifications: TNotificationManager;
    FConnection: TTsConnection;
    FEnableNotifications: boolean;
    FServer: TTsServer;
    FTag: IntPtr;
    procedure EnableNotifications(AEnable: boolean);
  protected
    procedure OnDescriptionNotification(Sender: TObject; ChannelID: integer);
    procedure OnEditNotification(Sender: TObject;
      AData: TChannelEditedNotification);
  public
    constructor Create(AServer: TTsServer; AConnection: TTsConnection;
      ANotifications: TNotificationManager = nil; DataString: String);
    destructor Destroy; override;

    procedure UpdateChannelData;

    property Connection: TTsConnection read FConnection write FConnection;
    property NotificationManager: TNotificationManager
      read FNotifications write FNotifications;
    property UseNotifications: boolean read FEnableNotifications
      write EnableNotifications;
    property ChannelData: TChannelData read FChannelData;
    property Tag: IntPtr read FTag write FTag;
    property Server: TTsServer read FServer write FServer;
  end;

  { TTsServer }

  TTsServer = class
  private
    FServerData: TServerData;
    FChannels: TTsChannelList;
    FNotifications: TNotificationManager;
    FConnection: TTsConnection;
    FEnableNotifications: boolean;
    FTag: IntPtr;
    procedure EnableNotifications(AEnable: boolean);
  protected
    procedure OnEditNotification(Sender: TObject; AData: TServerEditNotification);
  public
    constructor Create(AConnection: TTsConnection;
      ANotifications: TNotificationManager = nil);
    destructor Destroy; override;

    procedure UpdateClientList;
    procedure UpdateChannelList;
    procedure UpdateServerData;

    property Connection: TTsConnection read FConnection write FConnection;
    property NotificationManager: TNotificationManager
      read FNotifications write FNotifications;
    property UseNotifications: boolean read FEnableNotifications
      write EnableNotifications;
    property ServerData: TServerData read FServerData;
    property Tag: IntPtr read FTag write FTag;
  end;

implementation

{ TTsChannel }

procedure TTsChannel.EnableNotifications(AEnable: boolean);
begin
  if AEnable = FEnableNotifications then
    exit;
  if FNotifications = nil then
    raise
    EServerException.Create('No notification Manager found');
  if AEnable then
  begin
    FNotifications.RegisterChannelEdit(@OnEditNotification, FChannelData.ID);
    FNotifications.RegisterChannelDescription(@OnDescriptionNotification,
      FChannelData.ID);
  end
  else
  begin
    FNotifications.UnregisterNotification(ntChannelEdited, @OnEditNotification);
    FNotifications.UnregisterNotification(ntChanneldescriptionChanged,
      @OnDescriptionNotification);
  end;
  FEnableNotifications := AEnable;
end;

procedure TTsChannel.OnDescriptionNotification(Sender: TObject; ChannelID: integer);
begin
  UpdateChannelData;
end;

procedure TTsChannel.OnEditNotification(Sender: TObject;
  AData: TChannelEditedNotification);
var
  i: integer;
begin
  for i := 0 to Length(AData.Changes) - 1 do
    SetChannelData(AData.Changes[i][0], AData.Changes[i][1], FChannelData);
end;

constructor TTsChannel.Create(AServer: TTsServer; AConnection: TTsConnection;
  ANotifications: TNotificationManager; DataString: String);
begin
  FServer:=AServer;
  FConnection:=AConnection;
  FNotifications:=ANotifications;
  FEnableNotifications:=False;
  FChannels := TTsChannelList.Create(True);
  FClients := TTsClientList.Create(True);
  FChannelData = DataString;
end;

destructor TTsChannel.Destroy;
begin
  FChannels.Free;
  FClients.Free;
  inherited Destroy;
end;

procedure TTsChannel.UpdateChannelData;
var
  SDString: string;
  Res: TStatusResponse;
begin
  Res := FConnection.ExecCommand(Format('channelinfo cid=%d', [FChannelData.ID]), SDString);
  if Res.ErrNo <> 0 then
    raise EChannelDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  FServerData := SDString;
end;

{ TTsServer }

procedure TTsServer.EnableNotifications(AEnable: boolean);
begin
  if AEnable = FEnableNotifications then
    exit;
  if FNotifications = nil then
    raise
    EServerException.Create('No notification Manager found');
  if AEnable then
    FNotifications.RegisterServerEdit(@OnEditNotification)
  else
    FNotifications.UnregisterNotification(ntServerEdited, @OnEditNotification);
  FEnableNotifications := AEnable;
end;

procedure TTsServer.OnEditNotification(Sender: TObject; AData: TServerEditNotification);
var
  i: integer;
begin
  for i := 0 to Length(AData.Changes) - 1 do
    SetServerData(AData.Changes[i][0], AData.Changes[i][1], FServerData);
end;

constructor TTsServer.Create(AConnection: TTsConnection;
  ANotifications: TNotificationManager);
begin
  FConnection := AConnection;
  FNotifications := ANotifications;
  FChannels := TTsChannelList.Create(True);
  FEnableNotifications:=False;
end;

destructor TTsServer.Destroy;
begin
  FChannels.Free;
  inherited Destroy;
end;

procedure TTsServer.UpdateClientList;
begin

end;

procedure TTsServer.UpdateChannelList;
begin

end;

procedure TTsServer.UpdateServerData;
var
  SDString: string;
  Res: TStatusResponse;
begin
  Res := FConnection.ExecCommand('serverinfo', SDString);
  if Res.ErrNo <> 0 then
    raise EServerDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  FServerData := SDString;
end;

end.
