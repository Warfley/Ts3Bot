unit TsLib.Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Types, TsLib.connection, TsLib.ValueRead, TsLib.NotificationManager, fgl;


type
  TTsChannel = class

  end;

  TTsChannelList = specialize TFPGObjectList<TTsChannel>;

  { TTsServer }
  EServerDataException = Class(Exception);

  TTsServer = class
  private
    FServerData: TServerData;
    FChannels: TTsChannelList;
    FNotifications: TNotificationManager;
    FConnection: TTsConnection;
    procedure EnableNotifications(AEnable: Boolean);
    function NotificationsEnabled: Boolean;
  public
    constructor Create(AConnection: TTsConnection; ANotifications: TNotificationManager = nil);
    destructor Destroy; override;

    procedure UpdateChannelList;
    procedure UpdateServerData;

    property Connection: TTsConnection read FConnection write FConnection;
    property NotificationManager: TNotificationManager read FNotifications write FNotifications;
    property UseNotifications: Boolean read NotificationsEnabled write EnableNotifications;
    property ServerData: TServerData read FServerData;
  end;

implementation

{ TTsServer }

procedure TTsServer.EnableNotifications(AEnable: Boolean);
begin

end;

function TTsServer.NotificationsEnabled: Boolean;
begin

end;

constructor TTsServer.Create(AConnection: TTsConnection;
  ANotifications: TNotificationManager);
begin
  FConnection:=AConnection;
  FNotifications:=ANotifications;
end;

destructor TTsServer.Destroy;
begin
  inherited Destroy;
end;

procedure TTsServer.UpdateChannelList;
begin

end;

procedure TTsServer.UpdateServerData;
var
  SDString: String;
  Res: TStatusResponse;
begin
  Res:=FConnection.ExecCommand('serverinfo', SDString);
  if Res.ErrNo <> 0 then
    raise EServerDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  FServerData := SDString;
end;

end.

