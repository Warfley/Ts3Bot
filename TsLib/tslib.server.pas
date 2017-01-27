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

  TTsServer = class
  private
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

end;

destructor TTsServer.Destroy;
begin
  inherited Destroy;
end;

procedure TTsServer.UpdateChannelList;
begin

end;

procedure TTsServer.UpdateServerData;
begin

end;

end.

