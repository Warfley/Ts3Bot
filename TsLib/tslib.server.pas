unit TsLib.Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Types, TsLib.connection, TsLib.ValueRead,
  TsLib.NotificationManager, fgl, Logger;

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
  EServerMoveException = class(Exception);

  EChannelDataException = class(Exception);
  EChannelException = class(Exception);

  EClientDataException = class(Exception);

  { Events }
  TServerUpdateEvent = TNotifyEvent;
  TChannelUpdateEvent = procedure(Sender: TObject; Channel: TTsChannel) of object;
  TClientUpdateEvent = procedure(Sender: TObject; Client: TTsClient) of object;

  { TTsClient }

  TTsClient = class
  private
    FServer: TTsServer;
    FConnection: TTsConnection;
    FOnUpdate: TNotifyEvent;
    FClientData: TClientData;
    FTag: IntPtr;
    FFlag: boolean;
    procedure MoveToChannel(Dest: TTsChannel);
    function GetChannel: TTsChannel;
  public
    procedure ReadClientData(Data: string);
    procedure UpdateClientData;

    constructor Create(AServer: TTsServer; AConnection: TTsConnection;
      DataString: string = '');
    destructor Destroy; override;

    property Connection: TTsConnection read FConnection write FConnection;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property Tag: IntPtr read FTag write FTag;
    property Flag: boolean read FFlag write FFlag;
    property ClientData: TClientData read FClientData;
    property Channel: TTsChannel read GetChannel write MoveToChannel;
    property Server: TTsServer read FServer write FServer;
  end;

  { TTsChannel }

  TTsChannel = class
  private
    FChannelData: TChannelData;
    FClients: TTsClientList;
    FChannels: TTsChannelList;
    FNotifications: TNotificationManager;
    FConnection: TTsConnection;
    FEnableNotifications: boolean;
    FOnUpdate: TNotifyEvent;
    FServer: TTsServer;
    FTag: IntPtr;
    FFlag: boolean;
    procedure EnableNotifications(AEnable: boolean);
    function GetParent: TTsChannel;
    procedure SetParent(AValue: TTsChannel);
  protected
    procedure OnDescriptionNotification(Sender: TObject; ChannelID: integer);
    procedure OnEditNotification(Sender: TObject;
      AData: TChannelEditedNotification);
  public
    constructor Create(AServer: TTsServer; AConnection: TTsConnection;
      ANotifications: TNotificationManager = nil; DataString: string = '');
    destructor Destroy; override;

    procedure UpdateChannelData;

    procedure ReadChannelData(Data: string);

    property Connection: TTsConnection read FConnection write FConnection;
    property NotificationManager: TNotificationManager
      read FNotifications write FNotifications;
    property UseNotifications: boolean read FEnableNotifications
      write EnableNotifications;
    property ChannelData: TChannelData read FChannelData;
    property Tag: IntPtr read FTag write FTag;
    property Flag: boolean read FFlag write FFlag;
    property Server: TTsServer read FServer write FServer;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property Clients: TTsClientList read FClients;
    property Channels: TTsChannelList read FChannels;
    property Parent: TTsChannel read GetParent write SetParent;
  end;

  { TTsServer }

  TTsServer = class
  private
    FOnChannelUpdate: TChannelUpdateEvent;
    FOnClientUpdate: TClientUpdateEvent;
    FServerData: TServerData;
    FChannels: TTsChannelList;
    FClients: TTsClientList;
    FChannelTree: TTsChannelList;
    FNotifications: TNotificationManager;
    FConnection: TTsConnection;
    FEnableNotifications: boolean;
    FTag: IntPtr;
    FOnUpdate: TServerUpdateEvent;
    procedure EnableNotifications(AEnable: boolean);
  protected
    procedure ChannelUpdated(Sender: TObject);
    procedure ClientUpdated(Sender: TObject);
    procedure OnConnectNotification(Sender: TObject;
      AData: TClientConnectNotification);
    procedure OnDCNotification(Sender: TObject; AData: TClientDCNotification);
    procedure OnEditNotification(Sender: TObject; AData: TServerEditNotification);
    procedure OnMoveNotification(Sender: TObject; AData: TClientMoveNotification);
  public
    constructor Create(AConnection: TTsConnection;
      ANotifications: TNotificationManager = nil);
    destructor Destroy; override;

    procedure UpdateClientList(UpdateData: TClientUpdates = DefaultClientUpdate);
    procedure UpdateChannelList(UpdateData: TChannelUpdates = DefaultChannelUpdate);
    procedure UpdateServerData;

    function IndexOfChannel(ChannelID: integer): integer;
    function IndexOfChannel(ChannelName: string): integer;
    function GetChannelByID(CID: integer): TTsChannel;
    function GetChannelByName(Name: integer): TTsChannel;

    function IndexOfClientUID(UID: string): integer;
    function IndexOfClient(ClientID: integer): integer;
    function IndexOfClient(ClientName: string): integer;
    function GetClientByUID(UID: string): TTsClient;
    function GetClientByID(CID: integer): TTsClient;
    function GetClientByName(Name: integer): TTsClient;

    function MoveClient(ClientID: integer; ChannelID: integer): boolean;
    function MoveChannel(ChannelID, ParentID: integer; Order: integer = -1): boolean;

    property Connection: TTsConnection read FConnection write FConnection;
    property NotificationManager: TNotificationManager
      read FNotifications write FNotifications;
    property UseNotifications: boolean read FEnableNotifications
      write EnableNotifications;
    property ServerData: TServerData read FServerData;
    property Tag: IntPtr read FTag write FTag;
    property OnUpdate: TServerUpdateEvent read FOnUpdate write FOnUpdate;
    property OnClientUpdate: TClientUpdateEvent
      read FOnClientUpdate write FOnClientUpdate;
    property OnChannelUpdate: TChannelUpdateEvent
      read FOnChannelUpdate write FOnChannelUpdate;
  end;

implementation

{ TTsClient }

constructor TTsClient.Create(AServer: TTsServer; AConnection: TTsConnection;
  DataString: string);
begin
  FServer := AServer;
  FConnection := AConnection;
  FClientData := DataString;
  Tag := 0;
end;

destructor TTsClient.Destroy;
begin
  inherited Destroy;
end;

procedure TTsClient.MoveToChannel(Dest: TTsChannel);
begin
  if not Assigned(Dest) or (Dest.ChannelData.ID = ClientData.ChannelID) then
    Exit;
  try
    if FServer.MoveClient(ClientData.ID, Dest.ChannelData.ID) then
      FClientData.ChannelID := Dest.ChannelData.ID;
  except
    on e: EServerMoveException do ;
  end;
end;

function TTsClient.GetChannel: TTsChannel;
begin
  Result := FServer.GetChannelByID(ClientData.ChannelID);
end;

procedure TTsClient.ReadClientData(Data: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Data;
    for i := 0 to sl.Count - 1 do
      if sl.Names[i] = '' then
        SetClientData(sl[i], '', FClientData)
      else
        SetClientData(sl.Names[i], sl.ValueFromIndex[i], FClientData);
  finally
    sl.Free;
  end;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TTsClient.UpdateClientData;
var
  SDString: string;
  Res: TStatusResponse;
begin
  Res := FConnection.ExecCommand(Format('clientinfo clid=%d', [FClientData.ID]),
    SDString);
  if Res.ErrNo <> 0 then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EClientDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
  ReadClientData(SDString);
end;

{ TTsChannel }

procedure TTsChannel.EnableNotifications(AEnable: boolean);
begin
  if AEnable = FEnableNotifications then
    exit;
  if FNotifications = nil then
    raise EServerException.Create('No notification Manager found');
  if AEnable then
  begin
    FNotifications.RegisterChannelEdit(@OnEditNotification);
    FNotifications.RegisterChannelDescription(@OnDescriptionNotification);
  end
  else
  begin
    FNotifications.UnregisterNotification(ntChannelEdited, TMethod(@OnEditNotification));
    FNotifications.UnregisterNotification(ntChanneldescriptionChanged,
      TMethod(@OnDescriptionNotification));
  end;
  FEnableNotifications := AEnable;
end;

function TTsChannel.GetParent: TTsChannel;
begin
  Result := FServer.GetChannelByID(ChannelData.ParentID);
end;

procedure TTsChannel.SetParent(AValue: TTsChannel);
begin
  if not Assigned(AValue) or (AValue.ChannelData.ID = ChannelData.ParentID) then
    Exit;
  try
    if FServer.MoveChannel(ChannelData.ID, AValue.ChannelData.ID) then
      FChannelData.ParentID := Dest.ChannelData.ID;
  except
    on e: EServerMoveException do ;
  end;

end;

procedure TTsChannel.OnDescriptionNotification(Sender: TObject; ChannelID: integer);
begin
  if ChannelID <> FChannelData.ID then
    Exit;
  UpdateChannelData;
end;

procedure TTsChannel.OnEditNotification(Sender: TObject;
  AData: TChannelEditedNotification);
var
  i: integer;
begin
  if AData.ChannelID <> FChannelData.ID then
    Exit;
  if Length(AData.Changes) = 0 then
    UpdateChannelData
  else
  begin
    for i := 0 to Length(AData.Changes) - 1 do
      SetChannelData(AData.Changes[i][0], AData.Changes[i][1], FChannelData);
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

constructor TTsChannel.Create(AServer: TTsServer; AConnection: TTsConnection;
  ANotifications: TNotificationManager; DataString: string);
begin
  FServer := AServer;
  FConnection := AConnection;
  FNotifications := ANotifications;
  FEnableNotifications := False;
  FChannels := TTsChannelList.Create(False);
  FClients := TTsClientList.Create(False);
  FChannelData := DataString;
  Tag := 0;
end;

destructor TTsChannel.Destroy;
begin
  EnableNotifications(False);
  FChannels.Free;
  FClients.Free;
  inherited Destroy;
end;

procedure TTsChannel.UpdateChannelData;
var
  SDString: string;
  Res: TStatusResponse;
begin
  Res := FConnection.ExecCommand(Format('channelinfo cid=%d', [FChannelData.ID]),
    SDString);
  if Res.ErrNo <> 0 then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EChannelDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
  ReadChannelData(SDString);
end;

procedure TTsChannel.ReadChannelData(Data: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Data;
    for i := 0 to sl.Count - 1 do
      if sl.Names[i] = '' then
        SetChannelData(sl[i], '', FChannelData)
      else
        SetChannelData(sl.Names[i], sl.ValueFromIndex[i], FChannelData);
  finally
    sl.Free;
  end;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{ TTsServer }

procedure TTsServer.EnableNotifications(AEnable: boolean);
var
  i: integer;
begin
  if AEnable = FEnableNotifications then
    exit;
  if FNotifications = nil then
    raise EServerException.Create('No notification Manager found');
  if AEnable then
  begin
    FNotifications.RegisterServerEdit(@OnEditNotification);
    FNotifications.RegisterConnect(@OnConnectNotification);
    FNotifications.RegisterDisconnect(@OnDCNotification);
    FNotifications.RegisterClientMove(@OnMoveNotification);
  end
  else
  begin
    FNotifications.UnregisterNotification(ntServerEdited, TMethod(@OnEditNotification));
    FNotifications.UnregisterNotification(ntServerEdited,
      TMethod(@OnConnectNotification));
    FNotifications.UnregisterNotification(ntServerEdited, TMethod(@OnDCNotification));
    FNotifications.UnregisterNotification(ntServerEdited, TMethod(@OnMoveNotification));
  end;
  for i := 0 to FChannels.Count - 1 do
  begin
    FChannels[i].NotificationManager := NotificationManager;
    FChannels[i].UseNotifications := AEnable;
  end;
  FEnableNotifications := AEnable;
end;

procedure TTsServer.ChannelUpdated(Sender: TObject);
begin
  if Assigned(FOnChannelUpdate) then
    FOnChannelUpdate(Self, Sender as TTsChannel);
end;

procedure TTsServer.ClientUpdated(Sender: TObject);
begin
  if Assigned(FOnClientUpdate) then
    FOnClientUpdate(Self, Sender as TTsClient);
end;

procedure TTsServer.OnConnectNotification(Sender: TObject;
  AData: TClientConnectNotification);
begin
  { TODO : Handle connects }
end;

procedure TTsServer.OnDCNotification(Sender: TObject; AData: TClientDCNotification);
begin
  { TODO : Handle Disconnects }
end;

procedure TTsServer.OnEditNotification(Sender: TObject; AData: TServerEditNotification);
var
  i: integer;
begin
  for i := 0 to Length(AData.Changes) - 1 do
    SetServerData(AData.Changes[i][0], AData.Changes[i][1], FServerData);
  if Length(AData.Changes) = 0 then
    UpdateServerData
  else if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TTsServer.OnMoveNotification(Sender: TObject; AData: TClientMoveNotification);
begin
  { TODO : Handle Moves }
end;

constructor TTsServer.Create(AConnection: TTsConnection;
  ANotifications: TNotificationManager);
begin
  FConnection := AConnection;
  FNotifications := ANotifications;
  FChannels := TTsChannelList.Create(True);
  FChannelTree := TTsChannelList.Create(False);
  FClients := TTsClientList.Create(True);
  FEnableNotifications := False;
end;

destructor TTsServer.Destroy;
begin
  EnableNotifications(False);
  FClients.Free;
  FChannels.Free;
  FChannelTree.Free;
  inherited Destroy;
end;

procedure TTsServer.UpdateClientList(UpdateData: TClientUpdates);

  procedure AddClient(Data: string);
  var
    newc: TTsClient;
  begin
    newc := TTsClient.Create(Self, FConnection, Data);
    newc.OnUpdate := @ClientUpdated;
    FClients.Add(newc);
  end;

var
  sl: TStringList;
  Data, Command: string;
  FullUpdate: boolean;
  Res: TStatusResponse;
  i, ch: integer;
  c: TTsClient = nil;
  d: TClientData;
begin
  FullUpdate := UpdateData = FullClientUpdate;
  // Build update command
  Command := 'clientlist';
  if clUID in UpdateData then
    Command += ' -uid';
  if clAway in UpdateData then
    Command += ' -away';
  if clVoice in UpdateData then
    Command += ' -voice';
  if clTimes in UpdateData then
    Command += ' -times';
  if clGroups in UpdateData then
    Command += ' -groups';
  if clInfo in UpdateData then
    Command += ' -info';
  if clCountry in UpdateData then
    Command += ' -country';
  if clIP in UpdateData then
    Command += ' -ip';
  if clBadges in UpdateData then
    Command += ' -badges';


  // Run command
  Res := FConnection.ExecCommand(Command, Data);
  if Res.ErrNo <> 0 then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EClientDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
  // Clear clients
  if FullUpdate then
    FClients.Clear
  else
    for i := 0 to FChannels.Count - 1 do   // or mark as unupdate
      FClients[i].Flag := True;
  sl := TStringList.Create;
  try
    sl.StrictDelimiter := True;
    sl.Delimiter := '|';
    sl.DelimitedText := Data;
    // Update each client
    for i := 0 to sl.Count - 1 do
      if FullUpdate then // Either create new
        AddClient(sl[i])
      else
      begin
        // or find and update old
        d := sl[i];
        if d.ID > 0 then
          c := GetClientByID(d.ID);
        if Assigned(c) then
        begin
          c.ReadClientData(sl[i]);
          c.Flag := False;
        end
        else // not found -> create new
          AddClient(sl[i]);
      end;
    // Delete all non exsisting anymore channel
    if not FullUpdate then
      while i < FClients.Count do
        if FClients[i].Flag then
          FClients.Delete(i)
        else
          Inc(i);
  finally
    sl.Free;
  end;
  // Update channels Clientlist
  for i := 0 to FChannels.Count - 1 do
    FChannels[i].Clients.Clear;
  for i := 0 to FClients.Count - 1 do
  begin
    ch := IndexOfChannel(FClients[i].ClientData.ChannelID);
    if ch >= 0 then
      FChannels[ch].Clients.Add(FClients[i]);
  end;
end;

procedure TTsServer.UpdateChannelList(UpdateData: TChannelUpdates);

  procedure AddChannel(Data: string);
  var
    newc: TTsChannel;
  begin
    newc := TTsChannel.Create(Self, FConnection, FNotifications, Data);
    newc.UseNotifications := UseNotifications;
    newc.OnUpdate := @ChannelUpdated;
    FChannels.Add(newc);
  end;

var
  sl: TStringList;
  Data, Command: string;
  FullUpdate: boolean;
  Res: TStatusResponse;
  i: integer;
  c: TTsChannel = nil;
  d: TChannelData;
begin
  FullUpdate := UpdateData = FullChannelUpdate;
  // Build update command
  Command := 'channellist';
  if cuTopic in UpdateData then
    Command += ' -topic';
  if cuVoice in UpdateData then
    Command += ' -voice';
  if cuLimits in UpdateData then
    Command += ' -limits';
  if cuIcon in UpdateData then
    Command += ' -icon';
  if cuSecondsEmpty in UpdateData then
    Command += ' -secondsempty';

  // Run command
  Res := FConnection.ExecCommand(Command, Data);
  if Res.ErrNo <> 0 then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EChannelDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
  // Clear channels
  if FullUpdate then
    FChannels.Clear
  else
    for i := 0 to FChannels.Count - 1 do   // or mark as unupdate
      FChannels[i].Flag := True;
  sl := TStringList.Create;
  try
    sl.StrictDelimiter := True;
    sl.Delimiter := '|';
    sl.DelimitedText := Data;
    // Update each channel
    for i := 0 to sl.Count - 1 do
      if FullUpdate then // Either create new
        AddChannel(sl[i])
      else
      begin
        // or find and update old
        d := sl[i];
        if d.ID > 0 then
          c := GetChannelByID(d.ID);
        if Assigned(c) then
        begin
          c.UseNotifications := True;
          c.ReadChannelData(sl[i]);
          c.Channels.Clear;
          c.Flag := False;
        end
        else // not found -> create new
          AddChannel(sl[i]);
      end;
    // Delete all non exsisting anymore channel
    if not FullUpdate then
      while i < FChannels.Count do
        if FChannels[i].Flag then
          FChannels.Delete(i)
        else
          Inc(i);
  finally
    sl.Free;
  end;
  // Build Channel Tree
  FChannelTree.Clear;
  for i := 0 to FChannels.Count - 1 do
    if FChannels[i].ChannelData.ParentID = 0 then
      FChannelTree.Add(FChannels[i])
    else
    begin
      c := GetChannelByID(FChannels[i].ChannelData.ParentID);
      if Assigned(c) then
        c.Channels.Add(FChannels[i]);
    end;
end;

procedure TTsServer.UpdateServerData;
var
  SDString: string;
  Res: TStatusResponse;
begin
  Res := FConnection.ExecCommand('serverinfo', SDString);
  if Res.ErrNo <> 0 then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EServerDataException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
  FServerData := SDString;
end;

function TTsServer.IndexOfChannel(ChannelID: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FChannels.Count - 1 do
    if FChannels[i].ChannelData.ID = ChannelID then
    begin
      Result := i;
      Break;
    end;
end;

function TTsServer.IndexOfChannel(ChannelName: string): integer;
var
  i: integer;
begin
  Result := -1;
  ChannelName := LowerCase(ChannelName);
  for i := 0 to FChannels.Count - 1 do
    if LowerCase(FChannels[i].ChannelData.Name) = ChannelName then
    begin
      Result := i;
      Break;
    end;
end;

function TTsServer.GetChannelByID(CID: integer): TTsChannel;
var
  idx: integer;
begin
  idx := IndexOfChannel(CID);
  if idx >= 0 then
    Result := FChannels[idx]
  else
    Result := nil;
end;

function TTsServer.GetChannelByName(Name: integer): TTsChannel;
var
  idx: integer;
begin
  idx := IndexOfChannel(Name);
  if idx >= 0 then
    Result := FChannels[idx]
  else
    Result := nil;
end;

function TTsServer.IndexOfClientUID(UID: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FClients.Count - 1 do
    if FClients[i].ClientData.UID = UID then
    begin
      Result := i;
      Break;
    end;
end;

function TTsServer.IndexOfClient(ClientID: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FClients.Count - 1 do
    if FClients[i].ClientData.ID = ClientID then
    begin
      Result := i;
      Break;
    end;
end;

function TTsServer.IndexOfClient(ClientName: string): integer;
var
  i: integer;
begin
  Result := -1;
  ClientName := LowerCase(ClientName);
  for i := 0 to FClients.Count - 1 do
    if LowerCase(FClients[i].ClientData.Name) = ClientName then
    begin
      Result := i;
      Break;
    end;
end;

function TTsServer.GetClientByUID(UID: string): TTsClient;
var
  idx: integer;
begin
  idx := IndexOfClientUID(UID);
  if idx >= 0 then
    Result := FClients[idx]
  else
    Result := nil;
end;

function TTsServer.GetClientByID(CID: integer): TTsClient;
var
  idx: integer;
begin
  idx := IndexOfClient(CID);
  if idx >= 0 then
    Result := FClients[idx]
  else
    Result := nil;
end;

function TTsServer.GetClientByName(Name: integer): TTsClient;
var
  idx: integer;
begin
  idx := IndexOfClient(Name);
  if idx >= 0 then
    Result := FClients[idx]
  else
    Result := nil;
end;

function TTsServer.MoveClient(ClientID: integer; ChannelID: integer): boolean;
var
  res: TStatusResponse;
begin
  res := FConnection.ExecCommand(Format('clientmove clid=%d cid=%d',
    [ClientID, ChannelID]));
  Result := res.ErrNo = 0;
  if not Result then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EServerMoveException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
end;

function TTsServer.MoveChannel(ChannelID, ParentID: integer; Order: integer): boolean;
var
  res: TStatusResponse;
begin
  if Order >= 0 then
    res := FConnection.ExecCommand(Format('channelmove cid=%d cpid=%d order=%d',
      [ChannelID, ParentID, Order]))
  else
    res := FConnection.ExecCommand(Format('channelmove cid=%d cpid=%d',
      [ChannelID, ParentID]));
  Result := res.ErrNo = 0;
  if not Result then
  begin
    WriteError(res.ErrNo, res.Msg);
    raise EServerMoveException.Create(Format('Error [%d]: %s', [res.ErrNo, res.Msg]));
  end;
end;

end.
