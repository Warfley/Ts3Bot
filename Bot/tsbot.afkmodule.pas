unit TsBot.AfkModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.core, TsLib.connection, TsLib.NotificationManager,
  TsLib.Types, TsLib.Server, gvector, DOM, Logger;

type

  TAfkData = record
    UID: string;
    IdleTime: integer;
    InMutedTime: integer;
    OutMutedTime: integer;
    AwayTime: integer;
    LastChannel: integer;
    DestinationChannel: string;
  end;

  TAfkDataList = specialize TVector<TAfkData>;

  { TAfkModule }

  TAfkModule = class(TBotModule)
  private
    FEnabled: boolean;
    FConnection: TTsConnection;
    FServer: TTsServer;
    FCore: TTBCore;
    FDefaultAfkName: string;
    FNotifications: TNotificationManager;
    AfkData: TAfkDataList;
    function CheckForMove(Client: TTsClient; Data: TAfkData): Boolean;
  protected
    procedure ClientUpdated(Sender: TObject; Client: TTsClient);
    // enable/disable Module
    function GetEnabled: boolean; override;
    procedure SetEnabled(AValue: boolean); override;
    // Returns the name of the module
    function GetName: string; override;
    procedure TextMessage(Sender: TObject; AData: TTextNotification);
  public
    constructor Create(Core: TTBCore); override;
    destructor Destroy; override;

    // Called after the serverdata is collected
    procedure InitModule; override;
    // called before the cleanup
    procedure DoneModule; override;

    // for modules that want to use the config
    procedure ReadConfig(doc: TXMLDocument); override;
    procedure WriteConfig(doc: TXMLDocument); override;

    function ConfigModule(Config: TStringList): boolean; override;
    procedure GetConfigItems(Sl: TStringList); override;
    procedure GetConfig(SL: TStringList); override;
  end;

const
  DefaultAfkData: TAfkData = (UID: ''; IdleTime: -1; InMutedTime: -1;
    OutMutedTime: -1; AwayTime: -1; LastChannel: 0; DestinationChannel: '');

implementation

{ TAfkModule }

function TAfkModule.CheckForMove(Client: TTsClient; Data: TAfkData): Boolean;
begin
  Result:=False;
  with Data do
  begin
    if IdleTime>=0 then
      Result := Result or (Client.ClientData.IdleTime>IdleTime);
    if AwayTime>=0 then
      Result := Result or ((Client.ClientData.IdleTime>AwayTime) and Client.ClientData.IsAway);
    if InMutedTime>=0 then
      Result := Result or ((Client.ClientData.IdleTime>InMutedTime) and Client.ClientData.MutedInput);
    if OutMutedTime>=0 then
      Result := Result or ((Client.ClientData.IdleTime>OutMutedTime) and (Client.ClientData.MutedOutput or Client.ClientData.MutedOutputOnly));
  end;
end;

procedure TAfkModule.ClientUpdated(Sender: TObject; Client: TTsClient);
var
  i: integer;
  tc: Integer;
  doMove, inAfk: Boolean;
  d: TAfkData;
begin
  if not Enabled then
    exit;
  for i := 0 to AfkData.Size - 1 do
  begin
    d:=AfkData[i];
    with d do
    if Client.ClientData.UID=UID then
    begin
      tc := FServer.GetChannelByName(DestinationChannel).ChannelData.ID;
      doMove:=CheckForMove(Client, AfkData[i]);
      inAfk:=(tc = Client.ClientData.ChannelID);
      if inAfk and (LastChannel>0) and not doMove then //in afk channel but requirement not met
      begin
        Client.Channel:=FServer.GetChannelByID(LastChannel);
        LastChannel:=0;
      end
      else if not inAfk and doMove then
      begin
        LastChannel:=Client.ClientData.ChannelID;
        Client.Channel:=FServer.GetChannelByID(tc);
      end
      else if not inAfk and not doMove then
        LastChannel:=0;
    end;
    AfkData[i]:=d;
  end;
end;

function TAfkModule.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

procedure TAfkModule.SetEnabled(AValue: boolean);
begin
  FEnabled := AValue;
end;

function TAfkModule.GetName: string;
begin
  Result := 'afk move';
end;

procedure TAfkModule.TextMessage(Sender: TObject; AData: TTextNotification);

  function ReadToken(Str: string; var p: integer): string;
  var
    len: integer;
  begin
    Result := '';
    len := 0;
    while (p <= Length(Str)) and (Str[p] in [#1..' ']) do
      Inc(p);
    if p > Length(str) then
      Exit;
    if Str[p] = '"' then
    begin
      Inc(p);
      while (p + len <= Length(Str)) and (Str[p + len] <> '"') do
        Inc(len);
    end
    else
      while (p + len <= Length(Str)) and (Ord(Str[p + len]) > Ord(' ')) do
        Inc(len);
    Result := Copy(str, p, len);
    Inc(p, len);
  end;

var
  sl: TStringList;
  i: integer;
  u: integer;
  d: TAfkData;
begin
  if not Enabled then
    exit;

  sl := TStringList.Create;
  try
    i := 1;
    while i < Length(AData.Message) do
      sl.Add(ReadToken(AData.Message, i));
    if sl[0] <> '!afkmove' then
      exit;
    if sl[1] = 'help' then
    begin
      FServer.SendPrivateMessage('!afkmove [idle=Time] [inputmuted=Time] ' +
        '[outputmuted=Time] [away=Time] [channel="channel"]'#10 +
        'Where Time specifies the total count of milliseconds before you get' +
        ' moved. "channel" defines the channel you will be moved to (Default: ' +
        FDefaultAfkName + ')'#10'if you set Time < 0 this event will be ignored',
        AData.Invoker.ID);


      Exit;
    end;

    u := -1;
    d := DefaultAfkData;
    d.DestinationChannel := FDefaultAfkName;
    for i := 0 to AfkData.Size - 1 do
      if AfkData[i].UID = AData.Invoker.UID then
      begin
        u := i;
        d := AfkData[i];
        break;
      end;

    for i := 1 to sl.Count - 1 do
    begin
      if LowerCase(sl.Names[i]) = 'idle' then
        d.IdleTime := StrToInt(sl.ValueFromIndex[i])
      else if LowerCase(sl.Names[i]) = 'inputmuted' then
        d.InMutedTime := StrToInt(sl.ValueFromIndex[i])
      else if LowerCase(sl.Names[i]) = 'outputmuted' then
        d.OutMutedTime := StrToInt(sl.ValueFromIndex[i])
      else if LowerCase(sl.Names[i]) = 'away' then
        d.AwayTime := StrToInt(sl.ValueFromIndex[i])
      else if (LowerCase(sl[i]) = 'channel=') and (i < sl.Count - 1) then
        d.DestinationChannel := sl[i + 1]
      else if LowerCase(sl.Names[i]) = 'channel' then
        d.DestinationChannel := sl.ValueFromIndex[i];
    end;
    if u = -1 then
    begin
      d.UID := AData.Invoker.UID;
      AfkData.PushBack(d);
    end
    else
      AfkData[u] := d;
  finally
    sl.Free;
  end;

end;

constructor TAfkModule.Create(Core: TTBCore);
begin
  FCore := Core;
  AfkData := TAfkDataList.Create;
  FEnabled := True;
  FDefaultAfkName := 'AFK';
end;

destructor TAfkModule.Destroy;
begin
  AfkData.Free;
  inherited Destroy;
end;

procedure TAfkModule.InitModule;
begin
  FConnection := FCore.Connection;
  FServer := FCore.Server;
  FNotifications := FCore.NotificationManager;

  FNotifications.RegisterText(@TextMessage);
  FCore.RegisterClientUpdateEvent(@ClientUpdated);
  FCore.ClientUpdate := FCore.ClientUpdate + [clVoice, clTimes, clUID, clAway];
end;

procedure TAfkModule.DoneModule;
begin
  FNotifications.UnregisterNotification(ntTextMessage, TMethod(@TextMessage));
  FCore.UnregisterUpdateEvent(@ClientUpdated);
end;

procedure TAfkModule.ReadConfig(doc: TXMLDocument);
var
  Parent, Node: TDOMElement;
  attr: string;
  d: TAfkData;
  i: integer;
begin
  Parent := Doc.DocumentElement.FindNode('AFK') as TDOMElement;

  if Parent = nil then
  begin
    FDefaultAfkName := 'AFK';
    FEnabled := True;
    Exit;
  end;

  attr := Parent.AttribStrings['ChannelName'];
  if Length(attr) > 0 then
    FDefaultAfkName := attr
  else
    FDefaultAfkName := 'AFK';

  attr := Parent.AttribStrings['Enabled'];
  if Length(attr) > 0 then
    FEnabled := attr = '1'
  else
    FEnabled := True;

  for i := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node := Parent.ChildNodes[i] as TDOMElement;
    d := DefaultAfkData;

    attr := Node.AttribStrings['UID'];
    if Length(attr) > 0 then
      d.UID := attr;

    attr := Node.AttribStrings['DestinationChannel'];
    if Length(attr) > 0 then
      d.DestinationChannel := attr
    else
      d.DestinationChannel := FDefaultAfkName;

    attr := Node.AttribStrings['IdleTime'];
    if Length(attr) > 0 then
      d.IdleTime := StrToInt(attr);

    attr := Node.AttribStrings['InMutedTime'];
    if Length(attr) > 0 then
      d.InMutedTime := StrToInt(attr);

    attr := Node.AttribStrings['OutMutedTime'];
    if Length(attr) > 0 then
      d.OutMutedTime := StrToInt(attr);

    attr := Node.AttribStrings['AwayTime'];
    if Length(attr) > 0 then
      d.AwayTime := StrToInt(attr);

    AfkData.PushBack(d);
  end;

end;

procedure TAfkModule.WriteConfig(doc: TXMLDocument);
var
  Parent, Node: TDOMElement;
  i: integer;
begin
  Parent := Doc.CreateElement('AFK');
  doc.DocumentElement.AppendChild(Parent);

  Parent.AttribStrings['ChannelName'] := FDefaultAfkName;
  Parent.AttribStrings['Enabled'] := BoolToStr(FEnabled, '1', '0');

  for i := 0 to AfkData.Size - 1 do
  begin
    Node := doc.CreateElement('User');
    Parent.AppendChild(Node);

    Node.AttribStrings['UID'] := AfkData[i].UID;
    Node.AttribStrings['DestinationChannel'] := AfkData[i].DestinationChannel;
    Node.AttribStrings['IdleTime'] := IntToStr(AfkData[i].IdleTime);
    Node.AttribStrings['InMutedTime'] := IntToStr(AfkData[i].InMutedTime);
    Node.AttribStrings['OutMutedTime'] := IntToStr(AfkData[i].OutMutedTime);
    Node.AttribStrings['AwayTime'] := IntToStr(AfkData[i].AwayTime);
  end;

end;

function TAfkModule.ConfigModule(Config: TStringList): boolean;
begin
  FDefaultAfkName := Config[0];
end;

procedure TAfkModule.GetConfigItems(Sl: TStringList);
begin
  sl.Add('Default Afk Channel');
end;

procedure TAfkModule.GetConfig(SL: TStringList);
begin
  sl.Add('Default Afk Channel: ' + FDefaultAfkName);
end;

end.
