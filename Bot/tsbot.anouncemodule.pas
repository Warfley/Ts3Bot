unit TsBot.AnounceModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.core, TsLib.connection, TsLib.NotificationManager,
  TsLib.Types, TsLib.Server, TsBot.Utils, fgl, DOM;

type
  TMessageType = (mtPrivate, mtPoke);

  { TAnnouncement }

  TAnnouncement = class
  private
  protected
    FUsersSend: TStringList;
    FMessage: string;
    FType: TMessageType;
    FExpirationDate: TDateTime;
    FServerGroups: TIntegerList;
    FChannelGroups: TIntegerList;
    function GetExpired: boolean; virtual;
    function CheckUser(Client: TTsClient): boolean; virtual;
  public
    procedure DoAnouncement(Client: TTsClient); virtual;
    constructor Create(AMessage: string; AExpirationDate: TDateTime;
      AType: TMessageType = mtPrivate); virtual;
    destructor Destroy; override;

    procedure SetServerGroups(ServerGroup: integer);
    procedure RemoveServerGroups(ServerGroup: integer);

    procedure SetChannelGroups(ChannelGroup: integer);
    procedure RemoveChannelGroups(ChannelGroup: integer);

    procedure Save(Doc: TXMLDocument; Node: TDOMElement); virtual;
    procedure Load(Doc: TXMLDocument; Node: TDOMElement); virtual;

    property Expired: boolean read GetExpired;
  end;

  TAnnouncementList = specialize TFPGObjectList<TAnnouncement>;

  { TChannelAnnouncement }

  TChannelAnnouncement = class(TAnnouncement)
  private
    FChannelName: string;
  protected
    function CheckUser(Client: TTsClient): boolean; override;
  public
    property ChannelName: String read FChannelName write FChannelName;
  end;

  { TAnnouncementModule }

  TAnnouncementModule = class(TBotModule)
  private
    FEnabled: boolean;
    FConnection: TTsConnection;
    FServer: TTsServer;
    FCore: TTBCore;
    FServerAnnouncements: TAnnouncementList;
    FChannelAnnouncements: TAnnouncementList;
    FNotifications: TNotificationManager;
    FRequiredServerGroups: TStringList;
  protected
    procedure ClientConnected(Sender: TObject; Client: TTsClient);
    procedure ClientMoved(Sender: TObject; Client: TTsClient; Source,
      Target: TTsChannel);
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

    function GetHelp: string; override;

    function ConfigModule(Config: TStringList): boolean; override;
    procedure GetConfigItems(Sl: TStringList); override;
    procedure GetConfig(SL: TStringList); override;
  end;

const
  NoTime: TDateTime = TDateTime(0);

implementation

uses dateutils, Math, Logger, strutils;

{ TAnnouncementModule }

procedure TAnnouncementModule.ClientConnected(Sender: TObject; Client: TTsClient
  );
begin

end;

procedure TAnnouncementModule.ClientMoved(Sender: TObject; Client: TTsClient;
  Source, Target: TTsChannel);
begin

end;

function TAnnouncementModule.GetEnabled: boolean;
begin
  Result:=FEnabled;
end;

procedure TAnnouncementModule.SetEnabled(AValue: boolean);
begin
  FEnabled:=AValue;
end;

function TAnnouncementModule.GetName: string;
begin
  Result:='announcements';
end;

procedure TAnnouncementModule.TextMessage(Sender: TObject;
  AData: TTextNotification);
begin

end;

constructor TAnnouncementModule.Create(Core: TTBCore);
begin
  FRequiredServerGroups:=TStringList.Create;
  FServerAnnouncements:= TAnnouncementList.Create(True);
    FChannelAnnouncements:= TAnnouncementList.Create(True);
  FCore:=Core;
end;

destructor TAnnouncementModule.Destroy;
begin
  FRequiredServerGroups.Free;
  FServerAnnouncements.Free;
  FChannelAnnouncements.Free;
  inherited Destroy;
end;

procedure TAnnouncementModule.InitModule;
begin
  FConnection := FCore.Connection;
  FServer := FCore.Server;
  FNotifications := FCore.NotificationManager;

  FNotifications.RegisterText(@TextMessage);
  FCore.RegisterClientConnectEvent(@ClientConnected);
  FCore.RegisterClientMoveEvent(@ClientMoved);
end;

procedure TAnnouncementModule.DoneModule;
begin
  FNotifications.UnregisterNotification(ntTextMessage, TMethod(@TextMessage));
  FCore.UnregisterConnectedEvent(@ClientConnected);
  FCore.UnregisterMoveEvent(@ClientMoved);
end;

procedure TAnnouncementModule.ReadConfig(doc: TXMLDocument);
var
  Parent: TDOMElement;
  Node: TDOMElement;
  attr: String;
  i: Integer;
  a: TAnnouncement;
begin
  FRequiredServerGroups.Clear;
  Parent := Doc.DocumentElement.FindNode('Announcements') as TDOMElement;

  if not Assigned(Parent) then
  begin
    FRequiredServerGroups.Add('Server Admin');
    FEnabled := True;
    Exit;
  end;

  attr := Parent.AttribStrings['Enabled'];
  if Length(attr) > 0 then
    FEnabled := attr = '1'
  else
    FEnabled := True;

  for i:=0 to Parent.ChildNodes.Count-1 do
  begin
    Node:=Parent.ChildNodes[i] as TDOMElement;
    if Node.TagName = 'Required' then
    begin
      attr := Parent.AttribStrings['Group'];
      if Length(attr) > 0 then
        FRequiredServerGroups.Add(attr);
    end
    else if Node.TagName='ServerAnnouncement' then
    begin
      a:=TAnnouncement.Create('', NoTime);
      a.Load(doc,Node);
      FServerAnnouncements.Add(a);
    end
    else if Node.TagName='ChannelAnnouncement' then
    begin
      a:=TChannelAnnouncement.Create('', NoTime);
      a.Load(doc,Node);
      FChannelAnnouncements.Add(a);
    end
  end;

  if FRequiredServerGroups.Count=0 then
    FRequiredServerGroups.Add('Server Admin');
end;

procedure TAnnouncementModule.WriteConfig(doc: TXMLDocument);
var
  Parent, Node: TDOMElement;
  i: integer;
begin
  Parent := Doc.CreateElement('Announcements');
  doc.DocumentElement.AppendChild(Parent);

  Parent.AttribStrings['Enabled'] := BoolToStr(FEnabled, '1', '0');

  for i:=0 to FRequiredServerGroups.Count -1 do
  begin
    Node:=doc.CreateElement('Required');
    Parent.AppendChild(Node);
    Node.AttribStrings['Group']:=FRequiredServerGroups[i];
  end;

  for i:=0 to FServerAnnouncements.Count -1 do
  begin
    Node:=doc.CreateElement('ServerAnnouncement');
    Parent.AppendChild(Node);
    FServerAnnouncements[i].Save(doc, Node);
  end;

  for i:=0 to FChannelAnnouncements.Count -1 do
  begin
    Node:=doc.CreateElement('ChannelAnnouncement');
    Parent.AppendChild(Node);
    FServerAnnouncements[i].Save(doc, Node);
  end;

end;

function TAnnouncementModule.GetHelp: string;
begin
  Result := 'Announcements: !announce help for more information';
end;

function TAnnouncementModule.ConfigModule(Config: TStringList): boolean;
var sl: TStringList;
begin
  if not Assigned(Config) or (Config.Count=0) then exit;
  sl:=TStringList.Create;
  try
    sl.Delimiter:=';';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=Config[0];
    FRequiredServerGroups.Clear;
    FRequiredServerGroups.AddStrings(sl);
  finally
    sl.Free;
  end;
end;

procedure TAnnouncementModule.GetConfigItems(Sl: TStringList);
begin
  if not Assigned(sl) then exit;
  sl.Add('Required servergroups (separated by ;)');
end;

procedure TAnnouncementModule.GetConfig(SL: TStringList);
var
  i: Integer;
begin
  if not Assigned(sl) then exit;
  sl.Add('Required servergroups:');
  sl.AddStrings(FRequiredServerGroups);
end;

{ TChannelAnnouncement }

function TChannelAnnouncement.CheckUser(Client: TTsClient): boolean;
begin
  Result:=inherited CheckUser(Client);
  if not Result then exit;
  Result:=LowerCase(Client.Channel.ChannelData.Name) = LowerCase(FChannelName);
end;

{ TAnnouncement }

function TAnnouncement.GetExpired: boolean;
begin
  Result := (FExpirationDate <> NoTime) and
    (CompareDateTime(FExpirationDate, Now) = LessThanValue);
end;

function TAnnouncement.CheckUser(Client: TTsClient): boolean;
var
  i: integer;
begin
  Result := (FServerGroups.Count = 0) and (FChannelGroups.Count = 0);
  if Result then
    Exit;

  for i := 0 to Length(Client.ClientData.ServerGroups) - 1 do
    if FServerGroups.IndexOf(Client.ClientData.ServerGroups[i]) >= 0 then
    begin
      Result := True;
      Exit;
    end;

  Result := FChannelGroups.IndexOf(Client.ClientData.ChannelGroup) >= 0;
end;

procedure TAnnouncement.DoAnouncement(Client: TTsClient);
begin
  // Check if user needs to get the message
  if CheckUser(Client) and (FUsersSend.IndexOf(Client.ClientData.UID) < 0) then
  begin
    // send
    if FType = mtPoke then
      Client.PokeMessage(FMessage)
    else
      Client.SendMessage(FMessage);
    // save to not send again
    FUsersSend.Add(Client.ClientData.UID);
  end;
end;

constructor TAnnouncement.Create(AMessage: string; AExpirationDate: TDateTime;
  AType: TMessageType);
begin
  FMessage := AMessage;
  FExpirationDate := AExpirationDate;
  FType := AType;
  FUsersSend := TStringList.Create;
end;

destructor TAnnouncement.Destroy;
begin
  FUsersSend.Free;
  inherited Destroy;
end;

procedure TAnnouncement.SetServerGroups(ServerGroup: integer);
var
  i: integer;
begin
  if FServerGroups.IndexOf(ServerGroup) < 0 then
    FServerGroups.Add(ServerGroup);
end;

procedure TAnnouncement.RemoveServerGroups(ServerGroup: integer);
var
  i: integer;
begin
  FServerGroups.Remove(ServerGroup);
end;

procedure TAnnouncement.SetChannelGroups(ChannelGroup: integer);
var
  i: integer;
begin
  if FChannelGroups.IndexOf(ChannelGroup) < 0 then
    FChannelGroups.Add(ChannelGroup);
end;

procedure TAnnouncement.RemoveChannelGroups(ChannelGroup: integer);
var
  i: integer;
begin
  FChannelGroups.Remove(ChannelGroup);
end;

procedure TAnnouncement.Save(Doc: TXMLDocument; Node: TDOMElement);
var
  i: integer;
  n: TDOMElement;
begin
  Node.AttribStrings['Expires'] := DateTimeToStr(FExpirationDate);
  Node.AttribStrings['Type'] := IfThen(FType = mtPoke, 'poke', 'private');
  for i := 0 to FUsersSend.Count - 1 do
  begin
    n := Doc.CreateElement('Arrived');
    Node.AppendChild(n);
    n.AttribStrings['UID'] := FUsersSend[i];
  end;
  n := Doc.CreateElement('Message');
  Node.AppendChild(n);
  n.TextContent := FMessage;
end;

procedure TAnnouncement.Load(Doc: TXMLDocument; Node: TDOMElement);
var
  i: integer;
  n: TDOMElement;
  attr: string;
begin
  // Read expiration Date
  FExpirationDate := NoTime;
  attr := Node.AttribStrings['Expires'];
  if Length(attr) > 0 then
    FExpirationDate := StrToDateTime(attr);
  // Load type
  attr := Node.AttribStrings['Type'];
  if attr = 'poke' then
    FType := mtPoke
  else
    FType := mtPrivate;

  // Read Userdata and Message
  FUsersSend.Clear;
  for i := 0 to Node.ChildNodes.Count - 1 do
  begin
    n := Node.ChildNodes[i] as TDOMElement;
    if n.TagName = 'Arrived' then
    begin
      attr := n.AttribStrings['UID'];
      if Length(attr) > 0 then
        FUsersSend.Add(attr);
    end
    else if n.TagName = 'Message' then
      FMessage := n.TextContent;
  end;
end;

end.
