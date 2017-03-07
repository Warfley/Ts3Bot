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
    FType: TMessageType;
    FServer: TTsServer;
    FExpirationDate: TDateTime;
    FServerGroups: TStringList;
    FChannelGroups: TStringList;
    FMessage: String;
    function GetExpired: boolean; virtual;
    function CheckUser(Client: TTsClient): boolean; virtual;
  public
    procedure DoAnouncement(Client: TTsClient); virtual;
    constructor Create(AMessage: string; AExpirationDate: TDateTime;
      AServer: TTsServer; AType: TMessageType = mtPrivate; AChannelGroups: String='';
      AServerGroups: String=''); virtual;
    destructor Destroy; override;

    procedure AddServerGroup(ServerGroup: string);
    procedure RemoveServerGroup(ServerGroup: string);

    procedure AddChannelGroup(ChannelGroup: String);
    procedure RemoveChannelGroup(ChannelGroup: String);

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
    property ChannelName: string read FChannelName write FChannelName;
  end;

  { TAnnouncementModule }

  TAnnouncementModule = class(TBotModule)
  private
    FEnabled: boolean;
    FConnection: TTsConnection;
    FServer: TTsServer;
    FServerAnnouncements: TAnnouncementList;
    FChannelAnnouncements: TAnnouncementList;
    FNotifications: TNotificationManager;
    FRequiredServerGroups: TStringList;
  protected
    procedure CheckExpired(Sender: TObject; Data: IntPtr);
    procedure ClientConnected(Sender: TObject; Client: TTsClient);
    procedure ClientMoved(Sender: TObject; Client: TTsClient;
      Source, Target: TTsChannel);
    procedure DoAnnounce(Sender: TObject; Data: IntPtr);
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

uses dateutils, Math, Logger, strutils, TsLib.ValueRead;

{ TAnnouncementModule }

procedure TAnnouncementModule.CheckExpired(Sender: TObject; Data: IntPtr);
var
  i: Integer;
begin
  i:=0;
  while i<FServerAnnouncements.Count do
    if FServerAnnouncements[i].Expired then
      FServerAnnouncements.Delete(i)
    else
      inc(i);

  while i<FChannelAnnouncements.Count do
    if FChannelAnnouncements[i].Expired then
      FChannelAnnouncements.Delete(i)
    else
      inc(i);
end;

procedure TAnnouncementModule.ClientConnected(Sender: TObject; Client: TTsClient);
var
  i: Integer;
begin
  if not FEnabled then exit;
  for i:=0 to FServerAnnouncements.Count-1 do
    FServerAnnouncements[i].DoAnouncement(Client);
  for i:=0 to FChannelAnnouncements.Count-1 do
    FChannelAnnouncements[i].DoAnouncement(Client);
end;

procedure TAnnouncementModule.ClientMoved(Sender: TObject; Client: TTsClient;
  Source, Target: TTsChannel);
var
  i: Integer;
begin
  if not FEnabled then exit;
  for i:=0 to FChannelAnnouncements.Count-1 do
    FChannelAnnouncements[i].DoAnouncement(Client);
end;

procedure TAnnouncementModule.DoAnnounce(Sender: TObject; Data: IntPtr);
var
  i: Integer;
begin
  for i:=0 to FServer.Clients.Count-1 do
    ClientConnected(Self, FServer.Clients[i]);
end;

function TAnnouncementModule.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

procedure TAnnouncementModule.SetEnabled(AValue: boolean);
begin
  FEnabled := AValue;
end;

function TAnnouncementModule.GetName: string;
begin
  Result := 'announcements';
end;

procedure TAnnouncementModule.TextMessage(Sender: TObject; AData: TTextNotification);
var sl:TStringList;
  i, x: Integer;
  n: String;
  c: TTsClient;
  ReqMet, True: Boolean;
  Channel: String;
  SGroups: String;
  CGroups: String;
  Expires: TDateTime;
  ExpStr: String;
  MType: TMessageType;
  Msg: String;
  a: TAnnouncement;
begin
  if not FEnabled then exit;
  sl:=TStringList.Create;
  try
    i:=1;
    while i<=Length(AData.Message) do
      sl.Add(ReadArgument(AData.Message, i));

    if (sl.Count=0) or (sl[0] <> '!announce') then
      exit;

    c:=FServer.GetClientByID(AData.Invoker.ID);
    if not Assigned(c) then
      exit;

    ReqMet:=False;

     for x:=0 to Length(c.ClientData.ServerGroups)-1 do
     begin
       n:=LowerCase(FServer.GetServerGroupByID(c.ClientData.ServerGroups[x]).Name);
       for i:=0 to FRequiredServerGroups.Count-1 do
         if LowerCase(FRequiredServerGroups[i])=n then
         begin
           ReqMet:=True;
           Break;
         end;
       if ReqMet then Break;
     end;

    if not ReqMet then
    begin
      SendPrivateMessage(FServer,'You require one of the following servergroups to '+
      'perform this'#10+FRequiredServerGroups.Text, AData.Invoker.ID);
      Exit;
    end;

    if (sl.Count=1) or (sl[1] = 'help') then
    begin
      SendPrivateMessage(FServer, '!announce [channel="channel"]'+
      ' [cgroups="Group1;Group2;..."] [sgroups="Group1;Group2;..."] '+
      '[type=poke|private] [expires="Time"] message="Textmessage ..."'#10+
      'The time format is "A-B-C-D-E-F-G" with A years B month'+
      ' C days E hours F minutes and G seconds from now on you can cut '+
      ' of leading 0 values'#10+
      'E.g.: !announce sgroups="Guest" expires="5-12-0-0" message="welcome"'#10+
      'If no type is specified private messages are sent', AData.Invoker.ID);
      Exit;
    end;
    MType:=mtPrivate;
    for i:=1 to sl.Count-1 do
      if LowerCase(sl.Names[i]) = 'channel' then
        Channel:=sl.ValueFromIndex[i]
      else if LowerCase(sl.Names[i]) = 'cgroups' then
        CGroups:=sl.ValueFromIndex[i]
      else if LowerCase(sl.Names[i]) = 'sgroups' then
        SGroups:=sl.ValueFromIndex[i]
      else if LowerCase(sl.Names[i]) = 'type' then
        MType:=TMessageType(IfThen(sl.ValueFromIndex[i]='poke', ord(mtPoke), ord(mtPrivate)))
      else if LowerCase(sl.Names[i]) = 'expires' then
        ExpStr:=sl.ValueFromIndex[i]
      else if LowerCase(sl.Names[i]) = 'message' then
        Msg:=sl.ValueFromIndex[i];

    sl.Clear;
    sl.Delimiter:='-';
    sl.DelimitedText:=ExpStr;
    for i:=0 to sl.Count-1 do
      if not IsNumeric(sl[i]) then
      begin
        SendPrivateMessage(FServer, 'Expirationdate is in the wrong format',
        AData.Invoker.ID);
        exit;
      end;
      if Msg='' then
      begin
        SendPrivateMessage(FServer, 'No message', AData.Invoker.ID);
        exit;
      end;
    if ExpStr='' then
      Expires:=NoTime
    else
      Expires:=now;
    for i:=1 to sl.Count do
      case i of
      1: Expires:=IncSecond(Expires, StrToInt64(sl[sl.Count-i]));
      2: Expires:=IncMinute(Expires, StrToInt64(sl[sl.Count-i]));
      3: Expires:=IncHour(Expires, StrToInt64(sl[sl.Count-i]));
      4: Expires:=IncDay(Expires, StrToInt64(sl[sl.Count-i]));
      5: Expires:=IncMonth(Expires, StrToInt64(sl[sl.Count-i]));
      6: Expires:=IncYear(Expires, StrToInt64(sl[sl.Count-i]));
      end;
    if Channel<>'' then
    begin
      a:=TChannelAnnouncement.Create(Msg, Expires, FServer, MType, CGroups, SGroups);
      (a as TChannelAnnouncement).ChannelName:=Channel;
      FChannelAnnouncements.Add(a);
    end
    else
    begin
      a:=TAnnouncement.Create(Msg, Expires, FServer, MType, CGroups, SGroups);
      FServerAnnouncements.Add(a);
    end;
  finally
    sl.Free;
  end;
  FCore.RegisterSchedule(0, @DoAnnounce, 0, True);
end;

constructor TAnnouncementModule.Create(Core: TTBCore);
begin
  inherited Create(Core);
  FRequiredServerGroups := TStringList.Create;
  FServerAnnouncements := TAnnouncementList.Create(True);
  FChannelAnnouncements := TAnnouncementList.Create(True);
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
  FCore.RegisterSchedule(Minutes5, @CheckExpired);
end;

procedure TAnnouncementModule.DoneModule;
begin
  FNotifications.UnregisterNotification(ntTextMessage, TMethod(@TextMessage));
  FCore.UnregisterConnectedEvent(@ClientConnected);
  FCore.UnregisterMoveEvent(@ClientMoved);
  FCore.RemoveSchedule(@CheckExpired);
end;

procedure TAnnouncementModule.ReadConfig(doc: TXMLDocument);
var
  Parent: TDOMElement;
  Node: TDOMElement;
  attr: string;
  i: integer;
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

  for i := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node := Parent.ChildNodes[i] as TDOMElement;
    if Node.TagName = 'Required' then
    begin
      attr := Parent.AttribStrings['Group'];
      if Length(attr) > 0 then
        FRequiredServerGroups.Add(attr);
    end
    else if Node.TagName = 'ServerAnnouncement' then
    begin
      a := TAnnouncement.Create('', NoTime, FServer);
      a.Load(doc, Node);
      FServerAnnouncements.Add(a);
    end
    else if Node.TagName = 'ChannelAnnouncement' then
    begin
      a := TChannelAnnouncement.Create('', NoTime, FServer);
      a.Load(doc, Node);
      FChannelAnnouncements.Add(a);
    end;
  end;

  if FRequiredServerGroups.Count = 0 then
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

  for i := 0 to FRequiredServerGroups.Count - 1 do
  begin
    Node := doc.CreateElement('Required');
    Parent.AppendChild(Node);
    Node.AttribStrings['Group'] := FRequiredServerGroups[i];
  end;

  for i := 0 to FServerAnnouncements.Count - 1 do
  begin
    Node := doc.CreateElement('ServerAnnouncement');
    Parent.AppendChild(Node);
    FServerAnnouncements[i].Save(doc, Node);
  end;

  for i := 0 to FChannelAnnouncements.Count - 1 do
  begin
    Node := doc.CreateElement('ChannelAnnouncement');
    Parent.AppendChild(Node);
    FServerAnnouncements[i].Save(doc, Node);
  end;

end;

function TAnnouncementModule.GetHelp: string;
begin
  Result := 'Announcements: !announce help for more information';
end;

function TAnnouncementModule.ConfigModule(Config: TStringList): boolean;
var
  sl: TStringList;
begin
  if not Assigned(Config) or (Config.Count = 0) then
    exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Config[0];
    FRequiredServerGroups.Clear;
    FRequiredServerGroups.AddStrings(sl);
  finally
    sl.Free;
  end;
end;

procedure TAnnouncementModule.GetConfigItems(Sl: TStringList);
begin
  if not Assigned(sl) then
    exit;
  sl.Add('Required servergroups (separated by ;)');
end;

procedure TAnnouncementModule.GetConfig(SL: TStringList);
begin
  if not Assigned(sl) then
    exit;
  sl.Add('Required servergroups:');
  sl.AddStrings(FRequiredServerGroups);
end;

{ TChannelAnnouncement }

function TChannelAnnouncement.CheckUser(Client: TTsClient): boolean;
begin
  Result := inherited CheckUser(Client) and Assigned(Client.Channel);
  if not Result then
    exit;
  Result := LowerCase(Client.Channel.ChannelData.Name) = LowerCase(FChannelName);
end;

{ TAnnouncement }

function TAnnouncement.GetExpired: boolean;
begin
  Result := (FExpirationDate <> NoTime) and
    (CompareDateTime(FExpirationDate, Now) = LessThanValue);
end;

function TAnnouncement.CheckUser(Client: TTsClient): boolean;
var
  i, x: integer;
  g: integer;
begin
  Result := (FServerGroups.Count = 0) and (FChannelGroups.Count = 0);
  if Result then
    Exit;
  for x := 0 to FServerGroups.Count - 1 do
  begin
    g := FServer.GetServerGroupByName(FServerGroups[x]).ID;
    if g < 0 then
      Continue;
    for i := 0 to Length(Client.ClientData.ServerGroups) - 1 do
      if Client.ClientData.ServerGroups[i] = g then
      begin
        Result := True;
        Exit;
      end;
  end;

  for x := 0 to FChannelGroups.Count - 1 do
  begin
    g := FServer.GetChannelGroupByName(FChannelGroups[x]).ID;
    if Client.ClientData.ChannelGroup = g then
    begin
      Result := True;
      Exit;
    end;
  end;

end;

procedure TAnnouncement.DoAnouncement(Client: TTsClient);
begin
  // Check if user needs to get the message
  if CheckUser(Client) and (FUsersSend.IndexOf(Client.ClientData.UID) < 0) then
  begin
    WriteStatus('Sending announcement to '+Client.ClientData.Name);
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
  AServer: TTsServer; AType: TMessageType; AChannelGroups: String;
  AServerGroups: String);
begin
  FMessage := AMessage;
  FExpirationDate := AExpirationDate;
  FType := AType;
  FUsersSend := TStringList.Create;
  FServer := AServer;
  FServerGroups := TStringList.Create;
  FChannelGroups := TStringList.Create;
  FServerGroups.Delimiter:=';';
  FServerGroups.StrictDelimiter:=True;
  FServerGroups.DelimitedText:=AServerGroups;
  FChannelGroups.Delimiter:=';';
  FChannelGroups.StrictDelimiter:=True;
  FChannelGroups.DelimitedText:=AChannelGroups;
end;

destructor TAnnouncement.Destroy;
begin
  FServerGroups.Free;
  FChannelGroups.Free;
  FUsersSend.Free;
  inherited Destroy;
end;

procedure TAnnouncement.AddServerGroup(ServerGroup: string);
begin
  if FServerGroups.IndexOf(ServerGroup) < 0 then
    FServerGroups.Add(ServerGroup);
end;

procedure TAnnouncement.RemoveServerGroup(ServerGroup: string);
var
  i: integer;
begin
  i := FServerGroups.IndexOf(ServerGroup);
  if i >= 0 then
    FServerGroups.Delete(i);
end;

procedure TAnnouncement.AddChannelGroup(ChannelGroup: String);
begin
  if FChannelGroups.IndexOf(ChannelGroup) < 0 then
    FChannelGroups.Add(ChannelGroup);
end;

procedure TAnnouncement.RemoveChannelGroup(ChannelGroup: String);
var
  i: integer;
begin
  i:=FChannelGroups.IndexOf(ChannelGroup);
  if i>=0 then
  FChannelGroups.Delete(i);
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
  for i := 0 to FChannelGroups.Count - 1 do
  begin
    n := Doc.CreateElement('Channel');
    Node.AppendChild(n);
    n.AttribStrings['Group'] := FChannelGroups[i];
  end;
  for i := 0 to FServerGroups.Count - 1 do
  begin
    n := Doc.CreateElement('Server');
    Node.AppendChild(n);
    n.AttribStrings['Group'] := FServerGroups[i];
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

  // Read Userdata and Message and Groups
  FUsersSend.Clear;
  FChannelGroups.Clear;
  FServerGroups.Clear;

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
      FMessage := n.TextContent
    else if n.TagName = 'Channel' then
    begin
      attr := n.AttribStrings['Group'];
      if Length(attr) > 0 then
        FChannelGroups.Add(attr);
    end
    else if n.TagName = 'Server' then
    begin
      attr := n.AttribStrings['Group'];
      if Length(attr) > 0 then
        FServerGroups.Add(attr);
    end;
  end;
end;

end.
