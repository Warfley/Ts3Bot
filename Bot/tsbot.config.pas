unit TsBot.config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite;

type
  TConfig = record
    Username,
    Password,
    IPAddress: string;
    Port: integer;
    ServerID: integer;
    LogPath: string;
    UpdateServerData: Integer;
    UpdateChannelList: Integer;
    UpdateClientList: Integer;
    FloodCommands, FloodTime: Integer;
  end;

  TConfigEvent = procedure(Doc: TXMLDocument) of object;

const
  DefaultConf: TConfig = (Username: 'User'; Password: 'Pass';
    IPAddress: '127.0.0.1'; Port: 10011; ServerID: 1;
    LogPath: {$IfDef DEBUG}'./log.txt'{$Else}''{$EndIf};
    UpdateServerData: -1; UpdateChannelList: 10000;
    UpdateClientList: 1000; FloodCommands: -1;
    FloodTime: -1);
  ConfigVersion = 1;

function ReadConfig(Path: string; OnRead: TConfigEvent): TConfig;
procedure WriteConfig(Path: string; Config: TConfig; OnWrite: TConfigEvent);

implementation

function ReadConfig(Path: string; OnRead: TConfigEvent): TConfig;


function IsNumeric(Str: String): Boolean;
var
  c: Char;
begin
  Result:=Length(Str)>0;
  for c in str do
    if not (c in ['0'..'9']) then
    begin
      Result:=False;
      Break;
    end;
end;

  procedure ReadLoginData(Doc: TXMLDocument);
  var
    Node: TDOMElement;
    attr: string;
  begin
    Node := Doc.DocumentElement.FindNode('Login') as TDOMElement;
    if not Assigned(Node) then
      Exit;
    // IP
    attr := Node.AttribStrings['IP'];
    if Length(attr) > 0 then
      Result.IPAddress := attr;
    // Port
    attr := Node.AttribStrings['Port'];
    if Length(attr) > 0 then
      Result.Port := StrToInt(attr);
    // ServerID
    attr := Node.AttribStrings['ServerID'];
    if Length(attr) > 0 then
      Result.ServerID := StrToInt(attr);
    // ServerID
    attr := Node.AttribStrings['LogFile'];
    if Length(attr) > 0 then
      Result.LogPath := attr;

    Node := Node.FindNode('User') as TDOMElement;
    if not Assigned(Node) then
      Exit;
    // Password
    attr := Node.AttribStrings['Pass'];
    if Length(attr) > 0 then
      Result.Password := attr;
    // User
    attr := Node.AttribStrings['User'];
    if Length(attr) > 0 then
      Result.Username := attr;
  end;

  procedure ReadUpdateInterval(Doc: TXMLDocument);
  var
    Node: TDOMElement;
    attr: string;
  begin
    Node := Doc.DocumentElement.FindNode('UpdateInterval') as TDOMElement;

    if not Assigned(Node) then
      Exit;

    attr := Node.AttribStrings['ServerData'];
    if IsNumeric(attr) then
      Result.UpdateServerData := StrToInt(attr);

    attr := Node.AttribStrings['Channels'];
    if IsNumeric(attr)then
      Result.UpdateChannelList := StrToInt(attr);

    attr := Node.AttribStrings['Clients'];
    if IsNumeric(attr) then
      Result.UpdateClientList := StrToInt(attr);
  end;

  procedure ReadFloodControl(Doc: TXMLDocument);
  var
    Node: TDOMElement;
    attr: string;
  begin
    Node := Doc.DocumentElement.FindNode('AntiFlood') as TDOMElement;

    if not Assigned(Node) then
      Exit;

    attr := Node.AttribStrings['Time'];
    if IsNumeric(attr) then
      Result.FloodTime := StrToInt(attr);

    attr := Node.AttribStrings['Commands'];
    if IsNumeric(attr)then
      Result.FloodCommands := StrToInt(attr);
  end;

var
  doc: TXMLDocument;
begin
  Result := DefaultConf;
  if not FileExists(Path) then
    exit;
  ReadXMLFile(doc, Path);
  try
    ReadLoginData(doc);
    ReadUpdateInterval(doc);
    ReadFloodControl(doc);

    if Assigned(OnRead) then
      OnRead(doc);

  finally
    doc.Free;
  end;
end;

procedure WriteConfig(Path: string; Config: TConfig; OnWrite: TConfigEvent);
var
  doc: TXMLDocument;
  RootNode, ParentNode, DataNode: TDOMNode;
begin
  doc := TXMLDocument.Create;
  try
    RootNode := doc.CreateElement('Config');
    doc.AppendChild(RootNode);
    // Adding Config version attribute to Root node
    TDOMElement(RootNode).SetAttribute('Version', IntToStr(ConfigVersion));

    // Adding Login data
    ParentNode := doc.CreateElement('Login');
    RootNode.AppendChild(ParentNode);
    // Adding connection Attributes
    TDOMElement(ParentNode).SetAttribute('IP', Config.IPAddress);
    TDOMElement(ParentNode).SetAttribute('Port', IntToStr(Config.Port));
    TDOMElement(ParentNode).SetAttribute('ServerID', IntToStr(Config.ServerID));
    if Length(Config.LogPath) > 0 then
      TDOMElement(ParentNode).SetAttribute('LogFile', Config.LogPath);

    // Adding Userdata
    DataNode := doc.CreateElement('User');
    ParentNode.AppendChild(DataNode);
    // Adding Userdata Attributes
    TDOMElement(DataNode).SetAttribute('User', Config.Username);
    TDOMElement(DataNode).SetAttribute('Pass', Config.Password);

    // Adding Update Interval info
    ParentNode:=doc.CreateElement('UpdateInterval');
    RootNode.AppendChild(ParentNode);

    // Adding update intervalls
    TDOMElement(ParentNode).SetAttribute('ServerData', IntToStr(Config.UpdateServerData));
    TDOMElement(ParentNode).SetAttribute('Channels', IntToStr(Config.UpdateChannelList));
    TDOMElement(ParentNode).SetAttribute('Clients', IntToStr(Config.UpdateClientList));

    // Adding FloodControl info
    ParentNode:=doc.CreateElement('AntiFlood');
    RootNode.AppendChild(ParentNode);

    // Adding update intervalls
    TDOMElement(ParentNode).SetAttribute('Time', IntToStr(Config.FloodTime));
    TDOMElement(ParentNode).SetAttribute('Commands', IntToStr(Config.FloodCommands));

    if Assigned(OnWrite) then
      OnWrite(doc);

    WriteXMLFile(doc, Path);
  finally
    doc.Free;
  end;
end;

end.
