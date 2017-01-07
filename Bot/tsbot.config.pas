unit TsBot.config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TConfig = record
    Username,
    Password,
    IPAddress: string;
    Port: integer;
    ServerID: integer;
    LogPath: string;
  end;

const
  DefaultConf: TConfig = (Username: 'User'; Password: 'Pass';
    IPAddress: '127.0.0.1'; Port: 10011; ServerID: 1;
    LogPath: {$IfDef DEBUG}'./log.txt'{$Else}''{$EndIf});
  ConfigVersion = 1;

function ReadConfig(Path: string): TConfig;
procedure WriteConfig(Path: string; Config: TConfig);

implementation

uses DOM, XMLRead, XMLWrite;

function ReadConfig(Path: string): TConfig;

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

var
  doc: TXMLDocument;
begin
  Result := DefaultConf;
  if not FileExists(Path) then
    exit;
  ReadXMLFile(doc, Path);
  try
    ReadLoginData(doc);
  finally
    doc.Free;
  end;
end;

procedure WriteConfig(Path: string; Config: TConfig);
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

    WriteXMLFile(doc, Path);
  finally
    doc.Free;
  end;
end;

end.
