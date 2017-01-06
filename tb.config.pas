unit tb.Config;

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
  end;

const
  DefaultConf: TConfig = (Username: 'User'; Password: 'Pass';
    IPAddress: '127.0.0.1'; Port: 10011);
  ConfigVersion = 1;

function ReadConfig(Path: String): TConfig;
procedure WriteConfig(Path: String; Config: TConfig);
implementation

uses DOM, XMLRead, XMLWrite;

function ReadConfig(Path: String): TConfig;

  procedure ReadLoginData(Doc: TXMLDocument);
  var
    Node: TDOMElement;
  begin
    Node := Doc.DocumentElement.FindNode('Login') as TDOMElement;
    if not Assigned(Node) then
      Exit;
    Result.IPAddress := AnsiString(Node.AttribStrings['IP']);
    Result.Port := StrToInt(Node.AttribStrings['Port']);

    Node := Node.FindNode('User') as TDOMElement;
    if not Assigned(Node) then
      Exit;
    Result.Username := Node.AttribStrings['User'];
    Result.Password := Node.AttribStrings['Pass'];
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

procedure WriteConfig(Path: String; Config: TConfig);
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

