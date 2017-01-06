unit tb.Core;

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
  end;

  { TLogger }

  TLogger = class
  private
    FLog: TextFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteError(ErrNo: integer; Err: string);
    procedure WriteHint(Hint: string);
    procedure WriteWarning(Warning: string);
    procedure WriteStatus(Status: string);
  end;

  { TTBCore }

  TTBCore = class(TThread)
  private
    FLogger: TLogger;
    FConfig: TConfig;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Execute; override;
    procedure ReadConfig(Path: string);
    procedure WriteConfig(Path: string);
  public
    constructor Create(AOnTerminate: TNotifyEvent);
    destructor Destroy; override;
  end;

const
  SLogPath = './run.log';
  SConfPath = './.tbconf';
  DefaultConf: TConfig = (Username: 'User'; Password: 'Pass';
    IPAddress: '127.0.0.1'; Port: 10011);
  ConfigVersion = 1;

implementation

{ TLogger }

constructor TLogger.Create;
begin
  {$IfDef Debug}
  FLog:=StdOut;
  {$Else}
  AssignFile(FLog, SLogPath);
  Append(FLog);
  {$EndIf}
  WriteLn(FLog, '--------------Service Start--------------');
end;

destructor TLogger.Destroy;
begin
  WriteLn(FLog, '--------------End of Log--------------');
  {$IfNDef Debug}
  CloseFile(FLog);
  {$EndIf}
  inherited Destroy;
end;

procedure TLogger.WriteError(ErrNo: integer; Err: string);
begin
  WriteLn(FLog, Format('[Error No%d][%s]%s', [ErrNo, DateTimeToStr(Now), Err]));
  Flush(FLog);
end;

procedure TLogger.WriteHint(Hint: string);
begin
  WriteLn(FLog, Format('[Hint][%s]%s', [DateTimeToStr(Now), Hint]));
  Flush(FLog);
end;

procedure TLogger.WriteWarning(Warning: string);
begin
  WriteLn(FLog, Format('[Warning][%s]%s', [DateTimeToStr(Now), Warning]));
  Flush(FLog);
end;

procedure TLogger.WriteStatus(Status: string);
begin
  WriteLn(FLog, Format('[%s]%s', [DateTimeToStr(Now), Status]));
  Flush(FLog);
end;

{ TTBCore }

procedure TTBCore.Execute;
begin
  ReadConfig(SConfPath);
  { TODO : Thread Loop }
  WriteConfig(SConfPath);
end;

procedure TTBCore.ReadConfig(Path: string);

  procedure ReadLoginData(Doc: TXMLDocument);
  var
    Node: TDOMElement;
  begin
    Node := Doc.DocumentElement.FindNode('Login') as TDOMElement;
    if not Assigned(Node) then
      Exit;
    FConfig.IPAddress := AnsiString(Node.AttribStrings['IP']);
    FConfig.Port := StrToInt(Node.AttribStrings['Port']);

    Node := Node.FindNode('User') as TDOMElement;
    if not Assigned(Node) then
      Exit;
    FConfig.Username := Node.AttribStrings['User'];
    FConfig.Password := Node.AttribStrings['Pass'];
  end;

var
  doc: TXMLDocument;
begin
  FConfig := DefaultConf;
  if not FileExists(SConfPath) then
    exit;
  ReadXMLFile(doc, SConfPath);
  try
    ReadLoginData(doc);
  finally
    doc.Free;
  end;
end;

procedure TTBCore.WriteConfig(Path: string);
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
    TDOMElement(ParentNode).SetAttribute('IP', FConfig.IPAddress);
    TDOMElement(ParentNode).SetAttribute('Port', IntToStr(FConfig.Port));

    // Adding Userdata
    DataNode := doc.CreateElement('User');
    ParentNode.AppendChild(DataNode);
    // Adding Userdata Attributes
    TDOMElement(DataNode).SetAttribute('User', FConfig.Username);
    TDOMElement(DataNode).SetAttribute('Pass', FConfig.Password);

    WriteXMLFile(doc, SConfPath);
  finally
    doc.Free;
  end;
end;

constructor TTBCore.Create(AOnTerminate: TNotifyEvent);
begin
  FreeOnTerminate := True;
  OnTerminate := AOnTerminate;
  FLogger := TLogger.Create;

  inherited Create(False);
end;

destructor TTBCore.Destroy;
begin
  WriteConfig(SConfPath);
  FLogger.Free;
  inherited Destroy;
end;

end.
