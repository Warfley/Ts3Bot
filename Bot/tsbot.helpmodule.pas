unit TsBot.HelpModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.core, TsLib.connection, TsLib.NotificationManager,
  TsLib.Types, TsLib.Server, TsBot.Utils, DOM, Logger;

type

  { THelpModule }

  THelpModule = class(TBotModule)
  private
    FCore: TTBCore;
    FEnabled: Boolean;
  protected
    procedure SendHelp(Sender: TObject; Data: IntPtr);
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

implementation

{ THelpModule }

procedure THelpModule.SendHelp(Sender: TObject; Data: IntPtr);
var sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to FCore.ModuleCount-1 do
      sl.Add(FCore.Module[i].GetHelp);
    FCore.Server.SendPrivateMessage(sl.Text, Data);
  finally
    sl.Free;
  end;
end;

function THelpModule.GetEnabled: boolean;
begin
  Result:=FEnabled;
end;

procedure THelpModule.SetEnabled(AValue: boolean);
begin
  FEnabled:=AValue;
end;

function THelpModule.GetName: string;
begin
  Result:='help';
end;

procedure THelpModule.TextMessage(Sender: TObject; AData: TTextNotification);
begin
  if FEnabled and (Trim(AData.Message)='!help') then
    FCore.RegisterSchedule(0, @SendHelp, AData.Invoker.ID, True);
end;

constructor THelpModule.Create(Core: TTBCore);
begin
  FCore:=Core;
  FEnabled:=True;
end;

destructor THelpModule.Destroy;
begin
  inherited Destroy;
end;

procedure THelpModule.InitModule;
begin
  FCore.NotificationManager.RegisterText(@TextMessage);
end;

procedure THelpModule.DoneModule;
begin
  FCore.NotificationManager.UnregisterNotification(ntTextMessage, TMethod(@TextMessage));
end;

procedure THelpModule.ReadConfig(doc: TXMLDocument);
var n: TDOMElement;
  attr: WideString;
begin
  FEnabled:=True;
  n := Doc.DocumentElement.FindNode('Help') as TDOMElement;
  if Assigned(n) then
  begin
  attr := n.AttribStrings['Enabled'];
  if Length(attr) > 0 then
    FEnabled := attr = '1';
  end;
end;

procedure THelpModule.WriteConfig(doc: TXMLDocument);
var n: TDOMElement;
begin
  n := Doc.CreateElement('AFK');
  doc.DocumentElement.AppendChild(n);

  n.AttribStrings['Enabled'] := BoolToStr(FEnabled, '1', '0');
end;

function THelpModule.GetHelp: string;
begin
  Result:='!help: shows the help for all modules';
end;

function THelpModule.ConfigModule(Config: TStringList): boolean;
begin
  //noop
end;

procedure THelpModule.GetConfigItems(Sl: TStringList);
begin
  //noop
end;

procedure THelpModule.GetConfig(SL: TStringList);
begin
  //noop
end;

end.

