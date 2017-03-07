unit TsBot.ControlModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.core, TsBotUI.Types, gvector,
  TsLib.Types, TsLib.Server, TsBot.Utils, DOM, Logger;

type
  TLastCommand = record
    UserUID: String;
    Command: TCommandType;
  end;

  TLastCommandList = specialize TVector<TLastCommand>;

  { TBotControlModule }

  TBotControlModule = class(TBotModule)
  private
    FEnabled: Boolean;
    FLastCommands: TLastCommandList;
    FRequiredServerGroups: TStringList;
  protected
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

{ TBotControlModule }

function TBotControlModule.GetEnabled: boolean;
begin

end;

procedure TBotControlModule.SetEnabled(AValue: boolean);
begin

end;

function TBotControlModule.GetName: string;
begin

end;

procedure TBotControlModule.TextMessage(Sender: TObject;
  AData: TTextNotification);
begin

end;

constructor TBotControlModule.Create(Core: TTBCore);
begin
  inherited Create(Core);
end;

destructor TBotControlModule.Destroy;
begin
  inherited Destroy;
end;

procedure TBotControlModule.InitModule;
begin

end;

procedure TBotControlModule.DoneModule;
begin

end;

procedure TBotControlModule.ReadConfig(doc: TXMLDocument);
begin

end;

procedure TBotControlModule.WriteConfig(doc: TXMLDocument);
begin

end;

function TBotControlModule.GetHelp: string;
begin

end;

function TBotControlModule.ConfigModule(Config: TStringList): boolean;
begin

end;

procedure TBotControlModule.GetConfigItems(Sl: TStringList);
begin

end;

procedure TBotControlModule.GetConfig(SL: TStringList);
begin

end;

end.

