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

end.

