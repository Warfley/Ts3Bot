unit tb.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tb.Config, Logger, ts.Connection;

type

  { TTBCore }

  TTBCore = class(TThread)
  private
    FConfig: TConfig;
    FConnection: TTsConnection;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    constructor Create(AOnTerminate: TNotifyEvent);
    destructor Destroy; override;
  end;

const
  SLogPath = './run.log';
  SConfPath = './.tbconf';

implementation

{ TTBCore }

procedure TTBCore.Execute;
begin
  FConfig := ReadConfig(SConfPath);
  FConnection:=TTsConnection.Create(FConfig.IPAddress, FConfig.Port);
  try
    FConnection.Connect();
    Sleep(100);
    if FConnection.LogIn(FConfig.Username, FConfig.Password) then
    begin
    Sleep(100);
    FConnection.LogOut;
    Sleep(100);
    end;
    FConnection.Disconnect();
    Sleep(100);
  finally
  WriteConfig(SConfPath, FConfig);
  end;
end;

constructor TTBCore.Create(AOnTerminate: TNotifyEvent);
begin
  FreeOnTerminate := True;
  OnTerminate := AOnTerminate;
  {$IfDef DEBUG}
  CreateDebugLogger;
  {$Else}
  CreateFileLogger(SLogPath);
  {$EndIf}

  inherited Create(False);
end;

destructor TTBCore.Destroy;
begin
  DestroyLogger;
  inherited Destroy;
end;

end.
