unit TsBot.core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.Config, Logger, TsLib.Types, TsLib.Connection,
  TsLib.NotificationManager;

type

  { TTBCore }

  TTBCore = class(TThread)
  private
    FConfigPath: string;
    FConfig: TConfig;
    FConnection: TTsConnection;
    procedure SleepAndCheck(Time: integer; Step: integer = 100);
    { Private declarations }
  protected
    { Protected declarations }
    function SetUp: boolean;
    procedure CleanUp;
    procedure Run;
    procedure Execute; override;
  public
    constructor Create(AOnTerminate: TNotifyEvent; ConfPath: string);
    destructor Destroy; override;
    property Config: TConfig read FConfig;
  end;

implementation

{ TTBCore }

procedure TTBCore.SleepAndCheck(Time: integer; Step: integer);
begin
  while (Time > 0) and not Terminated do
  begin
    Sleep(Step);
    CheckSynchronize();
    Dec(Time, Step);
  end;
end;

function TTBCore.SetUp: boolean;
begin
  FConnection := TTsConnection.Create(FConfig.IPAddress, FConfig.Port);
    with FConnection do
      Result := (Connect()
                and LogIn(FConfig.Username, FConfig.Password)
                and SwitchServer(FConfig.ServerID));
end;

procedure TTBCore.CleanUp;
begin
  if Assigned(FConnection) then
  begin
    FConnection.LogOut;
    FConnection.Disconnect();
    FreeAndNil(FConnection);
  end;
end;

procedure TTBCore.Run;
begin
  while not Terminated do
    Sleep(100);
  { TODO : Implement Something here }
end;

procedure TTBCore.Execute;
begin
  FConfig := ReadConfig(FConfigPath);
  // Init Log
  {$IfDef DEBUG}
  CreateDebugLogger;
  {$Else}
  if Length(FConfig.LogPath)>0 then
    CreateFileLogger(FConfig.LogPath);
  {$EndIf}
  try
    // While the thread is active
    while not Terminated do
    begin
      // Setup Bot
      try
        if SetUp then
        begin
          try
            // Start run
            Run;
          except
            { TODO : Errorhandling }
          end;
        end
        else
        begin
          // SOmething went wrong
        end;
      finally
        // Deinitialize Bot
        CleanUp;
      end;
      {$IfDef DEBUG}
      Terminate;
      {$EndIf}
    end;
  finally
    WriteConfig(FConfigPath, FConfig);
  end;
end;

constructor TTBCore.Create(AOnTerminate: TNotifyEvent; ConfPath: string);
begin
  FConfigPath := ConfPath;
  FreeOnTerminate := True;
  OnTerminate := AOnTerminate;

  inherited Create(False);
end;

destructor TTBCore.Destroy;
begin
  CleanUp;
  DestroyLogger;
  inherited Destroy;
end;

end.
