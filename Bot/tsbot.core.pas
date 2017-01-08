unit TsBot.core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.Config, Logger, TsLib.Connection;

type

  { TTBCore }

  TTBCore = class(TThread)
  private
    FConfigPath: string;
    FConfig: TConfig;
    FConnection: TTsConnection;
    procedure SleepAndCheck(Time: Integer; Step: Integer = 100);
    { Private declarations }
  protected
    { Protected declarations }
    function SetUp: Boolean;
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

procedure TTBCore.SleepAndCheck(Time: Integer; Step: Integer);
begin
  while (Time>0) and not Terminated do
  begin
    Sleep(Step);
    Dec(Time, Step);
  end;
end;

function TTBCore.SetUp: Boolean;
begin
  FConnection := TTsConnection.Create(FConfig.IPAddress, FConfig.Port);
  with FConnection do
    Result:= not (Connect()
             and LogIn(FConfig.Username, FConfig.Password)
             and SwitchServer(FConfig.ServerID)
             );
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
      if not SetUp then
      begin
        //Something went wrong
        SleepAndCheck(2000);
        Continue;
      end;
      try
        try
          // Start run
          Run;
        Except
          { TODO : Errorhandling }
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
