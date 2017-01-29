unit TsBot.core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.Config, Logger, TsLib.Types, TsLib.Connection,
  TsLib.NotificationManager, TsBotUI.Types, syncobjs, TsLib.Server;

type

  { TTBCore }

  TTBCore = class(TThread)
  private
    FConfigPath: string;
    FConfig: TConfig;
    FConnection: TTsConnection;
    FCommandList: TCommandList;
    FNotificationManager: TNotificationManager;
    DoRestart: boolean;
    FAutoRestart: boolean;
    CommandCS, ConfigCS: TRTLCriticalSection;
    FIdleToTerminate: boolean;
    function GetConfig: TConfig;
    procedure SetAutoRestart(AValue: boolean);
    procedure SleepAndCheck(Time: integer; Step: integer = 100);
    procedure RunCommands;
    { Private declarations }
  protected
    { Protected declarations }
    function SetUp: boolean;
    procedure CleanUp;
    procedure Run;
    procedure Execute; override;
  public
    procedure Restart;
    procedure RegisterCommand(C: TCommandEventData);
    constructor Create(AOnTerminate: TNotifyEvent; ConfPath: string;
      AAutoRestart: boolean; AIdleToTerminate: boolean = True);
    destructor Destroy; override;
    property Config: TConfig read GetConfig;
    property AutoRestart: boolean read FAutoRestart write SetAutoRestart;
    property IdleToTerminate: boolean read FIdleToTerminate write FIdleToTerminate;
  end;

implementation

{ TTBCore }

procedure TTBCore.SleepAndCheck(Time: integer; Step: integer);
begin
  while (Time > 0) and not Terminated do
  begin
    Sleep(Step);
    Dec(Time, Step);
  end;
end;

procedure TTBCore.SetAutoRestart(AValue: boolean);
begin
  if FAutoRestart = AValue then
    Exit;
  EnterCriticalsection(CommandCS);
  try
    FAutoRestart := AValue;
  finally
    LeaveCriticalsection(CommandCS);
  end;
end;

function TTBCore.GetConfig: TConfig;
begin
  // Check if currently accessed, if so wait till it is free
  EnterCriticalsection(ConfigCS);
  try
    Result:=FConfig;
  finally
    LeaveCriticalsection(ConfigCS);
  end;
end;

procedure TTBCore.RunCommands;
function ifThen(Cond: Boolean; const TStr, FStr: String): String;
begin
  if Cond then
    Result:=TStr
  else
    Result:=FStr;
end;

var
  c: TCommandEventData;
  s: boolean;
begin
  EnterCriticalsection(CommandCS);
  try
    while not FCommandList.IsEmpty do
    begin
      c := FCommandList.Back;
      case c.CommandType of
        ctQuit:
        begin
          s := not Terminated;
          Terminate;
        end;
        ctRestart:
        begin
          DoRestart := True;
          s := True;
        end;
        ctSetConnectionData:
          try
            EnterCriticalsection(ConfigCS);
            with PConnectionData(c.Data)^ do
            begin
              FConfig.ServerID := ServerID;
              FConfig.IPAddress := IP;
              FConfig.Port := Port;
              FConfig.Username := UserName;
              FConfig.Password := Password;
            end;
            DoRestart := True;
            s := True;
          finally
            LeaveCriticalsection(ConfigCS);
            Dispose(PConnectionData(c.Data));
          end;
        ctSwitchServer:
        begin
          EnterCriticalsection(ConfigCS);
          try
            FConfig.ServerID := c.Data;
            if Assigned(FConnection) then
              s := FConnection.SwitchServer(c.Data)
            else
              DoRestart := True;
          finally
            LeaveCriticalsection(ConfigCS);
          end;
        end;
        ctGetConnectionData:
        begin
          EnterCriticalsection(ConfigCS);
          try
            with TStringList(c.Data) do
            begin
              Add(Format('IP Address: %s', [FConfig.IPAddress]));
              Add(Format('Port: %d', [FConfig.Port]));
              Add(Format('Server ID: %d', [FConfig.ServerID]));
              Add(Format('Username: %s', [FConfig.Username]));
              Add(Format('Password: %s', [FConfig.Password]));
              Add(Format('Status: %s', [ifThen(Assigned(FConnection) And FConnection.Connected,'Connected', 'Not Connected')]));
            end;
            s:=True
          finally
            LeaveCriticalsection(ConfigCS);
          end;
        end;
        ctGetLogPath:
        begin
          EnterCriticalsection(ConfigCS);
          try
            TStringList(c.Data).Add(FConfig.LogPath);
            s:=True
          finally
            LeaveCriticalsection(ConfigCS);
          end;
        end;
        ctChangeLogPath:
          try
            EnterCriticalsection(ConfigCS);
            FConfig.LogPath := PString(c.Data)^;
            WriteHint('Changed Logpath to file: '+FConfig.LogPath);
            DestroyFileLogger;
            CreateFileLogger(FConfig.LogPath);
            s := True;
          finally
            LeaveCriticalsection(ConfigCS);
            Dispose(PString(c.Data));
          end;
        ctResetConfig:
        try
          EnterCriticalsection(ConfigCS);
          FConfig:=ReadConfig('');
          Restart;
          s:=True;
        finally
          LeaveCriticalsection(ConfigCS);
        end;
      end;
      if Assigned(c.OnFinished) then
        c.OnFinished(Self, c.CommandType, s);
      FCommandList.PopBack;
    end;
  finally
    LeaveCriticalsection(CommandCS);
  end;
end;

function TTBCore.SetUp: boolean;
begin
  EnterCriticalsection(ConfigCS);
  try
    DoRestart := False;
    FConnection := TTsConnection.Create(FConfig.IPAddress, FConfig.Port);
    with FConnection do
      Result := (Connect() and LogIn(FConfig.Username,
        FConfig.Password) and SwitchServer(FConfig.ServerID));
    if Result then
      FNotificationManager := TNotificationManager.Create(FConnection);
  finally
    LeaveCriticalsection(ConfigCS);
  end;
end;

procedure TTBCore.CleanUp;
begin
  if Assigned(FConnection) then
  begin
    FConnection.LogOut;
    FConnection.Disconnect();
    FreeAndNil(FNotificationManager);
    FreeAndNil(FConnection);
  end;
end;

procedure TTBCore.Run;
var
  FServer: TTsServer;
begin
  FServer:=TTsServer.Create(FConnection, FNotificationManager);
  try
    WriteStatus('Requesting serverdata');
    FServer.UpdateServerData;

    while not (Terminated or DoRestart) do
    begin
      Sleep(100);
      RunCommands;
    end;
    { TODO : Implement Something here }
  finally
    FServer.Free;
  end;
end;

procedure TTBCore.Execute;
begin
  EnterCriticalsection(ConfigCS);
  try
    FConfig := ReadConfig(FConfigPath);
  finally
    LeaveCriticalsection(ConfigCS);
  end;
  // Init Log
  if Length(FConfig.LogPath) > 0 then
    CreateFileLogger(FConfig.LogPath);
  try
    // While the thread is active
    while not Terminated do
    begin
      if FAutoRestart or DoRestart then
      begin
        // Setup Bot
        try
          if SetUp then
          begin
            try
              // Start run
              Run;
            except
              on e: Exception do
              begin
                WriteError(-1, e.Message);
                WriteStatus('Restarting...');
                DoRestart:=True;
              end;
            end;
          end
          else
          begin
            // Something went wrong
          end;
        finally
          // Deinitialize Bot
          CleanUp;
        end;

      end
      else if FIdleToTerminate then
      begin
        RunCommands;
        Sleep(100);
      end;
    end;
  finally
    EnterCriticalsection(ConfigCS);
    try
      WriteConfig(FConfigPath, FConfig);
    finally
      LeaveCriticalsection(ConfigCS);
    end;
  end;
end;

procedure TTBCore.Restart;
begin
  EnterCriticalsection(CommandCS);
  try
    DoRestart := True;
  finally
    LeaveCriticalsection(CommandCS);
  end;
end;

procedure TTBCore.RegisterCommand(C: TCommandEventData);
begin
  EnterCriticalsection(CommandCS);
  try
    FCommandList.PushBack(C);
  finally
    LeaveCriticalsection(CommandCS);
  end;
end;

constructor TTBCore.Create(AOnTerminate: TNotifyEvent; ConfPath: string;
  AAutoRestart: boolean; AIdleToTerminate: boolean);
begin
  FAutoRestart := AAutoRestart;
  FConfigPath := ConfPath;
  FreeOnTerminate := True;
  OnTerminate := AOnTerminate;
  InitCriticalSection(CommandCS);
  InitCriticalSection(ConfigCS);
  DoRestart := True;
  FCommandList := TCommandList.Create;
  FIdleToTerminate := AIdleToTerminate;

  inherited Create(False);
end;

destructor TTBCore.Destroy;
begin
  CleanUp;
  DestroyFileLogger;
  DoneCriticalsection(CommandCS);
  DoneCriticalsection(ConfigCS);
  FCommandList.Free;
  inherited Destroy;
end;

end.
