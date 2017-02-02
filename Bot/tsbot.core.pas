unit TsBot.core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.Config, Logger, TsLib.Types, TsLib.Connection,
  TsLib.NotificationManager, TsBotUI.Types, syncobjs, TsLib.Server, gvector;

type

  { TTBCore }

  TScheduleInfo = record
    Time: Integer;
    Remaining: Integer;
    Code: TNotifyEvent;
  end;

  TSchedules = specialize TVector<TScheduleInfo>;

  TTBCore = class(TThread)
  private
    FConfigPath: string;
    FConfig: TConfig;
    FConnection: TTsConnection;
    FCommandList: TCommandList;
    FNotificationManager: TNotificationManager;
    DoRestart: boolean;
    FAutoRestart: boolean;
    CommandCS, ConfigCS, ScheduleCS: TRTLCriticalSection;
    FIdleToTerminate: boolean;
    FSchedules: TSchedules;
    FServer: TTsServer;
    function GetConfig: TConfig;
    procedure SetAutoRestart(AValue: boolean);
    procedure SleepAndCheck(Time: integer; Step: integer = 100);
    procedure RunCommands;
    procedure RunSchedules(Time: QWord);
    { Private declarations }
  protected
    { Protected declarations }
    function SetUp: boolean;
    procedure CleanUp;
    procedure Run;
    procedure Execute; override;
    procedure UpdateChannels(Sender: TObject);
    procedure UpdateClients(Sender: TObject);
    procedure UpdateServer(Sender: TObject);
  public
    procedure ClearSchedules;
    procedure RegisterSchedule(Time: Integer; Code: TNotifyEvent);
    procedure RemoveSchedule(Code: TNotifyEvent);
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
    Result := FConfig;
  finally
    LeaveCriticalsection(ConfigCS);
  end;
end;

procedure TTBCore.RunCommands;

  function ifThen(Cond: boolean; const TStr, FStr: string): string;
  begin
    if Cond then
      Result := TStr
    else
      Result := FStr;
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
              Add(Format('Status: %s',
                [ifThen(Assigned(FConnection) and FConnection.Connected, 'Connected',
                'Not Connected')]));
            end;
            s := True
          finally
            LeaveCriticalsection(ConfigCS);
          end;
        end;
        ctGetLogPath:
        begin
          EnterCriticalsection(ConfigCS);
          try
            TStringList(c.Data).Add(FConfig.LogPath);
            s := True
          finally
            LeaveCriticalsection(ConfigCS);
          end;
        end;
        ctChangeLogPath:
          try
            EnterCriticalsection(ConfigCS);
            FConfig.LogPath := PString(c.Data)^;
            WriteHint('Changed Logpath to file: ' + FConfig.LogPath);
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
            FConfig := ReadConfig('');
            Restart;
            s := True;
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

procedure TTBCore.RunSchedules(Time: QWord);
var
  s: TScheduleInfo;
  i: Integer;
begin
  EnterCriticalsection(ScheduleCS);
  try
    for i:=0 to FSchedules.Size -1 do
    begin
      s:=FSchedules[i];
      s.Remaining := s.Remaining - Time;
      if s.Remaining<=0 then
      begin
        if Assigned(s.Code) then
          s.Code(Self);
        s.Remaining:=s.Time;
      end;
    end;
  finally
    LeaveCriticalsection(ScheduleCS);
  end;
end;

function TTBCore.SetUp: boolean;
begin
  EnterCriticalsection(ConfigCS);
  try
    DoRestart := False;
    FConnection := TTsConnection.Create(FConfig.IPAddress, FConfig.Port);
    with FConnection do
      Result := (Connect() and LogIn(FConfig.Username, FConfig.Password) and
        SwitchServer(FConfig.ServerID));
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
  LastTick, Diff: QWord;
begin
  FServer := TTsServer.Create(FConnection, FNotificationManager);
  try
    WriteStatus('Requesting serverdata');
    FServer.UpdateServerData;
    WriteStatus('Requesting channellist');
    FServer.UpdateChannelList(FullChannelUpdate);
    // Adding schedules
    if Config.UpdateServerData >= 0 then
      RegisterSchedule(Config.UpdateServerData, @UpdateServer);
    if Config.UpdateChannelList >= 0 then
      RegisterSchedule(Config.UpdateChannelList, @UpdateChannels);
    if Config.UpdateClientList >= 0 then
      RegisterSchedule(Config.UpdateClientList, @UpdateClients);

    // Set up notifications
    FServer.UseNotifications := True;
    FNotificationManager.Active := True;

    LastTick := GetTickCount64;

    while not (Terminated or DoRestart) do
    begin
      // TimeDifference
      Diff := GetTickCount64 - LastTick;
      LastTick := GetTickCount64;

      // Check schedules
      RunSchedules(Diff);
      FNotificationManager.SendNotifications;
      RunCommands;
      Sleep(100);
    end;
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
                DoRestart := True;
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

procedure TTBCore.UpdateChannels(Sender: TObject);
begin
  FServer.UpdateChannelList;
end;

procedure TTBCore.UpdateClients(Sender: TObject);
begin
  FServer.UpdateClientList;
end;

procedure TTBCore.UpdateServer(Sender: TObject);
begin
  FServer.UpdateServerData;
end;

procedure TTBCore.ClearSchedules;
begin
  EnterCriticalsection(ScheduleCS);
  try
    FSchedules.Clear;
  finally
    LeaveCriticalsection(ScheduleCS);
  end;
end;

procedure TTBCore.RegisterSchedule(Time: Integer; Code: TNotifyEvent);
var s: TScheduleInfo;
begin
  EnterCriticalsection(ScheduleCS);
  try
    s.Code:=Code;
    s.Time:=Time;
    s.Remaining:=Time;
    FSchedules.PushBack(s);
  finally
    LeaveCriticalsection(ScheduleCS);
  end;
end;

procedure TTBCore.RemoveSchedule(Code: TNotifyEvent);
var
  i: Integer;
  s: TScheduleInfo;
begin
  EnterCriticalsection(ScheduleCS);
  try
    for i:=0 to FSchedules.Size-1 do
    begin
      s:=FSchedules[i];
      if s.Code = Code then
      begin
        FSchedules.Erase(i);
        break;
      end;
    end;
  finally
    LeaveCriticalsection(ScheduleCS);
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
  InitCriticalSection(ScheduleCS);
  DoRestart := True;
  FCommandList := TCommandList.Create;
  FIdleToTerminate := AIdleToTerminate;
  FSchedules:=TSchedules.Create;
  inherited Create(False);
end;

destructor TTBCore.Destroy;
begin
  CleanUp;
  DestroyFileLogger;
  DoneCriticalsection(CommandCS);
  DoneCriticalsection(ConfigCS);
  DoneCriticalsection(ScheduleCS);
  FSchedules.Free;
  FCommandList.Free;
  inherited Destroy;
end;

end.
