unit TsBot.core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBot.Config, Logger, TsLib.Types, TsLib.Connection,
  TsLib.NotificationManager, TsBotUI.Types, syncobjs, TsLib.Server, gvector,
  fgl, DOM;

type
  TTBCore = class(TThread);

  { TTBCore }

  TBotModule = class
  protected
    // enable/disable Module
    function GetEnabled: Boolean; virtual; abstract;
    procedure SetEnabled(AValue: Boolean); virtual; abstract;
    // Returns the name of the module
    function GetName: String; virtual; abstract;
  public
    constructor Create(Core: TTBCore); virtual; abstract;

    // Called after the serverdata is collected
    procedure InitModule; virtual; abstract;
    // called before the cleanup
    procedure DoneModule; virtual; abstract;

    // for modules that want to use the config
    procedure ReadConfig(doc: TXMLDocument); virtual; abstract;
    procedure WriteConfig(doc: TXMLDocument); virtual; abstract;

    function ConfigModule(Config: TStringList): Boolean; virtual; abstract;
    procedure GetConfigItems(Sl: TStringList); virtual; abstract;
    procedure GetConfig(SL: TStringList); virtual; abstract;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Name: String read GetName;
  end;

  TDataEvent = procedure (Sender: TObject; Data: IntPtr) of object;

  TScheduleInfo = record
    Time: Integer;
    Remaining: Integer;
    Code: TDataEvent;
    Data: IntPtr;
  end;

  TSchedules = specialize TVector<TScheduleInfo>;
  TModuleList = specialize TFPGObjectList<TBotModule>;

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
    Modules: TModuleList;
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
    procedure UpdateChannels(Sender: TObject; Data: IntPtr);
    procedure UpdateClients(Sender: TObject; Data: IntPtr);
    procedure UpdateServer(Sender: TObject; Data: IntPtr);
  public
    function FindModule(Name: String): TBotModule;
    procedure ClearSchedules;
    procedure RegisterSchedule(Time: Integer; Code: TDataEvent; Data: Integer=0);
    procedure RemoveSchedule(Code: TDataEvent; Data: Integer=0);
    procedure Restart;
    procedure RegisterCommand(C: TCommandEventData);
    constructor Create(AOnTerminate: TNotifyEvent; ConfPath: string;
      AAutoRestart: boolean; AIdleToTerminate: boolean = True);
    destructor Destroy; override;
    property Config: TConfig read GetConfig;
    property AutoRestart: boolean read FAutoRestart write SetAutoRestart;
    property IdleToTerminate: boolean read FIdleToTerminate write FIdleToTerminate;
    property Connection: TTsConnection read FConnection;
    property Server: TTsServer read FServer;
    property NotificationManager: TNotificationManager read FNotificationManager;
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
  str: string;
  i: Integer;
  m: TBotModule;
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
        ctGetAntiflood:
        begin
          EnterCriticalsection(ConfigCS);
          try
            TStringList(c.Data).Add(Format('%d commands per %d seconds', [FConfig.FloodCommands, FConfig.FloodTime]));
            s := True
          finally
            LeaveCriticalsection(ConfigCS);
          end;
        end;
        ctSetAntiflood:
          try
            EnterCriticalsection(ConfigCS);
            FConfig.FloodCommands := PAntiFloodInfo(c.Data)^.Commands;
            FConfig.FloodTime := PAntiFloodInfo(c.Data)^.Time;
            FConnection.FloodControl(FConfig.FloodCommands, FConfig.FloodTime);
            s := True;
          finally
            LeaveCriticalsection(ConfigCS);
            Dispose(PAntiFloodInfo(c.Data));
          end;
        ctModuleList:
        begin
          for i:=0 to Modules.Count-1 do
            TStringList(c.Data).Add(Modules[i].Name);
          s := True;
        end;
        ctGetModuleConfig:
        begin
          m:=FindModule(TStringList(c.Data)[0]);
          TStringList(c.Data).Clear;
          s:=Assigned(m);
          if s then
            m.GetConfig(TStringList(c.Data))
          else
            WriteError(1295, 'Module not found');
        end;
        ctEnableModule:
        try
          m:=FindModule(PString(c.Data)^);
          s:=Assigned(m);
          if s then
            m.Enabled:=True
          else
            WriteError(1295, 'Module not found');
        finally
          Dispose(PString(c.Data));
        end;
        ctDisableModule:
        try
          m:=FindModule(PString(c.Data)^);
          s:=Assigned(m);
          if s then
            m.Enabled:=False
          else
            WriteError(1295, 'Module not found');
        finally
          Dispose(PString(c.Data));
        end;
        ctGetModuleConfData:
        begin
          str:=TStringList(c.Data)[0];
          m:=FindModule(str);
          TStringList(c.Data).Clear;
          s:=Assigned(m);
          if s then
          begin
            m.GetConfigItems(TStringList(c.Data));
            TStringList(c.Data).Insert(0, str);
          end
          else
            WriteError(1295, 'Module not found');
        end;
        ctConfigModule:
        begin
          m:=FindModule(TStringList(c.Data)[0]);
          TStringList(c.Data).Delete(0);
          s:=Assigned(m);
          if s then
            s:=m.ConfigModule(TStringList(c.Data))
          else
            WriteError(1295, 'Module not found');
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
          s.Code(Self, s.Data);
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
    FConnection.FloodControl(FConfig.FloodCommands, FConfig.FloodTime);
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
    FreeAndNil(FNotificationManager);
    FConnection.LogOut;
    FConnection.Disconnect();
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
    WriteStatus('Requesting clientlist');
    FServer.UpdateClientList(FullClientUpdate);

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

procedure TTBCore.UpdateChannels(Sender: TObject; Data: IntPtr);
begin
  FServer.UpdateChannelList;
end;

procedure TTBCore.UpdateClients(Sender: TObject; Data: IntPtr);
begin
  FServer.UpdateClientList;
end;

procedure TTBCore.UpdateServer(Sender: TObject; Data: IntPtr);
begin
  FServer.UpdateServerData;
end;

function TTBCore.FindModule(Name: String): TBotModule;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Modules.Count-1 do
    if Modules[i].Name=Name then
    begin
      Result:=Modules[i];
      break;
    end;
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

procedure TTBCore.RegisterSchedule(Time: Integer; Code: TDataEvent;
  Data: Integer);
var s: TScheduleInfo;
begin
  EnterCriticalsection(ScheduleCS);
  try
    s.Code:=Code;
    s.Time:=Time;
    s.Remaining:=Time;
    s.Data:=Data;
    FSchedules.PushBack(s);
  finally
    LeaveCriticalsection(ScheduleCS);
  end;
end;

procedure TTBCore.RemoveSchedule(Code: TDataEvent; Data: Integer);
var
  i: Integer;
  s: TScheduleInfo;
begin
  EnterCriticalsection(ScheduleCS);
  try
    for i:=0 to FSchedules.Size-1 do
    begin
      s:=FSchedules[i];
      if (s.Code = Code) and (s.Data = Data) then
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
  Modules:=TModuleList.Create(True);
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
  Modules.Free;
  inherited Destroy;
end;

end.
