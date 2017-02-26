unit TsBot.core;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Logger,
  TsBot.Config,
  TsBot.Utils,
  TsLib.Types,
  TsLib.Connection,
  TsLib.NotificationManager,
  TsLib.Server,
  TsBotUI.Types,
  syncobjs,
  gvector,
  fgl,
  DOM;

type
  TTBCore = class;

  PUserMessageInfo = ^TUserMessageInfo;
  TUserMessageInfo = record
    User: Integer;
    Message: String;
    Server: TTsServer;
  end;

  { TBotModule }

  TBotModule = class
  private
    procedure DoSendPrivate(Sender: TObject; Data: IntPtr);
  protected
    FCore: TTBCore;
    // enable/disable Module
    function GetEnabled: boolean; virtual; abstract;
    procedure SetEnabled(AValue: boolean); virtual; abstract;
    // Returns the name of the module
    function GetName: string; virtual; abstract;
    procedure SendPrivateMessage(Server: TTsServer; Message: String; Client: Integer); virtual;
  public
    constructor Create(Core: TTBCore); virtual;

    // Called after the serverdata is collected
    procedure InitModule; virtual; abstract;
    // called before the cleanup
    procedure DoneModule; virtual; abstract;

    // for modules that want to use the config
    procedure ReadConfig(doc: TXMLDocument); virtual; abstract;
    procedure WriteConfig(doc: TXMLDocument); virtual; abstract;

    function ConfigModule(Config: TStringList): boolean; virtual; abstract;
    procedure GetConfigItems(Sl: TStringList); virtual; abstract;
    procedure GetConfig(SL: TStringList); virtual; abstract;

    function GetHelp: string; virtual; abstract;

    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName;
  end;

  TDataEvent = procedure(Sender: TObject; Data: IntPtr) of object;

  TScheduleInfo = record
    Time: int64;
    Remaining: int64;
    Code: TDataEvent;
    Data: IntPtr;
    Once: Boolean;
  end;

  TSchedules = specialize TVector<TScheduleInfo>;
  TModuleList = specialize TFPGObjectList<TBotModule>;

  TRegisterModuleEvent = procedure(Sender: TObject; ModuleList: TModuleList) of object;

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
    ClientUpdateEvents: TClientUpdateEventList;
    ServerUpdateEvents: TServerUpdateEventList;
    ChannelUpdateEvents: TChannelUpdateEventList;
    ClientMoveEvents: TClientMoveEventList;
    ClientConnectEvents: TClientConnectedEventList;
    ClientDisconnectEvents: TClientDisconnectedEventList;
    FClientUpdate: TClientUpdates;
    FChannelUpdate: TChannelUpdates;
    function GetConfig: TConfig;
    function GetModule(Index: Integer): TBotModule;
    function GetModuleCount: Integer;
    procedure SetAutoRestart(AValue: boolean);
    procedure SleepAndCheck(Time: integer; Step: integer = 100);
    procedure RunCommands;
    procedure RunSchedules(Time: int64);
    { Private declarations }
  protected
    procedure ChannelUpdated(Sender: TObject; Channel: TTsChannel);
    procedure ClientConnected(Sender: TObject; Client: TTsClient);
    procedure ClientDisconnected(Sender: TObject; Client: TTsClient);
    procedure ClientMoved(Sender: TObject; Client: TTsClient;
      Source, Target: TTsChannel);
    procedure ClientUpdated(Sender: TObject; Client: TTsClient);
    procedure ScheduleRestart(Sender: TObject; Data: IntPtr);
    procedure ServerUpdated(Sender: TObject);
    { Protected declarations }
    function SetUp: boolean;
    procedure CleanUp;
    procedure Run;
    procedure Execute; override;
    procedure UpdateChannelGroups(Sender: TObject; Data: IntPtr);
    procedure UpdateChannels(Sender: TObject; Data: IntPtr);
    procedure UpdateClients(Sender: TObject; Data: IntPtr);
    procedure UpdateServer(Sender: TObject; Data: IntPtr);
    procedure SaveConfig(Sender: TObject; Data: IntPtr);
    procedure RegisterModules; virtual;
    procedure InitModules; virtual;
    procedure DoneModules; virtual;
    procedure LoadModuleConfig(Doc: TXMLDocument);
    procedure SaveModuleConfig(Doc: TXMLDocument);
    procedure UpdateServerGroups(Sender: TObject; Data: IntPtr);
  public
    function FindModule(Name: string): TBotModule;
    procedure ClearSchedules;
    procedure RegisterSchedule(Time: int64; Code: TDataEvent; Data: IntPtr = 0; Once: Boolean = False);
    procedure RemoveSchedule(Code: TDataEvent; Data: integer = 0);
    procedure Restart;
    procedure RegisterCommand(C: TCommandEventData);
    constructor Create(AOnTerminate: TNotifyEvent; ConfPath: string;
      AAutoRestart: boolean; AIdleToTerminate: boolean = True);
    destructor Destroy; override;
    procedure RegisterClientUpdateEvent(Event: TClientUpdateEvent);
    procedure RegisterServerUpdateEvent(Event: TServerUpdateEvent);
    procedure RegisterChannelUpdateEvent(Event: TChannelUpdateEvent);
    procedure RegisterClientConnectEvent(Event: TClientConnectedEvent);
    procedure RegisterClientDisconnectEvent(Event: TClientDisconnectedEvent);
    procedure RegisterClientMoveEvent(Event: TClientMoveEvent);
    procedure UnregisterUpdateEvent(Event: TClientUpdateEvent);
    procedure UnregisterUpdateEvent(Event: TServerUpdateEvent);
    procedure UnregisterUpdateEvent(Event: TChannelUpdateEvent);
    procedure UnregisterConnectedEvent(Event: TClientConnectedEvent);
    procedure UnregisterDisconnectedEvent(Event: TClientDisconnectedEvent);
    procedure UnregisterMoveEvent(Event: TClientMoveEvent);

    property ModuleCount: Integer read GetModuleCount;
    property Module[Index: Integer]: TBotModule read GetModule;
    property Config: TConfig read GetConfig;
    property AutoRestart: boolean read FAutoRestart write SetAutoRestart;
    property IdleToTerminate: boolean read FIdleToTerminate write FIdleToTerminate;
    property Connection: TTsConnection read FConnection;
    property Server: TTsServer read FServer;
    property NotificationManager: TNotificationManager read FNotificationManager;
    property ClientUpdate: TClientUpdates read FClientUpdate write FClientUpdate;
    property ChannelUpdate: TChannelUpdates read FChannelUpdate write FChannelUpdate;
  end;

implementation

uses TsBot.AfkModule, TsBot.AnounceModule, TsBot.HelpModule;

{ TBotModule }

procedure TBotModule.DoSendPrivate(Sender: TObject; Data: IntPtr);
var
  pmsg: PUserMessageInfo;
begin
  pmsg:=PUserMessageInfo(Data);
  pmsg^.Server.SendPrivateMessage(pmsg^.Message, pmsg^.User);
  Dispose(pmsg);
end;

procedure TBotModule.SendPrivateMessage(Server: TTsServer; Message: String;
  Client: Integer);
var
  pmsg: PUserMessageInfo;
begin
  new(pmsg);
  pmsg^.Message:=Message;
  pmsg^.Server:=Server;
  pmsg^.User:=Client;
  FCore.RegisterSchedule(0, @DoSendPrivate, IntPtr(pmsg), True);
end;

constructor TBotModule.Create(Core: TTBCore);
begin
  FCore:=Core;
end;

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

function TTBCore.GetModule(Index: Integer): TBotModule;
begin
  Result:=nil;
  if (Index<0) or (Index>=Modules.Count) then exit;
  Result:=Modules[Index];
end;

function TTBCore.GetModuleCount: Integer;
begin
  Result:=Modules.Count;
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
  i: integer;
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
                [ifThen(Assigned(FConnection) and FConnection.Connected,
                'Connected', 'Not Connected')]));
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
            TStringList(c.Data).Add(Format('%d commands per %d seconds',
              [FConfig.FloodCommands, FConfig.FloodTime]));
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
          for i := 0 to Modules.Count - 1 do
            TStringList(c.Data).Add(Modules[i].Name +
              ' Active: ' + BoolToStr(Modules[i].Enabled, True));
          s := True;
        end;
        ctGetModuleConfig:
        begin
          m := FindModule(TStringList(c.Data)[0]);
          TStringList(c.Data).Clear;
          s := Assigned(m);
          if s then
            m.GetConfig(TStringList(c.Data))
          else
            WriteError(1295, 'Module not found');
        end;
        ctEnableModule:
          try
            m := FindModule(PString(c.Data)^);
            s := Assigned(m);
            if s then
              m.Enabled := True
            else
              WriteError(1295, 'Module not found');
          finally
            Dispose(PString(c.Data));
          end;
        ctDisableModule:
          try
            m := FindModule(PString(c.Data)^);
            s := Assigned(m);
            if s then
              m.Enabled := False
            else
              WriteError(1295, 'Module not found');
          finally
            Dispose(PString(c.Data));
          end;
        ctGetModuleConfData:
        begin
          str := TStringList(c.Data)[0];
          m := FindModule(str);
          TStringList(c.Data).Clear;
          s := Assigned(m);
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
          m := FindModule(TStringList(c.Data)[0]);
          TStringList(c.Data).Delete(0);
          s := Assigned(m);
          if s then
            s := m.ConfigModule(TStringList(c.Data))
          else
            WriteError(1295, 'Module not found');
        end;
        ctResetConfig:
          try
            EnterCriticalsection(ConfigCS);
            FConfig := ReadConfig('', @LoadModuleConfig);
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

procedure TTBCore.RunSchedules(Time: int64);
var
  s: TScheduleInfo;
  i: integer;
begin
  EnterCriticalsection(ScheduleCS);
  try
    i:=0;
    While i < FSchedules.Size do
    begin
      s := FSchedules[i];
      s.Remaining := s.Remaining - Time;
      if s.Remaining <= 0 then
      begin
        if Assigned(s.Code) then
          s.Code(Self, s.Data);
        s.Remaining := s.Time;
        if s.Once then
        begin
          FSchedules.Erase(i);
          Continue;
        end;
      end;
      FSchedules[i] := s;
      inc(i);
    end;
  finally
    LeaveCriticalsection(ScheduleCS);
  end;
end;

procedure TTBCore.ChannelUpdated(Sender: TObject; Channel: TTsChannel);
var
  i: integer;
begin
  for i := 0 to ChannelUpdateEvents.Count - 1 do
    ChannelUpdateEvents[i](Sender, Channel);
end;

procedure TTBCore.ClientConnected(Sender: TObject; Client: TTsClient);
var
  i: integer;
begin
  for i := 0 to ClientConnectEvents.Count - 1 do
    ClientConnectEvents[i](Sender, Client);
end;

procedure TTBCore.ClientDisconnected(Sender: TObject; Client: TTsClient);
var
  i: integer;
begin
  for i := 0 to ClientDisconnectEvents.Count - 1 do
    ClientDisconnectEvents[i](Sender, Client);
end;

procedure TTBCore.ClientMoved(Sender: TObject; Client: TTsClient;
  Source, Target: TTsChannel);
var
  i: integer;
begin
  WriteStatus('Client '+Client.ClientData.Name+' moved');
  for i := 0 to ClientMoveEvents.Count - 1 do
    ClientMoveEvents[i](Sender, Client, Source, Target);
end;

procedure TTBCore.ClientUpdated(Sender: TObject; Client: TTsClient);
var
  i: integer;
begin
  for i := 0 to ClientUpdateEvents.Count - 1 do
    ClientUpdateEvents[i](Sender, Client);
end;

procedure TTBCore.ScheduleRestart(Sender: TObject; Data: IntPtr);
begin
  Restart;
end;

procedure TTBCore.ServerUpdated(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ServerUpdateEvents.Count - 1 do
    ServerUpdateEvents[i](Sender);
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
    EnterCriticalsection(ScheduleCS);
    try
      FSchedules.Clear;
    finally
      LeaveCriticalsection(ScheduleCS);
    end;
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
    WriteStatus('Requesting servergroups');
    FServer.UpdateServerGroups;
    WriteStatus('Requesting channelgroups');
    FServer.UpdateChannelGroups;
    FServer.OnChannelUpdate := @ChannelUpdated;
    FServer.OnClientUpdate := @ClientUpdated;
    FServer.OnUpdate := @ServerUpdated;
    FServer.OnClientMove := @ClientMoved;
    FServer.OnClientConnect := @ClientConnected;
    FServer.OnClientDisconnect := @ClientDisconnected;

    // Adding schedules
    if Config.UpdateServerData >= 0 then
      RegisterSchedule(Config.UpdateServerData, @UpdateServer);
    if Config.UpdateChannelList >= 0 then
      RegisterSchedule(Config.UpdateChannelList, @UpdateChannels);
    if Config.UpdateClientList >= 0 then
      RegisterSchedule(Config.UpdateClientList, @UpdateClients);
    if Config.UpdateServerGroups >= 0 then
      RegisterSchedule(Config.UpdateServerGroups, @UpdateServerGroups);
    if Config.UpdateChannelGroups >= 0 then
      RegisterSchedule(Config.UpdateChannelGroups, @UpdateChannelGroups);
    RegisterSchedule(Hour*2, @ScheduleRestart);

    // save config every 30 minutes
    RegisterSchedule(Minutes30, @SaveConfig);

    InitModules();

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

    DoneModules();
  finally
    FServer.Free;
  end;
end;

procedure TTBCore.Execute;
begin
  RegisterModules;
  EnterCriticalsection(ConfigCS);
  try
    FConfig := ReadConfig(FConfigPath, @LoadModuleConfig);
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
    SaveConfig(Self, 0);
  end;
end;

procedure TTBCore.UpdateChannelGroups(Sender: TObject; Data: IntPtr);
begin
  try
    WriteStatus('Updateing channelgroups');
    FServer.UpdateChannelGroups;
  except
    on E: EChannelDataException do ;
  end;
end;

procedure TTBCore.UpdateChannels(Sender: TObject; Data: IntPtr);
begin
  try
    WriteStatus('Updateing channellist');
    FServer.UpdateChannelList(FChannelUpdate);
  except
    on E: EChannelDataException do ;
  end;
end;

procedure TTBCore.UpdateClients(Sender: TObject; Data: IntPtr);
begin
  try
    WriteStatus('Updating clientlist');
    FServer.UpdateClientList(FClientUpdate);
  except
    on E: EClientDataException do ;
  end;
end;

procedure TTBCore.UpdateServer(Sender: TObject; Data: IntPtr);
begin
  try
    WriteStatus('Updating serverinfo');
    FServer.UpdateServerData;
  except
    on E: EServerDataException do ;
  end;
end;

procedure TTBCore.SaveConfig(Sender: TObject; Data: IntPtr);
begin
  EnterCriticalsection(ConfigCS);
  try
    try
      WriteConfig(FConfigPath, FConfig, @SaveModuleConfig);
      WriteStatus('Configuration successfully saved');

    except
      on e: Exception do
        WriteError(IConfigWriteError, e.Message);
    end;
  finally
    LeaveCriticalsection(ConfigCS);
  end;
end;

procedure TTBCore.RegisterModules;
begin
  Modules.Add(TAfkModule.Create(Self));
  Modules.Add(TAnnouncementModule.Create(Self));
  Modules.Add(THelpModule.Create(Self));
end;

procedure TTBCore.InitModules;
var
  i: integer;
begin
  for i := 0 to Modules.Count - 1 do
    Modules[i].InitModule;
end;

procedure TTBCore.DoneModules;
var
  i: integer;
begin
  for i := 0 to Modules.Count - 1 do
    Modules[i].DoneModule;
end;

procedure TTBCore.LoadModuleConfig(Doc: TXMLDocument);
var
  i: integer;
begin
  for i := 0 to Modules.Count - 1 do
    Modules[i].ReadConfig(Doc);
end;

procedure TTBCore.SaveModuleConfig(Doc: TXMLDocument);
var
  i: integer;
begin
  for i := 0 to Modules.Count - 1 do
    Modules[i].WriteConfig(Doc);
end;

procedure TTBCore.UpdateServerGroups(Sender: TObject; Data: IntPtr);
begin
  try
    WriteStatus('Updateing servergroups');
    FServer.UpdateServerGroups;
  except
    on E: EServerDataException do ;
  end;
end;

function TTBCore.FindModule(Name: string): TBotModule;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Modules.Count - 1 do
    if Modules[i].Name = Name then
    begin
      Result := Modules[i];
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

procedure TTBCore.RegisterSchedule(Time: int64; Code: TDataEvent; Data: IntPtr;
  Once: Boolean);
var
  s: TScheduleInfo;
begin
  EnterCriticalsection(ScheduleCS);
  try
    s.Code := Code;
    s.Time := Time;
    s.Remaining := Time;
    s.Data := Data;
    s.Once:=Once;
    FSchedules.PushBack(s);
  finally
    LeaveCriticalsection(ScheduleCS);
  end;
end;

procedure TTBCore.RemoveSchedule(Code: TDataEvent; Data: integer);
var
  i: integer;
  s: TScheduleInfo;
begin
  EnterCriticalsection(ScheduleCS);
  try
    for i := 0 to FSchedules.Size - 1 do
    begin
      s := FSchedules[i];
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
  FSchedules := TSchedules.Create;
  Modules := TModuleList.Create(True);
  ChannelUpdateEvents := TChannelUpdateEventList.Create;
  ClientUpdateEvents := TClientUpdateEventList.Create;
  ServerUpdateEvents := TServerUpdateEventList.Create;
  ClientMoveEvents := TClientMoveEventList.Create;
  ClientConnectEvents := TClientConnectedEventList.Create;
  ClientDisconnectEvents := TClientDisconnectedEventList.Create;
  FClientUpdate := [];
  FChannelUpdate := [];
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
  ChannelUpdateEvents.Free;
  ClientUpdateEvents.Free;
  ServerUpdateEvents.Free;
  ClientMoveEvents.Free;
  ClientConnectEvents.Free;
  ClientDisconnectEvents.Free;
  inherited Destroy;
end;

procedure TTBCore.RegisterClientUpdateEvent(Event: TClientUpdateEvent);
begin
  if ClientUpdateEvents.IndexOf(Event) < 0 then
    ClientUpdateEvents.Add(Event);
end;

procedure TTBCore.RegisterServerUpdateEvent(Event: TServerUpdateEvent);
begin
  if ServerUpdateEvents.IndexOf(Event) < 0 then
    ServerUpdateEvents.Add(Event);
end;

procedure TTBCore.RegisterChannelUpdateEvent(Event: TChannelUpdateEvent);
begin
  if ChannelUpdateEvents.IndexOf(Event) < 0 then
    ChannelUpdateEvents.Add(Event);
end;

procedure TTBCore.RegisterClientConnectEvent(Event: TClientConnectedEvent);
begin
  if ClientConnectEvents.IndexOf(Event) < 0 then
    ClientConnectEvents.Add(Event);
end;

procedure TTBCore.RegisterClientDisconnectEvent(Event: TClientDisconnectedEvent);
begin
  if ClientDisconnectEvents.IndexOf(Event) < 0 then
    ClientDisconnectEvents.Add(Event);
end;

procedure TTBCore.RegisterClientMoveEvent(Event: TClientMoveEvent);
begin
  if ClientMoveEvents.IndexOf(Event) < 0 then
    ClientMoveEvents.Add(Event);
end;

procedure TTBCore.UnregisterUpdateEvent(Event: TClientUpdateEvent);
begin
  ClientUpdateEvents.Remove(Event);
end;

procedure TTBCore.UnregisterUpdateEvent(Event: TServerUpdateEvent);
begin
  ServerUpdateEvents.Remove(Event);
end;

procedure TTBCore.UnregisterUpdateEvent(Event: TChannelUpdateEvent);
begin
  ChannelUpdateEvents.Remove(Event);
end;

procedure TTBCore.UnregisterConnectedEvent(Event: TClientConnectedEvent);
begin
  ClientConnectEvents.Remove(Event);
end;

procedure TTBCore.UnregisterDisconnectedEvent(Event: TClientDisconnectedEvent);
begin
  ClientDisconnectEvents.Remove(Event);
end;

procedure TTBCore.UnregisterMoveEvent(Event: TClientMoveEvent);
begin
  ClientMoveEvents.Remove(Event);
end;

end.
