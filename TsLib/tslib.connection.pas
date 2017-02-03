unit TsLib.connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  TsLib.Types,
  TsLib.ValueRead,
  // Required for indy
  Interfaces,
  // indy
  IdGlobal, IdException, IdTelnet, IdStack,
  // Log
  Logger,
  // Critical sections
  syncobjs;

type

  { TTsConnection }

  TAntiFloodData = record
    UseAntiflood: Boolean;
    Commands, Time: Integer;
  end;

  TTsConnection = class
  private
    FRecieverThread: TThread;
    FRecieversThreaded: boolean;
    TelnetConnection: TIdTelnet;
    Recievers: TRecieveList;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FLoggedIn: boolean;
    FOnLogin: TNotifyEvent;
    FWaitingForStatus: boolean;
    FLastError: TStatusResponse;
    FLastMessage: String;
    FServerID: integer;
    FSyncData, FSyncRecievers: TRTLCriticalSection;
    FIncomeData: string;
    FAntiFloodShit: QWord;
    FAntiFloodData: TAntiFloodData;
    FAntiFloodConf: TAntiFloodData;
    function RecieveStatus(Sender: TObject; Data: string;
      var RemoveFromList: boolean): boolean;
    function getConnected: boolean;
    function GetLoggedIn: boolean;
    procedure SendDataToReciever;
  protected
    procedure TelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
    procedure TelnetConnected(Sender: TObject);
    procedure TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure TelnetDisconnected(Sender: TObject);
  public
    procedure ClearReciever;
    procedure FloodControl(Count, Time: Integer);
    constructor Create(Host: string; Port: integer);
    destructor Destroy; override;
    function Connect: boolean;
    procedure Disconnect;
    procedure AddReciever(Reciever: TRecieveEvent);
    procedure DeleteReciever(Reciever: TRecieveEvent);
    procedure SendCommand(Cmd: string);
    function ExecCommand(Cmd: string): TStatusResponse;
    function ExecCommand(Cmd: string; var Response: String): TStatusResponse;
    function LogIn(Username, Password: string): boolean;
    function SwitchServer(NewSID: integer): boolean;
    procedure LogOut;

    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnConnected;
    property Connected: boolean read getConnected;
    property LoggedIn: boolean read GetLoggedIn;
    property OnLogin: TNotifyEvent read FOnLogin write FOnLogin;
    property ServerID: integer read FServerID;
    property RecieverThread: TThread read FRecieverThread write FRecieverThread;
    property RecieversThreaded: boolean read FRecieversThreaded write FRecieversThreaded;
  end;

implementation

uses strutils;

{ TTsConnection }

function TTsConnection.getConnected: boolean;
begin
  if not Assigned(TelnetConnection) then
  begin
    Result:=False;
    Exit;
  end;
  TelnetConnection.CheckForGracefulDisconnect(False);
  Result := TelnetConnection.Connected;
end;

function TTsConnection.GetLoggedIn: boolean;
begin
  Result := FLoggedIn;
end;

procedure TTsConnection.SendDataToReciever;
var
  i: integer;
  Remove, DoBreak: boolean;
begin
  EnterCriticalsection(FSyncRecievers);
  try
    // Send to every Reciever
    i := 0;
    while i < Recievers.Count do
    begin
      Remove := Assigned(Recievers[i]);
      if Remove then
        DoBreak := Recievers[i](Self, FIncomeData, Remove);
      // Delete Reciever afterwards
      if Remove then
        Recievers.Delete(i)
      else if DoBreak then // Delete messega afterwards
        break
      else
        Inc(i);
    end;
  finally
    LeaveCriticalsection(FSyncRecievers);
  end;
end;

function TTsConnection.SwitchServer(NewSID: integer): boolean;
var
  res: TStatusResponse;
begin
  if not Connected or (FServerID = NewSID) then
    Exit;
  res := ExecCommand(Format('use %d', [NewSID]));
  Result := res.ErrNo = 0;
  if Result then
  begin
    FServerID := NewSID;
    WriteStatus(Format('Switched to server %d', [NewSID]));
  end;
end;

function TTsConnection.RecieveStatus(Sender: TObject; Data: string;
  var RemoveFromList: boolean): boolean;
var
  sl: TStringList;
  ErrorString: String;
begin
  Data := Trim(Data);
  Result:=not AnsiStartsStr('notify', Data);
  RemoveFromList:=False;
  if not Result then
  begin
    // Not Processed
    Result := False;
    RemoveFromList := False;
    Exit;
  end;

  sl:=TStringList.Create;
  try
    sl.Text:=Data;
  if ((sl.Count > 0) And AnsiStartsStr('error id=', Trim(sl[sl.Count-1]))) then
  begin
    //Last part recieved
  ErrorString := Trim(sl[sl.Count-1]);

  // Get the number
  Delete(ErrorString, 1, 9);
  FLastError.ErrNo := StrToInt(Copy(ErrorString, 1, Pos(' ', ErrorString) - 1));
  // Get the Message
  Delete(ErrorString, 1, Pos('=', ErrorString));
  ReadValue(Trim(ErrorString), FLastError.Msg);

  sl.Delete(sl.Count-1);
  // Take away the reciever and the message
    RemoveFromList := True;
  end;

  FLastMessage+=sl.Text;


  finally
    sl.Free;
  end;

  // unblock the waiting status
  if RemoveFromList then
    FWaitingForStatus := False;
end;

procedure TTsConnection.TelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
begin
  //Noop
end;

procedure TTsConnection.TelnetConnected(Sender: TObject);
begin
  // Writing log and raise event
  WriteStatus('Connected.');
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TTsConnection.TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
begin
  // Lock data
  EnterCriticalsection(FSyncData);
  try
    // Load Data as String
    SetString(FIncomeData, PAnsiChar(Buffer), Length(Buffer));
    // Check if threaded
    if RecieversThreaded then
      SendDataToReciever // Send in seperate thread
    else
      TThread.Synchronize(RecieverThread, @SendDataToReciever);
    // Send in event Thread
  finally
    LeaveCriticalsection(FSyncData);
  end;
end;

procedure TTsConnection.TelnetDisconnected(Sender: TObject);
begin
  // Writing log and raise event
  WriteStatus('Disconnected.');
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TTsConnection.ClearReciever;
begin
  EnterCriticalsection(FSyncRecievers);
  try
    Recievers.Clear;
  finally
    LeaveCriticalsection(FSyncRecievers);
  end;
end;

procedure TTsConnection.FloodControl(Count, Time: Integer);
begin
  FAntiFloodData.UseAntiFlood:= (Count >0) and (Time>0);
  FAntiFloodData.Commands:=Count;
  FAntiFloodData.Time:=Time;
  FAntiFloodShit:=GetTickCount64;
end;

constructor TTsConnection.Create(Host: string; Port: integer);
begin
  FRecieverThread := TThread.CurrentThread;
  FRecieversThreaded := True;
  InitCriticalSection(FSyncData);
  InitCriticalSection(FSyncRecievers);
  WriteStatus('Setting up Telnet Client');
  // Initializing Reciever List
  Recievers := TRecieveList.Create;
  // Creating Connection object
  TelnetConnection := TIdTelnet.Create;
  // Setting up Connection data
  TelnetConnection.Host := Host;
  TelnetConnection.Port := Port;
  // Activate Threading
  TelnetConnection.ThreadedEvent := True;
  // Events
  TelnetConnection.OnDataAvailable := @TelnetDataAvailable;
  TelnetConnection.OnConnected := @TelnetConnected;
  TelnetConnection.OnDisconnected := @TelnetDisconnected;
  TelnetConnection.OnTelnetCommand := @TelnetCommand;
  //Not logged in
  FLoggedIn := False;
end;

destructor TTsConnection.Destroy;
begin
  // Close current connection
  if Connected then
    Disconnect;
  // Clean up Memory
  Recievers.Free;
  DoneCriticalsection(FSyncData);
  DoneCriticalsection(FSyncRecievers);
  TelnetConnection.Free;
  inherited Destroy;
end;

function TTsConnection.Connect: boolean;
begin
  // Already connected?
  if Connected then
    Exit;
  // Write Log
  WriteStatus(Format('Connecting to %s:%d', [TelnetConnection.Host,
    TelnetConnection.Port]));
  // Connect to Server
  try
    TelnetConnection.Connect;
  except
    on e: EIdSocketError do
      WriteError(100, e.Message);
    on e: Exception do
      WriteError(1, e.Message);
  end;
  Result := TelnetConnection.Connected;
end;

procedure TTsConnection.Disconnect;
begin
  // exit if no active connection
  if not Connected then
    exit;
  // Write Log
  WriteStatus('Closing Connection');
  try
    // Close Connection
    SendCommand('quit');
    sleep(100);
    TelnetConnection.CheckForGracefulDisconnect;
  except
    // Ignore connection closed by other side
    on e: EIdConnClosedGracefully do
      WriteStatus('Disconnected');
  end;
end;

procedure TTsConnection.AddReciever(Reciever: TRecieveEvent);
begin
  EnterCriticalsection(FSyncRecievers);
  try
    if Recievers.IndexOf(Reciever) < 0 then
      Recievers.Add(Reciever);
  finally
    LeaveCriticalsection(FSyncRecievers);
  end;
end;

procedure TTsConnection.DeleteReciever(Reciever: TRecieveEvent);
begin
  EnterCriticalsection(FSyncRecievers);
  try
    Recievers.Remove(Reciever);
  finally
    LeaveCriticalsection(FSyncRecievers);
  end;
end;

procedure TTsConnection.SendCommand(Cmd: string);
var
  c: char;
  diff: QWord;
begin

  if FAntiFloodData.UseAntiFlood then
  begin
    Diff:=GetTickCount64-FAntiFloodShit;
    FAntiFloodShit:=GetTickCount64;
    if Diff>FAntiFloodConf.Time then
    begin
      FAntiFloodConf.Time:=FAntiFloodData.Time*1000;
      FAntiFloodConf.Commands:=FAntiFloodData.Commands-1;
    end
    else
      dec(FAntiFloodConf.Time, diff);
    if FAntiFloodConf.Commands>0 then
      dec(FAntiFloodConf.Commands)
    else
      Sleep(FAntiFloodConf.Time);
  end;

  // Write command via Telnet
  for c in Cmd do
    TelnetConnection.SendCh(c);
  TelnetConnection.SendCh(#13);

end;

function TTsConnection.ExecCommand(Cmd: string): TStatusResponse;
var
  i: integer;
begin
  FLastMessage:='';
  while FWaitingForStatus do //Someone waiting for another command
    Sleep(100);

  // Set state to Waiting
  FWaitingForStatus := True;
  // Add Reciever to get result
  AddReciever(@RecieveStatus);
  try
  // Send command
  SendCommand(Cmd);

  // Wait at most 2 Seconds for a result
  for i := 1 to 20 do
  begin
    Sleep(100);
    if not FWaitingForStatus then // Result found
      Break;
  end;

  if FWaitingForStatus then
  begin
    // Stop if nothing was found
    FWaitingForStatus := False;
    DeleteReciever(@RecieveStatus);
    FLastError.ErrNo := -1;
    FLastError.Msg := 'No response recieved';
  end;

  if FLastError.ErrNo <> 0 then
    WriteError(FLastError.ErrNo, FLastError.Msg);
  except
    on e:EIdConnClosedGracefully do
    begin
      FLastError.ErrNo:=1;
      FLastError.Msg:='Connection closed by server';
    end;
  end;
  // Return result
  Result := FLastError;
end;

function TTsConnection.ExecCommand(Cmd: string; var Response: String
  ): TStatusResponse;
begin
  Result:=ExecCommand(Cmd);
  Response:=FLastMessage;
end;

function TTsConnection.LogIn(Username, Password: string): boolean;
var
  err: TStatusResponse;
begin
  // Already logged in?
  if LoggedIn then
    exit;
  err := ExecCommand(Format('login %s %s', [Username, Password]));
  Result := err.ErrNo = 0;
  FLoggedIn := Result;
  // Write Log
  if Result then
    WriteStatus('Login successful');
end;

procedure TTsConnection.LogOut;
var
  err: TStatusResponse;
  res: boolean;
begin
  // Already logged in?
  if not LoggedIn then
    exit;
  err := ExecCommand('logout');
  res := err.ErrNo = 0;
  FLoggedIn := False;
  // Write Log
  if res then
    WriteStatus('Logout successful')
  else
    WriteStatus('Logout failed ' + err.Msg);
end;

end.
