unit ts.Connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Interfaces, IdGlobal, IdException, IdTelnet, fgl, Logger;

type

  TTsConnection = class;

  TRecieveEvent = function(Sender: TTsConnection; Data: string;
    var RemoveFromList: boolean): boolean of object;

  TRecieveList = specialize TFPGList<TRecieveEvent>;

  TStatusResponse = record
    ErrNo: integer;
    Msg: string;
  end;

  { TTsConnection }

  TTsConnection = class
  private
    CheckLocked: boolean;
    TelnetConnection: TIdTelnet;
    Recievers: TRecieveList;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FLoggedIn: boolean;
    FOnLogin: TNotifyEvent;
    FWaitingForStatus: boolean;
    FLastError: TStatusResponse;
    function RecieveStatus(Sender: TTsConnection; Data: string;
      var RemoveFromList: boolean): boolean;
    function getConnected: boolean;
    function GetLoggedIn: boolean;
  protected
    procedure TelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
    procedure TelnetConnected(Sender: TObject);
    procedure TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure TelnetDisconnected(Sender: TObject);
  public
    constructor Create(Host: string; Port: integer);
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure AddReciever(Reciever: TRecieveEvent);
    procedure DeleteReciever(Reciever: TRecieveEvent);
    procedure SendCommand(Cmd: string);
    function CommandWithResponse(Cmd: string): TStatusResponse;
    function LogIn(Username, Password: string): boolean;
    procedure LogOut;

    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnConnected;
    property Connected: boolean read getConnected;
    property LoggedIn: boolean read GetLoggedIn;
    property OnLogin: TNotifyEvent read FOnLogin write FOnLogin;
  end;

implementation

uses strutils;

{ TTsConnection }

function TTsConnection.getConnected: boolean;
begin
  Result := TelnetConnection.Connected;
end;

function TTsConnection.GetLoggedIn: boolean;
begin
  Result := FLoggedIn;
end;

function TTsConnection.RecieveStatus(Sender: TTsConnection; Data: string;
  var RemoveFromList: boolean): boolean;
begin
  Data:=Trim(Data);
  if not AnsiStartsStr('error id=', Data) then
  begin
    // Not Processed
    Result := False;
    RemoveFromList := False;
    Exit;
  end;

  // Get the number
  Delete(Data, 1, 9);
  FLastError.ErrNo := StrToInt(Copy(Data, 1, Pos(' ', Data)-1));
  // Get the Message
  Delete(Data, 1, Pos('=', Data));
  FLastError.Msg := Data;

  // Take away the reciever and the message
  Result := True;
  RemoveFromList := True;

  // unblock the waiting status
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
var
  Response: string;
  i: integer;
  Remove, DoBreak: boolean;
begin
  // Load Data as String
  SetString(Response, PAnsiChar(Buffer), Length(Buffer));
  // Send to every Reciever
  i := 0;
  while i < Recievers.Count do
  begin
    Remove := Assigned(Recievers[i]);
    if Remove then
      DoBreak := Recievers[i](Self, Response, Remove);
    // Delete Reciever afterwards
    if Remove then
      Recievers.Delete(i)
    else if DoBreak then // Delete messega afterwards
      break
    else
      Inc(i);
  end;
end;

procedure TTsConnection.TelnetDisconnected(Sender: TObject);
begin
  // Writing log and raise event
  WriteStatus('Disconnected.');
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

constructor TTsConnection.Create(Host: string; Port: integer);
begin
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
  inherited Destroy;
end;

procedure TTsConnection.Connect;
begin
  // Already connected?
  if Connected then
    Exit;
  // Write Log
  WriteStatus(Format('Connecting to %s:%d', [TelnetConnection.Host,
    TelnetConnection.Port]));
  // Connect to Server
  TelnetConnection.Connect;
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
    TelnetConnection.Disconnect(True);
  except
    // Ignore connection closed by other side
    on e: EIdConnClosedGracefully do ;
  end;
end;

procedure TTsConnection.AddReciever(Reciever: TRecieveEvent);
begin
  if Recievers.IndexOf(Reciever) < 0 then
    Recievers.Add(Reciever);
end;

procedure TTsConnection.DeleteReciever(Reciever: TRecieveEvent);
begin
  Recievers.Remove(Reciever);
end;

procedure TTsConnection.SendCommand(Cmd: string);
var
  c: char;
begin
  // Write command via Telnet
  for c in Cmd do
    TelnetConnection.SendCh(c);
  TelnetConnection.SendCh(#13);

end;

function TTsConnection.CommandWithResponse(Cmd: string): TStatusResponse;
var
  i: integer;
begin
  while FWaitingForStatus do //Someone waiting for another command
    Sleep(100);

  // Set state to Waiting
  FWaitingForStatus := True;
  // Add Reciever to get result
  AddReciever(@RecieveStatus);
  // Send command
  SendCommand(Cmd);

  // Wait at most 5 Seconds for a result
  for i := 1 to 50 do
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

  // Return result
  Result := FLastError;
end;

function TTsConnection.LogIn(Username, Password: string): boolean;
var
  err: TStatusResponse;
begin
  // Already logged in?
  if LoggedIn then
    exit;
  err:= CommandWithResponse(Format('login %s %s', [Username, Password]));
  Result:=err.ErrNo=0;
  FLoggedIn:=Result;
  // Write Log
  if Result then
    WriteStatus('Login successful')
  else
    WriteStatus('Login failed ' +err.Msg);
end;

procedure TTsConnection.LogOut;
var
  err: TStatusResponse;
  res: Boolean;
begin
  // Already logged in?
  if not LoggedIn then
    exit;
  err:= CommandWithResponse('logout');
  res:=err.ErrNo=0;
  FLoggedIn:=false;
  // Write Log
  if res then
    WriteStatus('Logout successful')
  else
    WriteStatus('Logout failed ' +err.Msg);
end;

end.
