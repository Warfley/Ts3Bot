unit TsBotUI.CLI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBotUI.Types, syncobjs
  {$IfDef UNIX}
  , BaseUnix, termio
  {$Else}
  , Windows,
  crt
  {$EndIf}  ;

type

  { TCommandLineInterface }

  TCommandLineInterface = class(TThread)
  private
    { Private declarations }
    FEventToFire: TCommandEventData;
    FOnCommandMethod: TCommandEventMethod;
    FOnCommand: TCommandEvent;
    FPrintList: TStringList;
    PrintListCS: TRTLCriticalSection;
    procedure FireEvent;
    function ReadDaShit(out str: string): boolean;
    procedure PrintHelp;
    procedure CommandExecuted(Sender: TObject; Command: TCommandType; Status: boolean);
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    constructor Create(AOnCommand: TCommandEventMethod);
    constructor Create(AOnCommand: TCommandEvent);
    destructor Destroy; override;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
    property OnCommandMethod: TCommandEventMethod
      read FOnCommandMethod write FOnCommandMethod;
  end;

implementation

uses Math;

{$IfDef UNIX}

function ReadChar(Blocking: boolean = True): char;
var
  oTIO, nTIO: Termios;
    {$IfDef NonBlockingStdIn}
  flags,
    {$Else}
  fdsin: tfdSet;
    {$EndIf}
  res: integer;
begin
  res := 1;
  Result := #0;
  TCGetAttr(1, oTIO);
  nTIO := oTIO;
  CFMakeRaw(nTIO);
  TCSetAttr(1, TCSANOW, nTIO);
  if not Blocking then
  begin
    {$ifDef NonBlockingStdIn}
    flags := FpFcntl(StdInputHandle, F_GetFl, 0);
    FpFcntl(StdInputHandle, F_SetFl, flags or O_NONBLOCK);
    {$Else}
    fpFD_ZERO(fdsin);
    fpFD_SET(StdInputHandle, fdsin);
    res := fpSelect(StdInputHandle + 1, @fdsin, nil, nil, 0);
    {$EndIf}
  end;
  if res > 0 then
    res := FpRead(StdInputHandle, Result, 1);

  {$ifDef NonBlockingStdIn}
  if res = 0 then
    Result := #0;
  {$EndIf}

  //restore settings
  TCSetAttr(1, TCSANOW, oTIO);
  {$ifDef NonBlockingStdIn}
  if not Blocking then
    FpFcntl(StdInputHandle, F_SetFl, flags);
  {$EndIf}
end;

{$Else}

// found at http://www.cplusplus.com/forum/articles/19975/
function ReadChar(Blocking: boolean = True): char;
var
  hstdin: HANDLE;
  irInputRecord: INPUT_RECORD;
  dwEventsRead: DWORD;
  r: DWORD;
begin
  hStdin := GetStdHandle(STD_INPUT_HANDLE);
  GetNumberOfConsoleInputEvents(hstdin, r);
  if Blocking or (r > 0) then
    while ReadConsoleInputA(hStdin, irInputRecord, 1, dwEventsRead) do
      if (irInputRecord.EventType = KEY_EVENT) and
        (irInputRecord.Event.KeyEvent.wVirtualKeyCode <> VK_SHIFT) and
        (irInputRecord.Event.KeyEvent.wVirtualKeyCode <> VK_MENU) and
        (irInputRecord.Event.KeyEvent.wVirtualKeyCode <> VK_CONTROL) then
      begin
        Result := irInputRecord.Event.KeyEvent.AsciiChar;
        if irInputRecord.Event.KeyEvent.wVirtualKeyCode in [VK_LEFT, VK_RIGHT] then
          Result := char(irInputRecord.Event.KeyEvent.wVirtualKeyCode - 33)
        else if irInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_DELETE then
          Result := #1;
        ReadConsoleInputA(hStdin, irInputRecord, 1, dwEventsRead);
        Exit;
      end
      else if not Blocking then
        break;
  Result := #0;
end;

{$EndIf}

procedure GotoX(X: integer);
begin
  {$IfDef WINDOWS}
  GotoXY(X, WhereY);
  {$EndIf}
end;

{ TCommandLineInterface }

procedure TCommandLineInterface.FireEvent;
begin
  if Assigned(FOnCommand) then
    FOnCommand(FEventToFire);
  if Assigned(FOnCommandMethod) then
    FOnCommandMethod(FEventToFire);
end;

function TCommandLineInterface.ReadDaShit(out str: string): boolean;
var
  c: char;
  currPos: integer;
begin
  str := '';
  c := #0;
  currPos := 1;
  while (c <> #13) and not Terminated do
  begin
    c := ReadChar(False);
    case c of
    {$IFDEF Windows}
    #9, #32..#45, #47..#255:
    {$Else}
      #9, #32, #48..#255:
    {$EndIf}
      begin
        str := Copy(str, 1, currPos - 1) + c + Copy(str, currPos, Length(str));
        Inc(currPos);
      end;
      #8:
      begin
        if currPos > 1 then
        begin
          Dec(currPos);
          Delete(str, currPos, 1);
        end;
      end;
    {$IFDEF Windows}
    #1:
    {$Else}
      #37:
    {$EndIf}
        Delete(str, currPos, 1);
      #27:
      begin
        str := '';
        c := #0;
        currPos := 1;
      end;
    {$IFDEF Windows}
    #4:
    {$Else}
      #37:
    {$EndIf}
        currPos := max(currPos - 1, 1);
    {$IFDEF Windows}
    #6:
    {$Else}
      #39:
    {$EndIf}
        currPos := min(currPos + 1, Length(str) + 1);
    end;
    if c in [
{$IFDEF Windows} #4, #6 {$Else}
      #37, #39
{$EndIf}
      ] then
    begin
      GotoX(currPos + 5);
    end
    else if c <> #0 then
    begin
      {$IfDef WINDOWS}
      if c in [#9, #32..#45, #47..#255] then
        Write(#13, 'CLI> ',str)
      else
      begin
        Write(#13);
        ClrEol;
        Write('CLI> ', str);
      end;
      {$Else}
      Write(#13, 'CLI> ', str);
      {$EndIf}
      GotoX(currPos + 5);
    end;
    Sleep(10);
  end;
  WriteLn('');
  Result := not Terminated;
end;

procedure TCommandLineInterface.PrintHelp;
begin
  WriteLn('/----------------------Commandlineinterface help----------------------\');
  WriteLn('|       Command      |                 Description                    |');
  WriteLn('|---------------------------------------------------------------------|');
  WriteLn('|quit                | Stops the bot                                  |');
  WriteLn('|restart             | Restarts the bot                               |');
  WriteLn('|configconnection    | Change the connection data and restarts the Bot|');
  WriteLn('|configlog           | Sets the path to the logfile                   |');
  WriteLn('|logpath             | Displays the path to the logfile               |');
  WriteLn('|switchserver        | Switches to another virtual server             |');
  WriteLn('|connectiondata      | Prints out the connection configuration        |');
  WriteLn('|resetconfig         | Resets to default (empty) configuration        |');
  WriteLn('|help                | Shows this text                                |');
  WriteLn('\---------------------------------------------------------------------/');
end;

procedure TCommandLineInterface.CommandExecuted(Sender: TObject;
  Command: TCommandType; Status: boolean);
begin
  case Command of
    ctRestart:
      if Status then
        WriteLn('CLI> Restarting...')
      else
        WriteLn('CLI> Couln''t perform restart');
    ctQuit:
      if Status then
        WriteLn('CLI> Shutting down...')
      else
        WriteLn('CLI> Error on shutting down');
    ctSwitchServer:
      if Status then
        WriteLn('CLI> Server switched')
      else
        WriteLn('CLI> Error on switching server');
    ctSetConnectionData:
      if Status then
        WriteLn('CLI> Connection data successful set')
      else
        WriteLn('CLI> Couldn''t change connection data');
    ctGetConnectionData:
    begin
      if Status then
        Write(FPrintList.Text)
      else
        WriteLn('Something went wrong. This should never happen');
      LeaveCriticalSection(PrintListCS);
    end;
    ctChangeLogPath:
    if Status then
      WriteLn('Log path successfully changed')
    else
      WriteLn('Something went wrong. This should never happen');
    ctGetLogPath:
    begin
    if Status then
      Write('Log:'+FPrintList.Text)
    else
      WriteLn('Something went wrong. This should never happen');
    LeaveCriticalSection(PrintListCS);
    end;
    ctResetConfig:
    if Status then
      WriteLn('Config successfully reseted')
    else
      WriteLn('Something went wrong. This should never happen');
  end;
end;

procedure TCommandLineInterface.Execute;
var
  s: string;
begin
  while ReadDaShit(s) do
  begin
    s := LowerCase(s);
    with FEventToFire do
      if s = 'quit' then
      begin
        CommandType := ctQuit;
        Data := 0;
      end
      else if s = 'restart' then
      begin
        CommandType := ctRestart;
        Data := 0;
      end
      else if s = 'switchserver' then
      begin
        CommandType := ctSwitchServer;
        Write('ServerID: ');
        ReadLn(Data);
      end
      else if s = 'configconnection' then
      begin
        CommandType := ctSetConnectionData;
        new(PConnectionData(Data));
        with PConnectionData(Data)^ do
        begin
          Write('IP: ');
          ReadLn(IP);
          Write('Port: ');
          ReadLn(Port);
          Write('ServerID: ');
          ReadLn(ServerID);
          Write('Username: ');
          ReadLn(UserName);
          Write('Password: ');
          ReadLn(Password);
        end;
      end
      else if s='connectiondata' then
      begin
        EnterCriticalSection(PrintListCS);
        FPrintList.Clear;
        Data:=PtrInt(FPrintList);
        CommandType:=ctGetConnectionData;
      end
      else if s='configlog' then
      begin
        New(PString(Data));
        Write('Log path: ');
        ReadLn(PString(Data)^);
        CommandType:=ctChangeLogPath;
      end
      else if s='logpath' then
      begin
        EnterCriticalSection(PrintListCS);
        FPrintList.Clear;
        Data:=PtrInt(FPrintList);
        CommandType:=ctGetLogPath;
      end
      else if s='resetconfig' then
      begin
        Data:=0;
        CommandType:=ctResetConfig;
      end
      else if s = 'help' then
      begin
        PrintHelp;
        Continue;
      end
      else
      begin
        WriteLn('Unknown command');
        Continue;
      end;
    FEventToFire.OnFinished := @CommandExecuted;
    Synchronize(@FireEvent);
  end;
end;

constructor TCommandLineInterface.Create(AOnCommand: TCommandEventMethod);
begin
  FOnCommandMethod := AOnCommand;
  Create(TCommandEvent(nil));
end;

constructor TCommandLineInterface.Create(AOnCommand: TCommandEvent);
begin
  FOnCommand := AOnCommand;
  FreeOnTerminate := True;
  FPrintList:=TStringList.Create;
  InitCriticalSection(PrintListCS);
  inherited Create(False);
end;

destructor TCommandLineInterface.Destroy;
begin
  FPrintList.Free;
  DoneCriticalsection(PrintListCS);
  inherited Destroy;
end;

end.
