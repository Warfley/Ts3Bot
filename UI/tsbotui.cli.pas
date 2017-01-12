unit TsBotUI.CLI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsBotUI.Types
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
    procedure FireEvent;
    function ReadDaShit(out str: String): Boolean;
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    constructor Create(AOnCommand: TCommandEventMethod);
    constructor Create(AOnCommand: TCommandEvent);
    destructor Destroy; override;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
    property OnCommandMethod: TCommandEventMethod read FOnCommandMethod write FOnCommandMethod;
  end;

implementation

uses math;

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
          Result:=Char(irInputRecord.Event.KeyEvent.wVirtualKeyCode-33);
        ReadConsoleInputA(hStdin, irInputRecord, 1, dwEventsRead);
        Exit;
      end
      else if not Blocking then
        break;
  Result := #0;
end;

{$EndIf}

procedure GotoX(X: Integer);
begin
  {$IfDef WINDOWS}
  GotoXY(X, WhereY);
  {$EndIf}
end;

{ TCommandLineInterface }

procedure TCommandLineInterface.FireEvent;
begin
  if Assigned(FOnCommand) then
    FOnCommand(FEventToFire.CommandType, FEventToFire.Data);
  if Assigned(FOnCommandMethod) then
    FOnCommandMethod(FEventToFire.CommandType, FEventToFire.Data);
end;

function TCommandLineInterface.ReadDaShit(out str: String): Boolean;
var
  c: Char;
  currPos: Integer;
begin
  str:='';
  c:=#0;
  currPos:=1;
  while (c<>#13) and not Terminated do
  begin
    c:=ReadChar(False);
    case c of
    {$IFDEF Windows}
    #9, #33..#45, #47..#255:
    {$Else}
    #9, #48..#255:
    {$EndIf}
    begin
      str:=Copy(str, 1, currPos-1)+c+Copy(str,currPos, Length(str));
      inc(currPos);
    end;
    #8:
    begin
      if currPos>1 then
      begin
        Dec(currPos);
        Delete(str, currPos, 1);
      end;
    end;
    #46:
      Delete(str, currPos, 1);
    #27:
    begin
      str:='';
      c:=#0;
      currPos:=1;
    end;
    {$IFDEF Windows}
    #4:
    {$Else}
    #37:
    {$EndIf}
      currPos:=max(currPos-1, 1);
    {$IFDEF Windows}
    #6:
    {$Else}
    #39:
    {$EndIf}
      inc(currPos);
    end;
    if c<>#0 then
    begin
      {$IfDef WINDOWS}
      Write(#13);
      ClrEol;
      {$EndIf}
      Write({$IfDef UNIX}#13, {$EndIf}'cmd> ',str);
      GotoX(currPos+5);
      Sleep(10);
    end;
  end;
  WriteLn('');
  Result:=not Terminated;
end;

procedure TCommandLineInterface.Execute;
var s: String;
begin
  while ReadDaShit(s) do
  begin
    s:=LowerCase(s);
    with FEventToFire do
    if s='quit' then
    begin
      CommandType:=ctQuit;
      Data:=0;
    end
    else if s='configconnection' then
    begin
      CommandType:=ctSetConnectionData;
      new(PConnectionData(Data));
      with PConnectionData(Data)^ do
      begin
        Write('IP:');
        ReadLn(IP);
        Write('Port:');
        ReadLn(Port);
        Write('ServerID:');
        ReadLn(ServerID);
        Write('Username:');
        ReadLn(UserName);
        Write('Password:');
        ReadLn(Password);
      end;
    end
    else
    begin
      WriteLn('Unknown command');
      Continue;
    end;
    Synchronize(@FireEvent);
    case FEventToFire.CommandType of
    ctSetConnectionData: Dispose(PConnectionData(FEventToFire.Data));
    end;
  end;
end;

constructor TCommandLineInterface.Create(AOnCommand: TCommandEventMethod);
begin
  FOnCommandMethod:=AOnCommand;
  FreeOnTerminate:=True;
  inherited Create(False);
end;

constructor TCommandLineInterface.Create(AOnCommand: TCommandEvent);
begin
  FOnCommand:=AOnCommand;
  FreeOnTerminate:=True;
  inherited Create(False);
end;

destructor TCommandLineInterface.Destroy;
begin
  inherited Destroy;
end;

end.

