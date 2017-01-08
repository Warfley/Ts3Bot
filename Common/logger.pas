unit Logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

    procedure CreateFileLogger(Path: String);
    procedure CreateDebugLogger;
    procedure DestroyLogger;
    procedure WriteError(ErrNo: integer; Err: string);
    procedure WriteHint(Hint: string);
    procedure WriteWarning(Warning: string);
    procedure WriteStatus(Status: string);

implementation

var LogFile: THandle;
  DbgLog: Boolean;
  FileOpend: Boolean = False;

procedure WriteLogLn(Line: String);
begin
  if not FileOpend then exit;
  line+=#10;
  FileWrite(LogFile, Line[1], Length(Line));
end;

procedure CreateFileLogger(Path: String);
begin
  if FileOpend then Exit;
  DbgLog:=False;
  if FileExists(Path) then
    LogFile:=FileOpen(Path, fmAppend)
  else
    LogFile:=FileCreate(Path);
  FileOpend:=True;
  WriteLogLn( '--------------Start of Service--------------');
end;

procedure CreateDebugLogger;
begin
  if FileOpend then Exit;
  DbgLog:=True;
  LogFile:=StdOutputHandle;
  FileOpend:=True;
  WriteLogLn( '--------------Start of Service--------------');
end;

procedure DestroyLogger;
begin
  if not FileOpend then Exit;
  WriteLogLn( '-----------------End of Log-----------------');
  if not DbgLog then
    FileClose(LogFile);
  FileOpend:=False;
end;

procedure WriteError(ErrNo: integer; Err: string);
begin
  if not FileOpend then Exit;
  WriteLogLn( Format('[Error No%d][%s]%s', [ErrNo, DateTimeToStr(Now), Err]));
end;

procedure WriteHint(Hint: string);
begin
  if not FileOpend then Exit;
  WriteLogLn( Format('[Hint][%s]%s', [DateTimeToStr(Now), Hint]));
end;

procedure WriteWarning(Warning: string);
begin
  if not FileOpend then Exit;
  WriteLogLn( Format('[Warning][%s]%s', [DateTimeToStr(Now), Warning]));
end;

procedure WriteStatus(Status: string);
begin
  if not FileOpend then Exit;
  WriteLogLn( Format('[%s]%s', [DateTimeToStr(Now), Status]));
end;

end.

