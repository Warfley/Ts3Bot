unit Logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure CreateFileLogger(Path: string; DoAppend: Boolean = False);
procedure CreateDebugLogger;
procedure DestroyFileLogger;
procedure DestroyDebugLogger;
procedure WriteError(ErrNo: integer; Err: string);
procedure WriteHint(Hint: string);
procedure WriteWarning(Warning: string);
procedure WriteStatus(Status: string);

implementation

var
  LogFile: TextFile;
  LogCreated: boolean;
  DbgLog: boolean;
  FileOpend: boolean = False;

procedure WriteLogLn(Line: string);
begin
  if FileOpend then
  begin
    WriteLn(LogFile, Line);
    Flush(LogFile);
  end;
  if DbgLog then
    WriteLn(Line);
end;

procedure CreateFileLogger(Path: string; DoAppend: Boolean);
begin
  if FileOpend then
    Exit;
  AssignFile(LogFile, Path);
  if FileExists(Path) and DoAppend then
    Append(LogFile)
  else
    Rewrite(LogFile);
  FileOpend := True;
  LogCreated := True;
  WriteLn(LogFile, '--------------Start of Service--------------');
end;

procedure CreateDebugLogger;
begin
  if DbgLog then
    Exit;
  DbgLog := True;
  LogCreated := True;
  WriteLn('--------------Start of Service--------------');
end;

procedure DestroyFileLogger;
begin
  if not FileOpend then
    Exit;
  WriteLn(LogFile, '-----------------End of Log-----------------');
  CloseFile(LogFile);
  FileOpend := False;
  LogCreated := FileOpend or DbgLog;
end;

procedure DestroyDebugLogger;
begin
  if not DbgLog then
    Exit;
  WriteLn('-----------------End of Log-----------------');
  if not DbgLog then
    Close(LogFile);
  FileOpend := False;
  LogCreated := FileOpend or DbgLog;
end;

procedure WriteError(ErrNo: integer; Err: string);
begin
  if not LogCreated then
    Exit;
  WriteLogLn(Format('[Error No%d][%s]%s', [ErrNo, DateTimeToStr(Now), Err]));
end;

procedure WriteHint(Hint: string);
begin
  if not LogCreated then
    Exit;
  WriteLogLn(Format('[Hint][%s]%s', [DateTimeToStr(Now), Hint]));
end;

procedure WriteWarning(Warning: string);
begin
  if not LogCreated then
    Exit;
  WriteLogLn(Format('[Warning][%s]%s', [DateTimeToStr(Now), Warning]));
end;

procedure WriteStatus(Status: string);
begin
  if not LogCreated then
    Exit;
  WriteLogLn(Format('[%s]%s', [DateTimeToStr(Now), Status]));
end;

end.
