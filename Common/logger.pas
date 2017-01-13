unit Logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, syncobjs;

procedure CreateFileLogger(Path: string; DoAppend: boolean = False);
procedure CreateDebugLogger;
procedure DestroyFileLogger;
procedure DestroyDebugLogger;
procedure WriteError(ErrNo: integer; Err: string);
procedure WriteHint(Hint: string);
procedure WriteWarning(Warning: string);
procedure WriteStatus(Status: string);

implementation

var
  FileCS: TRTLCriticalSection;
  LogFile: TextFile;
  LogCreated: boolean;
  DbgLog: boolean;
  FileOpend: boolean = False;

procedure WriteLogLn(Line: string);
begin
  if FileOpend then
  begin
    EnterCriticalsection(FileCS);
    try
      WriteLn(LogFile, Line);
      Flush(LogFile);
    finally
      LeaveCriticalsection(FileCS);
    end;
  end;
  if DbgLog then
    WriteLn(Line);
end;

procedure CreateFileLogger(Path: string; DoAppend: boolean);
begin
  if FileOpend then
    Exit;
  AssignFile(LogFile, Path);
  if FileExists(Path) and DoAppend then
    Append(LogFile)
  else
    Rewrite(LogFile);
  InitCriticalSection(FileCS);
  EnterCriticalsection(FileCS);
  FileOpend := True;
  LogCreated := True;
  try
    WriteLn(LogFile, '--------------Start of Service--------------');
  finally
    LeaveCriticalsection(FileCS);
  end;
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
  EnterCriticalsection(FileCS);
  try
    WriteLn(LogFile, '-----------------End of Log-----------------');
    CloseFile(LogFile);
    FileOpend := False;
    LogCreated := FileOpend or DbgLog;
  finally
    LeaveCriticalsection(FileCS);
    DoneCriticalsection(FileCS);
  end;
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
