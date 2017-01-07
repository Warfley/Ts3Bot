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

var LogFile: TextFile;
  DbgLog: Boolean;
  FileOpend: Boolean = False;

{ TLogger }

procedure CreateFileLogger(Path: String);
begin
  if FileOpend then Exit;
  DbgLog:=False;
  AssignFile(LogFile, Path);
  if FileExists(Path) then
    Append(LogFile)
  else
    Rewrite(LogFile);
  WriteLn(LogFile, '--------------Start of Service--------------');
  FileOpend:=True;
end;

procedure CreateDebugLogger;
begin
  if FileOpend then Exit;
  DbgLog:=True;
  LogFile:=StdOut;
  WriteLn(LogFile, '--------------Start of Service--------------');
  FileOpend:=True;
end;

procedure DestroyLogger;
begin
  if not FileOpend then Exit;
  WriteLn(LogFile, '-----------------End of Log-----------------');
  if not DbgLog then
    CloseFile(LogFile);
  FileOpend:=False;
end;

procedure WriteError(ErrNo: integer; Err: string);
begin
  if not FileOpend then Exit;
  WriteLn(LogFile, Format('[Error No%d][%s]%s', [ErrNo, DateTimeToStr(Now), Err]));
  Flush(LogFile);
end;

procedure WriteHint(Hint: string);
begin
  if not FileOpend then Exit;
  WriteLn(LogFile, Format('[Hint][%s]%s', [DateTimeToStr(Now), Hint]));
  Flush(LogFile);
end;

procedure WriteWarning(Warning: string);
begin
  if not FileOpend then Exit;
  WriteLn(LogFile, Format('[Warning][%s]%s', [DateTimeToStr(Now), Warning]));
  Flush(LogFile);
end;

procedure WriteStatus(Status: string);
begin
  if not FileOpend then Exit;
  WriteLn(LogFile, Format('[%s]%s', [DateTimeToStr(Now), Status]));
  Flush(LogFile);
end;

end.

