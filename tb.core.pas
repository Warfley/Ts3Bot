unit tb.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tb.Config;

type

  { TLogger }

  TLogger = class
  private
    FLog: TextFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteError(ErrNo: integer; Err: string);
    procedure WriteHint(Hint: string);
    procedure WriteWarning(Warning: string);
    procedure WriteStatus(Status: string);
  end;

  { TTBCore }

  TTBCore = class(TThread)
  private
    FLogger: TLogger;
    FConfig: TConfig;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    constructor Create(AOnTerminate: TNotifyEvent);
    destructor Destroy; override;
  end;

const
  SLogPath = './run.log';
  SConfPath = './.tbconf';

implementation

{ TLogger }

constructor TLogger.Create;
begin
  {$IfDef Debug}
  FLog:=StdOut;
  {$Else}
  AssignFile(FLog, SLogPath);
  Append(FLog);
  {$EndIf}
  WriteLn(FLog, '--------------Start of Service--------------');
end;

destructor TLogger.Destroy;
begin
  WriteLn(FLog, '-----------------End of Log-----------------');
  {$IfNDef Debug}
  CloseFile(FLog);
  {$EndIf}
  inherited Destroy;
end;

procedure TLogger.WriteError(ErrNo: integer; Err: string);
begin
  WriteLn(FLog, Format('[Error No%d][%s]%s', [ErrNo, DateTimeToStr(Now), Err]));
  Flush(FLog);
end;

procedure TLogger.WriteHint(Hint: string);
begin
  WriteLn(FLog, Format('[Hint][%s]%s', [DateTimeToStr(Now), Hint]));
  Flush(FLog);
end;

procedure TLogger.WriteWarning(Warning: string);
begin
  WriteLn(FLog, Format('[Warning][%s]%s', [DateTimeToStr(Now), Warning]));
  Flush(FLog);
end;

procedure TLogger.WriteStatus(Status: string);
begin
  WriteLn(FLog, Format('[%s]%s', [DateTimeToStr(Now), Status]));
  Flush(FLog);
end;

{ TTBCore }

procedure TTBCore.Execute;
begin
  FConfig := ReadConfig(SConfPath);
  { TODO : Thread Loop }
  WriteConfig(SConfPath, FConfig);
end;

constructor TTBCore.Create(AOnTerminate: TNotifyEvent);
begin
  FreeOnTerminate := True;
  OnTerminate := AOnTerminate;
  FLogger := TLogger.Create;

  inherited Create(False);
end;

destructor TTBCore.Destroy;
begin
  FLogger.Free;
  inherited Destroy;
end;

end.
