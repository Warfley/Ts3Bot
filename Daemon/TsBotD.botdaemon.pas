unit TsBotD.botdaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp, TsBot.Core;

type

  { TBotDaemon }

  TBotDaemon = class(TDaemon)
    procedure DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: boolean);
  private
    FThread: TTBCore;
    procedure ThreadStopped(Sender: TObject);
    { private declarations }
  public
    { public declarations }
  end;

var
  BotDaemon: TBotDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TBotDaemon);
end;

{$R *.lfm}

{ TBotDaemon }

procedure TBotDaemon.DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
begin
  OK:=Assigned(FThread);
  if OK then
    FThread.Resume;
end;

procedure TBotDaemon.DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
begin
  OK:=Assigned(FThread);
  if OK then
    FThread.Suspend;
end;

procedure TBotDaemon.DataModuleShutDown(Sender: TCustomDaemon);
var ok:Boolean;
begin
  DataModuleStop(Sender, ok);
end;

procedure TBotDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
begin
  OK := FThread = nil;
  if OK then
    FThread := TTBCore.Create(@ThreadStopped, './config');
end;

procedure TBotDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: boolean);
var
  I: Integer;
begin
  OK := FThread = nil;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    I := 0;
    // Wait at most 5 seconds.
    while (FThread <> nil) and (I < 50) do
    begin
      Sleep(100);
      ReportStatus;
    end;
    // Let the thread die silently.
    OK := FThread = nil;
    if (FThread <> nil) then
    begin
      FThread.OnTerminate := nil;
      FThread := nil;
    end;
  end;
end;

procedure TBotDaemon.ThreadStopped(Sender: TObject);
begin
  FThread := nil;
end;

{ TBotDaemon }


initialization
  RegisterDaemon;
end.

