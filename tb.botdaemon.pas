unit tb.BotDaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type

  { TBotDaemon }

  TBotDaemon = class(TDaemon)
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  BotDaemon: TBotDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TBotDaemon)
end;

{$R *.lfm}

{ TBotDaemon }

procedure TBotDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin

end;


initialization
  RegisterDaemon;
end.

