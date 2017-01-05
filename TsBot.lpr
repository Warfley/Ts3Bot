Program TsBot;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, tb.DaemonMapper, tb.BotDaemon
  { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
