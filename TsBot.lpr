Program TsBot;

{$Define UseCThreads}

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, tb.DaemonMapper, tb.BotDaemon, tb.core
  { add your units here };

begin
  Application.Title:='Daemon application';
  Application.Initialize;
  Application.Run;
end.
