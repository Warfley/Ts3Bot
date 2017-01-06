Program TsBot;

{$Define UseCThreads}

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, tb.DaemonMapper, tb.BotDaemon, tb.core, indylaz,
  ts.Connection
  { add your units here };

begin
  Application.Title:='Teamspeak Bot Daemon';
  Application.Initialize;
  Application.Run;
end.
