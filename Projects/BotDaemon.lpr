Program BotDaemon;

{$Define UseCThreads}

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, sysutils, lazdaemonapp, tb.DaemonMapper, tb.BotDaemon, tb.core, indylaz,
  ts.Connection, Logger
  { add your units here };

begin
  {$IfDef DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$EndIf}
  Application.Title:='Teamspeak Bot Daemon';
  Application.Initialize;
  Application.Run;
end.
