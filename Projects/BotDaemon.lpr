Program BotDaemon;

{$Define UseCThreads}

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, sysutils, lazdaemonapp, TsBotD.DaemonMapper, TsBotD.BotDaemon, TsBot.core, indylaz,
  TsLib.Connection, Logger
  { add your units here };

begin
  {$IfDef DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$EndIf}
  Application.Title:='Daemon application';
  Application.Initialize;
  Application.Run;
end.
