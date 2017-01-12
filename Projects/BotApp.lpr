program BotApp;

{$mode objfpc}{$H+}
{$Define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, TsBot.core, TsLib.NotificationManager, TsLib.Types,
TsBotUI.CLI, TsBotUI.Types, Logger;

var Bot: TTBCore;

procedure ThreadStopped(Sender:TObject);
begin
  Bot:=nil;
end;

procedure CommandEntered(Cmd: TCommandEventData);
begin
  if Assigned(Bot) then
    Bot.RegisterCommand(Cmd);
end;

var
  m: TMethod;
  cli: TCommandLineInterface;

begin
  {$IfDef DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$EndIf}
  CreateDebugLogger;

  // Create command line interface
  cli:=TCommandLineInterface.Create(@CommandEntered);
  // Thread stopped event
  m.Code:=@ThreadStopped;
  m.Data:=Nil;
  // Create Bot
  Bot:=TTBCore.Create(TNotifyEvent(m), './config', False);
  // Run Bot
  bot.WaitFor;
  cli.Terminate;
  cli.WaitFor;
  DestroyDebugLogger;
  {$If defined(DEBUG) AND defined(Windows)}
  ReadLn;
  {$EndIf}
end.

