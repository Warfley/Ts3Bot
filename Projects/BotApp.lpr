program BotApp;

{$mode objfpc}{$H+}
{$Define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, TsBot.core, TsLib.NotificationManager, TsLib.Types,
TsBotUI.CLI, TsBotUI.Types;

var Bot: TTBCore;

procedure ThreadStopped(Sender:TObject);
begin
  Bot:=nil;
end;

procedure CommandEntered(CommandType: TCommandType; Data: PtrInt);
begin
  if CommandType=ctQuit then
      Bot.Terminate;
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

  // Command line event
  m.Code:=@CommandEntered;
  m.Data:=Nil;
  // Create command line interface
  cli:=TCommandLineInterface.Create(TCommandEvent(m));
  // Thread stopped event
  m.Code:=@ThreadStopped;
  m.Data:=Nil;
  // Create Bot
  Bot:=TTBCore.Create(TNotifyEvent(m), './config');
  // Run Bot
  bot.WaitFor;
  cli.Terminate;
  {$If defined(DEBUG) AND defined(Windows)}
  ReadLn;
  {$EndIf}
end.

