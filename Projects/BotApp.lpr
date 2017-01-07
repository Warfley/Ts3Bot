program BotApp;

{$mode objfpc}{$H+}
{$Define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, tb.Core
  { you can add units after this };

var Bot: TTBCore;

procedure ThreadStopped(Sender:TObject);
begin
  Bot:=nil;
end;

var
  m: TMethod;

begin
  {$IfDef DEBUG}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$EndIf}
  m.Code:=@ThreadStopped;
  m.Data:=Nil;
  Bot:=TTBCore.Create(TNotifyEvent(m));
  while Assigned(Bot) and not Bot.Finished do Sleep(200);
  if Assigned(Bot) and Bot.Finished then FreeAndNil(Bot);
  ReadLn;
end.

