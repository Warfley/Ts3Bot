unit TsLib.Types;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, fgl, gvector;

type
  TRecieveEvent = function(Sender: TObject; Data: string;
    var RemoveFromList: boolean): boolean of object;

  TRecieveList = specialize TFPGList<TRecieveEvent>;

  {$Include include/NotificationTypes.inc}

  TIntegerList = specialize TFPGList<Integer>;

  TStatusResponse = record
    ErrNo: integer;
    Msg: string;
  end;


function GetNotificationType(Str: String): TNotificationType;
implementation

function GetNotificationType(Str: String): TNotificationType;
begin
  if Str='notifytextmessage' then
    Result:=ntTextMessage
  else if Str='notifyserveredited' then
    Result:=ntServerEdited
  else if Str='notifyclientmoved' then
    Result:=ntClientMoved
  else if Str='notifyclientleftview' then
    Result:=ntClientDisconnected
  else if Str='notifycliententerview' then
    Result:=ntClientConnected
  else if Str='notifychanneledited' then
    Result:=ntChannelEdited
  else if Str='notifychanneldescriptionchanged' then
    Result:=ntChanneldescriptionChanged;
end;

end.

