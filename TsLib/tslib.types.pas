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

  TNotificationType = (ntChannelMv=0, ntServer=1, ntChannelMessage=2, ntPersonalMessage=3, ntServerMessage=4);

  TNotificationEventType = (netChannel=0, netServer=1, netChannelMessage=2, netPersonalMessage=3, netServerMessage=4);

  TNotificationEvent = procedure(Sender: TObject; AType: TNotificationType;
    AData: TStringList);

  TNotifyEventList = specialize TFPGList<TNotificationEvent>;

  TNotifyEventMap = array[Low(TNotificationEventType)..High(TNotificationEventType)]
    of TNotifyEventList;

  TNotificationData = record
    NType: TNotificationType;
    Data: String;
  end;

  TNotificationList = specialize TVector<TNotificationData>;

  TIntegerList = specialize TFPGList<Integer>;

  TStatusResponse = record
    ErrNo: integer;
    Msg: string;
  end;


function GetNotificationType(Str: String): TNotificationType;
implementation

function GetNotificationType(Str: String): TNotificationType;
begin
  if Str='notifyclientmoved' then
    Result:=ntChannelMv;
end;

end.

