unit TsBotUI.Types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector;

type
  TCommandType = (ctQuit, ctSetConnectionData);

  PConnectionData = ^TConnectionData;
  TConnectionData = record
    IP: String;
    Port: Integer;
    ServerID: Integer;
    UserName: String;
    Password: String;
  end;


  TCommandEvent = procedure(CommandType: TCommandType; Data: PtrInt) of object;

  TCommandEventData = record
    CommandType: TCommandType;
    Data: PtrInt;
  end;

implementation

end.

