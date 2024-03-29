unit TsBotUI.Types;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, gvector;

type
  TCommandType = (ctQuit, ctRestart, ctSetConnectionData, ctSwitchServer, ctGetConnectionData,
                 ctChangeLogPath, ctGetLogPath, ctResetConfig, ctGetAntiflood, ctSetAntiflood,
                 ctModuleList, ctEnableModule, ctDisableModule, ctGetModuleConfig,
                 ctGetModuleConfData, ctConfigModule);

  PConnectionData = ^TConnectionData;

  TConnectionData = record
    IP: string;
    Port: integer;
    ServerID: integer;
    UserName: string;
    Password: string;
  end;

  PAntiFloodInfo = ^TAntiFloodInfo;

  TAntiFloodInfo = record
    Time, Commands: Integer;
  end;

  TCommandFinishedEvent = procedure(Sender: TObject; Command: TCommandType;
    Status: boolean) of object;

  TCommandEventData = record
    CommandType: TCommandType;
    Data: PtrInt;
    OnFinished: TCommandFinishedEvent;
  end;

  TCommandEventMethod = procedure(Cmd: TCommandEventData) of object;
  TCommandEvent = procedure(Cmd: TCommandEventData);


  TCommandList = specialize TVector<TCommandEventData>;

implementation

end.
