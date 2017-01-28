unit TsLib.Types;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, fgl, gvector;

type

  {$Region Server Types}

  TOperatingSystem = (osLinux, osWindows, osMacOS, osOther);
  TBannerMode = (bmNoAdjust=0, bmStretch, bmScale);
  TMessageMode = (mmNone=0, mmLog, mmModal, mmModalQuit);
  TCodecEncryptionMode = (cmIndividual = 0, cmDisabled, cmEnabled);

  TConnectionData = record
    TransferRateOut: Integer;
    TransferRateIn: Integer;
    PaketsOut: UInt64;
    PaketsIn: UInt64;
    BytesIn: UInt64;
    BytesOut: UInt64;
    BandwidthSecOut: Integer;
    BandwidthSecIn: Integer;
    BandwidthMinOut: Integer;
    BandwidthMinIn: Integer
  end;

  TServerData = record
    UID: string;
    ID: Integer;
    MachineID: Integer;
    Name: string;
    PhoneticName: String;
    WelcomeMessage: string;
    IP: String;
    Platform: TOperatingSystem;
    HasPassword: Boolean;
    Password: String;
    MaxClients: Integer;
    Clients: Integer;
    QueryClients: Integer;
    Channels: Integer;
    CreationDate: TDateTime;
    Uptime: Cardinal;
    HostMessage: String;
    MessageMode: Boolean;
    DefaultSGroup: Integer;
    DefaultCGroup: Integer;
    DefaultCAdmin: Integer;
    MaxDLRate: Integer;
    DLQuota: Integer;
    MaxULRate: Integer;
    ULQuota: Integer;
    BannerURL: String;
    BannerImage: String;
    HBTooltip: String;
    HBImage: String;
    HBUrl: String;
    IconID: Integer;
    BannerMode: TBannerMode;
    BannerUpdateInterval: Integer;
    ComplainBanCount: Integer;
    AutobanTime: Integer;
    ComplainRemoveTime: Integer;
    ForceSilenceMinRequried: Integer;
    PriorityDim: Integer;
    AFTickReduce: Integer;
    AFBlockCount: Integer;
    AFIPBlockCount: Integer;
    PriviledgeKeyUnused: Boolean;
    BytesDLTotal: UInt64;
    BytesDLMonth: UInt64;
    BytesULTotal: UInt64;
    BytesULMonth: UInt64;
    PacketLossSpeech: Double;
    PacketLossKA: Double;
    PacketLossControl: Double;
    PacketLossTotal: Double;
    Ping: Integer;
    UDPPort: Word;
    Autostart: Boolean;
    Connection: TConnectionData;
    Status: String;
    LogClients: Boolean;
    LogQuerys: Boolean;
    LogChannels: Boolean;
    LogPermissions: Boolean;
    LogServer: Boolean;
    LogFileTransfers: Boolean;
    MinClientVersion: Integer;
    ReservedSlots: Integer;
    WebList: Boolean;
    FileBase: String;
    CodecEncryptionMode: TCodecEncryptionMode;
  end;

  {$EndRegion}

  {$Region Channel Types}

  TCodec = (ccSpeexNarrowband = 0, ccSpeexWideband, ccSpeexUltrawideband, ccCeltecMono);

  TChannelData = record
    Name: String;
    ID: Integer;
    ParentID: Integer;
    Description: String;
    Topic: String;
    HasPassword: Boolean;
    Password: String;
    PhoneticName: String;

    Codec: TCodec;
    CodecQuality: Integer;

    MaxClients: Integer;
    MaxClientsFamily: Integer;

    IsPermament: Boolean;
    IsSemipermament: Boolean;
    IsTemporary: Boolean;
    IsDefault: Boolean;

    IsUnlimited: Boolean;
    IsFamilyUnlimited: Boolean;
    Order: Integer;

    Talkpower: Integer;

    FilePath: String;

    IsSilenced: Boolean;
    Icon: String;

    Unencrypted: Boolean;
  end;

  {$EndRegion}

  {$Region Client Types}

  TFlagAvatar = string[32];

  TDynIntArray = array of integer;

  TClientData = record
    ID: integer;
    UID: string;
    Name: string;
    MutedInput: boolean;
    MutedOutput: boolean;
    MutedOutputOnly: boolean;
    InputHardware: boolean;
    OutputHardware: boolean;
    MetaData: string;
    Recording: boolean;
    DatabaseID: integer;
    ChannelGroup: integer;
    ServerGroups: TDynIntArray;
    IsAway: boolean;
    AfkMessage: string;
    CType: integer;
    FlagAvatar: TFlagAvatar;
    TalkPower: integer;
    TalkRequest: integer;
    TalkRequestMessage: string;
    Description: string;
    IsTalker: boolean;
    IsPrioritySpeaker: boolean;
    UnreadMessages: integer;
    PhoneticNickname: string;
    NeededServerQuerryViewPower: integer;
    IconID: integer;
    IsChannelCommander: boolean;
    Country: string;
    ChannelGroupInheritedChannel: integer;
    Badges: string;
  end;

  {$EndRegion}



  {$Region Notification Types}

  TRecieveEvent = function(Sender: TObject; Data: string;
    var RemoveFromList: boolean): boolean of object;

  TRecieveList = specialize TFPGList<TRecieveEvent>;

  {$Include include/NotificationTypes.inc}

  {$EndRegion}


  TIntegerList = specialize TFPGList<integer>;

  TStatusResponse = record
    ErrNo: integer;
    Msg: string;
  end;

operator := (Str: string) Client: TClientData;
procedure SetClientData(Name: String; Value: String; var Client: TClientData);

operator := (Str: string) Server: TServerData;
procedure SetServerData(Name: String; Value: String; var Server: TServerData);

operator := (Str: string) Channel: TChannelData;
procedure SetChannelData(Name: String; Value: String; var Channel: TChannelData);

function GetNotificationType(Str: string): TNotificationType;

implementation

uses TsLib.ValueRead;

operator := (Str: string) Client: TClientData;
var
  sl: TStringList;
begin
  Finalize(Client);
  FillChar(Client,SizeOf(TClientData), 0);
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetClientData(sl[i], '', Client)
      else
        SetClientData(sl.Names[i], sl.ValueFromIndex[i], Client);
  finally
    sl.Free;
  end;
end;

procedure SetClientData(Name: String; Value: String; var Client: TClientData);
begin
  with Client do
    if Name = 'clid' then
      ReadValue(Value, ID)
    else if Name = 'client_unique_identifier' then
      ReadValue(Value, UID)
    else if Name = 'client_nickname' then
      ReadValue(Value, Name)
    else if Name = 'client_input_muted' then
      ReadValue(Value, MutedInput)
    else if Name = 'client_output_muted' then
      ReadValue(Value, MutedOutput)
    else if Name = 'client_outputonly_muted' then
      ReadValue(Value, MutedOutputOnly)
    else if Name = 'client_input_hardware' then
      ReadValue(Value, InputHardware)
    else if Name = 'client_output_hardware' then
      ReadValue(Value, OutputHardware)
    else if Name = 'client_meta_data' then
      ReadValue(Value, MetaData)
    else if Name = 'client_is_recording' then
      ReadValue(Value, Recording)
    else if Name = 'client_database_id' then
      ReadValue(Value, DatabaseID)
    else if Name = 'client_channel_group_id' then
      ReadValue(Value, ChannelGroup)
    else if Name = 'client_servergroups' then
      ReadValue(Value, ServerGroups)
    else if Name = 'client_away' then
      ReadValue(Value, IsAway)
    else if Name = 'client_away_message' then
      ReadValue(Value, AfkMessage)
    else if Name = 'client_type' then
      ReadValue(Value, CType)
    else if Name = 'client_flag_avatar' then
      ReadValue(Value, FlagAvatar)
    else if Name = 'client_talk_power' then
      ReadValue(Value, TalkPower)
    else if Name = 'client_talk_request' then
      ReadValue(Value, TalkRequest)
    else if Name = 'client_talk_request_msg' then
      ReadValue(Value, TalkRequestMessage)
    else if Name = 'client_description' then
      ReadValue(Value, Description)
    else if Name = 'client_is_talker' then
      ReadValue(Value, IsTalker)
    else if Name = 'client_is_priority_speaker' then
      ReadValue(Value, IsPrioritySpeaker)
    else if Name = 'client_unread_messages' then
      ReadValue(Value, UnreadMessages)
    else if Name = 'client_nickname_phonetic' then
      ReadValue(Value, PhoneticNickname)
    else if Name = 'client_needed_serverquery_view_power' then
      ReadValue(Value, NeededServerQuerryViewPower)
    else if Name = 'client_icon_id' then
      ReadValue(Value, IconID)
    else if Name = 'client_is_channel_commander' then
      ReadValue(Value, IsChannelCommander)
    else if Name = 'client_country' then
      ReadValue(Value, Country)
    else if Name = 'client_channel_group_inherited_channel_id' then
      ReadValue(Value,
        ChannelGroupInheritedChannel)
    else if Name = 'client_badges' then
      ReadValue(Value, Badges);
end;

operator := (Str: string) Server: TServerData;
var
  sl: TStringList;
begin
  Finalize(Server);
  FillChar(Client,SizeOf(TServerData), 0);
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetClientData(sl[i], '', Server)
      else
        SetServerData(sl.Names[i], sl.ValueFromIndex[i], Server);
  finally
    sl.Free;
  end;
end;

procedure SetServerData(Name: String; Value: String; var Server: TClientData);
begin
  with Server do
    if Name = 'virtualserver_unique_identifier' then
      ReadValue(Value, UID)
      { TODO : Finish }
end;


operator := (Str: string) Channel: TChannelData;
var
  sl: TStringList;
begin
  Finalize(Channel);
  FillChar(Channel,SizeOf(Channel), 0);
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetClientData(sl[i], '', Channel)
      else
        SetChannelData(sl.Names[i], sl.ValueFromIndex[i], Channel);
  finally
    sl.Free;
  end;
end;

procedure SetChannelData(Name: String; Value: String; var Channel: TChannelData);
begin
  with Channel do
    if Name = 'cid' then
      ReadValue(Value, ID)
      { TODO : Finish }
end;

function GetNotificationType(Str: string): TNotificationType;
begin
  if Str = 'notifytextmessage' then
    Result := ntTextMessage
  else if Str = 'notifyserveredited' then
    Result := ntServerEdited
  else if Str = 'notifyclientmoved' then
    Result := ntClientMoved
  else if Str = 'notifyclientleftview' then
    Result := ntClientDisconnected
  else if Str = 'notifycliententerview' then
    Result := ntClientConnected
  else if Str = 'notifychanneledited' then
    Result := ntChannelEdited
  else if Str = 'notifychanneldescriptionchanged' then
    Result := ntChanneldescriptionChanged;
end;

end.
