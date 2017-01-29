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
    FileBandwidthIn: Integer;
    FileBandwidthOut: Integer;
    PacketsOutSpeech: UInt64;
    BytesOutSpeech: UInt64;
    PacketsInSpeech: UInt64;
    BytesInSpeech: UInt64;
    PacketsOutKeepAlive: UInt64;
    BytesOutKeepAlive: UInt64;
    PacketsInKeepAlive: UInt64;
    BytesInKeepAlive: UInt64;
    PacketsOutControl: UInt64;
    BytesOutControl: UInt64;
    PacketsInControl: UInt64;
    BytesInControl: UInt64;
    PacketsOut: UInt64;
    BytesOut: UInt64;
    PacketsIn: UInt64;
    BytesIn: UInt64;
    BandwidthSecOut: Integer;
    BandwidthSecIn: Integer;
    BandwidthMinOut: Integer;
    BandwidthMinIn: Integer;
  end;

  TServerData = record
    UID: string;
    ID: Integer;
    MachineID: Integer;
    Name: string;
    Version: String;
    PhoneticName: String;
    WelcomeMessage: string;
    IP: String;
    Platform: TOperatingSystem;
    HasPassword: Boolean;
    Password: String;
    MaxClients: Integer;
    Clients: Integer;
    QueryConnections: Integer;
    QueryClients: Integer;
    Channels: Integer;
    CreationDate: TDateTime;
    Uptime: Cardinal;
    HostMessage: String;
    MessageMode: Boolean;
    DefaultSGroup: Integer;
    DefaultCGroup: Integer;
    DefaultCAGroup: Integer;
    MaxDLBandwidth: Int64;
    DLQuota: Int64;
    MaxULBandwith: Int64;
    ULQuota: Int64;
    BannerURL: String;
    BannerImage: String;
    HostButtonTooltip: String;
    HostButtonImage: String;
    HostButtonUrl: String;
    IconID: Integer;
    BannerMode: TBannerMode;
    BannerUpdateInterval: Integer;
    ComplainBanCount: Integer;
    ComplainBanTime: Integer;
    ComplainRemoveTime: Integer;
    MinClientsForcedSilence: Integer;
    PriorityDim: Integer;
    AFTickReduce: Integer;
    AFBlockCount: Integer;
    AFIPBlockCount: Integer;
    PriviledgeKeyUnused: Boolean;
    BytesDLTotal: UInt64;
    BytesDLMonth: UInt64;
    BytesULTotal: UInt64;
    BytesULMonth: UInt64;
    Port: Integer;
    PacketLossSpeech: Double;
    PacketLossKeepalive: Double;
    PacketLossControl: Double;
    PacketLossTotal: Double;
    Ping: Integer;
    UDPPort: Word;
    Autostart: Boolean;
    SecurityLevelRequired: Integer;
    Connection: TConnectionData;
    Status: String;
    LogClients: Boolean;
    LogQuerys: Boolean;
    LogChannels: Boolean;
    LogPermissions: Boolean;
    LogServer: Boolean;
    LogFileTransfers: Boolean;
    DefaultTempChannelDeletionTime: Integer;
    MinClientVersion: Integer;
    ReservedSlots: Integer;
    WebList: Boolean;
    FileBase: String;
    CodecEncryptionMode: TCodecEncryptionMode;
    MinAndroidVersion: Integer;
    MinIOSVersion: Integer;
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

operator := (Str: string) Connection: TConnectionData;
procedure SetConnectionData(Name: String; Value: String; var Connection: TConnectionData);

operator := (Str: string) Channel: TChannelData;
procedure SetChannelData(Name: String; Value: String; var Channel: TChannelData);

function GetNotificationType(Str: string): TNotificationType;

implementation

uses TsLib.ValueRead;

operator := (Str: string) Client: TClientData;
var
  sl: TStringList;
  i: Integer;
begin
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
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetServerData(sl[i], '', Server)
      else
        SetServerData(sl.Names[i], sl.ValueFromIndex[i], Server);
    Server.Connection := Str ;
  finally
    sl.Free;
  end;
end;

procedure SetServerData(Name: String; Value: String; var Server: TServerData);
begin
  with Server do
    if Name = 'virtualserver_unique_identifier' then
      ReadValue(Value, UID)
    else if Name = 'virtualserver_name' then
      ReadValue(Value, Name)
    else if Name = 'virtualserver_welcomemessage' then
      ReadValue(Value, WelcomeMessage)
    else if Name = 'virtualserver_platform' then
      ReadValue(Value, Platform)
    else if Name = 'virtualserver_version' then
      ReadValue(Value, Version)
    else if Name = 'virtualserver_maxclients' then
      ReadValue(Value, MaxClients)
    else if Name = 'virtualserver_password' then
      ReadValue(Value, Password)
    else if Name = 'virtualserver_clientsonline' then
      ReadValue(Value, Clients)
    else if Name = 'virtualserver_channelsonline' then
      ReadValue(Value, Channels)
    else if Name = 'virtualserver_created' then
      ReadValue(Value, CreationDate)
    else if Name = 'virtualserver_uptime' then
      ReadValue(Value, Uptime)
    else if Name = 'virtualserver_codec_encryption_mode' then
      ReadValue(Value, CodecEncryptionMode)
    else if Name = 'virtualserver_hostmessage' then
      ReadValue(Value, HostMessage)
    else if Name = 'virtualserver_hostmessage_mode' then
      ReadValue(Value, MessageMode)
    else if Name = 'virtualserver_filebase' then
      ReadValue(Value, FileBase)
    else if Name = 'virtualserver_default_server_group' then
      ReadValue(Value, DefaultSGroup)
    else if Name = 'virtualserver_default_channel_group' then
      ReadValue(Value, DefaultCGroup)
    else if Name = 'virtualserver_flag_password' then
      ReadValue(Value, HasPassword)
    else if Name = 'virtualserver_default_channel_admin_group' then
      ReadValue(Value, DefaultCAGroup)
    else if Name = 'virtualserver_max_download_total_bandwidth' then
      ReadValue(Value, MaxDLBandwidth)
    else if Name = 'virtualserver_max_upload_total_bandwidth' then
      ReadValue(Value, MaxULBandwith)
    else if Name = 'virtualserver_hostbanner_url' then
      ReadValue(Value, BannerURL)
    else if Name = 'virtualserver_hostbanner_gfx_url' then
      ReadValue(Value, BannerImage)
    else if Name = 'virtualserver_hostbanner_gfx_interval' then
      ReadValue(Value, BannerUpdateInterval)
    else if Name = 'virtualserver_complain_autoban_count' then
      ReadValue(Value, ComplainBanCount)
    else if Name = 'virtualserver_complain_autoban_time' then
      ReadValue(Value, ComplainBanTime)
    else if Name = 'virtualserver_complain_remove_time' then
      ReadValue(Value, ComplainRemoveTime)
    else if Name = 'virtualserver_min_clients_in_channel_before_forced_silence' then
      ReadValue(Value, MinClientsForcedSilence)
    else if Name = 'virtualserver_priority_speaker_dimm_modificator' then
      ReadValue(Value, PriorityDim)
    else if Name = 'virtualserver_id' then
      ReadValue(Value, ID)
    else if Name = 'virtualserver_antiflood_points_tick_reduce' then
      ReadValue(Value, AFTickReduce)
    else if Name = 'virtualserver_antiflood_points_needed_command_block' then
      ReadValue(Value, AFBlockCount)
    else if Name = 'virtualserver_antiflood_points_needed_ip_block' then
      ReadValue(Value, AFIPBlockCount)
    else if Name = 'virtualserver_client_connections' then
      ReadValue(Value, Clients)
    else if Name = 'virtualserver_query_client_connections' then
      ReadValue(Value, QueryConnections)
    else if Name = 'virtualserver_hostbutton_tooltip' then
      ReadValue(Value, HostButtonTooltip)
    else if Name = 'virtualserver_hostbutton_url' then
      ReadValue(Value, HostButtonUrl)
    else if Name = 'virtualserver_hostbutton_gfx_url' then
      ReadValue(Value, HostButtonImage)
    else if Name = 'virtualserver_queryclientsonline' then
      ReadValue(Value, QueryClients)
    else if Name = 'virtualserver_download_quota' then
      ReadValue(Value, DLQuota)
    else if Name = 'virtualserver_upload_quota' then
      ReadValue(Value, ULQuota)
    else if Name = 'virtualserver_month_bytes_downloaded' then
      ReadValue(Value, BytesDLMonth)
    else if Name = 'virtualserver_month_bytes_uploaded' then
      ReadValue(Value, BytesULMonth)
    else if Name = 'virtualserver_total_bytes_downloaded' then
      ReadValue(Value, BytesDLTotal)
    else if Name = 'virtualserver_total_bytes_uploaded' then
      ReadValue(Value, BytesULTotal)
    else if Name = 'virtualserver_port' then
      ReadValue(Value, Port)
    else if Name = 'virtualserver_autostart' then
      ReadValue(Value, Autostart)
    else if Name = 'virtualserver_machine_id' then
      ReadValue(Value, MachineID)
    else if Name = 'virtualserver_needed_identity_security_level' then
      ReadValue(Value, SecurityLevelRequired)
    else if Name = 'virtualserver_log_client' then
      ReadValue(Value, LogClients)
    else if Name = 'virtualserver_log_query' then
      ReadValue(Value, LogQuerys)
    else if Name = 'virtualserver_log_channel' then
      ReadValue(Value, LogChannels)
    else if Name = 'virtualserver_log_permissions' then
      ReadValue(Value, LogPermissions)
    else if Name = 'virtualserver_log_server' then
      ReadValue(Value, LogServer)
    else if Name = 'virtualserver_log_filetransfer' then
      ReadValue(Value, LogFileTransfers)
    else if Name = 'virtualserver_min_client_version' then
      ReadValue(Value, MinClientVersion)
    else if Name = 'virtualserver_name_phonetic' then
      ReadValue(Value, PhoneticName)
    else if Name = 'virtualserver_icon_id' then
      ReadValue(Value, IconID)
    else if Name = 'virtualserver_reserved_slots' then
      ReadValue(Value, ReservedSlots)
    else if Name = 'virtualserver_total_packetloss_speech' then
      ReadValue(Value, PacketLossSpeech)
    else if Name = 'virtualserver_total_packetloss_keepalive' then
      ReadValue(Value, PacketLossKeepalive)
    else if Name = 'virtualserver_total_packetloss_control' then
      ReadValue(Value, PacketLossControl)
    else if Name = 'virtualserver_total_packetloss_total' then
      ReadValue(Value, PacketLossTotal)
    else if Name = 'virtualserver_total_ping' then
      ReadValue(Value, Ping)
    else if Name = 'virtualserver_ip' then
      ReadValue(Value, IP)
    else if Name = 'virtualserver_weblist_enabled' then
      ReadValue(Value, WebList)
    else if Name = 'virtualserver_ask_for_privilegekey' then
      ReadValue(Value, PriviledgeKeyUnused)
    else if Name = 'virtualserver_hostbanner_mode' then
      ReadValue(Value, BannerMode)
    else if Name = 'virtualserver_channel_temp_delete_delay_default' then
      ReadValue(Value, DefaultTempChannelDeletionTime)
    else if Name = 'virtualserver_min_android_version' then
      ReadValue(Value, MinAndroidVersion)
    else if Name = 'virtualserver_min_ios_version' then
      ReadValue(Value, MinIOSVersion)
    else if Name = 'virtualserver_status' then
      ReadValue(Value, Status);
end;

operator := (Str: string) Connection: TConnectionData;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetConnectionData(sl[i], '', Connection)
      else
        SetConnectionData(sl.Names[i], sl.ValueFromIndex[i], Connection);
  finally
    sl.Free;
  end;
end;

procedure SetConnectionData(Name: String; Value: String; var Connection: TConnectionData);
begin
  with Connection do
    if Name = 'connection_filetransfer_bandwidth_sent' then
      ReadValue(Value, FileBandwidthOut)
    else if Name = 'connection_filetransfer_bandwidth_received' then
      ReadValue(Value, FileBandwidthIn)
    else if Name = 'connection_filetransfer_bytes_sent_total' then
      ReadValue(Value, BytesOut)
    else if Name = 'connection_filetransfer_bytes_received_total' then
      ReadValue(Value, BytesIn)
    else if Name = 'connection_packets_sent_speech' then
      ReadValue(Value, PacketsOutSpeech)
    else if Name = 'connection_bytes_sent_speech' then
      ReadValue(Value, BytesOutSpeech)
    else if Name = 'connection_packets_received_speech' then
      ReadValue(Value, PacketsInSpeech)
    else if Name = 'connection_bytes_received_speech' then
      ReadValue(Value, BytesInSpeech)
    else if Name = 'connection_packets_sent_keepalive' then
      ReadValue(Value, PacketsOutKeepAlive)
    else if Name = 'connection_bytes_sent_keepalive' then
      ReadValue(Value, BytesOutKeepAlive)
    else if Name = 'connection_packets_received_keepalive' then
      ReadValue(Value, PacketsInKeepAlive)
    else if Name = 'connection_bytes_received_keepalive' then
      ReadValue(Value, BytesInKeepAlive)
    else if Name = 'connection_packets_sent_control' then
      ReadValue(Value, PacketsOutControl)
    else if Name = 'connection_bytes_sent_control' then
      ReadValue(Value, PacketsInControl)
    else if Name = 'connection_packets_received_control' then
      ReadValue(Value, FileBandwidthOut)
    else if Name = 'connection_bytes_received_control' then
      ReadValue(Value, BytesInControl)
    else if Name = 'connection_packets_sent_total' then
      ReadValue(Value, PacketsOut)
    else if Name = 'connection_bytes_sent_total' then
      ReadValue(Value, BytesOut)
    else if Name = 'connection_packets_received_total' then
      ReadValue(Value, PacketsIn)
    else if Name = 'connection_bytes_received_total' then
      ReadValue(Value, BytesIn)
    else if Name = 'connection_bandwidth_sent_last_second_total' then
      ReadValue(Value, BandwidthSecOut)
    else if Name = 'connection_bandwidth_sent_last_minute_total' then
      ReadValue(Value, BandwidthMinOut)
    else if Name = 'connection_bandwidth_received_last_second_total' then
      ReadValue(Value, BandwidthSecIn)
    else if Name = 'connection_bandwidth_received_last_minute_total' then
      ReadValue(Value, BandwidthMinIn);
end;

operator := (Str: string) Channel: TChannelData;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetChannelData(sl[i], '', Channel)
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
