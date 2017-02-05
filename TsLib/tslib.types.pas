unit TsLib.Types;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, fgl, gvector;

type

  {$Region Groups}

  TServerGroup = record
    ID: Integer;
    Name: String;
    GroupType: Integer;
    IconID: Integer;
    SaveDB: Boolean;
    SortID: Integer;
    NameMode: Integer;
    ModifyP: Integer;
    MemberAddP: Integer;
    MemberRemoveP: Integer;
  end;

  TServerGroupList = specialize TVector<TServerGroup>;

  TChannelGroup = record
    ID: Integer;
    Name: String;
    GroupType: Integer;
    IconID: Integer;
    SaveDB: Boolean;
    SortID: Integer;
    NameMode: Integer;
    ModifyP: Integer;
    MemberAddP: Integer;
    MemberRemoveP: Integer;
  end;

  TChannelGroupList = specialize TVector<TChannelGroup>;

  {$EndRegion}

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
    Uptime: UInt64;
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

  TChannelUpdate = (cuTopic, cuFlags, cuVoice, cuLimits, cuIcon, cuSecondsEmpty);
  TChannelUpdates = set of TChannelUpdate;

  TChannelData = record
    Name: String;
    ID: Integer;
    ParentID: Integer;
    Description: String;
    Topic: String;
    HasPassword: Boolean;
    Password: String;
    PasswordSalt: String;
    PhoneticName: String;

    Codec: TCodec;
    CodecQuality: Integer;
    CodecLatencyFactor: Integer;

    MaxClients: Integer;
    MaxClientsFamily: Integer;
    FamilyClients: Integer;
    Clients: Integer;
    DeleteDelay: Integer;

    IsPermament: Boolean;
    IsSemipermament: Boolean;
    IsTemporary: Boolean;
    IsDefault: Boolean;
    IsPrivate: Boolean;

    IsUnlimited: Boolean;
    IsFamilyUnlimited: Boolean;
    Order: Integer;
    IsMaxClientsInherited: Integer;

    Talkpower: Integer;
    SubscribePower: Integer;

    FilePath: String;

    IsSilenced: Boolean;
    IconID: Integer;

    Unencrypted: Boolean;
    SecondsEmpty: Integer;
  end;

  {$EndRegion}

  {$Region Client Types}

  TFlagAvatar = string[32];

  TDynIntArray = array of integer;

  TClientUpdate = (clUID, clAway, clVoice, clTimes, clGroups, clInfo, clCountry, clIP, clBadges);
  TClientUpdates = set of TClientUpdate;

  TClientData = record
    ID: integer;
    ChannelID: Integer;
    UID: string;
    UIDHash: string;
    Name: string;
    LoginName: string;
    IsTalking: Boolean;
    MutedInput: boolean;
    MutedOutput: boolean;
    MutedOutputOnly: boolean;
    InputHardware: Integer;
    OutputHardware: Integer;
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
    DefaultChannel: String;
    IsPrioritySpeaker: boolean;
    UnreadMessages: integer;
    PhoneticNickname: string;
    NeededServerQuerryViewPower: integer;
    IconID: integer;
    IsChannelCommander: boolean;
    Version: String;
    VersionHash: String;
    SecurityHash: String;
    Country: string;
    IP: String;
    TotalConnections: Integer;
    BytesDLTotal: UInt64;
    BytesDLMonth: UInt64;
    BytesULTotal: UInt64;
    BytesULMonth: UInt64;
    CreationDate: TDateTime;
    LastConnectionDate: TDateTime;
    IdleTime: QWord;
    Platform: TOperatingSystem;
    DefaultToken: String;
    ChannelGroupInheritedChannel: integer;
    Badges: string;
    ConnectionTime: UInt64;
    Connection: TConnectionData;
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
procedure SetClientData(AName: String; Value: String; var Client: TClientData);

operator := (Str: string) Server: TServerData;
procedure SetServerData(AName: String; Value: String; var Server: TServerData);

operator := (Str: string) Connection: TConnectionData;
procedure SetConnectionData(AName: String; Value: String; var Connection: TConnectionData);

operator := (Str: string) Channel: TChannelData;
procedure SetChannelData(AName: String; Value: String; var Channel: TChannelData);

function CreateServerGroup(Str: String): TServerGroup;
function CreateChannelGroup(Str: String): TChannelGroup;
procedure SetGroupData(AName: String; Value: String; var Group: TServerGroup);

function GetNotificationType(Str: string): TNotificationType;

const
  FullChannelUpdate = [cuTopic..cuSecondsEmpty];
  DefaultChannelUpdate = [cuSecondsEmpty];
  FullClientUpdate = [clUID..clBadges];
  DefaultClientUpdate = [clUID, clVoice, clTimes, clAway];

implementation

uses TsLib.ValueRead;

operator := (Str: string) Client: TClientData;
var
  sl: TStringList;
  i: Integer;
begin
  Finalize(Client);
  FillByte(Client, SizeOf(Client), 0);
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
    Client.Connection := Str;
  finally
    sl.Free;
  end;
end;

procedure SetClientData(AName: String; Value: String; var Client: TClientData);
begin
  with Client do
    if AName = 'clid' then
      ReadValue(Value, ID)
    else if AName = 'cid' then
      ReadValue(Value, ChannelID)
    else if AName = 'client_unique_identifier' then
      ReadValue(Value, UID)
    else if AName = 'client_base64HashClientUID' then
      ReadValue(Value, UIDHash)
    else if AName = 'client_nickname' then
      ReadValue(Value, Name)
    else if AName = 'client_flag_talking' then
      ReadValue(Value, isTalking)
    else if AName = 'client_input_muted' then
      ReadValue(Value, MutedInput)
    else if AName = 'client_output_muted' then
      ReadValue(Value, MutedOutput)
    else if AName = 'client_outputonly_muted' then
      ReadValue(Value, MutedOutputOnly)
    else if AName = 'client_input_hardware' then
      ReadValue(Value, InputHardware)
    else if AName = 'client_output_hardware' then
      ReadValue(Value, OutputHardware)
    else if AName = 'client_meta_data' then
      ReadValue(Value, MetaData)
    else if AName = 'client_is_recording' then
      ReadValue(Value, Recording)
    else if AName = 'client_database_id' then
      ReadValue(Value, DatabaseID)
    else if AName = 'client_channel_group_id' then
      ReadValue(Value, ChannelGroup)
    else if AName = 'client_servergroups' then
      ReadValue(Value, ServerGroups)
    else if AName = 'client_away' then
      ReadValue(Value, IsAway)
    else if AName = 'client_away_message' then
      ReadValue(Value, AfkMessage)
    else if AName = 'client_idle_time' then
      ReadValue(Value, IdleTime)
    else if AName = 'client_platform' then
      ReadValue(Value, Platform)
    else if AName = 'client_lastconnected' then
      ReadValue(Value, LastConnectionDate)
    else if AName = 'client_created' then
      ReadValue(Value, CreationDate)
    else if AName = 'client_type' then
      ReadValue(Value, CType)
    else if AName = 'client_flag_avatar' then
      ReadValue(Value, FlagAvatar)
    else if AName = 'client_talk_power' then
      ReadValue(Value, TalkPower)
    else if AName = 'client_talk_request' then
      ReadValue(Value, TalkRequest)
    else if AName = 'client_talk_request_msg' then
      ReadValue(Value, TalkRequestMessage)
    else if AName = 'client_description' then
      ReadValue(Value, Description)
    else if AName = 'client_is_talker' then
      ReadValue(Value, IsTalker)
    else if AName = 'client_is_priority_speaker' then
      ReadValue(Value, IsPrioritySpeaker)
    else if AName = 'client_unread_messages' then
      ReadValue(Value, UnreadMessages)
    else if AName = 'client_nickname_phonetic' then
      ReadValue(Value, PhoneticNickname)
    else if AName = 'client_needed_serverquery_view_power' then
      ReadValue(Value, NeededServerQuerryViewPower)
    else if AName = 'client_icon_id' then
      ReadValue(Value, IconID)
    else if AName = 'client_version' then
      ReadValue(Value, Version)
    else if AName = 'client_is_channel_commander' then
      ReadValue(Value, IsChannelCommander)
    else if AName = 'client_country' then
      ReadValue(Value, Country)
    else if AName = 'connection_client_ip' then
      ReadValue(Value, IP)
    else if AName = 'client_default_channel' then
      ReadValue(Value, DefaultChannel)
    else if AName = 'client_version_sign' then
      ReadValue(Value, VersionHash)
    else if AName = 'client_security_hash' then
      ReadValue(Value, SecurityHash)
    else if AName = 'client_login_name' then
      ReadValue(Value, LoginName)
    else if AName = 'client_totalconnections' then
      ReadValue(Value, TotalConnections)
    else if AName = 'client_month_bytes_uploaded' then
      ReadValue(Value, BytesULMonth)
    else if AName = 'client_month_bytes_downloaded' then
      ReadValue(Value, BytesDLMonth)
    else if AName = 'client_total_bytes_uploaded' then
      ReadValue(Value, BytesULTotal)
    else if AName = 'client_total_bytes_downloaded' then
      ReadValue(Value, BytesDLTotal)
    else if AName = 'client_default_token' then
      ReadValue(Value, DefaultToken)
    else if AName = 'connection_connected_time' then
      ReadValue(Value, ConnectionTime)
    else if AName = 'client_channel_group_inherited_channel_id' then
      ReadValue(Value,
        ChannelGroupInheritedChannel)
    else if AName = 'client_badges' then
      ReadValue(Value, Badges);
end;

operator := (Str: string) Server: TServerData;
var
  sl: TStringList;
  i: Integer;
begin
  Finalize(Server);
  FillByte(Server, SizeOf(Server), 0);
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

procedure SetServerData(AName: String; Value: String; var Server: TServerData);
begin
  with Server do
    if AName = 'virtualserver_unique_identifier' then
      ReadValue(Value, UID)
    else if AName = 'virtualserver_name' then
      ReadValue(Value, Name)
    else if AName = 'virtualserver_welcomemessage' then
      ReadValue(Value, WelcomeMessage)
    else if AName = 'virtualserver_platform' then
      ReadValue(Value, Platform)
    else if AName = 'virtualserver_version' then
      ReadValue(Value, Version)
    else if AName = 'virtualserver_maxclients' then
      ReadValue(Value, MaxClients)
    else if AName = 'virtualserver_password' then
      ReadValue(Value, Password)
    else if AName = 'virtualserver_clientsonline' then
      ReadValue(Value, Clients)
    else if AName = 'virtualserver_channelsonline' then
      ReadValue(Value, Channels)
    else if AName = 'virtualserver_created' then
      ReadValue(Value, CreationDate)
    else if AName = 'virtualserver_uptime' then
      ReadValue(Value, Uptime)
    else if AName = 'virtualserver_codec_encryption_mode' then
      ReadValue(Value, CodecEncryptionMode)
    else if AName = 'virtualserver_hostmessage' then
      ReadValue(Value, HostMessage)
    else if AName = 'virtualserver_hostmessage_mode' then
      ReadValue(Value, MessageMode)
    else if AName = 'virtualserver_filebase' then
      ReadValue(Value, FileBase)
    else if AName = 'virtualserver_default_server_group' then
      ReadValue(Value, DefaultSGroup)
    else if AName = 'virtualserver_default_channel_group' then
      ReadValue(Value, DefaultCGroup)
    else if AName = 'virtualserver_flag_password' then
      ReadValue(Value, HasPassword)
    else if AName = 'virtualserver_default_channel_admin_group' then
      ReadValue(Value, DefaultCAGroup)
    else if AName = 'virtualserver_max_download_total_bandwidth' then
      ReadValue(Value, MaxDLBandwidth)
    else if AName = 'virtualserver_max_upload_total_bandwidth' then
      ReadValue(Value, MaxULBandwith)
    else if AName = 'virtualserver_hostbanner_url' then
      ReadValue(Value, BannerURL)
    else if AName = 'virtualserver_hostbanner_gfx_url' then
      ReadValue(Value, BannerImage)
    else if AName = 'virtualserver_hostbanner_gfx_interval' then
      ReadValue(Value, BannerUpdateInterval)
    else if AName = 'virtualserver_complain_autoban_count' then
      ReadValue(Value, ComplainBanCount)
    else if AName = 'virtualserver_complain_autoban_time' then
      ReadValue(Value, ComplainBanTime)
    else if AName = 'virtualserver_complain_remove_time' then
      ReadValue(Value, ComplainRemoveTime)
    else if AName = 'virtualserver_min_clients_in_channel_before_forced_silence' then
      ReadValue(Value, MinClientsForcedSilence)
    else if AName = 'virtualserver_priority_speaker_dimm_modificator' then
      ReadValue(Value, PriorityDim)
    else if AName = 'virtualserver_id' then
      ReadValue(Value, ID)
    else if AName = 'virtualserver_antiflood_points_tick_reduce' then
      ReadValue(Value, AFTickReduce)
    else if AName = 'virtualserver_antiflood_points_needed_command_block' then
      ReadValue(Value, AFBlockCount)
    else if AName = 'virtualserver_antiflood_points_needed_ip_block' then
      ReadValue(Value, AFIPBlockCount)
    else if AName = 'virtualserver_client_connections' then
      ReadValue(Value, Clients)
    else if AName = 'virtualserver_query_client_connections' then
      ReadValue(Value, QueryConnections)
    else if AName = 'virtualserver_hostbutton_tooltip' then
      ReadValue(Value, HostButtonTooltip)
    else if AName = 'virtualserver_hostbutton_url' then
      ReadValue(Value, HostButtonUrl)
    else if AName = 'virtualserver_hostbutton_gfx_url' then
      ReadValue(Value, HostButtonImage)
    else if AName = 'virtualserver_queryclientsonline' then
      ReadValue(Value, QueryClients)
    else if AName = 'virtualserver_download_quota' then
      ReadValue(Value, DLQuota)
    else if AName = 'virtualserver_upload_quota' then
      ReadValue(Value, ULQuota)
    else if AName = 'virtualserver_month_bytes_downloaded' then
      ReadValue(Value, BytesDLMonth)
    else if AName = 'virtualserver_month_bytes_uploaded' then
      ReadValue(Value, BytesULMonth)
    else if AName = 'virtualserver_total_bytes_downloaded' then
      ReadValue(Value, BytesDLTotal)
    else if AName = 'virtualserver_total_bytes_uploaded' then
      ReadValue(Value, BytesULTotal)
    else if AName = 'virtualserver_port' then
      ReadValue(Value, Port)
    else if AName = 'virtualserver_autostart' then
      ReadValue(Value, Autostart)
    else if AName = 'virtualserver_machine_id' then
      ReadValue(Value, MachineID)
    else if AName = 'virtualserver_needed_identity_security_level' then
      ReadValue(Value, SecurityLevelRequired)
    else if AName = 'virtualserver_log_client' then
      ReadValue(Value, LogClients)
    else if AName = 'virtualserver_log_query' then
      ReadValue(Value, LogQuerys)
    else if AName = 'virtualserver_log_channel' then
      ReadValue(Value, LogChannels)
    else if AName = 'virtualserver_log_permissions' then
      ReadValue(Value, LogPermissions)
    else if AName = 'virtualserver_log_server' then
      ReadValue(Value, LogServer)
    else if AName = 'virtualserver_log_filetransfer' then
      ReadValue(Value, LogFileTransfers)
    else if AName = 'virtualserver_min_client_version' then
      ReadValue(Value, MinClientVersion)
    else if AName = 'virtualserver_name_phonetic' then
      ReadValue(Value, PhoneticName)
    else if AName = 'virtualserver_icon_id' then
      ReadValue(Value, IconID)
    else if AName = 'virtualserver_reserved_slots' then
      ReadValue(Value, ReservedSlots)
    else if AName = 'virtualserver_total_packetloss_speech' then
      ReadValue(Value, PacketLossSpeech)
    else if AName = 'virtualserver_total_packetloss_keepalive' then
      ReadValue(Value, PacketLossKeepalive)
    else if AName = 'virtualserver_total_packetloss_control' then
      ReadValue(Value, PacketLossControl)
    else if AName = 'virtualserver_total_packetloss_total' then
      ReadValue(Value, PacketLossTotal)
    else if AName = 'virtualserver_total_ping' then
      ReadValue(Value, Ping)
    else if AName = 'virtualserver_ip' then
      ReadValue(Value, IP)
    else if AName = 'virtualserver_weblist_enabled' then
      ReadValue(Value, WebList)
    else if AName = 'virtualserver_ask_for_privilegekey' then
      ReadValue(Value, PriviledgeKeyUnused)
    else if AName = 'virtualserver_hostbanner_mode' then
      ReadValue(Value, BannerMode)
    else if AName = 'virtualserver_channel_temp_delete_delay_default' then
      ReadValue(Value, DefaultTempChannelDeletionTime)
    else if AName = 'virtualserver_min_android_version' then
      ReadValue(Value, MinAndroidVersion)
    else if AName = 'virtualserver_min_ios_version' then
      ReadValue(Value, MinIOSVersion)
    else if AName = 'virtualserver_status' then
      ReadValue(Value, Status);
end;

operator := (Str: string) Connection: TConnectionData;
var
  sl: TStringList;
  i: Integer;
begin
  Finalize(Connection);
  FillByte(Connection, SizeOf(Connection), 0);
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

procedure SetConnectionData(AName: String; Value: String; var Connection: TConnectionData);
begin
  with Connection do
    if AName = 'connection_filetransfer_bandwidth_sent' then
      ReadValue(Value, FileBandwidthOut)
    else if AName = 'connection_filetransfer_bandwidth_received' then
      ReadValue(Value, FileBandwidthIn)
    else if AName = 'connection_filetransfer_bytes_sent_total' then
      ReadValue(Value, BytesOut)
    else if AName = 'connection_filetransfer_bytes_received_total' then
      ReadValue(Value, BytesIn)
    else if AName = 'connection_packets_sent_speech' then
      ReadValue(Value, PacketsOutSpeech)
    else if AName = 'connection_bytes_sent_speech' then
      ReadValue(Value, BytesOutSpeech)
    else if AName = 'connection_packets_received_speech' then
      ReadValue(Value, PacketsInSpeech)
    else if AName = 'connection_bytes_received_speech' then
      ReadValue(Value, BytesInSpeech)
    else if AName = 'connection_packets_sent_keepalive' then
      ReadValue(Value, PacketsOutKeepAlive)
    else if AName = 'connection_bytes_sent_keepalive' then
      ReadValue(Value, BytesOutKeepAlive)
    else if AName = 'connection_packets_received_keepalive' then
      ReadValue(Value, PacketsInKeepAlive)
    else if AName = 'connection_bytes_received_keepalive' then
      ReadValue(Value, BytesInKeepAlive)
    else if AName = 'connection_packets_sent_control' then
      ReadValue(Value, PacketsOutControl)
    else if AName = 'connection_bytes_sent_control' then
      ReadValue(Value, PacketsInControl)
    else if AName = 'connection_packets_received_control' then
      ReadValue(Value, FileBandwidthOut)
    else if AName = 'connection_bytes_received_control' then
      ReadValue(Value, BytesInControl)
    else if AName = 'connection_packets_sent_total' then
      ReadValue(Value, PacketsOut)
    else if AName = 'connection_bytes_sent_total' then
      ReadValue(Value, BytesOut)
    else if AName = 'connection_packets_received_total' then
      ReadValue(Value, PacketsIn)
    else if AName = 'connection_bytes_received_total' then
      ReadValue(Value, BytesIn)
    else if AName = 'connection_bandwidth_sent_last_second_total' then
      ReadValue(Value, BandwidthSecOut)
    else if AName = 'connection_bandwidth_sent_last_minute_total' then
      ReadValue(Value, BandwidthMinOut)
    else if AName = 'connection_bandwidth_received_last_second_total' then
      ReadValue(Value, BandwidthSecIn)
    else if AName = 'connection_bandwidth_received_last_minute_total' then
      ReadValue(Value, BandwidthMinIn);
end;

operator := (Str: string) Channel: TChannelData;
var
  sl: TStringList;
  i: Integer;
begin
  Finalize(Channel);
  FillByte(Channel, SizeOf(Channel), 0);
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

procedure SetChannelData(AName: String; Value: String; var Channel: TChannelData);
begin
  with Channel do
    if AName = 'cid' then
      ReadValue(Value, ID)
    else if AName = 'pid' then
      ReadValue(Value, ParentID)
    else if AName = 'channel_name' then
      ReadValue(Value, Name)
    else if AName = 'channel_topic' then
      ReadValue(Value, Topic)
    else if AName = 'channel_description' then
      ReadValue(Value, Description)
    else if AName = 'channel_password' then
      ReadValue(Value, Password)
    else if AName = 'channel_codec' then
      ReadValue(Value, Codec)
    else if AName = 'channel_codec_quality' then
      ReadValue(Value, CodecQuality)
    else if AName = 'channel_maxclients' then
      ReadValue(Value, MaxClients)
    else if AName = 'channel_maxfamilyclients' then
      ReadValue(Value, MaxClientsFamily)
    else if AName = 'channel_order' then
      ReadValue(Value, Order)
    else if AName = 'channel_flag_permanent' then
      ReadValue(Value, IsPermament)
    else if AName = 'channel_flag_semi_permanent' then
      ReadValue(Value, IsSemipermament)
    else if AName = 'channel_flag_default' then
      ReadValue(Value, IsDefault)
    else if AName = 'channel_flag_password' then
      ReadValue(Value, HasPassword)
    else if AName = 'channel_codec_latency_factor' then
      ReadValue(Value, CodecQuality)
    else if AName = 'channel_codec_is_unencrypted' then
      ReadValue(Value, Unencrypted)
    else if AName = 'channel_security_salt' then
      ReadValue(Value, PasswordSalt)
    else if AName = 'channel_delete_delay' then
      ReadValue(Value, DeleteDelay)
    else if AName = 'channel_flag_maxclients_unlimited' then
      ReadValue(Value, IsUnlimited)
    else if AName = 'channel_flag_maxfamilyclients_unlimited' then
      ReadValue(Value, IsFamilyUnlimited)
    else if AName = 'channel_flag_maxfamilyclients_inherited' then
      ReadValue(Value, IsMaxClientsInherited)
    else if AName = 'channel_filepath' then
      ReadValue(Value, FilePath)
    else if AName = 'channel_needed_talk_power' then
      ReadValue(Value, Talkpower)
    else if AName = 'channel_forced_silence' then
      ReadValue(Value, IsSilenced)
    else if AName = 'channel_name_phonetic' then
      ReadValue(Value, PhoneticName)
    else if AName = 'channel_icon_id' then
      ReadValue(Value, IconID)
    else if AName = 'channel_flag_private' then
      ReadValue(Value, IsPrivate)
    else if AName = 'seconds_empty' then
      ReadValue(Value, SecondsEmpty)
    else if AName = 'total_clients_family' then
      ReadValue(Value, FamilyClients)
    else if AName = 'total_clients' then
      ReadValue(Value, Clients)
    else if AName = 'channel_needed_subscribe_power' then
      ReadValue(Value, SubscribePower);
end;


function CreateServerGroup(Str: String): TServerGroup;
var
  sl: TStringList;
  i: Integer;
begin
  Finalize(Result);
  FillByte(Result, SizeOf(Result), 0);
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetGroupData(sl[i], '', Result)
      else
        SetGroupData(sl.Names[i], sl.ValueFromIndex[i], Result);
  finally
    sl.Free;
  end;
end;

function CreateChannelGroup(Str: String): TChannelGroup;
var
  sl: TStringList;
  i: Integer;
begin
  Finalize(Result);
  FillByte(Result, SizeOf(Result), 0);
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Str;
    for i:=0 to sl.Count-1 do
      if sl.Names[i]='' then
        SetGroupData(sl[i], '', TServerGroup(Result))
      else
        SetGroupData(sl.Names[i], sl.ValueFromIndex[i], TServerGroup(Result));
  finally
    sl.Free;
  end;
end;

procedure SetGroupData(AName: String; Value: String; var Group: TServerGroup);
begin
  with Group do
    if (AName = 'sgid') or (AName = 'cgid')then
      ReadValue(Value, ID)
    else if AName = 'name' then
      ReadValue(Value, Name)
    else if AName = 'iconid' then
      ReadValue(Value, IconID)
    else if AName = 'savedb' then
      ReadValue(Value, SaveDB)
    else if AName = 'sortid' then
      ReadValue(Value, SortID)
    else if AName = 'namemode' then
      ReadValue(Value, NameMode)
    else if AName = 'n_modifyp' then
      ReadValue(Value, ModifyP)
    else if AName = 'n_member_addp' then
      ReadValue(Value, MemberAddP)
    else if AName = 'n_member_removep' then
      ReadValue(Value, MemberRemoveP);
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
