  { Type of a Notification Message }

  TNotificationType = (ntTextMessage, ntServerEdited, ntClientMoved,
    ntClientDisconnected, ntClientConnected, ntChannelEdited,
    ntChanneldescriptionChanged);

  { Data of an incoming Notification (Not parsed)}

  PNotificationData = ^TNotificationData;

  TNotificationData = record
    NType: TNotificationType;
    Data: string;
  end;

  { Parsed Notification Data }

  TTextNotification = record
    TargetMode: integer;
    Message: string;
    Target: integer;
    SenderID: integer;
    SenderName: string;
    SenderUID: string;
  end;

  TServerEditNotification = record
    ReasonID: integer;
    InvokerID: integer;
    InvokerName: string;
    InvokerUID: integer;
    Changes: Array of Array[0..1] of String;
  end;

  TClientMoveNotification = record
    ChannelTargetID: integer;
    ReasonID: integer;
    ClientID: integer;
  end;

  TClientDCNotification = record
    ChannelID: integer;
    ChannelTargetID: integer;
    ResonID: integer;
    ReasonMessage: string;
    ClientID: integer;
  end;

  TFlagAvatar =  String[32];

  TNotificationClientData = record
    ID: Integer;
    UID: String;
    Name: String;
    MutedInput: Boolean;
    MutedOutput: Boolean;
    MutedOutputOnly: Boolean;
    InputHardware: Boolean;
    OutputHardware: Boolean;
    MetaData: String;
    Recording: Boolean;
    DatabaseID: Integer;
    ChannelGroup: Integer;
    ServerGroups: array of Integer;
    IsAway: Boolean;
    AfkMessage: String;
    CType: Integer;
    FlagAvatar: TFlagAvatar;
    TalkPower: Integer;
    TalkRequest: Integer;
    TalkRequestMessage: String;
    Description: String;
    IsTalker: Boolean;
    IsPrioritySpeaker: Boolean;
    UnreadMessages: Integer;
    PhoneticNickname: String;
    NeededServerQuerryViewPower: Integer;
    IconID: Integer;
    IsChannelCommander: Boolean;
    Country: String;
    ChannelGroupInheritedChannel: Integer;
    Badges: String;
  end;

  TClientConnectNotification = record
    ChannelFromID: integer;
    ChannelTargetID: integer;
    ReasonID: integer;
    Client: TNotificationClientData;
  end;

  TChannelEditedNotification = record
    ChannelID: integer;
    ResonID: integer;
    ClientID: integer;
    ClientName: string;
    ClientUID: string;
    Changes: Array of Array[0..1] of String;
  end;

  { Events to register }

  TTextNotificationEvent = procedure(Sender: TObject; AData: TTextNotification) of object;
  TTextEventList = specialize TFPGList<TTextNotificationEvent>;

  TServerEditNotificationEvent = procedure(Sender: TObject; AData: TServerEditNotification) of object;
  TServerEditEventList = specialize TFPGList<TServerEditNotificationEvent>;

  TClientMoveNotificationEvent = procedure(Sender: TObject; AData: TClientMoveNotification) of object;
  TClientMoveEventList = specialize TFPGList<TClientMoveNotificationEvent>;

  TClientDCNotificationEvent = procedure(Sender: TObject; AData: TClientDCNotification) of object;
  TDisconnectedEventList = specialize TFPGList<TClientDCNotificationEvent>;

  TClientConnectNotificationEvent = procedure(Sender: TObject; AData: TClientConnectNotification) of object;
  TConnectedEventList = specialize TFPGList<TClientConnectNotificationEvent>;

  TChannelEditedNotificationEvent = procedure(Sender: TObject; AData: TChannelEditedNotification) of object;
  TChannelEditedEventList = specialize TFPGList<TChannelEditedNotificationEvent>;

  TChannelDescriptionChangedEvent = procedure(Sender: TObject; ChannelID: Integer) of object;
  TDescriptionEventList = specialize TFPGList<TChannelDescriptionChangedEvent>;

  TEventLists = record
    Texts: TTextEventList;
    ServerEdits: TServerEditEventList;
    ClientMoves: TClientMoveEventList;
    Disconnects: TDisconnectedEventList;
    Connects: TConnectedEventList;
    ChannelEdits: TChannelEditedEventList;
    DescriptionChanges: TDescriptionEventList;
  end;
