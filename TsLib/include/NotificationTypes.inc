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

  TInvokerData = record
    ID: Integer;
    Name: String;
    UID: String;
  end;

  TReasonData = record
    ID: Integer;
    Message: String;
  end;

  TTextNotification = record
    TargetMode: integer;
    Message: string;
    Target: integer;
    Invoker: TInvokerData;
  end;

  TServerEditNotification = record
    Reason: TReasonData;
    Invoker: TInvokerData;
    Changes: Array of Array[0..1] of String;
  end;

  TClientMoveNotification = record
    ChannelTargetID: integer;
    Reason: TReasonData;
    ClientID: integer;
    Invoker: TInvokerData;
  end;

  TClientDCNotification = record
    ChannelID: integer;
    ChannelTargetID: integer;
    Reason: TReasonData;
    ReasonMessage: string;
    ClientID: integer;
    Invoker: TInvokerData
  end;

  TClientConnectNotification = record
    ChannelFromID: integer;
    ChannelTargetID: integer;
    Reason: TReasonData;
    Client: TClientData;
  end;

  TChannelEditedNotification = record
    ChannelID: integer;
    Reason: TReasonData;
    Invoker: TInvokerData;
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
