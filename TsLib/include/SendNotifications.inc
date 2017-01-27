  function ReadInvokerData(SL: TStringList): TInvokerData;
  begin
    with Result do
    begin
      ReadValue(sl.Values['invokerid'], ID);
      ReadValue(sl.Values['invokername'], Name);
      ReadValue(sl.Values['invokeruid'], UID);
    end;
  end;

  function ReadReasonData(SL: TStringList): TReasonData;
  begin
    with Result do
    begin
      ReadValue(sl.Values['reasonid'], ID);
      ReadValue(sl.Values['reasonmsg'], Message);
    end;
  end;

  procedure SendTM(n: TNotificationData);
  var
    Data: TTextNotification;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      with Data do
      begin
        ReadValue(sl.Values['targetmode'], TargetMode);
        ReadValue(sl.Values['msg'], Message);
        ReadValue(sl.Values['target'], Target);
        Invoker:=ReadInvokerData(sl);
      end;
      // Send to every registered event
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
    finally
      sl.Free;
    end;
  end;

  procedure SendSE(n: TNotificationData);
  var
    Data: TServerEditNotification;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      with Data do
      begin
        Reason:=ReadReasonData(sl);
        Invoker:=ReadInvokerData(sl);
        SetLength(Changes, sl.Count-4);
        for i:=4 to sl.Count-1 do
          if sl.Names[i]='' then
            Changes[i-4][0]:=sl[i]
          else
          begin
            Changes[i-4][0]:=sl.Names[i];
            Changes[i-4][1]:=sl.ValueFromIndex[i];
          end;
      end;
      // Send to every registered event
      for i:=0 to Events.ServerEdits.Count-1 do
        Events.ServerEdits[i](Self, Data);
    finally
      sl.Free;
    end;
  end;

  procedure SendCM(n: TNotificationData);
  var
    Data: TClientMoveNotification;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      with Data do
      begin
        ReadValue(sl.Values['ctid'], ChannelTargetID);
        Reason:=ReadReasonData(sl);
        ReadValue(sl.Values['clid'], ClientID);
        Invoker:=ReadInvokerData(sl);
      end;
      // Send to every registered event
      for i:=0 to Events.ClientMoves.Count-1 do
        Events.ClientMoves[i](Self, Data);
    finally
      sl.Free;
    end;
  end;

  procedure SendCD(n: TNotificationData);
  var
    Data: TClientDCNotification;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      with Data do
      begin
        ReadValue(sl.Values['cfid'], ChannelID);
        ReadValue(sl.Values['ctid'], ChannelTargetID);
        ReadValue(sl.Values['clid'], ClientID);
        Reason:=ReadReasonData(SL);
        Invoker:=ReadInvokerData(sl);
      end;
      // Send to every registered event
      for i:=0 to Events.Disconnects.Count-1 do
        Events.Disconnects[i](Self, Data);
    finally
      sl.Free;
    end;
  end;

  procedure SendCC(n: TNotificationData);
  var
    Data: TClientConnectNotification;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      with Data do
      begin
        ReadValue(sl.Values['cfid'], ChannelFromID);
        ReadValue(sl.Values['ctid'], ChannelTargetID);
        Reason:=ReadReasonData(sl);
      end;
    finally
      sl.Free;
    end;
    Data.Client := n.Data;
    // Send to every registered event
    for i:=0 to Events.Connects.Count-1 do
      Events.Connects[i](Self, Data);
  end;

  procedure SendCE(n: TNotificationData);
  var
    Data: TChannelEditedNotification;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      with Data do
      begin
        ReadValue(sl.Values['cid'], ChannelID);
        Reason:=ReadReasonData(SL);
        Invoker:=ReadInvokerData(sl);
        SetLength(Changes, sl.Count-5);
        for i:=5 to sl.Count-1 do
          if sl.Names[i]='' then
            Changes[i-5][0]:=sl[i]
          else
          begin
            Changes[i-5][0]:=sl.Names[i];
            Changes[i-5][1]:=sl.ValueFromIndex[i];
          end;
      end;
      // Send to every registered event
      for i:=0 to Events.ChannelEdits.Count-1 do
        Events.ChannelEdits[i](Self, Data);
    finally
      sl.Free;
    end;
    QueuedNotifications.Clear;
  end;

  procedure SendDC(n: TNotificationData);

  var
    Data: Integer;
    sl: TStringList;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Delimiter:=' ';
      sl.StrictDelimiter:=True;
      sl.DelimitedText:=n.Data;
      ReadValue(sl.Values['cid'], Data);
      // Send to every registered event
      for i:=0 to Events.DescriptionChanges.Count-1 do
        Events.DescriptionChanges[i](Self, Data);
    finally
      sl.Free;
    end;
  end;
