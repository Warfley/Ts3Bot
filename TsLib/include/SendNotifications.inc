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
        ReadValue(sl.Values['invokerid'], SenderID);
        ReadValue(sl.Values['invokername'], SenderName);
        ReadValue(sl.Values['invokeruid'], SenderUID);
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
        ReadValue(SL.Values['reasonid'], ReasonID);
        ReadValue(SL.Values['invokerid'], InvokerID);
        ReadValue(SL.Values['invokername'], InvokerName);
        ReadValue(SL.Values['invokeruid'], InvokerUID);
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

      end;
      // Send to every registered event
      for i:=0 to Events.Connects.Count-1 do
        Events.Connects[i](Self, Data);
    finally
      sl.Free;
    end;
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

      end;
      // Send to every registered event
      for i:=0 to Events.ChannelEdits.Count-1 do
        Events.ChannelEdits[i](Self, Data);
    finally
      sl.Free;
    end;
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
