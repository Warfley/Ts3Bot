  procedure CheckNSet(Val: String; out Result: Integer);
  begin
    if not TryStrToInt(Val, Result) then Result:=0;
  end;

  function CheckNSet(Val: String): Boolean;
  begin
    Result:=Val='1';
  end;

  procedure CheckNSet(Val: String; out Result: String);
  var
    pos, i, len: Integer;
  begin
    len:=Length(Val);
    SetLength(Result, len);
    i:=1;
    pos:=1;
    while i<=len do
    begin
      if Val[i] = '\' then
      begin
        case Val[i+1] of
        's':
        begin
          Result[pos]:=' ';
          inc(pos);
        end;
        't':
        begin
          Result[pos]:=#9;
          inc(pos);
        end;
        '\':
        begin
          Result[pos]:='\';
          inc(pos);
        end;
        'n':
        begin
          Result[pos]:=#10;
          inc(pos);
        end;
        end;
        inc(i);
      end
      else
      begin
        Result[pos]:=Val[i];
        inc(pos);
      end;
      inc(i);
    end;
    SetLength(Result, pos);
  end;

  procedure CheckNSet(Val: String; out Result: TFlagAvatar);
  begin
    Result:='';
    if Length(Val)=32 then Result:=Val;
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
        CheckNSet(sl.Values['targetmode'], TargetMode);
        CheckNSet(sl.Values['msg'], Message);
        CheckNSet(sl.Values['target'], Target);
        CheckNSet(sl.Values['invokerid'], SenderID);
        CheckNSet(sl.Values['invokername'], SenderName);
        CheckNSet(sl.Values['invokeruid'], SenderUID);
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
        { TODO : TODO }
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
      CheckNSet(sl.Values['cid'], Data);
      // Send to every registered event
      for i:=0 to Events.DescriptionChanges.Count-1 do
        Events.DescriptionChanges[i](Self, Data);
    finally
      sl.Free;
    end;
  end;
