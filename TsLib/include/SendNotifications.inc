  function CheckNSet(Val: String): Integer;
  begin
    if not TryStrToInt(Result) then Result:=0;
  end;

  function CheckNSet(Val: String): Boolean;
  begin
    Result:=Val='1';
  end;

  function CheckNSet(Val: String): String;
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

  function CheckNSet(Val: String): TFlagAvatar;
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
        TargetMode := CheckNSet(sl.Values['targetmode']);
        Message := CheckNSet(sl.Values['msg']);
        Target := CheckNSet(sl.Values['target']);
        SenderID := CheckNSet(sl.Values['invokerid']);
        SenderName := CheckNSet(sl.Values['invokername']);
        SenderUID := CheckNSet(sl.Values['invokeruid']);
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
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
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
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
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
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
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
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
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
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
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
      Data:=CheckNSet(sl.Values['cid']);
      // Send to every registered event
      for i:=0 to Events.Texts.Count-1 do
        Events.Texts[i](Self, Data);
    finally
      sl.Free;
    end;
  end;
