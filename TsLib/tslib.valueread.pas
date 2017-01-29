unit TsLib.ValueRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Types, dateutils;

procedure ReadValue(Val: String; out Result: Integer);
procedure ReadValue(Val: String; out Result: Cardinal);
procedure ReadValue(Val: String; out Result: Int64);
procedure ReadValue(Val: String; out Result: QWord);
procedure ReadValue(Val: String; out Result: Boolean);
procedure ReadValue(Val: String; out Result: String);
procedure ReadValue(Val: String; out Result: TFlagAvatar);
procedure ReadValue(Val: String; out Result: TDynIntArray);
procedure ReadValue(Val: String; out Result: TOperatingSystem);
procedure ReadValue(Val: String; out Result: TDateTime);
procedure ReadValue(Val: String; out Result: TCodecEncryptionMode);
procedure ReadValue(Val: String; out Result: TMessageMode);
procedure ReadValue(Val: String; out Result: TBannerMode);

function IsNumeric(str: String): Boolean;

implementation

procedure ReadValue(Val: String; out Result: Integer);
begin
  if not TryStrToInt(Val, Result) then Result:=0;
end;

procedure ReadValue(Val: String; out Result: Cardinal);
begin
  if IsNumeric(Val) then Result:=StrToInt(Val)
 else Result:=0;
end;

procedure ReadValue(Val: String; out Result: Int64);
begin
  if not TryStrToInt64(Val, Result) then Result:=0;
end;

procedure ReadValue(Val: String; out Result: QWord);
begin
  if IsNumeric(Val) then Result:=StrToInt64(Val)
 else Result:=0;
end;

procedure ReadValue(Val: String; out Result: Boolean);
begin
  Result:=Val='1';
end;

procedure ReadValue(Val: String; out Result: String);
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

procedure ReadValue(Val: String; out Result: TFlagAvatar);
begin
  Result:='';
  if Length(Val)=32 then Result:=Val;
end;

procedure ReadValue(Val: String; out Result: TDynIntArray);
var sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    sl.Delimiter:=',';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=Val;
    SetLength(Result, sl.Count);
    for i:=0 to sl.Count-1 do
      if IsNumeric(sl[i]) then
        Result[i]:=StrToInt(sl[i])
      else
      begin
        SetLength(Result, 0);
        Break;
      end;
  finally
    sl.Free;
  end;
end;

procedure ReadValue(Val: String; out Result: TOperatingSystem);
begin
  if Val = 'Linux' then Result:=osLinux
  else if Val = 'Windows' then Result:=osWindows
  else if Val = 'OS\sX' then Result:=osMacOS
  else Result:=osOther;
end;

procedure ReadValue(Val: String; out Result: TDateTime);
var
  unixtime: Integer;
begin
  ReadValue(Val, unixtime);
  Result:=UnixToDateTime(unixtime);
end;

procedure ReadValue(Val: String; out Result: TCodecEncryptionMode);
var
  i: Integer;
begin
  ReadValue(val, i);
  Result:=TCodecEncryptionMode(i);
end;

procedure ReadValue(Val: String; out Result: TMessageMode);
var
  i: Integer;
begin
  ReadValue(val, i);
  Result:=TMessageMode(i);
end;

procedure ReadValue(Val: String; out Result: TBannerMode);
var
  i: Integer;
begin
  ReadValue(val, i);
  Result:=TBannerMode(i);
end;

function IsNumeric(Str: String): Boolean;
var
  c: Char;
begin
  Result:=Length(Str)>0;
  for c in str do
    if not (c in ['0'..'9']) then
    begin
      Result:=False;
      Break;
    end;
end;

end.

