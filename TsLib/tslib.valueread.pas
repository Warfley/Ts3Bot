unit TsLib.ValueRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Types, dateutils;

procedure ReadValue(Val: string; out Result: integer);
procedure ReadValue(Val: string; out Result: cardinal);
procedure ReadValue(Val: string; out Result: int64);
procedure ReadValue(Val: string; out Result: QWord);
procedure ReadValue(Val: string; out Result: boolean);
procedure ReadValue(Val: string; out Result: string);
procedure ReadValue(Val: string; out Result: TFlagAvatar);
procedure ReadValue(Val: string; out Result: TDynIntArray);
procedure ReadValue(Val: string; out Result: TOperatingSystem);
procedure ReadValue(Val: string; out Result: TDateTime);
procedure ReadValue(Val: string; out Result: TCodecEncryptionMode);
procedure ReadValue(Val: string; out Result: TMessageMode);
procedure ReadValue(Val: string; out Result: TBannerMode);
procedure ReadValue(Val: string; out Result: TCodec);

function IsNumeric(str: string): boolean;

implementation

procedure ReadValue(Val: string; out Result: integer);
begin
  if not TryStrToInt(Val, Result) then
    Result := 0;
end;

procedure ReadValue(Val: string; out Result: cardinal);
begin
  if IsNumeric(Val) then
    Result := StrToInt(Val)
  else
    Result := 0;
end;

procedure ReadValue(Val: string; out Result: int64);
begin
  if not TryStrToInt64(Val, Result) then
    Result := 0;
end;

procedure ReadValue(Val: string; out Result: QWord);
begin
  if IsNumeric(Val) then
    Result := StrToInt64(Val)
  else
    Result := 0;
end;

procedure ReadValue(Val: string; out Result: boolean);
begin
  Result := Val = '1';
end;

procedure ReadValue(Val: string; out Result: string);
var
  pos, i, len: integer;
begin
  len := Length(Val);
  SetLength(Result, len);
  i := 1;
  pos := 1;
  while i <= len do
  begin
    if Val[i] = '\' then
    begin
      case Val[i + 1] of
        '\':
        begin
          Result[pos] := '\';
        end;
        '/':
        begin
          Result[pos] := '/';
        end;
        's':
        begin
          Result[pos] := ' ';
        end;
        'p':
        begin
          Result[pos] := '|';
        end;
        'a':
        begin
          Result[pos] := #7;
        end;
        'b':
        begin
          Result[pos] := #8;
        end;
        'f':
        begin
          Result[pos] := #12;
        end;
        'n':
        begin
          Result[pos] := #10;
        end;
        'r':
        begin
          Result[pos] := #13;
        end;
        't':
        begin
          Result[pos] := #9;
        end;
        'v':
        begin
          Result[pos] := #11;
        end;
      end;
      Inc(i);
    end
    else
      Result[pos] := Val[i];
    Inc(pos);
    Inc(i);
  end;
  SetLength(Result, pos - 1);
  Result := Trim(Result);
end;

procedure ReadValue(Val: string; out Result: TFlagAvatar);
begin
  Result := '';
  if Length(Val) = 32 then
    Result := Val;
end;

procedure ReadValue(Val: string; out Result: TDynIntArray);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.StrictDelimiter := True;
    sl.DelimitedText := Val;
    SetLength(Result, sl.Count);
    for i := 0 to sl.Count - 1 do
      if IsNumeric(sl[i]) then
        Result[i] := StrToInt(sl[i])
      else
      begin
        SetLength(Result, 0);
        Break;
      end;
  finally
    sl.Free;
  end;
end;

procedure ReadValue(Val: string; out Result: TOperatingSystem);
begin
  if Val = 'Linux' then
    Result := osLinux
  else if Val = 'Windows' then
    Result := osWindows
  else if Val = 'OS\sX' then
    Result := osMacOS
  else
    Result := osOther;
end;

procedure ReadValue(Val: string; out Result: TDateTime);
var
  unixtime: integer;
begin
  ReadValue(Val, unixtime);
  Result := UnixToDateTime(unixtime);
end;

procedure ReadValue(Val: string; out Result: TCodecEncryptionMode);
var
  i: integer;
begin
  ReadValue(val, i);
  Result := TCodecEncryptionMode(i);
end;

procedure ReadValue(Val: string; out Result: TMessageMode);
var
  i: integer;
begin
  ReadValue(val, i);
  Result := TMessageMode(i);
end;

procedure ReadValue(Val: string; out Result: TBannerMode);
var
  i: integer;
begin
  ReadValue(val, i);
  Result := TBannerMode(i);
end;

procedure ReadValue(Val: string; out Result: TCodec);
var
  i: integer;
begin
  ReadValue(val, i);
  Result := TCodec(i);
end;

function IsNumeric(Str: string): boolean;
var
  c: char;
begin
  Result := Length(Str) > 0;
  for c in str do
    if not (c in ['0'..'9']) then
    begin
      Result := False;
      Break;
    end;
end;

end.
