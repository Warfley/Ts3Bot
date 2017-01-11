unit TsLib.ValueRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TsLib.Types;

procedure ReadValue(Val: String; out Result: Integer);
procedure ReadValue(Val: String; out Result: Boolean);
procedure ReadValue(Val: String; out Result: String);
procedure ReadValue(Val: String; out Result: TFlagAvatar);

implementation

procedure ReadValue(Val: String; out Result: Integer);
begin
  if not TryStrToInt(Val, Result) then Result:=0;
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

end.

