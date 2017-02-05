unit TsBot.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MSPerSec = 1000;
  SecPerMin = 60;
  Minutes5 = 5 * SecPerMin * MSPerSec;
  Minutes10 = 2 * Minutes5;
  Minutes30 = 3 * Minutes10;
  Hour = 2 * Minutes30;

  DefaultPort = 10011;
  LoopBackAddr = '127.0.0.1';
  DefaultServerID = 1;


  IConfigWriteError = 52;

function ReadArgument(Str: string; var p: integer): string;

implementation

function ReadArgument(Str: string; var p: integer): string;
var
  len: integer;
  inStr: Boolean;
begin
  Result := '';
  len := 0;
  while (p <= Length(Str)) and (Str[p] in [#1..' ']) do
    Inc(p);
  if p > Length(str) then
    Exit;
  inStr:=False;
  while p+len<=Length(Str) do
  begin
    case Str[p+Len] of
    '"': inStr:=not inStr;
    #0..' ': if not inStr then Break;
    end;
    inc(len);
  end;
  Result := StringReplace(Copy(str, p, len), '"', '', [rfReplaceAll]);
  Inc(p, len);
end;

end.

