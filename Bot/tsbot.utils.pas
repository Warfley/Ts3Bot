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

function ReadToken(Str: string; var p: integer): string;

implementation

function ReadToken(Str: string; var p: integer): string;
var
  len: integer;
begin
  Result := '';
  len := 0;
  while (p <= Length(Str)) and (Str[p] in [#1..' ']) do
    Inc(p);
  if p > Length(str) then
    Exit;
  if Str[p] = '"' then
  begin
    Inc(p);
    while (p + len <= Length(Str)) and (Str[p + len] <> '"') do
      Inc(len);
  end
  else
    while (p + len <= Length(Str)) and (Str[p + len] in [' '..#255]-[' ','"']) do
      Inc(len);
  Result := Copy(str, p, len);
  Inc(p, len);
end;

end.

