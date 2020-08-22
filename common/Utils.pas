unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  RXoshiro256State = record
    S: array [0 .. 3] of QWord;
  end;

function InitXoshiro256State: RXoshiro256State;
function Xoshiro256ss(var State: RXoshiro256State): QWord;

function HumanTimeString(TimeSec: double): string;

implementation

uses
  DateUtils;

function InitXoshiro256State: RXoshiro256State;
var
  I: integer;
begin
  Result.S[0] := GetTickCount64 * QWord(998244353);
  Result.S[1] := GetCurrentThreadId * QWord(998244353);
  Result.S[2] := QWord(MillisecondsBetween(Now, UnixEpoch)) * QWord(998244353);
  Result.S[3] := QWord(@Result) * QWord(998244353);
  for I := 0 to 99 do
    Xoshiro256ss(Result);
end;

function Xoshiro256ss(var State: RXoshiro256State): QWord;
var
  T: QWord;
begin
  with State do
  begin
    Result := RolQWord(S[1] * 5, 7) * 9;
    T := State.S[1] shl 17;
    S[2] := S[2] xor S[0];
    S[3] := S[3] xor S[1];
    S[1] := S[1] xor S[2];
    S[0] := S[0] xor S[3];
    S[2] := S[2] xor T;
    S[3] := RolQWord(S[3], 45);
  end;
end;

function HumanTimeString(TimeSec: double): string;
begin
  if TimeSec < 60 then
    exit(Format('%.2f sec', [TimeSec]));
  if TimeSec < 3600 then
    exit(Format('%.2f min', [TimeSec / 60]));
  if TimeSec < 3600 * 24 then
    exit(Format('%.2f hours', [TimeSec / 3600]));
  exit(Format('%.2f days', [TimeSec / (3600 * 24)]));
end;

end.

