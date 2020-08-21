unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function HumanTimeString(TimeSec: double): string;

implementation

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

