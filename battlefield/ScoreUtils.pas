unit ScoreUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ScorePairToStr(First, Draw, Second: integer): string;

implementation

function ScoreToStr(Score: integer): string;
begin
  if Score mod 2 = 0 then
    Result := IntToStr(Score div 2) + '.0'
  else
    Result := IntToStr(Score div 2) + '.5';
end;

function ScorePairToStr(First, Draw, Second: integer): string;
begin
  Result := ScoreToStr(First * 2 + Draw) + ':' + ScoreToStr(Second * 2 + Draw);
end;

end.

