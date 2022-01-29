{
  This file is part of Battlefield - a tool to run micro-matches between chess
  engines.

  Copyright © 2022 Alexander Kernozhitsky <sh200105@mail.ru>

  Battlefield is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Battlefield is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Battlefield.  If not, see <http://www.gnu.org/licenses/>.
}
unit BattleStatistics;

{$mode objfpc}{$H+}{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils;

type

  { TBattleResult }

  TBattleResult = record
    Win, Draw, Lose: integer;

    function WinRate: double;
    function WinRateStdDev: double;
    function Total: double;
    function ToString: string;
  end;

  TConfidenceLevel = (clNone = 0, cl90 = 1, cl95 = 2, cl97 = 3, cl99 = 4);
  TConfidenceSide = (csFirst, csUnclear, csSecond);

  { TConfidenceResult }

  TConfidenceResult = record
    Level: TConfidenceLevel;
    Side: TConfidenceSide;
  end;

  { TEloDifference }

  TEloDifference = record
    Low, Average, High: double;
  end;

function MakeBattleResult(AWin, ADraw, ALose: integer): TBattleResult;

function CalcLOS(const R: TBattleResult): double;
procedure PrintLOS(Value: double; var AFile: TextFile);

function CalcEloDifference(const R: TBattleResult): TEloDifference;
function CalcEloDifference(WinRate: double): double;
procedure PrintEloDifference(Value: double; var AFile: TextFile);
procedure PrintEloDifference(const Value: TEloDifference; var AFile: TextFile);

function CalcConfidence(const R: TBattleResult): TConfidenceResult;
procedure PrintConfidence(const C: TConfidenceResult; var AFile: TextFile);
procedure PrintConfidenceShort(const C: TConfidenceResult; var AFile: TextFile);

implementation

uses
  spe, Math, RichTextConsole;

const
  ProbValues: array [TConfidenceLevel] of double = (0.0, 0.90, 0.95, 0.97, 0.99);
  ProbThresholds: array [TConfidenceLevel] of double =
    (0.0, 1.64485362695147, 1.95996398454005, 2.17009037758456, 2.57582930354890);

function ValueToColor(Value: double): TConsoleColor;
begin
  if Value < 0.1 then
    exit(cclRed);
  if Value <= 0.9 then
    exit(cclYellow);
  Result := cclGreen;
end;

function ScoreToStr(Score: integer): string;
begin
  if Score mod 2 = 0 then
    Result := IntToStr(Score div 2) + '.0'
  else
    Result := IntToStr(Score div 2) + '.5';
end;

function MakeBattleResult(AWin, ADraw, ALose: integer): TBattleResult;
begin
  with Result do
  begin
    Win := AWin;
    Draw := ADraw;
    Lose := ALose;
  end;
end;

function CalcLOS(const R: TBattleResult): double;
begin
  if R.Win + R.Lose = 0 then
    exit(NaN);
  Result := 0.5 * (1.0 + speerf((R.Win - R.Lose) / Sqrt(2 * (R.Win + R.Lose))));
end;

procedure PrintLOS(Value: double; var AFile: TextFile);
begin
  Write(AFile, 'LOS = ');
  rtcSetBold(AFile);
  if IsNan(Value) then
  begin
    rtcSetFgColor(AFile, cclWhite);
    Write(AFile, 'N/A');
  end
  else
  begin
    rtcSetFgColor(AFile, ValueToColor(Value));
    Write(AFile, Value: 0: 2);
  end;
  rtcResetStyle(AFile);
  WriteLn(AFile);
end;

function CalcEloDifference(const R: TBattleResult): TEloDifference;
var
  Mu, Delta: double;
begin
  Mu := R.WinRate;
  Delta := R.WinRateStdDev * ProbThresholds[cl95];
  Result.Low := CalcEloDifference(Mu - Delta);
  Result.Average := CalcEloDifference(Mu);
  Result.High := CalcEloDifference(Mu + Delta);
end;

function CalcEloDifference(WinRate: double): double;
const
  Eps: double = 1e-12;
begin
  if WinRate >= 1.0 - Eps then
    exit(Infinity);
  if WinRate <= Eps then
    exit(-Infinity);
  Result := -Log10(1.0 / WinRate - 1.0) * 400.0;
end;

procedure PrintEloDifference(Value: double; var AFile: TextFile);
begin
  if IsInfinite(Value) then
  begin
    rtcSetBold(AFile);
    rtcSetFgColor(AFile, cclYellow);
    if Value > 0 then
      Write(AFile, 'oo')
    else
      Write(AFile, '-oo');
    rtcResetStyle(AFile);
    Exit;
  end;
  rtcSetBold(AFile);
  Write(AFile, Value: 0: 2);
  rtcResetStyle(AFile);
end;

procedure PrintEloDifference(const Value: TEloDifference; var AFile: TextFile);
begin
  Write(AFile, 'Elo difference = ');
  PrintEloDifference(Value.Low, AFile);
  Write(AFile, '/');
  PrintEloDifference(Value.Average, AFile);
  Write(AFile, '/');
  PrintEloDifference(Value.High, AFile);
  WriteLn(AFile, ' (low/avg/high, at p = 0.95)');
end;

function CalcConfidence(const R: TBattleResult): TConfidenceResult;
var
  Level: TConfidenceLevel;
  Mu, Sigma, Len: double;
begin
  Result.Level := clNone;
  Result.Side := csUnclear;

  Mu := R.WinRate;
  Sigma := R.WinRateStdDev;

  for Level := High(TConfidenceLevel) downto Low(TConfidenceLevel) do
  begin
    if Level = clNone then
      continue;

    Len := ProbThresholds[Level];

    if Mu - Len * Sigma >= 0.5 then
    begin
      Result.Level := Level;
      Result.Side := csFirst;
      exit;
    end;

    if Mu + Len * Sigma <= 0.5 then
    begin
      Result.Level := Level;
      Result.Side := csSecond;
      exit;
    end;
  end;
end;

procedure PrintConfidence(const C: TConfidenceResult; var AFile: TextFile);
var
  Level: TConfidenceLevel;
begin
  rtcSetBold(AFile);
  WriteLn(AFile, 'Confidence interval:');
  rtcResetStyle(AFile);
  for Level := Low(TConfidenceLevel) to High(TConfidenceLevel) do
  begin
    if Level = clNone then
      continue;
    Write(AFile, '  p = ', ProbValues[Level]: 0: 2, ': ');
    rtcSetBold(AFile);
    if integer(C.Level) < integer(Level) then
      WriteLn(AFile, 'Unclear')
    else if C.Side = csFirst then
    begin
      rtcSetFgColor(AFile, cclGreen);
      WriteLn(AFile, 'First wins');
    end
    else
    begin
      rtcSetFgColor(AFile, cclRed);
      WriteLn(AFile, 'Second wins');
    end;
    rtcResetStyle(AFile);
  end;
end;

procedure PrintConfidenceShort(const C: TConfidenceResult; var AFile: TextFile);
type
  TLevelType = (ltSmall, ltLarge);
const
  SideColors: array [TLevelType, TConfidenceSide] of TConsoleColor =
    ((cclBlue, cclYellow, cclPurple),
    (cclGreen, cclYellow, cclRed));
  FirstTitles: array [TConfidenceLevel] of string = ('', '+90', '+95', '+97', '+99');
  SecondTitles: array [TConfidenceLevel] of string = ('', '-90', '-95', '-97', '-99');
  LevelTypes: array [TConfidenceLevel] of TLevelType =
    (ltSmall, ltSmall, ltLarge, ltLarge, ltLarge);
begin
  rtcSetBold(AFile, integer(C.Level) > integer(cl95));
  rtcSetFgColor(AFile, SideColors[LevelTypes[C.Level], C.Side]);
  case C.Side of
    csFirst: Write(AFile, FirstTitles[C.Level]);
    csUnclear: Write(AFile, '?');
    csSecond: Write(AFile, SecondTitles[C.Level]);
  end;
  rtcResetStyle(AFile);
end;

{ TBattleResult }

function TBattleResult.WinRate: double;
begin
  Result := (Win + 0.5 * Draw) / (Win + Draw + Lose);
end;

function TBattleResult.WinRateStdDev: double;
var
  Mu: double;
  D: double;
begin
  Mu := WinRate;
  D := Mu * (1.0 - Mu) - Draw / (4.0 * Total);
  if D <= 0.0 then
    D := 0.0;
  Result := Sqrt(D) / Sqrt(Total);
end;

function TBattleResult.Total: double;
begin
  Result := Win + Draw + Lose;
end;

function TBattleResult.ToString: string;
begin
  Result := ScoreToStr(Win * 2 + Draw) + ':' + ScoreToStr(Lose * 2 + Draw);
end;

end.
