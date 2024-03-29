{
  This file is part of Battlefield - a tool to run micro-matches between chess
  engines.

  Copyright © 2020-2023 Alexander Kernozhitsky <sh200105@mail.ru>

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
program Battlefield;

{$Mode ObjFpc}{$H+}{$B-}

uses {$IFDEF UNIX}
  cthreads,
  cmem, {$ENDIF}
  Classes,
  SysUtils,
  EngineRunner,
  ParallelRunner,
  Progress,
  RichTextConsole,
  OpeningBook,
  VersionInfo,
  ChessTime,
  BattleStatistics;

const
  AppVersionCodename = 'Talk of Freedom';

  procedure ShowHelp(Banner: boolean = True);
  begin
    if Banner then
    begin
      WriteLn('"Clear the battlefield and let me see..."');
      WriteLn;
      WriteLn('BattleField - tool to run micro-matches between chess engines');
      WriteLn;
    end;
    WriteLn('Usage: battlefield [-h] [-v] [-q] [-j JOBS] [-o FILE] [-r FILE]');
    WriteLn('                   -g GAMES (-d DEPTH | -t TIME | -c CONTROL)');
    WriteLn('                   [-f FEN_FILE] [-s SCORE] ENGINE1 ENGINE2');
    WriteLn;
    WriteLn('  -h           Show this help and exit');
    WriteLn('  -v           Show version info and exit');
    WriteLn('  -q           Do not show progress');
    WriteLn('  -j JOBS      Specify number of games to run simultaneoulsly');
    WriteLn('  -o FILE      Write the played games into FILE, in PGN format');
    WriteLn('  -r FILE      Write the played games into FILE, in SoFGameSet format.');
    WriteLn('               See below to learn about SoFGameSet');
    WriteLn('  -g GAMES     Number of games to run');
    WriteLn('  -d DEPTH     Run engines on fixed depth. You must specify one of -d,');
    WriteLn('               -t or -c');
    WriteLn('  -t TIME      Run engines on fixed time (in milliseconds) per move.');
    WriteLn('               You must specify one of -d, -t or -c');
    WriteLn('  -c CONTROL   Run engines on time control CONTROL. The time control');
    WriteLn('               format is the same as in PGN files (see below for more');
    WriteLn('               details). You must specify one of -d, -t or -c');
    WriteLn('  -f FEN_FILE  Start games from positions found in FEN_FILE. By');
    WriteLn('               default, the games are started from positions in the');
    WriteLn('               built-in opening book');
    WriteLn('  -s SCORE     Terminate the game after both sides agree that the');
    WriteLn('               score is larger than SCORE centipawns for the same side');
    WriteLn;
    WriteLn('# Time control format');
    WriteLn;
    WriteLn('Time control format must consist of one or more stages separated by');
    WriteLn('":". Each stage must have one of the following formats: T, M/T, T+I');
    WriteLn('or M/T+I, where M is the number of moves in the stage, T is the');
    WriteLn('amount of time in seconds given for the stage, and I is the increment');
    WriteLn('in seconds per each move. Note that the last stage is repeated.');
    WriteLn('Infinite stages are not allowed, even though they are allowed by PGN');
    WriteLn('specs. You can also specify different time control for white and black');
    WriteLn('(though using this feature is disregarded). To do this, you must');
    WriteLn('separate time control for white and black with "|".');
    WriteLn;
    WriteLn('For example, "40/900+5:900+5" means 15 minutes for 40 moves plus 5');
    WriteLn('seconds each move. After 40 moves pass, you are given 15 minutes for');
    WriteLn('the rest of the game plus 5 seconds each move. And "300|240" means 5');
    WriteLn('minutes per game for white, and 4 minutes per game for black.');
    WriteLn;
    WriteLn('# SoFGameSet format');
    WriteLn;
    WriteLn('To learn about SoFGameSet format, see the following specification:');
    WriteLn('https://github.com/alex65536/sofcheck/blob/master/docs/gameset.md.');
  end;

  procedure ShowError(const Error: string);
  begin
    WriteLn(StdErr, 'Error: ', Error);
    WriteLn(StdErr, '---');
    ShowHelp(False);
    halt(1);
  end;

  function IsTimeControlValid(const S: string): boolean;
  var
    Control: TTimeControlPair;
  begin
    Control := TTimeControlPair.Create;
    try
      try
        Control.TimeControlString := S;
      except
        on E: ETimeControlStringRead do
        begin
          FreeAndNil(Control);
          Exit(False);
        end;
      end;
      Result := not Control.IsInfinite;
    finally
      FreeAndNil(Control);
    end;
  end;

var
  FirstEngine: string = '';
  SecondEngine: string = '';
  PgnFile: string = '';
  GamesetFile: string = '';
  FenFile: string = '';
  Options: REngineOptions;
  HasOptions: boolean = False;
  Games: integer = 0;
  Jobs: integer = 0;
  Quiet: boolean = False;

  Runner: TParallelRunner = nil;
  Stream: TFileStream = nil;
  RunnerProgress: TParallelRunnerProgress = nil;

  Book: TAbstractOpeningBook = nil;

  Param: integer = 1;

{$R *.res}

begin
  if ParamCount = 0 then
  begin
    ShowHelp(True);
    halt(0);
  end;
  while Param <= ParamCount do
  begin
    if ParamStr(Param) = '-h' then
    begin
      ShowHelp(True);
      Halt(0);
    end;
    if ParamStr(Param) = '-v' then
    begin
      WriteLn('BattleField version ' + GetAppVersion + ' "' + AppVersionCodename + '"');
      WriteLn('Copyright (C) 2020-2023 Alexander Kernozhitsky');
      WriteLn;
      WriteLn('Build time: ' + GetAppBuildTime);
      WriteLn('Target: ' + GetAppTarget);
      WriteLn('Compiler: FPC ' + FpcVersion);
      WriteLn;
      WriteLn('This is free software; see the source for copying conditions.  There is NO');
      WriteLn('warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
      WriteLn;
      WriteLn('BattleField uses Graham2014-1F.cgb opening book by Graham Banks.');
      WriteLn('Source: https://www.talkchess.com/forum3/viewtopic.php?t=50541#p549216.');
      Halt(0);
    end;
    if ParamStr(Param) = '-q' then
    begin
      Quiet := True;
      Inc(Param);
      continue;
    end;
    if ParamStr(Param) = '-j' then
    begin
      if Param = ParamCount then
        ShowError('JOBS expected');
      Jobs := StrToInt(ParamStr(Param + 1));
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-g' then
    begin
      if Param = ParamCount then
        ShowError('GAMES expected');
      Games := StrToInt(ParamStr(Param + 1));
      if Games = 0 then
        ShowError('GAMES must be positive');
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-d' then
    begin
      if HasOptions then
        ShowError('-d, -t or -c is already specified');
      if Param = ParamCount then
        ShowError('DEPTH expected');
      HasOptions := True;
      Options.TimeControlKind := eoFixedDepth;
      Options.FixedDepth := StrToInt(ParamStr(Param + 1));
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-t' then
    begin
      if HasOptions then
        ShowError('-d, -t or -c is already specified');
      if Param = ParamCount then
        ShowError('TIME expected');
      HasOptions := True;
      Options.TimeControlKind := eoFixedTime;
      Options.FixedTime := StrToInt(ParamStr(Param + 1));
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-c' then
    begin
      if HasOptions then
        ShowError('-d, -t or -c is already specified');
      if Param = ParamCount then
        ShowError('CONTROL expected');
      HasOptions := True;
      Options.TimeControlKind := eoTimeControl;
      Options.TimeControl := ParamStr(Param + 1);
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-o' then
    begin
      if Param = ParamCount then
        ShowError('FILE expected');
      PgnFile := ParamStr(Param + 1);
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-r' then
    begin
      if Param = ParamCount then
        ShowError('FILE expected');
      GamesetFile := ParamStr(Param + 1);
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-f' then
    begin
      if Param = ParamCount then
        ShowError('FEN_FILE expected');
      FenFile := ParamStr(Param + 1);
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-s' then
    begin
      if Param = ParamCount then
        ShowError('SCORE expected');
      Options.ScoreThreshold := StrToInt(ParamStr(Param + 1));
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param).StartsWith('-') then
      ShowError('Unknown flag ' + ParamStr(Param));
    if FirstEngine = '' then
      FirstEngine := ParamStr(Param)
    else if SecondEngine = '' then
      SecondEngine := ParamStr(Param)
    else
      ShowError('Extra arguments');
    Inc(Param);
  end;
  if Games = 0 then
    ShowError('GAMES not specified');
  if FirstEngine = '' then
    ShowError('ENGINE1 not specified');
  if SecondEngine = '' then
    ShowError('ENGINE2 not specified');
  if not HasOptions then
    ShowError('DEPTH, TIME or CONTROL not specified');
  if Games <= 0 then
    ShowError('GAMES must be positive');
  if Jobs < 0 then
    ShowError('JOBS must be non-negative');
  case Options.TimeControlKind of
    eoFixedTime:
      if Options.FixedTime <= 0 then
        ShowError('TIME must be positive');
    eoFixedDepth:
      if Options.FixedDepth <= 0 then
        ShowError('DEPTH must be positive');
    eoTimeControl:
      if not IsTimeControlValid(Options.TimeControl) then
        ShowError('CONTROL "' + Options.TimeControl + '" is either invalid or infinite');
  end;
  if Options.ScoreThreshold < 0 then
    ShowError('SCORE must be non-negative');

  try
    if FenFile <> '' then
      Book := TFenListOpeningBook.Create(FenFile);
    Runner := TParallelRunner.Create(Games, FirstEngine, SecondEngine,
      Options, Jobs, Book);
    if not Quiet then
      RunnerProgress := TParallelRunnerProgress.Create(Games, Runner.Jobs);
    Runner.Progress := RunnerProgress;
    Runner.Start;
    Runner.Join;
    if PgnFile <> '' then
    begin
      Stream := TFileStream.Create(PgnFile, fmCreate or fmOpenWrite);
      Runner.SaveGamesAsPgn(Stream);
    end;
    if GamesetFile <> '' then
    begin
      FreeAndNil(Stream);
      Stream := TFileStream.Create(GamesetFile, fmCreate or fmOpenWrite);
      Runner.SaveGamesAsGameset(Stream);
    end;
    WriteLn('Wins: ', Runner.FirstWins, ', Loses: ', Runner.SecondWins,
      ', Draws: ', Runner.Draws);
    WriteLn('Score: ', Runner.BattleResult.ToString);
    PrintConfidence(CalcConfidence(Runner.BattleResult), StdOut);
    rtcSetBold;
    WriteLn('Other stats:');
    rtcResetStyle;
    PrintLOS(CalcLOS(Runner.BattleResult), StdOut);
    PrintEloDifference(CalcEloDifference(Runner.BattleResult), StdOut);
  finally
    FreeAndNil(Runner);
    FreeAndNil(RunnerProgress);
    FreeAndNil(Stream);
  end;
end.
