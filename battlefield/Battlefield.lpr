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
  ScoreUtils,
  RichTextConsole,
  OpeningBook,
  VersionInfo;

  procedure ShowHelp(Banner: boolean = True);
  begin
    if Banner then
    begin
      WriteLn('"Clear the battlefield and let me see..."');
      WriteLn;
      WriteLn('BattleField - tool to run micro-matches between chess engines');
    end;
    WriteLn('Usage: battlefield [-h] [-v] [-q] [-m] [-j JOBS] [-o PGN_FILE]');
    WriteLn('                   [-r FILE] -g GAMES [-d DEPTH] [-t TIMES]');
    WriteLn('                   [-f FEN_FILE] [-s SCORE] ENGINE1 ENGINE2');
    WriteLn;
    WriteLn('  -h           Show this help and exit');
    WriteLn('  -v           Show version info and exit');
    WriteLn('  -q           Do not show progress');
    WriteLn('  -m           Print metrics during run');
    WriteLn('  -j JOBS      Specify number of games to run simultaneoulsly');
    WriteLn('  -o PGN_FILE  Write PGN of the games into PGN_FILE');
    WriteLn('  -r FILE      Write the positions occurred in the game, in format');
    WriteLn('               recognized by SoFCheck''s MakeDataset');
    WriteLn('  -g GAMES     Number of games to run');
    WriteLn('  -d DEPTH     Run engines on fixed depth. You must specify either -d');
    WriteLn('               or -t');
    WriteLn('  -t TIME      Run engines on fixed time (in milliseconds) per move.');
    WriteLn('               You must specify either -d or -t');
    WriteLn('  -f FEN_FILE  Start games from positions found in FEN_FILE. By');
    WriteLn('               default, the games are started from positions in the');
    WriteLn('               built-in opening book');
    WriteLn('  -s SCORE     Terminate the game after both sides agree that the');
    WriteLn('               score is larger than SCORE centipawns for the same side');
  end;

  procedure ShowError(const Error: string);
  begin
    WriteLn(StdErr, 'Error: ', Error);
    WriteLn(StdErr, '---');
    ShowHelp(False);
    halt(1);
  end;

const
  P0_9 = 1.64485362695147;
  P0_95 = 1.95996398454005;
  P0_97 = 2.17009037758456;
  P0_99 = 2.57582930354890;

  procedure ProbabilityCheck(P, Len: double; Win, Draw, Count: integer);
  var
    Window, Left, Right, Prob: double;
  begin
    Prob := (2 * Win + Draw) / (2 * Count);
    Window := Len * (Sqrt(Prob * (1 - Prob)) / Sqrt(Count));
    Left := Prob - Window;
    Right := Prob + Window;
    Write('  p = ', P: 0: 2, ': ');
    rtcSetBold;
    if Left >= 0.5 then
    begin
      rtcSetFgColor(cclGreen);
      WriteLn('First wins');
    end
    else if Right <= 0.5 then
    begin
      rtcSetFgColor(cclRed);
      WriteLn('Second wins');
    end
    else
      WriteLn('Unclear');
    rtcResetStyle;
  end;

var
  FirstEngine: string = '';
  SecondEngine: string = '';
  PgnFile: string = '';
  DatasetFile: string = '';
  FenFile: string = '';
  Options: REngineOptions;
  HasOptions: boolean = False;
  Games: integer = 0;
  Jobs: integer = 0;
  Quiet: boolean = False;
  PrintMetrics: boolean = False;

  Runner: TParallelRunner = nil;
  Stream: TFileStream = nil;
  RunnerProgress: TParallelRunnerProgress = nil;
  MetricPrinter: TParallelRunnerMetricPrinter = nil;

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
      WriteLn('BattleField version ' + GetAppVersion);
      WriteLn('Build time: ' + GetAppBuildTime);
      WriteLn('Target: ' + GetAppTarget);
      WriteLn('Compiler: FPC ' + FpcVersion);
      Halt(0);
    end;
    if ParamStr(Param) = '-q' then
    begin
      Quiet := True;
      Inc(Param);
      continue;
    end;
    if ParamStr(Param) = '-m' then
    begin
      PrintMetrics := True;
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
        ShowError('-d or -t is already specified');
      if Param = ParamCount then
        ShowError('DEPTH expected');
      HasOptions := True;
      Options.TimeControlKind := eoDepth;
      Options.TimeControl := StrToInt(ParamStr(Param + 1));
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-t' then
    begin
      if HasOptions then
        ShowError('-d or -t is already specified');
      if Param = ParamCount then
        ShowError('TIME expected');
      HasOptions := True;
      Options.TimeControlKind := eoTime;
      Options.TimeControl := StrToInt(ParamStr(Param + 1));
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-o' then
    begin
      if Param = ParamCount then
        ShowError('PGN_FILE expected');
      PgnFile := ParamStr(Param + 1);
      Inc(Param, 2);
      continue;
    end;
    if ParamStr(Param) = '-r' then
    begin
      if Param = ParamCount then
        ShowError('FILE expected');
      DatasetFile := ParamStr(Param + 1);
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
    ShowError('DEPTH or TIME not specified');
  if Games <= 0 then
    ShowError('GAMES must be positive');
  if Jobs < 0 then
    ShowError('JOBS must be non-negative');
  if Options.TimeControl <= 0 then
    ShowError('DEPTH and TIME must be positive');
  if Options.ScoreThreshold < 0 then
    ShowError('SCORE must be non-negative');

  try
    if not Quiet then
      RunnerProgress := TParallelRunnerProgress.Create(Games);
    if PrintMetrics then
      MetricPrinter := TParallelRunnerMetricPrinter.Create;
    if FenFile <> '' then
      Book := TFenListOpeningBook.Create(FenFile);
    Runner := TParallelRunner.Create(Games, FirstEngine, SecondEngine,
      Options, Jobs, Book);
    Runner.Progress := RunnerProgress;
    Runner.MetricPrinter := MetricPrinter;
    Runner.Join;
    if PgnFile <> '' then
    begin
      Stream := TFileStream.Create(PgnFile, fmCreate or fmOpenWrite);
      Runner.SaveGamesAsPgn(Stream);
    end;
    if DatasetFile <> '' then
    begin
      FreeAndNil(Stream);
      Stream := TFileStream.Create(DatasetFile, fmCreate or fmOpenWrite);
      Runner.SaveGamesAsDataset(Stream);
    end;
    WriteLn('Wins: ', Runner.FirstWins, ', Loses: ', Runner.SecondWins,
      ', Draws: ', Runner.Draws);
    WriteLn('Score: ', ScorePairToStr(Runner.FirstWins, Runner.Draws,
      Runner.SecondWins));
    WriteLn('Checking confidence interval:');
    ProbabilityCheck(0.9, P0_9, Runner.FirstWins, Runner.Draws, Games);
    ProbabilityCheck(0.95, P0_95, Runner.FirstWins, Runner.Draws, Games);
    ProbabilityCheck(0.97, P0_97, Runner.FirstWins, Runner.Draws, Games);
    ProbabilityCheck(0.99, P0_99, Runner.FirstWins, Runner.Draws, Games);
  finally
    FreeAndNil(Runner);
    FreeAndNil(RunnerProgress);
    FreeAndNil(MetricPrinter);
    FreeAndNil(Stream);
  end;
end.
