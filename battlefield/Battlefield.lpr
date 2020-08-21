program Battlefield;

{$Mode ObjFpc}{$H+}{$B-}

uses {$IFDEF UNIX}
  cthreads,
  cmem, {$ENDIF}
  Classes,
  SysUtils,
  EngineRunner,
  ParallelRunner,
  Progress;

  procedure ShowHelp(Banner: boolean = True);
  begin
    if Banner then
    begin
      WriteLn('"Clear the battlefield and let me see..."');
      WriteLn;
      WriteLn('BattleField - tool to run micro-tournaments between chess engines');
    end;
    WriteLn('Usage: battlefield [-h] [-q] [-j JOBS] [-o PGN_FILE] -g GAMES');
    WriteLn('                   [-d DEPTH] [-t TIMES] ENGINE1 ENGINE2');
    WriteLn;
    WriteLn('  -h           Show this help and exit');
    WriteLn('  -q           Do not show progress');
    WriteLn('  -j JOBS      Specify number of games to run simultaneoulsly');
    WriteLn('  -o PGN_FILE  Write PGN of the games into PGN_FILE');
    WriteLn('  -g GAMES     Number of games to run');
    WriteLn('  -d DEPTH     Run engines on fixed depth. You must specify either -d');
    WriteLn('               or -t');
    WriteLn('  -t TIME      Run engines on fixed time (in milliseconds) per move.');
    WriteLn('               You must specify either -d or -t');
  end;

  procedure ShowError(const Error: string);
  begin
    WriteLn(StdErr, 'Error: ', Error);
    WriteLn(StdErr, '---');
    ShowHelp(False);
    halt(1);
  end;

  function ScoreToStr(Score: integer): string;
  begin
    if Score mod 2 = 0 then
      Result := IntToStr(Score div 2) + '.0'
    else
      Result := IntToStr(Score div 2) + '.5';
  end;

var
  FirstEngine: string = '';
  SecondEngine: string = '';
  PgnFile: string = '';
  Options: REngineOptions;
  HasOptions: boolean = False;
  Games: integer = 0;
  Jobs: integer = 0;
  Quiet: boolean = False;

  Runner: TParallelRunner = nil;
  Stream: TFileStream = nil;
  RunnerProgress: TProgress = nil;
  FirstScore: integer = 0;
  SecondScore: integer = 0;

  Param: integer = 1;
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
        ShowError('-d or -t is already specified');
      if Param = ParamCount then
        ShowError('DEPTH expected');
      HasOptions := True;
      Options.Kind := eoDepth;
      Options.Value := StrToInt(ParamStr(Param + 1));
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
      Options.Kind := eoTime;
      Options.Value := StrToInt(ParamStr(Param + 1));
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
  if Options.Value <= 0 then
    ShowError('DEPTH and TIME must be positive');

  try
    if not Quiet then
      RunnerProgress := TProgress.Create(Games);
    Runner := TParallelRunner.Create(Games, FirstEngine, SecondEngine,
      Options, Jobs, RunnerProgress);
    Runner.Join;
    if PgnFile <> '' then
    begin
      Stream := TFileStream.Create(PgnFile, fmCreate or fmOpenWrite);
      Runner.SaveGamesToStream(Stream);
    end;
    WriteLn('Wins: ', Runner.FirstWins, ', Loses: ', Runner.SecondWins,
      ', Draws: ', Runner.Draws);
    FirstScore := Runner.FirstWins * 2 + Runner.Draws;
    SecondScore := Runner.SecondWins * 2 + Runner.Draws;
    WriteLn('Score: ', ScoreToStr(FirstScore), ':', ScoreToStr(SecondScore));
  finally
    FreeAndNil(Runner);
    FreeAndNil(RunnerProgress);
    FreeAndNil(Stream);
  end;
end.
