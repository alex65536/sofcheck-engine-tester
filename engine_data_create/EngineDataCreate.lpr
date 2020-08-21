program EngineDataCreate;

{$Mode ObjFpc}{$H+}{$B-}{$COperators On}

uses
  ChessRules,
  PGNSplitter,
  Classes,
  SysUtils,
  DateUtils,
  MoveConverters,
  ChessEngines,
  EngineScores,
  gutil,
  gset,
  gvector,
  Utils;

type
  TStringLess = specialize TLess<string>;
  TStringSet = specialize TSet<string, TStringLess>;
  TStringVector = specialize TVector<string>;

  { TBoardCollector }

  TBoardCollector = class
  public
    constructor Create(const AEngineExe: string);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure Add(const RawBoard: RRawBoard);
  private
    FBoard: TChessBoard;
    FUsedPositions: TStringSet;
    FEngine: TUCIChessEngine;
    FEngineRunning: boolean;

    procedure AddMoves;
    procedure EvaluateMove(const Move: RChessMove);

    procedure EngineAnalysisMessage(Sender: TObject; Message: TAnalysisMessage);
  end;

  { TBoardCollector }

  constructor TBoardCollector.Create(const AEngineExe: string);
  begin
    FBoard := TChessBoard.Create(False);
    FUsedPositions := TStringSet.Create;
    FEngine := TUCIChessEngine.Create(AEngineExe);
    FEngine.OnAnalysisMessage := @EngineAnalysisMessage;
    FEngineRunning := False;
    FEngine.Initialize;
  end;

  destructor TBoardCollector.Destroy;
  begin
    FreeAndNil(FBoard);
    FreeAndNil(FUsedPositions);
    FreeAndNil(FEngine);
    inherited;
  end;

  procedure TBoardCollector.BeforeDestruction;
  begin
    inherited BeforeDestruction;
    FEngine.Uninitialize;
  end;

  procedure TBoardCollector.Add(const RawBoard: RRawBoard);
  var
    FEN: string;
  begin
    FBoard.RawBoard := RawBoard;
    if FBoard.GetGameResult.Kind <> geNone then
      exit;
    FEN := FBoard.FENString;
    if FUsedPositions.Find(FEN) <> nil then
      exit;
    FUsedPositions.Insert(FEN);
    FBoard.GenerateMoves;
    WriteLn('fen ', FEN);
    AddMoves;
    WriteLn('end fen');
  end;

  procedure TBoardCollector.AddMoves;
  var
    MoveConverter: TUCIMoveConverter;
    Move: RChessMove;
    I: integer;
  begin
    MoveConverter := TUCIMoveConverter.Create(FBoard.RawBoard);
    try
      for I := 0 to FBoard.MoveCount - 1 do
      begin
        Move := FBoard.Moves[I];
        WriteLn('move ', MoveConverter.GetMoveString(Move));
        EvaluateMove(Move);
        WriteLn('end move');
      end;
    finally
      FreeAndNil(MoveConverter);
    end;
  end;

  procedure TBoardCollector.EvaluateMove(const Move: RChessMove);
  begin
    FEngineRunning := True;
    try
      with FEngine do
      begin
        MoveChain.Clear(FBoard.RawBoard);
        MoveChain.Add(Move);
        StartFixedTime(200);
        WaitForStop(MaxInt);
      end;
    finally
      FEngineRunning := False;
    end;
  end;

  procedure TBoardCollector.EngineAnalysisMessage(Sender: TObject;
    Message: TAnalysisMessage);
  var
    Line: TAnalysisLine;
  begin
    if not FEngineRunning then
      exit;
    if not (Message is TAnalysisLine) then
      exit;
    Line := Message as TAnalysisLine;
    Write('depth ', FEngine.State.Depth);
    if Line.Score.Mate = DefaultMate then
      Write(' score ', Line.Score.Score)
    else
      Write(' mate ', Line.Score.Mate);
    WriteLn;
  end;

  procedure PrintHelp;
  begin
    WriteLn(StdErr, 'Usage: ', ExtractFileName(ParamStr(0)), ' [-e ENGINE] PGNS...');
    halt(1);
  end;

var
  FileNames: TStringVector;
  List: TBoardList;
  Collector: TBoardCollector;
  Engine: string;
  I, J: integer;
  FileName: string;
  RawBoard: RRawBoard;
  Positions: integer;
  StartTime: TDateTime;
  TimeSec: double;
begin
  Randomize;
  Engine := 'stockfish';
  FileNames := nil;
  List := nil;
  Collector := nil;
  I := 1;
  try
    FileNames := TStringVector.Create;
    while I <= ParamCount do
    begin
      if ParamStr(I) = '-e' then
      begin
        if I = ParamCount then
          PrintHelp;
        Inc(I);
        Engine := ParamStr(I);
        Inc(I);
        continue;
      end;
      FileNames.PushBack(ParamStr(I));
      Inc(I);
    end;
    if FileNames.IsEmpty then
      PrintHelp;
    List := TBoardList.Create;
    for FileName in FileNames do
      SplitPGNFile(FileName, List);
    for I := 1 to integer(List.Size) - 1 do
    begin
      J := Random(I);
      RawBoard := List[I];
      List[I] := List[J];
      List[J] := RawBoard;
    end;
    Collector := TBoardCollector.Create(Engine);
    StartTime := Now;
    for RawBoard in List do
    begin
      Collector.Add(RawBoard);
      Inc(Positions);
      TimeSec := SecondSpan(StartTime, Now);
      WriteLn(StdErr, Format('Progress: %d/%d positions (%s/%s)',
        [Positions, List.Size, HumanTimeString(TimeSec),
        HumanTimeString(TimeSec / Positions * List.Size)]));
    end;
  finally
    FreeAndNil(Collector);
    FreeAndNil(List);
    FreeAndNil(FileNames);
  end;
end.
