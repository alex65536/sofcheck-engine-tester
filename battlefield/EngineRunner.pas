unit EngineRunner;

{$mode objfpc}{$H+}{$B-}{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, ChessRules, ChessEngines, gvector, MoveConverters,
  gvector, MoveConverters, OpeningBook, EngineScores, GameNotation, GameMetrics;

type
  TEngineMatchWinner = (ewFirst, ewDraw, ewSecond);

  TGameVector = specialize TVector<TMetricGameNotation>;

  TEngineTimeControlKind = (eoTime, eoDepth);

  REngineOptions = record
    TimeControlKind: TEngineTimeControlKind;
    TimeControl: int64;
    // Terminate the game when both sides agree that one of them wins with
    // Score >= ScoreThreshold. Must be set to zero for no threshold.
    ScoreThreshold: integer;
  end;

  { TEngineFactory }

  TEngineFactory = class
  public
    function CreateChessEngine: TAbstractChessEngine;
  protected
    function DoCreateChessEngine: TAbstractChessEngine; virtual; abstract;
  end;

  { TUciEngineFactory }

  TUciEngineFactory = class(TEngineFactory)
  private
    FExeName: string;
  public
    constructor Create(const AExeName: string);
  protected
    function DoCreateChessEngine: TAbstractChessEngine; override;
  end;

  { TEngineRunner }

  TEngineRunner = class
  private
    FFirstEngine: TAbstractChessEngine;
    FSecondEngine: TAbstractChessEngine;
    FFirstFactory: TEngineFactory;
    FSecondFactory: TEngineFactory;
    FGames: TGameVector;
    FEngineResult: RAnalysisResult;
    FBook: TAbstractOpeningBook;

    procedure EngineStop(Sender: TObject; const EngineResult: RAnalysisResult);

    function GetGames(I: integer): TMetricGameNotation;
    function GetGameCount: integer;
  public
    constructor Create(FirstFactory, SecondFactory: TEngineFactory;
      Book: TAbstractOpeningBook);
    destructor Destroy; override;

    function LastGame: TMetricGameNotation;

    property GameCount: integer read GetGameCount;
    property Games[I: integer]: TMetricGameNotation read GetGames;

    function Play(const Options: REngineOptions;
      SwitchSides: boolean): TEngineMatchWinner;

    procedure BeforeDestruction; override;
  end;

implementation

function PredictGameWinner(const Score: RPositionScore; Threshold: integer): TGameWinner;
begin
  if (Threshold = 0) or (Score = DefaultPositionScore) or (Score.Kind <> skNormal) then
    Exit(gwNone);
  if Score.Mate <> DefaultMate then
  begin
    if Score.Mate >= 0 then
      Result := gwWhite
    else
      Result := gwBlack;
    Exit;
  end;
  if Score.Score >= Threshold then
    Result := gwWhite
  else if Score.Score <= -Threshold then
    Result := gwBlack
  else
    Result := gwNone;
end;

{ TUciEngineFactory }

constructor TUciEngineFactory.Create(const AExeName: string);
begin
  FExeName := AExeName;
end;

function TUciEngineFactory.DoCreateChessEngine: TAbstractChessEngine;
begin
  Result := TUCIChessEngine.Create(FExeName);
end;

{ TEngineFactory }

function TEngineFactory.CreateChessEngine: TAbstractChessEngine;
begin
  Result := DoCreateChessEngine;
  try
    Result.Initialize;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TEngineRunner }

procedure TEngineRunner.EngineStop(Sender: TObject;
  const EngineResult: RAnalysisResult);
begin
  FEngineResult := EngineResult;
end;

function TEngineRunner.GetGames(I: integer): TMetricGameNotation;
begin
  Result := FGames[I];
end;

function TEngineRunner.GetGameCount: integer;
begin
  Result := FGames.Size;
end;

constructor TEngineRunner.Create(FirstFactory, SecondFactory: TEngineFactory;
  Book: TAbstractOpeningBook);
begin
  FBook := Book;
  FFirstFactory := FirstFactory;
  FSecondFactory := SecondFactory;
  FFirstEngine := FirstFactory.CreateChessEngine;
  FSecondEngine := SecondFactory.CreateChessEngine;
  FGames := TGameVector.Create;
end;

destructor TEngineRunner.Destroy;
var
  I: integer;
begin
  if FGames <> nil then
  begin
    for I := 0 to integer(FGames.Size) - 1 do
      FGames[I].Free;
  end;
  FreeAndNil(FGames);
  FreeAndNil(FFirstEngine);
  FreeAndNil(FSecondEngine);
  FreeAndNil(FBook);
  inherited;
end;

function TEngineRunner.LastGame: TMetricGameNotation;
begin
  Result := FGames.Back;
end;

function TEngineRunner.Play(const Options: REngineOptions;
  SwitchSides: boolean): TEngineMatchWinner;
var
  CurGame: TMetricGameNotation;
  Engines: array [TPieceColor] of TAbstractChessEngine;
  GameResult: RGameResult;
  Color: TPieceColor;
  Move: RChessMove;
  UciConverter: TUCIMoveConverter;
  WinPredicts: array [TPieceColor] of TGameWinner;
  RunOk: boolean;

  function RunEngine(Engine: TAbstractChessEngine): boolean;
  var
    TimeBudget: integer;
  begin
    TimeBudget := MaxInt;
    Engine.OnStop := @EngineStop;
    case Options.TimeControlKind of
      eoDepth: Engine.StartFixedDepth(Options.TimeControl);
      eoTime:
      begin
        Engine.StartFixedTime(Options.TimeControl);
        TimeBudget := Options.TimeControl + 500;
      end
      else
        raise Exception.Create('Some time control types are not supported');
    end;
    Result := Engine.WaitForStop(TimeBudget);
  end;

  procedure RestartEngine(Engine: TAbstractChessEngine);
  begin
    if Engine = FFirstEngine then
    begin
      FreeAndNil(FFirstEngine);
      FFirstEngine := FFirstFactory.CreateChessEngine;
    end
    else if Engine = FSecondEngine then
    begin
      FreeAndNil(FSecondEngine);
      FSecondEngine := FSecondFactory.CreateChessEngine;
    end;
  end;

begin
  UciConverter := nil;
  CurGame := TMetricGameNotation.Create;
  WinPredicts[pcWhite] := gwNone;
  WinPredicts[pcBlack] := gwNone;
  try
    FBook.FillOpening(CurGame.Chain);
    CurGame.SwitchSides := SwitchSides;
    CurGame.PadZeroScores;
    UciConverter := TUCIMoveConverter.Create;
    FFirstEngine.MoveChain.Assign(CurGame.Chain);
    FSecondEngine.MoveChain.Assign(CurGame.Chain);
    FFirstEngine.NewGame;
    FSecondEngine.NewGame;
    if SwitchSides then
    begin
      Engines[pcWhite] := FSecondEngine;
      Engines[pcBlack] := FFirstEngine;
    end
    else
    begin
      Engines[pcWhite] := FFirstEngine;
      Engines[pcBlack] := FSecondEngine;
    end;
    CurGame.WhiteName := Engines[pcWhite].Name + ' at ' + Engines[pcWhite].FileName;
    CurGame.BlackName := Engines[pcBlack].Name + ' at ' + Engines[pcBlack].FileName;
    while True do
    begin
      GameResult := CurGame.Chain.GetGameResult;
      if GameResult.Winner <> gwNone then
      begin
        CurGame.Winner := GameResult.Winner;
        break;
      end;
      Color := CurGame.Chain.Boards[CurGame.Chain.Count - 1].MoveSide;
      RunOk := RunEngine(Engines[Color]);
      if Engines[Color].Terminated or (not RunOk) then
      begin
        if Engines[Color].Terminated then
          WriteLn(StdErr, 'Engine "' + Engines[Color].FileName + '" died :(')
        else
        begin
          WriteLn(StdErr, 'Engine "' + Engines[Color].FileName +
            '" is not responding :(');
          Engines[Color].Kill;
        end;
        if Color = pcWhite then
          CurGame.Winner := gwBlack
        else
          CurGame.Winner := gwWhite;
        RestartEngine(Engines[Color]);
        break;
      end;
      Move := FEngineResult.BestMove;
      try
        CurGame.AddMove(Move, Engines[Color].State.Score);
        FFirstEngine.MoveChain.Add(Move);
        FSecondEngine.MoveChain.Add(Move);
      except
        on E: EChessRules do
        begin
          WriteLn(StdErr, 'Engine "' + Engines[Color].FileName +
            '" played an invalid move!');
          if Color = pcWhite then
            CurGame.Winner := gwBlack
          else
            CurGame.Winner := gwWhite;
          break;
        end;
      end;
      WinPredicts[Color] := PredictGameWinner(Engines[Color].State.Score,
        Options.ScoreThreshold);
      if (WinPredicts[pcWhite] = WinPredicts[pcBlack]) and
        (WinPredicts[pcWhite] <> gwNone) then
      begin
        CurGame.Winner := WinPredicts[pcWhite];
        break;
      end;
    end;
    case CurGame.Winner of
      gwWhite: if SwitchSides then
          Result := ewSecond
        else
          Result := ewFirst;
      gwBlack: if SwitchSides then
          Result := ewFirst
        else
          Result := ewSecond;
      gwDraw: Result := ewDraw;
    end;
    CurGame.CalculateMetrics;
  except
    FreeAndNil(CurGame);
    FreeAndNil(UciConverter);
    raise;
  end;
  FreeAndNil(UciConverter);
  FGames.PushBack(CurGame);
end;

procedure TEngineRunner.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FFirstEngine <> nil then
    FFirstEngine.Uninitialize;
  if FSecondEngine <> nil then
    FSecondEngine.Uninitialize;
end;

end.
