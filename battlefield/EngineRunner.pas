unit EngineRunner;

{$mode objfpc}{$H+}{$B-}{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, MoveChains, ChessRules, ChessEngines,
  gvector, PGNUtils, MoveConverters, NotationLists, OpeningBook, EngineScores;

type
  TEngineMatchWinner = (ewFirst, ewDraw, ewSecond);

  { RGame }

  RGame = record
    Chain: TMoveChain;
    WhiteName: string;
    BlackName: string;
    Winner: TGameWinner;

    function ToString: string;
  end;

  TGameVector = specialize TVector<RGame>;

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

    function GetGames(I: integer): RGame;
    function GetGameCount: integer;
  public
    constructor Create(FirstFactory, SecondFactory: TEngineFactory;
      Book: TAbstractOpeningBook);
    destructor Destroy; override;

    function LastGame: RGame;

    property GameCount: integer read GetGameCount;
    property Games[I: integer]: RGame read GetGames;

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

{ RGame }

function RGame.ToString: string;
var
  Converter: TPGNMoveConverter;
  Board: TChessBoard;
begin
  Converter := TPGNMoveConverter.Create;
  try
    Result := '[White "' + StringToTagValue(WhiteName) + '"]' +
      LineEnding + '[Black "' + StringToTagValue(BlackName) + '"]';
    if Chain.Boards[-1] <> GetInitialPosition then
    begin
      Board := TChessBoard.Create(False);
      try
        Board.RawBoard := Chain.Boards[-1];
        Result := Result + LineEnding + '[SetUp "1"]' + LineEnding +
          '[FEN "' + Board.FENString + '"]';
      finally
        FreeAndNil(Board);
      end;
    end;
    Result := Result + LineEnding + LineEnding +
      Chain.ConvertToString(Converter, ' ') + ' ' + GameResultMeanings[Winner] +
      LineEnding;
  finally
    FreeAndNil(Converter);
  end;
end;

{ TEngineRunner }

procedure TEngineRunner.EngineStop(Sender: TObject;
  const EngineResult: RAnalysisResult);
begin
  FEngineResult := EngineResult;
end;

function TEngineRunner.GetGames(I: integer): RGame;
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
      FGames[I].Chain.Free;
  end;
  FreeAndNil(FGames);
  FreeAndNil(FFirstEngine);
  FreeAndNil(FSecondEngine);
  FreeAndNil(FBook);
  inherited;
end;

function TEngineRunner.LastGame: RGame;
begin
  Result := FGames.Back;
end;

function TEngineRunner.Play(const Options: REngineOptions;
  SwitchSides: boolean): TEngineMatchWinner;
var
  CurGame: RGame;
  Chain: TMoveChain;
  Engines: array [TPieceColor] of TAbstractChessEngine;
  GameResult: RGameResult;
  Color: TPieceColor;
  Move: RChessMove;
  UciConverter: TUCIMoveConverter;
  WinPredicts: array [TPieceColor] of TGameWinner;

  procedure RunEngine(Engine: TAbstractChessEngine);
  begin
    Engine.OnStop := @EngineStop;
    case Options.TimeControlKind of
      eoDepth: Engine.StartFixedDepth(Options.TimeControl);
      eoTime: Engine.StartFixedTime(Options.TimeControl);
      else
        raise Exception.Create('Some time control types are not supported');
    end;
    Engine.WaitForStop(MaxInt);
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
  Chain := TMoveChain.Create;
  FBook.FillOpening(Chain);
  WinPredicts[pcWhite] := gwNone;
  WinPredicts[pcBlack] := gwNone;
  try
    UciConverter := TUCIMoveConverter.Create;
    FFirstEngine.MoveChain.Assign(Chain);
    FSecondEngine.MoveChain.Assign(Chain);
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
    CurGame.Chain := Chain;
    CurGame.WhiteName := Engines[pcWhite].Name + ' at ' + Engines[pcWhite].FileName;
    CurGame.BlackName := Engines[pcBlack].Name + ' at ' + Engines[pcBlack].FileName;
    while True do
    begin
      GameResult := Chain.GetGameResult;
      if GameResult.Winner <> gwNone then
      begin
        CurGame.Winner := GameResult.Winner;
        break;
      end;
      Color := Chain.Boards[Chain.Count - 1].MoveSide;
      RunEngine(Engines[Color]);
      Move := FEngineResult.BestMove;
      if Engines[Color].Terminated then
      begin
        WriteLn(StdErr, 'Engine "' + Engines[Color].FileName + '" died :(');
        if Color = pcWhite then
          CurGame.Winner := gwBlack
        else
          CurGame.Winner := gwWhite;
        RestartEngine(Engines[Color]);
        break;
      end;
      try
        Chain.Add(Move);
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
      WinPredicts[Color] := PredictGameWinner(Engines[Color].State.Score, Options.ScoreThreshold);
      if (WinPredicts[pcWhite] = WinPredicts[pcBlack]) and (WinPredicts[pcWhite] <> gwNone) then
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
  except
    FreeAndNil(Chain);
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
