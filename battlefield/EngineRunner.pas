unit EngineRunner;

{$mode objfpc}{$H+}{$B-}{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, MoveChains, ChessRules, ChessEngines,
  gvector, PGNUtils, MoveConverters, NotationLists, OpeningBook;

type

  { RGame }

  RGame = record
    Chain: TMoveChain;
    WhiteName: string;
    BlackName: string;
    Winner: TGameWinner;

    function ToString: string;
  end;

  TGameVector = specialize TVector<RGame>;

  TEngineOptionsKind = (eoTime, eoDepth);

  REngineOptions = record
    Kind: TEngineOptionsKind;
    Value: int64;
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
    FFirstWins: integer;
    FSecondWins: integer;
    FDraws: integer;
    FGames: TGameVector;
    FEngineResult: RAnalysisResult;
    FBook: TDefaultOpeningBook;

    procedure EngineStop(Sender: TObject; const EngineResult: RAnalysisResult);

    function GetGames(I: integer): RGame;
    function GetGameCount: integer;
  public
    constructor Create(FirstFactory, SecondFactory: TEngineFactory);
    destructor Destroy; override;

    property FirstWins: integer read FFirstWins;
    property SecondWins: integer read FSecondWins;
    property Draws: integer read FDraws;

    function LastGame: RGame;

    property GameCount: integer read GetGameCount;
    property Games[I: integer]: RGame read GetGames;

    procedure Play(const Options: REngineOptions; SwitchSides: boolean);

    procedure BeforeDestruction; override;
  end;

implementation

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
begin
  Converter := TPGNMoveConverter.Create;
  try
    Result := '[White "' + StringToTagValue(WhiteName) + '"]' +
      LineEnding + '[Black "' + StringToTagValue(BlackName) + '"]' +
      LineEnding + LineEnding + Chain.ConvertToString(Converter, ' ') +
      ' ' + GameResultMeanings[Winner] + LineEnding;
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

constructor TEngineRunner.Create(FirstFactory, SecondFactory: TEngineFactory);
begin
  FFirstFactory := FirstFactory;
  FSecondFactory := SecondFactory;
  FFirstEngine := FirstFactory.CreateChessEngine;
  FSecondEngine := SecondFactory.CreateChessEngine;
  FFirstWins := 0;
  FSecondWins := 0;
  FDraws := 0;
  FGames := TGameVector.Create;
  FBook := TDefaultOpeningBook.Create;
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

procedure TEngineRunner.Play(const Options: REngineOptions; SwitchSides: boolean);
var
  CurGame: RGame;
  Chain: TMoveChain;
  Engines: array [TPieceColor] of TAbstractChessEngine;
  GameResult: RGameResult;
  Color: TPieceColor;
  Move: RChessMove;
  UciConverter: TUCIMoveConverter;

  procedure RunEngine(Engine: TAbstractChessEngine);
  begin
    Engine.OnStop := @EngineStop;
    case Options.Kind of
      eoDepth: Engine.StartFixedDepth(Options.Value);
      eoTime: Engine.StartFixedTime(Options.Value);
      else
        raise Exception.Create('Some option types are not supported');
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
  try
    UciConverter := TUCIMoveConverter.Create;
    FFirstEngine.MoveChain.Assign(Chain);
    FSecondEngine.MoveChain.Assign(Chain);
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
      if Move.Kind = mkImpossible then
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
            '" played an invalid move ' + UciConverter.GetMoveString(Move));
          if Color = pcWhite then
            CurGame.Winner := gwBlack
          else
            CurGame.Winner := gwWhite;
          break;
        end;
      end;
    end;
    case CurGame.Winner of
      gwWhite: if SwitchSides then
          Inc(FSecondWins)
        else
          Inc(FFirstWins);
      gwBlack: if SwitchSides then
          Inc(FFirstWins)
        else
          Inc(FSecondWins);
      gwDraw: Inc(FDraws);
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
