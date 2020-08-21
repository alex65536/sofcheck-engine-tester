unit EngineRunner;

{$mode objfpc}{$H+}{$B-}{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, MoveChains, ChessRules, ChessEngines,
  gvector, PGNUtils, MoveConverters, NotationLists;

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

  { TEngineRunner }

  TEngineRunner = class
  private
    FFirstEngine: TAbstractChessEngine;
    FSecondEngine: TAbstractChessEngine;
    FSwitchSides: boolean;
    FFirstWins: integer;
    FSecondWins: integer;
    FDraws: integer;
    FGames: TGameVector;
    FEngineResult: RAnalysisResult;

    procedure EngineStop(Sender: TObject; const EngineResult: RAnalysisResult);

    function GetGames(I: integer): RGame;
    function GetGameCount: integer;
  public
    constructor Create(FirstEngine, SecondEngine: TAbstractChessEngine);
    destructor Destroy; override;

    property FirstWins: integer read FFirstWins;
    property SecondWins: integer read FSecondWins;
    property Draws: integer read FDraws;

    function LastGame: RGame;

    property GameCount: integer read GetGameCount;
    property Games[I: integer]: RGame read GetGames;

    procedure Play(const Options: REngineOptions);

    procedure Initialize;
    procedure Uninitialize;
  end;

implementation

{ RGame }

function RGame.ToString: string;
var
  Converter: TPGNMoveConverter;
begin
  Converter := TPGNMoveConverter.Create(Chain.Boards[-1]);
  try
    Result := '[White "' + StringToTagValue(WhiteName) + '"]' +
      LineEnding + '[Black" ' + StringToTagValue(BlackName) + '"]' +
      LineEnding + LineEnding + Chain.ConvertToString(Converter, ' ') + ' ' +
      GameResultMeanings[Winner] + LineEnding;
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

constructor TEngineRunner.Create(FirstEngine, SecondEngine: TAbstractChessEngine);
begin
  FFirstEngine := FirstEngine;
  FSecondEngine := SecondEngine;
  FSwitchSides := False;
  FFirstWins := 0;
  FSecondWins := 0;
  FDraws := 0;
  FGames := TGameVector.Create;
end;

destructor TEngineRunner.Destroy;
var
  I: integer;
begin
  for I := 0 to integer(FGames.Size) - 1 do
    FGames[I].Chain.Free;
  FreeAndNil(FGames);
  FreeAndNil(FFirstEngine);
  FreeAndNil(FSecondEngine);
  inherited;
end;

function TEngineRunner.LastGame: RGame;
begin
  Result := FGames.Back;
end;

procedure TEngineRunner.Play(const Options: REngineOptions);
var
  CurGame: RGame;
  Chain: TMoveChain;
  Engines: array [TPieceColor] of TAbstractChessEngine;
  GameResult: RGameResult;
  Color: TPieceColor;
  Move: RChessMove;

  procedure RunEngine(Engine: TAbstractChessEngine);
  begin
    Engine.OnStop := @EngineStop;
    case Options.Kind of
      eoDepth: Engine.StartFixedDepth(Options.Value);
      eoTime: Engine.StartFixedTime(Options.Value);
      else
        raise Exception.Create('Some option types are not supported');
    end;
    Engine.WaitForStop;
  end;

begin
  Chain := TMoveChain.Create;
  try
    FFirstEngine.MoveChain.Clear;
    FSecondEngine.MoveChain.Clear;
    if FSwitchSides then
    begin
      Engines[pcWhite] := FSecondEngine;
      Engines[pcBlack] := FFirstEngine;
    end
    else
    begin
      Engines[pcWhite] := FFirstEngine;
      Engines[pcBlack] := FSecondEngine;
    end;
    FSwitchSides := not FSwitchSides;
    CurGame.Chain := Chain;
    CurGame.WhiteName := Engines[pcWhite].Name;
    CurGame.BlackName := Engines[pcBlack].Name;
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
      FFirstEngine.MoveChain.Add(Move);
      FSecondEngine.MoveChain.Add(Move);
      Chain.Add(Move);
    end;
    case CurGame.Winner of
      gwWhite: if FSwitchSides then
          Inc(FFirstWins)
        else
          Inc(FSecondWins);
      gwBlack: if FSwitchSides then
          Inc(FSecondWins)
        else
          Inc(FFirstWins);
      gwDraw: Inc(FDraws);
    end;
  except
    FreeAndNil(Chain);
    raise;
  end;
  FGames.PushBack(CurGame);
end;

procedure TEngineRunner.Initialize;
begin
  FFirstEngine.Initialize;
  FSecondEngine.Initialize;
end;

procedure TEngineRunner.Uninitialize;
begin
  FFirstEngine.Uninitialize;
  FSecondEngine.Uninitialize;
end;

end.
