unit GameNotation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveChains, ChessRules, PGNUtils, MoveConverters,
  NotationLists, EngineScores, gvector;

type

  { TGameNotation }

  TGameNotation = class
  private
    FChain: TMoveChain;
    FWhiteName: string;
    FBlackName: string;
    FWinner: TGameWinner;
  protected
    function PGNAfterMoveString(I: integer): string; virtual;
  public
    property Chain: TMoveChain read FChain;
    property WhiteName: string read FWhiteName write FWhiteName;
    property BlackName: string read FBlackName write FBlackName;
    property Winner: TGameWinner read FWinner write FWinner;

    constructor Create;
    constructor Create(const ARawBoard: RRawBoard);
    destructor Destroy; override;

    function ToPGNString: string;
    function ToDataset(GameId: integer): string;
  end;

  { TScoredGameNotation }

  TScoredGameNotation = class(TGameNotation)
  private
    type TScoreVector = specialize TVector<RPositionScore>;
  private
    FScores: TScoreVector;
    function GetScoreCount: integer;
    function GetScores(I: integer): RPositionScore;
    procedure SetScores(I: integer; const AValue: RPositionScore);
  protected
    function PGNAfterMoveString(I: integer): string; override;
  public
    property Scores[I: integer]: RPositionScore read GetScores write SetScores;
    property ScoreCount: integer read GetScoreCount;

    procedure AddScore(const Score: RPositionScore);
    procedure AddMove(const Move: RChessMove; const Score: RPositionScore);
    procedure Clear;
    procedure PadZeroScores;

    constructor Create;
    constructor Create(const ARawBoard: RRawBoard);
    destructor Destroy; override;
  end;

implementation

{ TGameNotation }

{$HINTS OFF}
function TGameNotation.PGNAfterMoveString(I: integer): string;
begin
  Result := '';
end;

{$HINTS ON}

constructor TGameNotation.Create;
begin
  FChain := TMoveChain.Create;
end;

constructor TGameNotation.Create(const ARawBoard: RRawBoard);
begin
  FChain := TMoveChain.Create(ARawBoard);
end;

destructor TGameNotation.Destroy;
begin
  FreeAndNil(FChain);
  inherited Destroy;
end;

function TGameNotation.ToPGNString: string;
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
      Chain.ConvertToString(Converter, ' ', @PGNAfterMoveString) +
      ' ' + GameResultMeanings[Winner] + LineEnding;
  finally
    FreeAndNil(Converter);
  end;
end;

function TGameNotation.ToDataset(GameId: integer): string;
const
  WinnerStr: array [TGameWinner] of string = ('?', 'W', 'B', 'D');
var
  Board: TChessBoard;
  I: integer;
begin
  Board := TChessBoard.Create(False);
  try
    Result := 'game ' + WinnerStr[Winner] + ' ' + IntToStr(GameId) + LineEnding;
    for I := -1 to Chain.Count - 1 do
    begin
      Board.RawBoard := Chain.Boards[I];
      Result := Result + 'board ' + Board.FENString + LineEnding;
    end;
  finally
    FreeAndNil(Board);
  end;
end;

{ TScoredGameNotation }

function TScoredGameNotation.GetScoreCount: integer;
begin
  Result := FScores.Size;
end;

function TScoredGameNotation.GetScores(I: integer): RPositionScore;
begin
  Result := FScores[I];
end;

procedure TScoredGameNotation.SetScores(I: integer; const AValue: RPositionScore);
begin
  FScores[I] := AValue;
end;

function TScoredGameNotation.PGNAfterMoveString(I: integer): string;
begin
  Result := '{[' + PositionScoreToString(Scores[I]) + ']}';
end;

procedure TScoredGameNotation.AddScore(const Score: RPositionScore);
begin
  FScores.PushBack(Score);
end;

procedure TScoredGameNotation.AddMove(const Move: RChessMove;
  const Score: RPositionScore);
begin
  Chain.Add(Move);
  AddScore(Score);
end;

procedure TScoredGameNotation.Clear;
begin
  Chain.Clear;
  FScores.Clear;
end;

procedure TScoredGameNotation.PadZeroScores;
const
  ZeroScore: RPositionScore =
    (
    Kind: skNormal;
    Mate: DefaultMate;
    Score: 0
    );
begin
  while integer(FScores.Size) < Chain.Count do
    AddScore(ZeroScore);
end;

constructor TScoredGameNotation.Create;
begin
  inherited Create;
  FScores := TScoreVector.Create;
end;

constructor TScoredGameNotation.Create(const ARawBoard: RRawBoard);
begin
  inherited Create(ARawBoard);
  FScores := TScoreVector.Create;
end;

destructor TScoredGameNotation.Destroy;
begin
  FreeAndNil(FScores);
  inherited Destroy;
end;

end.
