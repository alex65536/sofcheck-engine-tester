unit GameNotation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveChains, ChessRules, PGNUtils, MoveConverters, NotationLists;

type

  { TGameNotation }

  TGameNotation = class
  private
    FChain: TMoveChain;
    FWhiteName: string;
    FBlackName: string;
    FWinner: TGameWinner;
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

implementation

{ TGameNotation }

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
      Chain.ConvertToString(Converter, ' ') + ' ' + GameResultMeanings[Winner] +
      LineEnding;
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

end.

