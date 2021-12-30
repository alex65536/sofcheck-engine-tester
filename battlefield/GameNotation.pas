{
  This file is part of Battlefield - a tool to run micro-matches between chess
  engines.

  Copyright © 2021 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit GameNotation;

{$mode objfpc}{$H+}{$COperators On}

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
    function ToGameset(const GameLabel: string): string;
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

function TGameNotation.ToGameset(const GameLabel: string): string;
const
  WinnerStr: array [TGameWinner] of string = ('?', 'W', 'B', 'D');
var
  Converter: TUCIMoveConverter;
begin
  Converter := TUCIMoveConverter.Create(Chain.Boards[-1]);
  try
    Result := 'game ' + WinnerStr[Winner] + ' ' + GameLabel + LineEnding;
    Result += 'title ' + WhiteName + ' vs ' + BlackName + LineEnding;
    if Chain.Boards[-1] = GetInitialPosition then
      Result += 'start' + LineEnding
    else
      with TChessBoard.Create(False, False) do
        try
          RawBoard := Chain.Boards[-1];
          Result += 'board ' + FENString + LineEnding;
        finally
          Free;
        end;
    Result += 'moves ' + Chain.ConvertToString(Converter, ' ') + LineEnding;
  finally
    FreeAndNil(Converter);
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
