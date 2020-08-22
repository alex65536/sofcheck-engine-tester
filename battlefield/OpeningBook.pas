unit OpeningBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveChains, Utils;

type
  TAbstractOpeningBook = class
  public
    procedure FillOpening(Chain: TMoveChain); virtual; abstract;
  end;

  { TDefaultOpeningBook }

  TDefaultOpeningBook = class(TAbstractOpeningBook)
  public
    procedure FillOpening(Chain: TMoveChain); override;
    constructor Create;
    destructor Destroy; override;
  private
    FRandomState: RXoshiro256State;
    FOpenings: array of TMoveChain;
  end;

implementation

uses
  ChessRules, ChessNotation;

{$I Openings.inc}

{ TDefaultOpeningBook }

constructor TDefaultOpeningBook.Create;
var
  I: integer;
  Board: TChessBoard = nil;
  Notation: TChessNotation = nil;
begin
  FRandomState := InitXoshiro256State;
  try
    Board := TChessBoard.Create(False);
    Notation := TChessNotation.Create(Board);
    SetLength(FOpenings, High(Openings) - Low(Openings) + 1);
    for I := Low(Openings) to High(Openings) do
    begin
      Notation.PGNString := Openings[I];
      FOpenings[I - Low(Openings)] := Notation.GetMoveChain;
    end;
  finally
    FreeAndNil(Notation);
    FreeAndNil(Board);
  end;
end;

procedure TDefaultOpeningBook.FillOpening(Chain: TMoveChain);
var
  Index: integer;
begin
  Index := Xoshiro256ss(FRandomState) mod QWord(Length(FOpenings));
  Chain.Assign(FOpenings[Index]);
end;

destructor TDefaultOpeningBook.Destroy;
var
  Chain: TMoveChain;
begin
  for Chain in FOpenings do
    Chain.Free;
  inherited;
end;

end.

