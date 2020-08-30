unit PGNSplitter;

{$Mode ObjFpc}{$H+}{$B-}{$COperators On}

interface

uses
  Classes, SysUtils, ChessRules, ChessNotation, MoveChains, gvector;

type
  TBoardList = specialize TVector<RRawBoard>;

procedure SplitChain(Chain: TMoveChain; var List: TBoardList);
procedure SplitSinglePGN(const PGN: string; var List: TBoardList);
procedure SplitPGN(PGN: TStringList; var List: TBoardList);
procedure SplitPGNFile(const FileName: string; var List: TBoardList);

implementation

procedure SplitChain(Chain: TMoveChain; var List: TBoardList);
var
  I: integer;
begin
  for I := -1 to Chain.Count - 1 do
    List.PushBack(Chain.Boards[I]);
end;

procedure SplitSinglePGN(const PGN: string; var List: TBoardList);
var
  Board: TChessBoard;
  Notation: TChessNotation;
  Chain: TMoveChain;
begin
  Board := TChessBoard.Create(False);
  try
    Notation := TChessNotation.Create(Board);
    try
      Notation.PGNString := PGN;
      Chain := Notation.GetMoveChain;
      try
        SplitChain(Chain, List);
      finally
        FreeAndNil(Chain);
      end;
    finally
      FreeAndNil(Notation);
    end;
  finally
    FreeAndNil(Board);
  end;
end;

procedure SplitPGN(PGN: TStringList; var List: TBoardList);
var
  Line: integer;
  GameString: string;
begin
  Line := 0;
  while Line < PGN.Count do
  begin
    GameString := '';
    while (Line < PGN.Count) and PGN[Line].StartsWith('[') do
    begin
      GameString += PGN[Line] + LineEnding;
      Inc(Line);
    end;
    while (Line < PGN.Count) and (not PGN[Line].StartsWith('[')) do
    begin
      GameString += PGN[Line] + LineEnding;
      Inc(Line);
    end;
    SplitSinglePGN(GameString, List);
  end;
end;

procedure SplitPGNFile(const FileName: string; var List: TBoardList);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    SplitPGN(Lines, List);
  finally
    FreeAndNil(Lines);
  end;
end;

end.

 
