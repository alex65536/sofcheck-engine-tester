{
  Copyright © 2020 Alexander Kernozhitsky <sh200105@mail.ru>

  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this library.  If not, see <https://www.gnu.org/licenses/>.
}
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

 
