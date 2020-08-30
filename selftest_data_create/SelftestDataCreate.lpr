program SelftestDataCreate;

uses
  MoveChains,
  ChessRules,
  Classes,
  SysUtils,
  PGNSplitter;

  procedure RandomGame;
  var
    Board: TChessBoard;
    Chain: TMoveChain;
    Move: RChessMove;
    I: integer;
  begin
    Board := TChessBoard.Create(True);
    try
      Chain := TMoveChain.Create(Board.RawBoard);
      try
        while Board.GetGameResult.Kind = geNone do
        begin
          I := Random(Board.MoveCount);
          Move := Board.Moves[I];
          Chain.Add(Move);
          Board.MakeMove(Move);
          WriteLn(Board.FENString);
        end;
      finally
        FreeAndNil(Chain);
      end;
    finally
      FreeAndNil(Board);
    end;
  end;

  procedure AddManualData;
  var
    Lines: TStringList;
    Line: string;
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile('..' + DirectorySeparator + 'boards.fen');
      for Line in Lines do
        WriteLn(Line);
    finally
      FreeAndNil(Lines);
    end;
  end;

  procedure AddPGNData;
  var
    List: TBoardList;
    Board: TChessBoard;
    RawBoard: RRawBoard;
  begin
    WriteLn('# These positions are added from games.pgn');
    List := TBoardList.Create;
    try
      SplitPGNFile('..' + DirectorySeparator + 'games.pgn', List);
      Board := TChessBoard.Create(False);
      try
        for RawBoard in List do
        begin
          Board.RawBoard := RawBoard;
          WriteLn(Board.FENString);
        end;
      finally
        FreeAndNil(Board);
      end;
    finally
      FreeAndNil(List);
    end;
  end;

var
  I: integer;
begin
  RandSeed := 42;
  WriteLn('# This file is generated automatically, DO NOT EDIT!');
  WriteLn('#');
  WriteLn('# This file contains the positions on which the self-tests will be run. It was generated using');
  WriteLn('# selftest_data_create tool, which can be found in sofcheck-engine-tester repository:');
  WriteLn('# https://github.com/alex65536/sofcheck-engine-tester/tree/master/selftest_data_create.');
  WriteLn('# The positions are taken from some real games (human vs human and engine vs engine), but many');
  WriteLn('# of them are taken from random games (where each side makes a random move). Some interesting');
  WriteLn('# positions are added manually.');
  WriteLn('#');
  WriteLn('# Note on the file format: each line should contain a valid FEN string. Any empty string or the');
  WriteLn('# string that starts with # is ignored');
  WriteLn;
  AddManualData;
  WriteLn;
  AddPGNData;
  WriteLn;
  WriteLn('# These positions are added from random games');
  for I := 1 to 100 do
    RandomGame;
end.
