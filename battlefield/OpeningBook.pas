{
  This file is part of Battlefield - a tool to run micro-matches between chess
  engines.

  Copyright © 2020 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit OpeningBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveChains, Utils, ChessRules;

type

  { EOpeningBook }

  EOpeningBook = class(Exception);

  { TAbstractOpeningBook }

  TAbstractOpeningBook = class
  public
    constructor Create; virtual;
    procedure FillOpening(Chain: TMoveChain); virtual; abstract;
    procedure Assign(Source: TAbstractOpeningBook);
    function Clone: TAbstractOpeningBook;
  protected
    procedure DoAssign(Source: TAbstractOpeningBook); virtual;
  end;

  { TRandomizedOpeningBook }

  TRandomizedOpeningBook = class(TAbstractOpeningBook)
  public
    constructor Create; override;
  protected
    function RandInt(Bound: QWord): QWord;
  private
    FRandomState: RXoshiro256State;
  end;

  { TDefaultOpeningBook }

  TDefaultOpeningBook = class(TRandomizedOpeningBook)
  public
    procedure FillOpening(Chain: TMoveChain); override;
    constructor Create; override;
    constructor CreateDefault;
    destructor Destroy; override;
  protected
    procedure DoAssign(Source: TAbstractOpeningBook); override;
  private
    FOpenings: array of TMoveChain;
  end;

  { TFenListOpeningBook }

  TFenListOpeningBook = class(TRandomizedOpeningBook)
  public
    procedure FillOpening(Chain: TMoveChain); override;
    constructor Create; override;
    constructor Create(const FileName: string);
    constructor Create(FenStrings: TStringList);
  protected
    procedure DoAssign(Source: TAbstractOpeningBook); override;
  private
    FBoards: array of RRawBoard;
  end;

implementation

uses
  ChessNotation;

{$I Openings.inc}

type
  TOpeningBookClass = class of TAbstractOpeningBook;

function IsFenStringComment(S: string): boolean;
begin
  S := S.Trim;
  Result := (S = '') or (S[1] = '#');
end;

{ TFenListOpeningBook }

procedure TFenListOpeningBook.FillOpening(Chain: TMoveChain);
begin
  Chain.Clear(FBoards[RandInt(Length(FBoards))]);
end;

constructor TFenListOpeningBook.Create;
begin
  inherited Create;
end;

constructor TFenListOpeningBook.Create(const FileName: string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.LoadFromFile(FileName);
    Create(List);
  finally
    FreeAndNil(List);
  end;
end;

constructor TFenListOpeningBook.Create(FenStrings: TStringList);
var
  Pos, Count: integer;
  I: integer;
  Board: TChessBoard;
begin
  Count := 0;
  for I := 0 to FenStrings.Count - 1 do
    if not IsFenStringComment(FenStrings[I]) then
      Inc(Count);
  if Count = 0 then
    raise EOpeningBook.Create('The specified FEN list is empty');
  SetLength(FBoards, Count);
  Board := TChessBoard.Create(false);
  try
    Pos := 0;
    for I := 0 to FenStrings.Count - 1 do
    begin
      if IsFenStringComment(FenStrings[I]) then
        continue;
      Board.FENString := FenStrings[I];
      FBoards[Pos] := Board.RawBoard;
      Inc(Pos);
    end;
  finally
    FreeAndNil(Board);
  end;
end;

procedure TFenListOpeningBook.DoAssign(Source: TAbstractOpeningBook);
begin
  inherited;
  FBoards := (Source as TFenListOpeningBook).FBoards;
end;

{ TAbstractOpeningBook }

constructor TAbstractOpeningBook.Create;
begin
end;

procedure TAbstractOpeningBook.Assign(Source: TAbstractOpeningBook);
begin
  if Source.ClassType <> ClassType then
    raise EConvertError.CreateFmt('Cannot assign %s to %s',
      [Source.ClassName, ClassName]);
  DoAssign(Source);
end;

function TAbstractOpeningBook.Clone: TAbstractOpeningBook;
begin
  Result := TOpeningBookClass(Self.ClassType).Create;
  try
    Result.Assign(Self);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TAbstractOpeningBook.DoAssign(Source: TAbstractOpeningBook);
begin
end;

{ TRandomizedOpeningBook }

constructor TRandomizedOpeningBook.Create;
begin
  FRandomState := InitXoshiro256State;
end;

function TRandomizedOpeningBook.RandInt(Bound: QWord): QWord;
begin
  Result := Xoshiro256ss(FRandomState) mod Bound;
end;

{ TDefaultOpeningBook }

constructor TDefaultOpeningBook.Create;
begin
  inherited;
end;

constructor TDefaultOpeningBook.CreateDefault;
var
  I: integer;
  Board: TChessBoard = nil;
  Notation: TChessNotation = nil;
begin
  inherited;
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
begin
  Chain.Assign(FOpenings[RandInt(Length(FOpenings))]);
end;

destructor TDefaultOpeningBook.Destroy;
var
  Chain: TMoveChain;
begin
  for Chain in FOpenings do
    Chain.Free;
  inherited;
end;

procedure TDefaultOpeningBook.DoAssign(Source: TAbstractOpeningBook);
var
  Openings: array of TMoveChain;
  I: integer;
begin
  inherited;
  for I := Low(FOpenings) to High(FOpenings) do
    FreeAndNil(FOpenings[I]);
  Openings := (Source as TDefaultOpeningBook).FOpenings;
  SetLength(FOpenings, Length(Openings));
  for I := Low(FOpenings) to High(FOpenings) do
  begin
    FOpenings[I] := TMoveChain.Create;
    FOpenings[I].Assign(Openings[I]);
  end;
end;

end.
