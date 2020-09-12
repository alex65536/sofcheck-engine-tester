unit OpeningBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MoveChains, Utils, ChessRules;

type

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
  I: integer;
  Board: TChessBoard;
begin
  SetLength(FBoards, FenStrings.Count);
  Board := TChessBoard.Create(false);
  try
    for I := 0 to FenStrings.Count - 1 do
    begin
      Board.FENString := FenStrings[I];
      FBoards[I] := Board.RawBoard;
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
