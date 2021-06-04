unit GameMetrics;

{$mode objfpc}{$H+}{$COperators On}

interface

uses
  Classes, SysUtils, ChessRules, GameNotation, EngineScores;

const
  MetricNames: array [0 .. 3] of string =
    ('mean_abs_expl_1', 'mean_abs_expl_2', 'mean_sqr_expl_1', 'mean_sqr_expl_2');

type
  TPlayerKind = (pkFirst, pkSecond);

  { TGameMetrics }

  TGameMetrics = class
  private
    FMetrics: array [0 .. 3] of double;
    function GetMeanAbsExplosion(K: TPlayerKind): double;
    function GetMeanSqrExplosion(K: TPlayerKind): double;
    procedure SetMeanAbsExplosion(K: TPlayerKind; AValue: double);
    procedure SetMeanSqrExplosion(K: TPlayerKind; AValue: double);
  public
    // Square root of mean over (s[i] - s[i-1])^2, where s[i] is the score of
    // the engine on the ith move
    property MeanSqrExplosion[K: TPlayerKind]: double
      read GetMeanSqrExplosion write SetMeanSqrExplosion;

    // Mean over |s[i] - s[i-1]|, where s[i] is the score of the engine on the ith move
    property MeanAbsExplosion[K: TPlayerKind]: double
      read GetMeanAbsExplosion write SetMeanAbsExplosion;

    procedure Calculate(Game: TScoredGameNotation; SwitchSides: boolean);
    function ToString: string; override;
  end;

  { TMetricGameNotation }

  TMetricGameNotation = class(TScoredGameNotation)
  private
    FMetrics: TGameMetrics;
    FSwitchSides: boolean;
  public
    property Metrics: TGameMetrics read FMetrics;
    property SwitchSides: boolean read FSwitchSides write FSwitchSides;

    constructor Create;
    constructor Create(const ARawBoard: RRawBoard);
    destructor Destroy; override;

    procedure CalculateMetrics;
  end;

implementation

{ TMetricGameNotation }

constructor TMetricGameNotation.Create;
begin
  inherited Create;
  FMetrics := TGameMetrics.Create;
  FSwitchSides := False;
end;

constructor TMetricGameNotation.Create(const ARawBoard: RRawBoard);
begin
  inherited Create(ARawBoard);
  FMetrics := TGameMetrics.Create;
  FSwitchSides := False;
end;

destructor TMetricGameNotation.Destroy;
begin
  FreeAndNil(FMetrics);
  inherited Destroy;
end;

procedure TMetricGameNotation.CalculateMetrics;
begin
  FMetrics.Calculate(Self, FSwitchSides);
end;

{ TGameMetrics }

function TGameMetrics.GetMeanAbsExplosion(K: TPlayerKind): double;
begin
  if K = pkFirst then
    Result := FMetrics[0]
  else
    Result := FMetrics[1];
end;

function TGameMetrics.GetMeanSqrExplosion(K: TPlayerKind): double;
begin
  if K = pkFirst then
    Result := FMetrics[2]
  else
    Result := FMetrics[3];
end;

procedure TGameMetrics.SetMeanAbsExplosion(K: TPlayerKind; AValue: double);
begin
  if K = pkFirst then
    FMetrics[0] := AValue
  else
    FMetrics[1] := AValue;
end;

procedure TGameMetrics.SetMeanSqrExplosion(K: TPlayerKind; AValue: double);
begin
  if K = pkFirst then
    FMetrics[2] := AValue
  else
    FMetrics[3] := AValue;
end;

function AbsNorm(X: double): double;
begin
  Result := Abs(X);
end;

function SqrNorm(X: double): double;
begin
  Result := X * X;
end;

procedure TGameMetrics.Calculate(Game: TScoredGameNotation; SwitchSides: boolean);
type
  TExplosionNorm = function(X: double): double;
  TExplosionCalcResult = array [TPlayerKind] of double;

  function IsGoodScore(const Score: RPositionScore): boolean;
  begin
    Result := (Score.Kind = skNormal) and (Score.Mate = DefaultMate);
  end;

  procedure CalcExplosion(Norm: TExplosionNorm; out Res: TExplosionCalcResult);
  var
    Counts: array [TPlayerKind] of integer;
    Sums: array [TPlayerKind] of double;
    K: TPlayerKind;
    I: integer;
    IsFirstPlayer: boolean;
    CurKind: TPlayerKind;
  begin
    for K in TPlayerKind do
    begin
      Sums[K] := 0;
      Counts[K] := 0;
    end;
    for I := 2 to Game.Chain.Count - 1 do
    begin
      if (not IsGoodScore(Game.Scores[I - 2])) or (not IsGoodScore(Game.Scores[I])) then
        continue;
      IsFirstPlayer := (I mod 2 = 0) xor (Game.Chain.Boards[-1].MoveSide = pcWhite) xor
        (not SwitchSides);
      if IsFirstPlayer then
        CurKind := pkFirst
      else
        CurKind := pkSecond;
      Counts[CurKind] += 1;
      Sums[CurKind] += Norm(Game.Scores[I].Score - Game.Scores[I - 2].Score);
    end;
    for K in TPlayerKind do
      if Counts[K] <> 0 then
        Res[K] := Sums[K] / Counts[K]
      else
        Res[K] := 0.0;
  end;

var
  I: integer;
  K: TPlayerKind;
  ExplRes: TExplosionCalcResult;
begin
  for I := Low(FMetrics) to High(FMetrics) do
    FMetrics[I] := 0.0;

  CalcExplosion(@AbsNorm, ExplRes);
  for K in TPlayerKind do
    MeanAbsExplosion[K] := ExplRes[K];
  CalcExplosion(@SqrNorm, ExplRes);
  for K in TPlayerKind do
    MeanSqrExplosion[K] := Sqrt(ExplRes[K]);
end;

function TGameMetrics.ToString: string;
var
  I: integer;
begin
  Result := '';
  for I := Low(FMetrics) to High(FMetrics) do
  begin
    if I <> Low(FMetrics) then
      Result += ', ';
    Result += MetricNames[I] + ' = ' + FloatToStrF(FMetrics[I], ffGeneral, 4, 4);
  end;
end;

end.
