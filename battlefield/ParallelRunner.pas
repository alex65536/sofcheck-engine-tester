unit ParallelRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EngineRunner, Progress, Utils, ScoreUtils;

type

  { TParallelRunner }

  TParallelRunner = class
  private
    function GetDraws: integer;
    function GetFirstWins: integer;
    function GetSecondWins: integer;
  public
    constructor Create(Games: integer;
      const FirstEngineExe, SecondEngineExe: string; const Options: REngineOptions;
      Jobs: integer = 0; Progress: TAbstractProgress = nil);

    procedure Join;

    // Run this only after the threads joined
    property FirstWins: integer read GetFirstWins;
    property SecondWins: integer read GetSecondWins;
    property Draws: integer read GetDraws;

    // Run this only after the threads joined
    procedure SaveGamesToStream(Stream: TStream);

    destructor Destroy; override;

    procedure BeforeDestruction; override;
  private
    type
    RThreadFuncData = record
      Instance: TParallelRunner;
      Index: integer;
    end;

    PThreadFuncData = ^RThreadFuncData;
  private
    FLock: TRTLCriticalSection;
    FGames: integer;
    FThreads: array of PtrInt;
    FRunners: array of TEngineRunner;
    FOptions: REngineOptions;
    FProgress: TAbstractProgress;
    FFirstFactory: TUciEngineFactory;
    FSecondFactory: TUciEngineFactory;
    FGameResults: array [TEngineMatchWinner] of integer;

    procedure ThreadFunc(Index: integer);
  end;

  { TParallelRunnerProgress }

  TParallelRunnerProgress = class(TAbstractProgress)
  public
    constructor Create(Total: integer);

    procedure Step(Sender: TObject); override;
  private
    FStartTime: int64;
    FTotal: integer;
    FCount: integer;
  end;

implementation

uses
  UTF8Process;

function StaticThreadFunc(DataPtr: Pointer): PtrInt;
var
  Data: TParallelRunner.RThreadFuncData;
begin
  Data := TParallelRunner.PThreadFuncData(DataPtr)^;
  Dispose(TParallelRunner.PThreadFuncData(DataPtr));
  Data.Instance.ThreadFunc(Data.Index);
  Result := 0;
end;

{ TParallelRunnerProgress }

constructor TParallelRunnerProgress.Create(Total: integer);
begin
  FTotal := Total;
  FStartTime := GetTickCount64;
end;

procedure TParallelRunnerProgress.Step(Sender: TObject);
var
  Time: double;
  PredictedTime: double;
  Runner: TParallelRunner;
begin
  Inc(FCount);
  Time := (GetTickCount64 - FStartTime) / 1000;
  PredictedTime := Time / FCount * FTotal;
  Runner := Sender as TParallelRunner;
  WriteLn(StdErr, Format(
    '%d/%d games completed (%s/%s), score = %s',
    [FCount, FTotal, HumanTimeString(Time), HumanTimeString(PredictedTime),
     ScorePairToStr(Runner.FirstWins, Runner.Draws, Runner.SecondWins)]));
end;

{ TParallelRunner }

function TParallelRunner.GetDraws: integer;
begin
  Result := FGameResults[ewDraw];
end;

function TParallelRunner.GetFirstWins: integer;
begin
  Result := FGameResults[ewFirst];
end;

function TParallelRunner.GetSecondWins: integer;
begin
  Result := FGameResults[ewSecond];
end;

constructor TParallelRunner.Create(Games: integer;
  const FirstEngineExe, SecondEngineExe: string; const Options: REngineOptions;
  Jobs: integer; Progress: TAbstractProgress);
var
  I: integer;
  Winner: TEngineMatchWinner;
  DataPtr: PThreadFuncData;
begin
  if Jobs = 0 then
    Jobs := GetSystemThreadCount;
  FProgress := Progress;
  FOptions := Options;
  InitCriticalSection(FLock);
  FGames := Games;
  for Winner in TEngineMatchWinner do
    FGameResults[Winner] := 0;
  SetLength(FThreads, Jobs);
  SetLength(FRunners, Jobs);
  FFirstFactory := TUciEngineFactory.Create(FirstEngineExe);
  FSecondFactory := TUciEngineFactory.Create(SecondEngineExe);
  for I := 0 to Jobs - 1 do
  begin
    FThreads[I] := 0;
    FRunners[I] := nil;
  end;
  for I := 0 to Jobs - 1 do
    FRunners[I] := TEngineRunner.Create(FFirstFactory, FSecondFactory);
  for I := 0 to Jobs - 1 do
  begin
    if not Assigned(FRunners[I]) then
      continue;
    New(DataPtr);
    DataPtr^.Instance := Self;
    DataPtr^.Index := I;
    FThreads[I] := BeginThread(@StaticThreadFunc, DataPtr);
  end;
end;

procedure TParallelRunner.Join;
var
  Thread: PtrInt;
begin
  for Thread in FThreads do
    if Thread <> 0 then
      WaitForThreadTerminate(Thread, 0);
end;

procedure TParallelRunner.BeforeDestruction;
begin
  Join;
  inherited;
end;

procedure TParallelRunner.SaveGamesToStream(Stream: TStream);
var
  Runner: TEngineRunner;
  I: integer;
  S: string;
begin
  for Runner in FRunners do
    for I := 0 to Runner.GameCount - 1 do
    begin
      S := Runner.Games[I].ToString + LineEnding;
      Stream.Write(PChar(S)^, Length(S));
    end;
end;

destructor TParallelRunner.Destroy;
var
  Thread: integer;
  Runner: TEngineRunner;
begin
  DoneCriticalSection(FLock);
  for Thread in FThreads do
    if Thread <> 0 then
      CloseThread(Thread);
  for Runner in FRunners do
    Runner.Free;
  FreeAndNil(FFirstFactory);
  FreeAndNil(FSecondFactory);
  inherited Destroy;
end;

procedure TParallelRunner.ThreadFunc(Index: integer);
var
  MustStop: boolean;
  SwitchSides: boolean;
  Winner: TEngineMatchWinner;
begin
  while True do
  begin
    EnterCriticalSection(FLock);
    try
      MustStop := True;
      if FGames <> 0 then
      begin
        MustStop := False;
        SwitchSides := FGames mod 2 = 0;
        Dec(FGames);
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
    if MustStop then
      break;
    try
      Winner := FRunners[Index].Play(FOptions, SwitchSides);
      EnterCriticalSection(FLock);
      try
        InterlockedIncrement(FGameResults[Winner]);
        if Assigned(FProgress) then
          FProgress.Step(Self);
      finally
        LeaveCriticalSection(FLock);
      end;
    except
      on E: Exception do
      begin
        EnterCriticalSection(FLock);
        try
          WriteLn(StdErr, 'Thread finished with exception:');
          WriteLn(StdErr, 'Exception ', E.ClassName, ': ', E.Message);
          DumpExceptionBackTrace(StdErr);
          WriteLn(StdErr, 'Terminating now.');
          halt(1);
        finally
          LeaveCriticalSection(FLock);
        end;
      end;
    end;
  end;
end;

end.

