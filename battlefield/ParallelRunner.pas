{
  This file is part of Battlefield - a tool to run micro-matches between chess
  engines.

  Copyright © 2020-2021 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit ParallelRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EngineRunner, Progress, Utils, ScoreUtils, OpeningBook;

type

  { TParallelRunner }

  TParallelRunner = class
  private
    FProgress: TAbstractProgress;

    function GetDraws: integer;
    function GetFirstWins: integer;
    function GetJobs: integer;
    function GetSecondWins: integer;
  public
    constructor Create(Games: integer;
      const FirstEngineExe, SecondEngineExe: string; const Options: REngineOptions;
      Jobs: integer = 0; Book: TAbstractOpeningBook = nil);
    procedure Start;

    // Must not be modified after start. Also, it's not owned by this class, so
    // call the destructor manually
    property Progress: TAbstractProgress read FProgress write FProgress;

    property Jobs: integer read GetJobs;

    procedure Join;

    // Run this only after the threads joined
    property FirstWins: integer read GetFirstWins;
    property SecondWins: integer read GetSecondWins;
    property Draws: integer read GetDraws;

    // Run this only after the threads joined
    procedure SaveGamesAsPGN(Stream: TStream);
    procedure SaveGamesAsGameset(Stream: TStream);

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
    FThreads: array of TThreadID;
    FRunners: array of TEngineRunner;
    FOptions: REngineOptions;
    FFirstFactory: TUciEngineFactory;
    FSecondFactory: TUciEngineFactory;
    FGameResults: array [TEngineMatchWinner] of integer;
    FBook: TAbstractOpeningBook;

    procedure ThreadFunc(Index: integer);
  end;

  { TParallelRunnerProgress }

  TParallelRunnerProgress = class(TAbstractProgress)
  public
    constructor Create(Total: integer; Jobs: integer);

    procedure Step(Sender: TObject); override;
  private
    FStartTime: int64;
    FTotal: integer;
    FCount: integer;
    FJobs: integer;
  end;

implementation

uses
  UTF8Process, Math;

function EstimateTime(Things, Jobs: integer): double; inline;
var
  Coeff: double;
begin
  Coeff := (1.0 - 2.0 / (Jobs + 1)) * Max(0.5, 1.0 - Things / (2.0 * Jobs));
  Result := Coeff * Things / Jobs;
end;

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

constructor TParallelRunnerProgress.Create(Total: integer; Jobs: integer);
begin
  FTotal := Total;
  FJobs := Jobs;
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
  PredictedTime := Time / EstimateTime(FCount, FJobs) * EstimateTime(FTotal, FJobs);
  Runner := Sender as TParallelRunner;
  WriteLn(StdErr, Format('%d/%d games completed (%s/%s), score = %s',
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

function TParallelRunner.GetJobs: integer;
begin
  Result := Length(FThreads);
end;

function TParallelRunner.GetSecondWins: integer;
begin
  Result := FGameResults[ewSecond];
end;

constructor TParallelRunner.Create(Games: integer;
  const FirstEngineExe, SecondEngineExe: string; const Options: REngineOptions;
  Jobs: integer; Book: TAbstractOpeningBook);
var
  I: integer;
  Winner: TEngineMatchWinner;
begin
  if Jobs = 0 then
    Jobs := GetSystemThreadCount;
  FBook := Book;
  if FBook = nil then
    FBook := TDefaultOpeningBook.CreateDefault;
  FProgress := nil;
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
    FThreads[I] := TThreadID(0);
    FRunners[I] := nil;
  end;
  for I := 0 to Jobs - 1 do
    FRunners[I] := TEngineRunner.Create(FFirstFactory, FSecondFactory, FBook.Clone);
  FreeAndNil(FBook);
end;

procedure TParallelRunner.Start;
var
  I: integer;
  DataPtr: PThreadFuncData;
begin
  for I := Low(FThreads) to High(FThreads) do
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
  I: integer;
begin
  for I := Low(FThreads) to High(FThreads) do
    if FThreads[I] <> TThreadID(0) then
    begin
      WaitForThreadTerminate(FThreads[I], 0);
      CloseThread(FThreads[I]);
      FThreads[I] := TThreadID(0);
    end;
end;

procedure TParallelRunner.BeforeDestruction;
begin
  Join;
  inherited;
end;

procedure TParallelRunner.SaveGamesAsPGN(Stream: TStream);
var
  Runner: TEngineRunner;
  I: integer;
  S: string;
begin
  for Runner in FRunners do
    for I := 0 to Runner.GameCount - 1 do
    begin
      S := Runner.Games[I].ToPGNString + LineEnding;
      Stream.Write(PChar(S)^, Length(S));
    end;
end;

procedure TParallelRunner.SaveGamesAsGameset(Stream: TStream);
var
  Runner: TEngineRunner;
  I: integer;
  S: string;
begin
  for Runner in FRunners do
    for I := 0 to Runner.GameCount - 1 do
    begin
      S := Runner.Games[I].ToGameset('-') + LineEnding;
      Stream.Write(PChar(S)^, Length(S));
    end;
end;

destructor TParallelRunner.Destroy;
var
  Thread: TThreadID;
  Runner: TEngineRunner;
begin
  DoneCriticalSection(FLock);
  for Thread in FThreads do
    if Thread <> TThreadID(0) then
      CloseThread(Thread);
  for Runner in FRunners do
    Runner.Free;
  FreeAndNil(FFirstFactory);
  FreeAndNil(FSecondFactory);
  FreeAndNil(FBook);
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
