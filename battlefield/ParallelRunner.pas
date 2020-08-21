unit ParallelRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ChessEngines, EngineRunner, Progress;

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

    procedure ThreadFunc(Index: integer);
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

{ TParallelRunner }

function TParallelRunner.GetDraws: integer;
var
  Runner: TEngineRunner;
begin
  Result := 0;
  for Runner in FRunners do
    Inc(Result, Runner.Draws);
end;

function TParallelRunner.GetFirstWins: integer;
var
  Runner: TEngineRunner;
begin
  Result := 0;
  for Runner in FRunners do
    Inc(Result, Runner.FirstWins);
end;

function TParallelRunner.GetSecondWins: integer;
var
  Runner: TEngineRunner;
begin
  Result := 0;
  for Runner in FRunners do
    Inc(Result, Runner.SecondWins);
end;

constructor TParallelRunner.Create(Games: integer;
  const FirstEngineExe, SecondEngineExe: string; const Options: REngineOptions;
  Jobs: integer; Progress: TAbstractProgress);
var
  I: integer;
  FirstEngine, SecondEngine: TUCIChessEngine;
  DataPtr: PThreadFuncData;
begin
  if Jobs = 0 then
    Jobs := GetSystemThreadCount;
  FProgress := Progress;
  FOptions := Options;
  InitCriticalSection(FLock);
  FGames := Games;
  SetLength(FThreads, Jobs);
  SetLength(FRunners, Jobs);
  for I := 0 to Jobs - 1 do
  begin
    FThreads[I] := 0;
    FRunners[I] := nil;
  end;
  for I := 0 to Jobs - 1 do
  begin
    FirstEngine := nil;
    SecondEngine := nil;
    try
      FirstEngine := TUCIChessEngine.Create(FirstEngineExe);
      SecondEngine := TUCIChessEngine.Create(SecondEngineExe);
      FRunners[I] := TEngineRunner.Create(FirstEngine, SecondEngine);
      FirstEngine := nil;
      SecondEngine := nil;
    finally
      FreeAndNil(FirstEngine);
      FreeAndNil(SecondEngine);
    end;
    FRunners[I].Initialize;
  end;
  for I := 0 to Jobs - 1 do
  begin
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
var
  Runner: TEngineRunner;
begin
  Join;
  for Runner in FRunners do
    if Runner <> nil then
      Runner.Uninitialize;
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
  DoneCriticalsection(FLock);
  for Thread in FThreads do
    CloseThread(Thread);
  for Runner in FRunners do
    Runner.Free;
  inherited Destroy;
end;

procedure TParallelRunner.ThreadFunc(Index: integer);
var
  MustStop: boolean;
begin
  while True do
  begin
    EnterCriticalsection(FLock);
    try
      MustStop := True;
      if FGames <> 0 then
      begin
        MustStop := False;
        Dec(FGames);
      end;
    finally
      LeaveCriticalsection(FLock);
    end;
    if MustStop then
      break;
    try
      FRunners[Index].Play(FOptions);
      if Assigned(FProgress) then
        FProgress.Step;
    except
      on E: Exception do
      begin
        EnterCriticalsection(FLock);
        try
          WriteLn(StdErr, 'Thread finished with exception:');
          WriteLn(StdErr, 'Exception ', E.ClassName, ': ', E.Message);
          DumpExceptionBackTrace(StdErr);
          WriteLn(StdErr, 'Terminating now.');
          halt(1);
        finally
          LeaveCriticalsection(FLock);
        end;
      end;
    end;
  end;
end;

end.


