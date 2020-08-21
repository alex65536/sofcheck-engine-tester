unit Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils;

type

  { TAbstractProgress }

  TAbstractProgress = class
  public
    procedure Step; virtual; abstract;
  end;

  { TProgress }

  TProgress = class(TAbstractProgress)
  public
    constructor Create(Total: integer);

    procedure Step; override;
  private
    FStartTime: int64;
    FTotal: integer;
    FCount: integer;
  end;

implementation

{ TProgress }

constructor TProgress.Create(Total: integer);
begin
  FTotal := Total;
  FStartTime := GetTickCount64;
end;

procedure TProgress.Step;
var
  Time: double;
  PredictedTime: double;
begin                                     
  Inc(FCount);
  Time := (GetTickCount64 - FStartTime) / 1000;
  PredictedTime := Time / FCount * FTotal;
  WriteLn(StdErr, Format('%d/%d games completed (%s/%s)',
    [FCount, FTotal, HumanTimeString(Time), HumanTimeString(PredictedTime)]));
end;

end.
