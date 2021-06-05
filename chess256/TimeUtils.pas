{
  This file is part of Chess 256.

  Copyright © 2021 Alexander Kernozhitsky <sh200105@mail.ru>

  Chess 256 is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Chess 256 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Chess 256.  If not, see <http://www.gnu.org/licenses/>.

  Abstract:
    This file contains the utility functions related to time.
}
unit TimeUtils;

{$mode objfpc}{$H+}

interface

procedure PreciseSleep(Millis: int64);

implementation

uses
  {$IfDef Windows}
  MMSystem, Windows,
{$EndIf}
  SysUtils;

{$IfDef Windows}

type

  { TSleeper }

  TSleeper = class
  private
    FEvent: PRTLEvent;
    FTimeEvent: MMRESULT;
  public
    constructor Create(Millis: int64);
    destructor Destroy; override;
    procedure Wait;
  end;

procedure TimeEventProc(uTimerID, uMsg: UINT; dwUser, dw1, dw2: DWORD_PTR); stdcall;
var
  Sleeper: TSleeper;
begin
  Sleeper := TSleeper(Pointer(dwUser));
  RTLEventSetEvent(Sleeper.FEvent);
end;

{ TSleeper }

constructor TSleeper.Create(Millis: int64);
begin
  FTimeEvent := timeSetEvent(Millis, 0, @TimeEventProc, DWORD_PTR(Self), TIME_ONESHOT);
  FEvent := RTLEventCreate;
  if FTimeEvent = 0 then
    raise EOSError.Create('Call to timeSetEvent failed');
end;

destructor TSleeper.Destroy;
begin
  RTLEventDestroy(FEvent);
  if FTimeEvent <> 0 then
    timeKillEvent(FTimeEvent);
  inherited Destroy;
end;

procedure TSleeper.Wait;
begin
  RTLEventWaitFor(FEvent);
end;

procedure PreciseSleep(Millis: int64);
var
  Sleeper: TSleeper;
begin
  Sleeper := TSleeper.Create(Millis);
  try
    Sleeper.Wait;
  finally
    FreeAndNil(Sleeper);
  end;
end;

{$Else}

procedure PreciseSleep(Millis: int64);
begin
  Sleep(Millis);
end;

{$EndIf}

end.
