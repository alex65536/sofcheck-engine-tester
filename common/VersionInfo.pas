{
  Copyright © 2020-2021 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit VersionInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  FpcVersion = {$I %FPCVERSION%};

function GetAppVersion: string;
function GetAppBuildTime: string;
function GetAppTarget: string;

implementation

uses
  FileInfo,
  {$IfDef Windows}
  winpeimagereader,
  {$Else}
  elfreader,
  {$EndIf}
  DateUtils;

const
  BuildDate = {$I %DATE%};
  BuildTime = {$I %TIME%};
  TargetOS = {$I %FPCTARGETOS%};
  TargetCPU = {$I %FPCTARGETCPU%};

function GetAppBuildTime: string;
var
  DateFmt: TFormatSettings;
begin
  DateFmt := DefaultFormatSettings;
  DateFmt.ShortDateFormat := 'y/m/d';
  DateFmt.DateSeparator := '/';
  Result := FormatDateTime('yyyy-mm-dd', StrToDateTime(BuildDate, DateFmt)) +
    ' ' + BuildTime;
end;

function GetAppTarget: string;
begin
  Result := TargetCPU + '-' + LowerCase(TargetOS);
end;

function GetAppVersion: string;
const
  AppVersionName = 'ProductVersion';
var
  VersionInfo: TFileVersionInfo;
begin
  VersionInfo := TFileVersionInfo.Create(nil);
  try
    VersionInfo.ReadFileInfo;
    Result := VersionInfo.VersionStrings.Values[AppVersionName];
  finally
    FreeAndNil(VersionInfo);
  end;
end;

end.

