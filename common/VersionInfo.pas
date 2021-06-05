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

