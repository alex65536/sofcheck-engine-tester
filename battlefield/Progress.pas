unit Progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAbstractProgress }

  TAbstractProgress = class
  public
    procedure Step(Sender: TObject); virtual; abstract;
  end;

implementation

end.
