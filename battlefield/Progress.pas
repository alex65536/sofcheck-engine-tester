{
  This file is part of Battlefield - a tool to run micro-matches between chess
  engines.

  Copyright © 2020 Alexander Kernozhitsky <sh200105@mail.ru>

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
