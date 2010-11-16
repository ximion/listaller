(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 * Matthias Klumpp
 *
 * This unit is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as publishedf by the Free Software
 * Foundation, version 3.
 *
 * This unit is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this unit. If not, see <http://www.gnu.org/licenses/>.
 *)
unit pkpasbind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TPKHeader2Pas = class
    private
      proc: TProcess;
      h2pas: String;
    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

{ TPKHeader2Pas }

constructor TPKHeader2Pas.Create;
begin
  inherited;
  h2pas := 'h2pas -d -l libpackagekit-glib2.so -i -p -c' + ' ';
  proc := TProcess.Create(nil);
end;

destructor TPKHeader2Pas.Destroy;
begin
  proc.Free;
  inherited;
end;

end.

