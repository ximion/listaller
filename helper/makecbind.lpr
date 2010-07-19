{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Helper for generating C-Headers from Pascal library source
program makecbind;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, pasconv;

type

  { TCBindGenerator }

  TCBindGenerator = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCBindGenerator }

procedure TCBindGenerator.DoRun;
var
  ErrorMsg: String;
  test: TStringList;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('s','source') then
  begin
    if not FileExists(GetOptionValue('s', 'source')) then
    begin
      writeLn('No source file found!');
      halt(1);
    end;
  end;

  if FileExists(paramstr(1)) then
  begin
    test := ConvertToCInfo(paramstr(1));
    writeLn(test.Text);
    test.Free;
  end;
  // stop program loop
  Terminate;
end;

constructor TCBindGenerator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCBindGenerator.Destroy;
begin
  inherited Destroy;
end;

procedure TCBindGenerator.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TCBindGenerator;

{$R *.res}

begin
  Application:=TCBindGenerator.Create(nil);
  Application.Title:='make-cbind';
  Application.Run;
  Application.Free;
end.

