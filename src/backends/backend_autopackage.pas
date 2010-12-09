(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 *  Matthias Klumpp
 *
 * This unit is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This unit is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this unit. If not, see <http://www.gnu.org/licenses/>.
 *)
//** Autopackage uninstaller backend
unit backend_autopackage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LiBackend, LiTypes, LiUtils, IniFiles, StrLocale, Process,
  LiFileUtil, LiApp;

type
  TAutopackageBackend = class(TLiBackend)
  private
    dskFileName: String;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(ai: TLiAppItem): Boolean; override;
    function CanBeUsed: Boolean; override;
    function Run: Boolean; override;
  end;

implementation

{ TAutopackageBackend }

constructor TAutopackageBackend.Create;
begin
  inherited;
end;

destructor TAutopackageBackend.Destroy;
begin
  inherited;
end;

function TAutopackageBackend.Initialize(ai: TLiAppItem): Boolean;
begin
  dskFileName := GetDesktopFileFromID(ai.AID);

  Result := true;
end;

function TAutopackageBackend.CanBeUsed: Boolean;
var
  d: TIniFile;
begin
  Result := true;
  d := TIniFile.Create(dskFileName);
  if not FileExists(dskFileName) then
    Result := false;
  //Check for Autopackage.org installation
  if pos('apkg-remove', LowerCase(d.ReadString('Desktop Entry',
    'Actions', ''))) > 0 then
    Result := true;
  d.Free;
end;

function TAutopackageBackend.Run: Boolean;
var
  inf: TIniFile;
  p: TProcess;
begin
  Result := true;
  inf := TIniFile.Create(dskFileName);
  p := TProcess.Create(nil);
  p.CommandLine := inf.ReadString('Desktop Entry', 'Actions', '');
  p.Options := [poUsePipes, poWaitonexit];
  p.Execute;
  if p.ExitStatus > 0 then
    Result := false;
  p.Free;
  inf.Free;
end;

end.

