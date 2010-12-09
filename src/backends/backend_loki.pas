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
//** LOKI/Mojo uninstaller backend
unit backend_loki;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LiBackend, LiTypes, LiUtils, IniFiles, StrLocale, Process,
  LiFileUtil, LiApp;

type
  TLokiBackend = class(TLiBackend)
  private
    dskFileName: String;
    mojo: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(ai: TLiAppItem): Boolean; override;
    function CanBeUsed: Boolean; override;
    function Run: Boolean; override;
  end;

implementation

{ TLokiBackend }

constructor TLokiBackend.Create;
begin
  inherited;
  mojo := false;
end;

destructor TLokiBackend.Destroy;
begin
  inherited;
end;

function TLokiBackend.Initialize(ai: TLiAppItem): Boolean;
begin
  dskFileName := GetDesktopFileFromID(ai.AID);

  Result := true;
end;

function TLokiBackend.CanBeUsed: Boolean;
var
  inf: TIniFile;
begin
  Result := true;
  inf := TIniFile.Create(dskFileName);
  if not FileExists(dskFileName) then
    Result := false
  else
  if not DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry',
    'Exec', '?'))) then
    Result := false
  else if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
    '.mojosetup') then
    mojo := true
  else if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry',
    'Exec', '?')) + '.manifest') then
    mojo := false
  else
    Result := false;
  inf.Free;
end;

function TLokiBackend.Run: Boolean;
var
  inf: TIniFile;
  tmp: TStringList;
  t: TProcess;
  mandir: String;
begin
  Result := true;
  pdebug('Mojo/LOKI remover using file ' + dskFileName);
  inf := TIniFile.Create(dskFileName);

  if mojo then
  begin
    //MOJO
    mandir := ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
      '.mojosetup';
    inf.Free;
    EmitInfoMsg('Mojo manifest found.');
    EmitProgress(40);
    tmp := TStringList.Create;
    tmp.Assign(FindAllFiles(mandir + '/manifest', '*.xml', false));
    if tmp.Count <= 0 then
      exit;
    EmitProgress(50);
    EmitInfoMsg(rsRemovingApp);
    t := TProcess.Create(nil);
    t.CommandLine := mandir + '/mojosetup uninstall ' + copy(
      ExtractFileName(tmp[0]), 1, pos('.', ExtractFileName(tmp[0])) - 1);
    t.Options := [poUsePipes, poWaitonexit];
    tmp.Free;
    EmitProgress(60);
    t.Execute;
    t.Free;
    EmitProgress(100);
  end
  else
    //LOKI
  begin
    EmitProgress(50);
    EmitInfoMsg(rsLOKISetupFound);
    EmitInfoMsg(rsRemovingApp);

    t := TProcess.Create(nil);
    t.CommandLine := ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
      '/uninstall';
    t.Options := [poUsePipes, poWaitonexit];

    EmitProgress(60);
    t.Execute;
    t.Free;
    EmitProgress(100);
  end;
end;

end.

