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
  LiFileUtil;

type
  TLokiBackend = class(TLiBackend)
  private
    dskFileName: string;
    mojo: boolean;
  public
    constructor Create;
    destructor Destroy;

    function Initialize(ai: LiAppInfo): boolean;
    function CanBeUsed: boolean;
    function Run: boolean;
  end;

implementation

{ TLokiBackend }

constructor TLokiBackend.Create;
begin
  inherited;
  mojo := False;
end;

destructor TLokiBackend.Destroy;
begin
  inherited;
end;

function TLokiBackend.Initialize(ai: LiAppInfo): boolean;
begin
  dskFileName := GetDesktopFile(ai.removeID);

  Result := True;
end;

function TLokiBackend.CanBeUsed: boolean;
var
  inf: TIniFile;
begin
  Result := True;
  if not DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry',
    'Exec', '?'))) then
    Result := False
  else if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
    '.mojosetup') then
    mojo := True
  else if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry',
    'Exec', '?')) + '.manifest') then
    mojo := False
  else
    Result := False;
end;

function TLokiBackend.Run: boolean;
var
  inf: TIniFile;
  tmp: TStringList;
  t: TProcess;
  mandir: string;
begin
  Result := True;
  pdebug('Mojo/LOKI remover using file ' + dskFileName);
  inf := TIniFile.Create(dskFileName);

  if mojo then
  begin
    //MOJO
    mandir := ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
      '.mojosetup';
    inf.Free;
    EmitMessage('Mojo manifest found.');
    EmitProgress(40);
    tmp := TStringList.Create;
    tmp.Assign(FindAllFiles(mandir + '/manifest', '*.xml', False));
    if tmp.Count <= 0 then
      exit;
    EmitProgress(50);
    EmitMessage(rsRemovingApp);
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
    EmitMessage(rsLOKISetupFound);
    EmitMessage(rsRemovingApp);

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

