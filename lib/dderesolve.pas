{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This library is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Resolve files parts to Debian packages
unit dderesolve;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, FileUtil, HTTPSend, SysUtils;

type
  DDEPackage = record
    PkName: String;
    Distro: String;
    Link: String;
    Path: String;
  end;

  TDDEResolver = class
  private
    HTTP: THTTPSend;
    DDEPkg: DDEPackage;
  public
    constructor Create;
    destructor Destroy; override;

    function ResolveString(Data: String): Boolean;
    function DownloadPackage: Boolean;
    property Pack: DDEPackage read DDEPkg;
  end;

implementation

constructor TDDEResolver.Create;
begin
  HTTP := THTTPSend.Create;
  HTTP.UserAgent := 'Listaller Fetch';
end;

destructor TDDEResolver.Destroy;
begin
  HTTP.Free;
  inherited;
end;

function TDDEResolver.ResolveString(Data: String): Boolean;
var
  arch: String;
  request: String;
  res: TStringList;
  pkg: DDEPackage;
  s: String;
begin
  arch := GetSystemArchitecture;
  Data := StrSubst(Data, '*', '');
  HTTP.Clear;
  request := 'http://dde.debian.net/dde/q/aptfile/byfile/all-' +
    arch + '/' + Data + '?t=text';
  Result := HTTP.HTTPMethod('GET', request);
  res := TStringList.Create;
  res.LoadFromStream(HTTP.Document);
  if res.Count <= 0 then
  begin
    Result := false;
    exit;
  end;
  s := res.Text;
  res.Free;
  if s[1] = '[' then
    Delete(s, 1, 1);
  if s[length(s) - 1] = ']' then
    Delete(s, length(s) - 1, 1);

  repeat
    while (s[1] <> '[') and (length(s) > 0) do
    begin
      Delete(s, 1, 2);
    end;
    if s[1] = '[' then
    begin
      Delete(s, 1, 2);
      pkg.Distro := copy(s, 1, Pos('''', s) - 1);
      Delete(s, 1, pos('''', s));
      Delete(s, 1, pos('''', s));
      pkg.PkName := copy(s, 1, Pos('''', s) - 1);
      Delete(s, 1, Pos(']', s));

      {
      p_debug('Distro: '+pkg.Distro);
      p_debug('Name: '+pkg.pkName);
      p_debug(s); }
    end;
    if pkg.Distro = 'sid' then
      break; //We prefer debian unstable packages, if available
  until length(s) = 0;
  DDEPkg := Pkg;

  if StrSubst(Pkg.PkName, ' ', '') = '' then
  begin
    DDEPkg.PkName := '';
    DDEPkg.Distro := '';
    DDEPkg.Path := '';
    Result := false;
  end
  else
    DDEPkg.Path := '#';
end;

function TDDEResolver.DownloadPackage: Boolean;
var
  url: String;
  txt: TStringList;
  i: Integer;
  s: String;
begin
  Result := false;
  if DDEPkg.PkName = '' then
    exit;
  if DDEPkg.Distro = '' then
    exit;

  HTTP.Clear;
  //First get downloads list
  url := 'http://packages.debian.org/' + DDEPkg.Distro + '/' + GetSystemArchitecture +
    '/' + DDEPkg.PkName + '/download';
  HTTP.HTTPMethod('GET', url);
  txt := TStringList.Create;
  txt.LoadFromStream(HTTP.Document);
  //Grab one server (should be optimized to choose from different servers)
  //Can't use IndexOf here...
  for i := 0 to txt.Count - 1 do
    if pos('href="http://ftp.de.debian.org/debian/pool/', txt[i]) > 0 then
    begin
      s := txt[i];
      break;
    end;
  txt.Free;

  Delete(s, 1, pos('"', s));
  s := copy(s, 1, pos('"', s) - 1);
  DDEPkg.Link := s;

  HTTP.Clear;
  Result := HTTP.HTTPMethod('GET', DDEPkg.Link);

  s := TMPDIR + 'download/' + ExtractFileName(s);
  ForceDirectory(ExtractFilePath(s));
  HTTP.Document.SaveToFile(s);
  DDEPkg.Path := s;
  if FileExists(s) then
    Result := true;
end;

end.

