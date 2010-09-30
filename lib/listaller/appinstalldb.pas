{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Implementation of PackageKit's AppInstall DB specs
unit appinstalldb;

{$mode objfpc}

interface

uses
  DB, Classes, LiUtils, SQLite3, SysUtils, SQLite3DS;

type
  TAppInstallDB = class
  private
    DBName: String;
    ds: TSQLite3Dataset;
    procedure ToApps;
    procedure ToLocale;
    function ValFormat(s: String): String;
  public
    constructor Create(useSystemDB: Boolean);
    destructor Destroy;

    function ContainsAppEntry(appID: String): Boolean;
    procedure AddApplication(appID, pkgName, groupNames, repoID,
      iconName, appName, appDesc: String);
    procedure Finalize;
  end;

implementation

{ TAppInstallDB }

constructor TAppInstallDB.Create(useSystemDB: Boolean);
begin
  //FIXME: Load db from settings
  if useSystemDB then
    DBName := '/usr/share/app-install/desktop.db'
  else
    DBName := SyblToPath('SHARE', false, false) + '/desktop.db';

  ds := TSQLite3Dataset.Create(nil);
  ds.FileName := DBName;
  //Create initial layout if necessary
  with ds do
  begin
    TableName := 'applications';
    if not TableExists then
    begin
      FieldDefs.Clear;
      FieldDefs.Add('application_id', ftString);
      FieldDefs.Add('package_name', ftString);
      FieldDefs.Add('categories', ftString);
      FieldDefs.Add('repo_id', ftString);
      FieldDefs.Add('icon_name', ftString);
      FieldDefs.Add('application_name', ftString);
      FieldDefs.Add('application_summary', ftString);
      CreateTable;
    end;
    TableName := 'translations';
    if not TableExists then
    begin
      FieldDefs.Clear;
      FieldDefs.Add('application_id', ftString);
      FieldDefs.Add('application_name', ftString);
      FieldDefs.Add('application_summary', ftString);
      FieldDefs.Add('locale', ftString);
      CreateTable;
    end;
  end;
  ds.Open; //Put DB in active state
  ds.Active := true;
  ds.ApplyUpdates;
end;

destructor TAppInstallDB.Destroy;
begin
  Finalize; //Make sure all stuff is written to disk
  ds.Free;
  inherited;
end;

procedure TAppInstallDB.ToApps;
begin
  ds.Close;
  ds.SQL := 'SELECT * FROM applications';
  ds.Open;
end;

procedure TAppInstallDB.ToLocale;
begin
  ds.Close;
  ds.SQL := 'SELECT * FROM translations';
  ds.Open;
end;

function TAppInstallDB.ContainsAppEntry(appID: String): Boolean;
begin
  ToApps;
  Result := false;
  appID := Trim(appID);
  if appID = '' then
    exit;

  Result := ds.Locate('application_id', appID, [loCaseInsensitive]);
end;

function TAppInstallDB.ValFormat(s: String): String;
begin
  Result := '''' + StrSubst(s, '''', '''''') + '''';
end;

procedure TAppInstallDB.AddApplication(appID, pkgName, groupNames,
  repoID, iconName, appName, appDesc: String);
var
  sql: WideString;
begin
  ToApps;
  ds.Edit;

  sql := ValFormat(appID) + ', ' + ValFormat(pkgName) + ', ' +
    ValFormat(groupNames) + ', ' + ValFormat(repoID) + ',' +
    ValFormat(iconName) + ',' + ValFormat(appName) + ',' + ValFormat(appDesc);

  sql := 'INSERT INTO applications (application_id, package_name, categories, ' +
    'repo_id, icon_name, application_name, application_summary) ' +
    'VALUES (' + sql + ');';

  ds.ExecSQL(sql);
  ds.Post;
end;

procedure TAppInstallDB.Finalize;
begin
  ds.ApplyUpdates;
end;

end.

