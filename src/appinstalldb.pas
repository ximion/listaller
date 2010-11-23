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

{$mode objfpc}{$H+}

interface

uses
  DB, Classes, LiUtils, SysUtils, SQLite3DS, LiTypes, ListallerDB;

type
  TAppInstallDB = class
  private
    DBName: String;
    ds: TSQLite3Dataset;
    EOF: Boolean;
    currField: TLiDBData;
    loaded: Boolean;

    FNewApp: LiNewAppEvent;
    onnewapp_udata: Pointer;
    procedure ToApps;
    procedure ToLocale;
    function ValFormat(s: String): String;
    function GetCurrentAppField: LiAppInfo;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open AppInstall database
    function Load(const rootmode: Boolean): Boolean;
    //** Get list of installed apps
    function GetApplicationList(filter: LiFilter; filter_text: String;
      blacklist: TStringList = nil): Boolean;
    //** Return true if application exists
    function ContainsAppEntry(appID: String): Boolean;
    //** Add application to the data
    procedure AddApplication(app: LiAppInfo);
    //** Write changes to disk
    procedure Finalize;
    //** Register call to on new app found
    procedure RegOnNewApp(call: LiNewAppEvent; user_data: Pointer);
    //** Open filtered applications list (and set pointer to beginning)
    procedure OpenFilter;
    //** Move one entry forward
    procedure NextField;
    //** Close (filter) connection
    procedure CloseFilter;
    //** Delete current app
    procedure DeleteCurrentApp;

    property EndReached: Boolean read EOF;
    property CurrentDataField: TLiDBData read CurrField;
    //** Event: Called if new application was found
    property OnNewApp: LiNewAppEvent read FNewApp write FNewApp;
  end;

implementation

{ TAppInstallDB }

constructor TAppInstallDB.Create;
begin
  ds := TSQLite3Dataset.Create(nil);
  EOF := true;
  onnewapp_udata := nil;
  loaded := false;
end;

procedure TAppInstallDB.RegOnNewApp(call: LiNewAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    onnewapp_udata := user_data;
    FNewApp := call;
  end
  else
    perror('Invalid NewAppEvent pointer received!');
end;

function TAppInstallDB.Load(const rootmode: Boolean): Boolean;
begin
  Result := false;
  //FIXME: Load db from settings
  if rootmode then
    DBName := '/usr/share/app-install/desktop.db'
  else
    DBName := SyblToPath('SHARE', false, false) + '/desktop.db';

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
  Result := true;
  loaded := true;
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

procedure TAppInstallDB.DeleteCurrentApp;
begin
  ds.TableName := 'applications';
  ds.ExecuteDirect('DELETE FROM applications WHERE rowid=' + IntToStr(ds.RecNo));
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

procedure TAppInstallDB.AddApplication(app: LiAppInfo);
var
  sql: Widestring;
begin
  ToApps;
  ds.Edit;

  sql := ValFormat(app.PkName) + ', ' + ValFormat(app.RemoveId) +
    ', ' + ValFormat(app.Categories) + ', ' + ValFormat('installer:local') +
    ',' + ValFormat(app.IconName) + ',' + ValFormat(app.Name) + ',' +
    ValFormat(app.Summary);

  sql := 'INSERT INTO applications (application_id, package_name, categories, ' +
    'repo_id, icon_name, application_name, application_summary) ' +
    'VALUES (' + sql + ');';

  ds.ExecSQL(sql);
  ds.Post;
end;

procedure TAppInstallDB.Finalize;
begin
  if loaded then
  begin
    ds.Active := true;
    ds.ApplyUpdates;
  end;
end;

function TAppInstallDB.GetCurrentAppField: LiAppInfo;
var
  r: LiAppInfo;
  h: String;
begin
  r.Name := PChar(ds.FieldByName('application_name').AsString);
  r.PkName := PChar(ds.FieldByName('package_name').AsString);
  h := LowerCase(ds.FieldByName('repo_id').AsString);
  if h = 'installer:local' then
    r.PkType := ptExtern
  else
    r.PkType := ptNative;

  r.Summary := PChar(ds.FieldByName('application_summary').AsString);
  r.Version := ''; //AppInstall data does not provide version information...
  r.Author := ''; //.. and info about the author
  r.IconName := PChar(ds.FieldByName('icon_name').AsString);
  r.Profile := ''; //@DEPRECATED

  r.Categories := PChar(ds.FieldByName('categories').AsString);

  r.Dependencies := '';
  Result := r;
end;

function TAppInstallDB.GetApplicationList(filter: LiFilter; filter_text: String;
  blacklist: TStringList = nil): Boolean;
var
  entry: LiAppInfo;
begin
  Result := false;
  ToApps;

  ds.Filtered := true;
  ds.First;

  while not ds.EOF do
  begin
    entry := GetCurrentAppField;
    entry.RemoveId := entry.PkName;

    if Assigned(blacklist) then
      blacklist.Add(entry.Name);

    if entry.Summary = '' then
      entry.Summary := 'No description available';

    filter_text := trim(filter_text);

    if (filter = fAppNative) and (entry.PkType <> ptNative) then
    else
    if (filter = fAppExtern) and (entry.PkType <> ptExtern) then
    else
    if ((filter_text = '*') or (filter_text = '')) or
      (pos(filter_text, entry.Summary) > 0) or (pos(filter_text, entry.Name) > 0) then
      if Assigned(FNewApp) then
        FNewApp(entry.Name, @entry, onnewapp_udata);

    ds.Next;
  end;
  ds.Close;
end;

procedure TAppInstallDB.OpenFilter;
begin
  ToApps;
  ds.Filtered := true;
  ds.First;
  EOF := ds.EOF;
  CurrField.App := GetCurrentAppField;
end;

procedure TAppInstallDB.NextField;
begin
  ds.Next;
  EOF := ds.EOF;
  CurrField.App := GetCurrentAppField;
end;

procedure TAppInstallDB.CloseFilter;
begin
  ds.Filtered := false;
  ds.Close;
end;

end.

