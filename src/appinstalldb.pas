(* Copyright (C) 2010-2011 Matthias Klumpp
 *
 * Licensed under the GNU General Public License Version 3
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
//** Implementation of PackageKit's AppInstall DB specs
unit appinstalldb;

{$mode objfpc}{$H+}

interface

uses
  DB, Classes, LiUtils, SysUtils, SQLite3DS, LiTypes, ListallerDB, LiApp;

type
  TAppInstallDB = class
  private
    DBName: String;
    ds: TSQLite3Dataset;
    EOF: Boolean;
    currField: TLiDBData;
    loaded: Boolean;

    FNewApp: LiAppEvent;
    onnewapp_udata: Pointer;
    procedure ToApps;
    procedure ToLocale;
    function ValFormat(s: String): String;
    function GetCurrentAppField: TLiAppItem;
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
    procedure AddApplication(app: TLiAppItem);
    //** Write changes to disk
    procedure Finalize;
    //** Register call to on new app found
    procedure RegOnNewApp(call: LiAppEvent; user_data: Pointer);
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
    property OnNewApp: LiAppEvent read FNewApp write FNewApp;
  end;

implementation

{ TAppInstallDB }

constructor TAppInstallDB.Create;
begin
  ds := TSQLite3Dataset.Create(nil);
  EOF := true;
  CurrField.App := nil;
  onnewapp_udata := nil;
  loaded := false;
end;

procedure TAppInstallDB.RegOnNewApp(call: LiAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    onnewapp_udata := user_data;
    FNewApp := call;
  end
  else
    perror('Invalid AppEvent pointer received!');
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
  FreeAndNil(CurrField.App);
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

procedure TAppInstallDB.AddApplication(app: TLiAppItem);
var
  sql: Widestring;
begin
  ToApps;
  ds.Edit;

  sql := ValFormat(app.FakePackageName) + ', ' +
    ValFormat(app.PkPackageId) + ', ' + ValFormat(app.Categories) + ', ' +
    ValFormat('local:%listaller') + ',' + ValFormat(app.IconName) +
    ',' + ValFormat(app.AName) + ',' + ValFormat(app.Summary);

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

function TAppInstallDB.GetCurrentAppField: TLiAppItem;
var
  r: TLiAppItem;
  h: String;
begin
  r := TLiAppItem.Create;
  r.AName := PChar(ds.FieldByName('application_name').AsString);
  r.AId := PChar(ds.FieldByName('package_name').AsString);
  h := LowerCase(ds.FieldByName('repo_id').AsString);
  if h = 'local:%listaller' then
    r.PkType := ptExtern
  else
    r.PkType := ptNative;

  r.PkPackageId := PChar(ds.FieldByName('package_name').AsString) + ';;;' + h;
  r.Summary := PChar(ds.FieldByName('application_summary').AsString);
  r.Version := '0.0'; //AppInstall data does not provide information about a pkg version...
  r.Author := ''; // ... or about the software author
  r.IconName := PChar(ds.FieldByName('icon_name').AsString);
  r.Categories := PChar(ds.FieldByName('categories').AsString);

  r.Dependencies := '';
  Result := r;
end;

function TAppInstallDB.GetApplicationList(filter: LiFilter; filter_text: String;
  blacklist: TStringList = nil): Boolean;
var
  entry: TLiAppItem;
begin
  Result := false;
  ToApps;

  ds.Filtered := true;
  ds.First;

  while not ds.EOF do
  begin
    // Fetch AppItem object
    entry := GetCurrentAppField;

    if Assigned(blacklist) then
      blacklist.Add(entry.AName);

    if entry.Summary = '' then
      entry.Summary := 'No description available';

    filter_text := trim(filter_text);

    if (filter = fAppNative) and (entry.PkType <> ptNative) then
    else
    if (filter = fAppExtern) and (entry.PkType <> ptExtern) then
    else
    if ((filter_text = '*') or (filter_text = '')) or
      (pos(filter_text, entry.Summary) > 0) or (pos(filter_text, entry.AName) > 0) then
      if Assigned(FNewApp) then
        FNewApp(Pointer(entry), onnewapp_udata)
      else
        FreeAndNil(entry);

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
  FreeAndNil(CurrField.App);
  CurrField.App := GetCurrentAppField;
end;

procedure TAppInstallDB.NextField;
begin
  ds.Next;
  EOF := ds.EOF;
  FreeAndNil(CurrField.App);
  CurrField.App := GetCurrentAppField;
end;

procedure TAppInstallDB.CloseFilter;
begin
  ds.Filtered := false;
  ds.Close;
end;

end.

