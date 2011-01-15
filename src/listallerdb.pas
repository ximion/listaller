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
//** Provide easy access to Listaller's software database
unit listallerdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3DS, LiUtils, LiTypes, DB, StrLocale, LiStatusObj, LiApp;

type
  TLiDBData = record
    App: TLiAppItem;
  end;

  //** Access to Listaller's internal software db
  TListallerDB = class(TLiStatusObject)
  private
    ds: TSQLite3Dataset;
    FNewApp: LiAppEvent;
    CurrField: TLiDBData;
    DBPath: String;
    AppDataDir: String;
    DepDataDir: String;
    EOF: Boolean;
    loaded: Boolean;
    onnewapp_udata: Pointer;

    procedure ToApps;
    procedure ToDeps;
    function GetAppField: TLiAppItem;
    function GetSQLiteVersion: String;
    function DBOkay: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open software database
    function Load(const rootmode: Boolean = false): Boolean;
    //** Get list of installed ipk apps
    function GetApplicationList(filter_text: String;
      blacklist: TStringList = nil): Boolean;
    //** Open filtered applications list (and set pointer to beginning)
    procedure OpenFilterAppList;
    //** Move one entry forward
    procedure NextField;
    //** Close (filter) connection
    procedure CloseFilter;
    //** Check if application is installed
    function AppExists(appid: String): Boolean;
    //** Delete current app
    procedure AppDeleteCurrent;
    //** Add a new application
    procedure AppAddNew(app: TLiAppItem);
    //** Update version of app
    procedure AppUpdateVersion(appID: String; newv: String);
    //** Add new dependency to databse
    procedure DepAddNew(Name: String; version: String; origin: String;
      depnames: Widestring);
    //** Check if dependency is already present
    function DepExists(Name, version: String): Boolean;
    //** Register call to on new app found
    procedure RegOnNewApp(call: LiAppEvent; user_data: Pointer);
    //** Write changes to disk
    function Finalize: Boolean;


    property EndReached: Boolean read EOF;
    property DataField: TLiDBData read CurrField;
    property SQLiteVersion: String read GetSQLiteVersion;
    property AppConfDir: String read AppDataDir;
    property DepConfDir: String read DepDataDir;
    //** Event: Called if new application was found
    property OnNewApp: LiAppEvent read FNewApp write FNewApp;
  end;

implementation

constructor TListallerDB.Create;
begin
  ds := TSQLite3Dataset.Create(nil);
  onnewapp_udata := nil;
  EOF := true;
  loaded := false;
  DBPath := '';
  CurrField.App := nil;
  // Default values
  AppDataDir := CleanFilePath(RootPkgRegDir + '/apps/');
  DepDataDir := CleanFilePath(RootPkgRegDir + '/deps/');
end;

destructor TListallerDB.Destroy;
begin
  FreeAndNil(CurrField.App);
  ds.Close; //Close, if opened
  ds.Active := false;
  ds.Free;
  inherited;
end;

function TListallerDB.GetSQLiteVersion: String;
begin
  Result := ds.SqliteVersion;
end;

procedure TListallerDB.RegOnNewApp(call: LiAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    onnewapp_udata := user_data;
    FNewApp := call;
  end
  else
    perror('Invalid LiAppEvent pointer received!');
end;

function TListallerDB.DBOkay: Boolean;
begin
  Result := false;
  // This should never happen, so raise a (cheap) exception
  if not loaded then
    raise Exception.Create('Listaller database was not loaded before!');

  if FileExists(ds.FileName) then
    if ds.TableExists('applications') then
      if ds.TableExists('dependencies') then
        Result := true;
end;

procedure TListallerDB.ToApps;
begin
  DBOkay;
  ds.Close;
  ds.SQL := 'SELECT * FROM applications';
  ds.TableName := 'applications';
  ds.Open;
end;

procedure TListallerDB.ToDeps;
begin
  DBOkay;
  ds.Close;
  ds.SQL := 'SELECT * FROM dependencies';
  ds.TableName := 'dependencies';
  ds.Open;
end;

function TListallerDB.Load(const rootmode: Boolean = false): Boolean;
var
  rd: String;
begin
  Result := true;
  pdebug('Opening Listaller db...');
  if rootmode then
    rd := RootPkgRegDir
  else
    rd := PkgRegDir;

  AppDataDir := CleanFilePath(rd + '/apps/');
  DepDataDir := CleanFilePath(rd + '/deps/');
  ForceDirectories(rd);
  ForceDirectories(AppDataDir);
  ForceDirectories(DepDataDir);
  if not DirectoryExists(rd) then
  begin
    Result := false;
    exit;
  end;

  DBPath := rd + 'software.db';
  ds.FileName := DBPath;
  with ds do
  begin
    pdebug('Database filename is' + FileName);
    if ((rootmode) and (not IsRoot) and (not FileExists(FileName))) then
      Result := false
    else
    begin
      //Create table for Applications
      TableName := 'applications';

      if not TableExists then
      begin
        with FieldDefs do
        begin
          Clear;
          Add('app_name', ftString, 0, true);
          Add('app_id', ftString, 0, true);
          Add('installation_type', ftString, 0, true);
          Add('app_summary', ftString, 0, false);
          Add('app_version', ftFloat, 0, true);
          Add('app_publisher', ftString, 0, false);
          Add('app_iconname', ftString, 0, false);
          Add('app_categories', ftString, 0, true);
          Add('app_timestamp', ftDateTime, 0, false);
          Add('app_dependencies', ftMemo, 0, false);
        end;
        CreateTable;
      end;
      //Create table for Dependencies
      TableName := 'dependencies';
      if not TableExists then
      begin
        with FieldDefs do
        begin
          Clear;
          Add('dep_name', ftString, 0, true);
          Add('dep_version', ftString, 0, true);
          Add('dep_origin', ftString, 0, true);
          Add('dep_dependencies', ftMemo, 0, true);
          Add('dep_timestamp', ftDateTime, 0, false);
        end;
        CreateTable;
      end;
    end;
  end;
  ds.Open;
  loaded := true;
  pinfo(rsDBOpened);
end;

function TListallerDB.GetApplicationList(filter_text: String;
  blacklist: TStringList = nil): Boolean;
var
  entry: TLiAppItem;
  p: Ansistring;
begin
  Result := false;
  if not DBOkay then
    exit;

  ToApps;
  ds.Filtered := true;
  ds.First;

  while not ds.EOF do
  begin
    // Fetch LiApplication object
    entry := GetAppField;

    if Assigned(blacklist) then
      blacklist.Add(entry.AName);

    entry.Version := PChar(rsVersion + ': ' + entry.Version);
    if entry.Author <> '' then
      entry.Author := PChar(rsAuthor + ': ' + entry.Author);

    p := AppDataDir + LowerCase(GetAppIDString(entry.AId)) + '/';

    //InstLst.Add(LowerCase(dsApp.FieldByName('ID').AsString));

    if entry.Summary = '' then
      entry.Summary := 'No description available';

    if FileExists(p + 'icon.png') then
      entry.IconName := PChar(p + 'icon.png');

    filter_text := trim(filter_text);

    if ((filter_text = '*') or (filter_text = '')) or
      (pos(filter_text, entry.Summary) > 0) or (pos(filter_text, entry.AName) > 0) then
      if Assigned(FNewApp) then
        FNewApp(Pointer(entry), onnewapp_udata)
      else
        FreeAndNil(FNewApp);

    ds.Next;
  end;
end;

function TListallerDB.GetAppField: TLiAppItem;
var
  r: TLiAppItem;
  h: String;

  function _(s: Widestring): PChar;
  begin
    Result := PChar(s);
  end;

begin
  ToApps;
  r := TLiAppItem.Create;
  r.AName := _(ds.FieldByName('app_name').AsString);
  h := LowerCase(ds.FieldByName('installation_type').AsString);
  if h = 'linstall' then
    r.PkType := ptLinstall
  else
  if h = 'dlink' then
    r.PkType := ptDLink
  else
  if h = 'container' then
    r.PkType := ptContainer;

  r.Summary := _(ds.FieldByName('app_summary').AsString);
  r.Version := _(ds.FieldByName('app_version').AsString);
  r.Author := _(ds.FieldByName('app_publisher').AsString);
  r.IconName := _(ds.FieldByName('app_iconname').AsString);
  r.AId := _(ds.FieldByName('app_id').AsString);
  r.Categories := _(ds.FieldByName('app_categories').AsString);

  r.TimeStamp := ds.FieldByName('app_timestamp').AsDateTime;
  r.Dependencies := PChar(ds.FieldByName('app_dependencies').AsWideString);
  Result := r;
end;

procedure TListallerDB.OpenFilterAppList;
begin
  if not DBOkay then
    exit;
  ToApps;
  ds.Filtered := true;
  ds.First;
  EOF := ds.EOF;
  // Destroy previous AppItem
  FreeAndNil(CurrField.App);
  CurrField.App := GetAppField;
end;

procedure TListallerDB.NextField;
begin
  ds.Next;
  EOF := ds.EOF;
  FreeAndNil(CurrField.App);
  CurrField.App := GetAppField;
end;

procedure TListallerDB.CloseFilter;
begin
  ds.Filtered := false;
  ds.Close;
end;

function TListallerDB.AppExists(appID: String): Boolean;
begin
  Result := false;
  if not DBOkay then
    exit;
  ToApps;
  appID := Trim(appID);
  if appID = '' then
    exit;

  Result := ds.Locate('app_id', appID, [loCaseInsensitive]);
end;

procedure TListallerDB.AppDeleteCurrent;
begin
  ToApps;
  ds.ExecuteDirect('DELETE FROM applications WHERE rowid=' + IntToStr(ds.RecNo));
  ds.ApplyUpdates;
end;

procedure TListallerDB.AppAddNew(app: TLiAppItem);
var
  g, h: String;
begin
  ToApps;
  ds.Edit;

  if app.PkType = ptLinstall then
    h := 'linstall';
  if app.PkType = ptDLink then
    h := 'dlink';
  if app.PkType = ptContainer then
    h := 'container';

  g := app.Categories;

  ds.Insert;
  ds.ExecuteDirect('INSERT INTO "applications" VALUES (''' + app.AName +
    ''', ''' + app.AId + ''', ''' + h + ''', ''' + app.Summary +
    ''',''' + app.Version + ''',''' + app.Author + ''',''' + 'icon' +
    ExtractFileExt(app.IconName) + ''',''' + g + ''',''' + GetDateAsString +
    ''', ''' + app.Dependencies + ''');');

  //Write changes
  ds.ApplyUpdates;
end;

procedure TListallerDB.AppUpdateVersion(appID: String; newv: String);
begin
  ToApps;
  ds.Filtered := true;
  ds.Edit;
  ds.First;
  ds.ExecuteDirect('UPDATE applications SET Version = ''' + newv +
    ''' WHERE AppId = ''' + appID + '''');
  ds.ApplyUpdates;
end;

procedure TListallerDB.DepAddNew(Name: String; version: String;
  origin: String; depnames: Widestring);
begin
  //Open database connection
  ToDeps;
  ds.Edit;
  ds.Insert;
  ds.ExecuteDirect('INSERT INTO dependencies VALUES (''' + Name +
    ''', ''' + Version + ''', ''' + origin + ''', ''' + depnames +
    ''', ''' + GetDateAsString + ''');');

  //Write changes
  ds.ApplyUpdates;
end;

function TListallerDB.DepExists(Name, version: String): Boolean;
begin
  Result := false;
  if not DBOkay then
    exit;
  ToDeps;
  ds.Filtered := true;
  ds.First;

  Result := false;
  while not ds.EOF do
  begin
    if (ds.FieldByName('dep_name').AsString = Name) and
      (ds.FieldByName('dep_version').AsString = Version) then
    begin
      Result := true;
      break;
    end
    else
      Result := false;
    ds.Next;
  end;
end;

function TListallerDB.Finalize: Boolean;
begin
  Result := false;
  if loaded then
  begin
    ToApps;
    Result := ds.ApplyUpdates;
  end;
end;

end.

