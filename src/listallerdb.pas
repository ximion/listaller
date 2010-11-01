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

{$mode objfpc}

interface

uses
  Classes, SysUtils, SQLite3DS, LiUtils, LiTypes, DB, StrLocale, LiStatusObj;

type
  TLiDBData = record
    App: LiAppInfo;
  end;

  //** Access to Listaller's IPK app db
  TListallerDB = class (TLiStatusObject)
  private
    dsApp: TSQLite3Dataset;
    FNewApp: LiNewAppEvent;
    CurrField: TLiDBData;
    AppDataDir: String;
    DepDataDir: String;
    EOF: Boolean;
    loaded: Boolean;
    onnewapp_udata: Pointer;

    procedure ToApps;
    procedure ToDeps;
    function GetAppField: LiAppInfo;
    function GetSQLiteVersion: String;
    function DBOkay: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open software database
    function Load(const rootmode: Boolean = false): Boolean;
    //** Get list of installed ipk apps
    function GetApplicationList(blacklist: TStringList = nil): Boolean;
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
    procedure AppAddNew(app: LiAppInfo);
    //** Update version of app
    procedure AppUpdateVersion(appID: String; newv: String);
    //** Add new dependency to databse
    procedure DepAddNew(Name: String; version: String; origin: String;
      depnames: Widestring);
    //** Check if dependency is already present
    function DepExists(Name, version: String): Boolean;
    //** Register call to on new app found
    procedure RegOnNewApp(call: LiNewAppEvent; user_data: Pointer);
    //** Write changes to disk
    function Finalize: Boolean;


    property EndReached: Boolean read EOF;
    property DataField: TLiDBData read CurrField;
    property SQLiteVersion: String read GetSQLiteVersion;
    property AppConfDir: String read AppDataDir;
    property DepConfDir: String read DepDataDir;
    //** Event: Called if new application was found
    property OnNewApp: LiNewAppEvent read FNewApp write FNewApp;
  end;

implementation

constructor TListallerDB.Create;
begin
  dsApp := TSQLite3Dataset.Create(nil);
  onnewapp_udata := nil;
  EOF := true;
  loaded := false;
end;

destructor TListallerDB.Destroy;
begin
  dsApp.Close; //Close, if opened
  dsApp.Active := false;
  dsApp.Free;
  inherited;
end;

function TListallerDB.GetSQLiteVersion: String;
begin
  Result := dsApp.SqliteVersion;
end;

procedure TListallerDB.RegOnNewApp(call: LiNewAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    onnewapp_udata := user_data;
    FNewApp := call;
  end
  else
    perror('Invalid LiNewAppEvent pointer received!');
end;

function TListallerDB.DBOkay: Boolean;
begin
  Result := false;
  if FileExists(dsApp.FileName) then
    if dsApp.TableExists('applications') then
      if dsApp.TableExists('dependencies') then
        Result := true;
end;

procedure TListallerDB.ToApps;
begin
  dsApp.Close;
  dsApp.SQL := 'SELECT * FROM applications';
  dsApp.TableName := 'applications';
  dsApp.Open;
end;

procedure TListallerDB.ToDeps;
begin
  dsApp.Close;
  dsApp.SQL := 'SELECT * FROM dependencies';
  dsApp.TableName := 'dependencies';
  dsApp.Open;
end;

function TListallerDB.Load(const rootmode: Boolean = false): Boolean;
var
  rd: String;
begin
  Result := true;

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

  with dsApp do
  begin
    FileName := rd + 'software.db';
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
          Add('Name', ftString, 0, true);
          Add('PkName', ftString, 0, true);
          Add('AppId', ftString, 0, true);
          Add('Type', ftString, 0, true);
          Add('Description', ftString, 0, false);
          Add('Version', ftFloat, 0, true);
          Add('Publisher', ftString, 0, false);
          Add('IconName', ftString, 0, false);
          Add('Profile', ftString, 0, false);
          Add('Category', ftString, 0, true);
          Add('InstallDate', ftDateTime, 0, false);
          Add('Dependencies', ftMemo, 0, false);
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
          Add('Name', ftString, 0, true);
          Add('Version', ftString, 0, true);
          Add('Origin', ftString, 0, true);
          Add('DepNames', ftMemo, 0, true);
          Add('InstallDate', ftDateTime, 0, false);
        end;
        CreateTable;
      end;
      Active := true;
      //ApplyUpdates;
      Open;
    end;
  end;
  ToApps;
  loaded := true;
  pinfo(rsDBOpened);
end;

function TListallerDB.GetApplicationList(blacklist: TStringList = nil): Boolean;
var
  entry: LiAppInfo;
  p: Ansistring;
begin
  Result := false;
  if not DBOkay then
    exit;

  ToApps;
  dsApp.Filtered := true;
  dsApp.First;

  while not dsApp.EOF do
  begin
    entry := GetAppField;
    entry.RemoveId := entry.PkName;

    if Assigned(blacklist) then
      blacklist.Add(entry.Name);

    entry.Version := PChar(rsVersion + ': ' + entry.Version);
    if entry.Author <> '' then
      entry.Author := PChar(rsAuthor + ': ' + entry.Author);

    p := AppDataDir + LowerCase(entry.PkName) + '/';

    //InstLst.Add(LowerCase(dsApp.FieldByName('ID').AsString));

    if entry.Summary = '' then
      entry.Summary := 'No description available';

    if FileExists(p + 'icon.png') then
      entry.IconName := PChar(p + 'icon.png');

    if Assigned(FNewApp) then
      FNewApp(entry.Name, @entry, onnewapp_udata);

    dsApp.Next;
  end;
  dsApp.Close;
end;

function TListallerDB.GetAppField: LiAppInfo;
var
  r: LiAppInfo;
  h: String;

  function _(s: Widestring): PChar;
  begin
    Result := PChar(s);
  end;

begin
  ToApps;
  r.Name := _(dsApp.FieldByName('Name').AsString);
  r.PkName := _(dsApp.FieldByName('PkName').AsString);
  h := LowerCase(dsApp.FieldByName('Type').AsString);
  if h = 'linstall' then
    r.PkType := ptLinstall
  else
  if h = 'dlink' then
    r.PkType := ptDLink
  else
  if h = 'container' then
    r.PkType := ptContainer;

  r.Summary := _(dsApp.FieldByName('Description').AsString);
  r.Version := _(dsApp.FieldByName('Version').AsString);
  r.Author := _(dsApp.FieldByName('Publisher').AsString);
  r.IconName := _(dsApp.FieldByName('IconName').AsString);
  r.Profile := _(dsApp.FieldByName('Profile').AsString);
  r.RemoveId := _(dsApp.FieldByName('AppId').AsString);
  r.Categories := _(dsApp.FieldByName('Category').AsString);

  r.InstallDate := dsApp.FieldByName('InstallDate').AsDateTime;
  r.Dependencies := PChar(dsApp.FieldByName('Dependencies').AsWideString);
  Result := r;
end;

procedure TListallerDB.OpenFilterAppList;
begin
  if not DBOkay then
    exit;
  ToApps;
  dsApp.Active := true;
  dsApp.Filtered := true;
  dsApp.First;
  EOF := dsApp.EOF;
  CurrField.App := GetAppField;
end;

procedure TListallerDB.NextField;
begin
  dsApp.Next;
  EOF := dsApp.EOF;
  CurrField.App := GetAppField;
end;

procedure TListallerDB.CloseFilter;
begin
  dsApp.Filtered := false;
  dsApp.Close;
end;

function TListallerDB.AppExists(appid: String): Boolean;
begin
  Result := false;
  if not DBOkay then
    exit;
  ToApps;
  dsApp.Filtered := true;
  dsApp.First;

  Result := false;
  while not dsApp.EOF do
  begin
    if (dsApp.FieldByName('AppId').AsString = appid) then
    begin
      Result := true;
      break;
    end
    else
      Result := false;
    dsApp.Next;
  end;
  dsApp.Close;
end;

procedure TListallerDB.AppDeleteCurrent;
begin
  ToApps;
  dsApp.ExecuteDirect('DELETE FROM applications WHERE rowid=' + IntToStr(dsApp.RecNo));
  dsApp.ApplyUpdates;
end;

procedure TListallerDB.AppAddNew(app: LiAppInfo);
var
  g, h: String;
begin
  //Open database connection
  ToApps;
  dsApp.Edit;

  if app.PkType = ptLinstall then
    h := 'linstall';
  if app.PkType = ptDLink then
    h := 'dlink';
  if app.PkType = ptContainer then
    h := 'container';

  g := app.Categories;

  dsApp.Insert;
  dsApp.ExecuteDirect('INSERT INTO "applications" VALUES (''' +
    app.Name + ''', ''' + app.PkName + ''', ''' + app.RemoveId +
    ''', ''' + h + ''', ''' + app.Summary + ''',''' + app.Version +
    ''',''' + app.Author + ''',''' + 'icon' + ExtractFileExt(app.IconName) +
    ''',''' + app.Profile + ''',''' + g + ''',''' + GetDateAsString +
    ''', ''' + app.Dependencies + ''');');

  //Write changes
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

procedure TListallerDB.AppUpdateVersion(appID: String; newv: String);
begin
  ToApps;
  dsApp.Filtered := true;
  dsApp.Edit;
  dsApp.First;
  dsApp.ExecuteDirect('UPDATE applications SET Version = ''' + newv +
    ''' WHERE AppId = ''' + appID + '''');
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

procedure TListallerDB.DepAddNew(Name: String; version: String;
  origin: String; depnames: Widestring);
begin
  //Open database connection
  ToDeps;
  dsApp.Edit;
  dsApp.Insert;
  dsApp.ExecuteDirect('INSERT INTO dependencies VALUES (''' + Name +
    ''', ''' + Version + ''', ''' + origin + ''', ''' + depnames +
    ''', ''' + GetDateAsString + ''');');

  //Write changes
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

function TListallerDB.DepExists(Name, version: String): Boolean;
begin
  Result := false;
  if not DBOkay then
    exit;
  ToDeps;
  dsApp.Filtered := true;
  dsApp.First;

  Result := false;
  while not dsApp.EOF do
  begin
    if (dsApp.FieldByName('Name').AsString = Name) and
      (dsApp.FieldByName('Version').AsString = Version) then
    begin
      Result := true;
      break;
    end
    else
      Result := false;
    dsApp.Next;
  end;
  dsApp.Close;
end;

function TListallerDB.Finalize: Boolean;
begin
  Result := false;
  if loaded then
  begin
    dsApp.Active := true;
    Result := dsApp.ApplyUpdates;
  end;
end;

end.

