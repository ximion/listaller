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
  Classes, SysUtils, SQLite3DS, LiUtils, LiTypes, DB, StrLocale;

type
  TLiDBData = record
    App: LiAppInfo;
  end;

  //** Access to Listaller's IPK app db
  TListallerDB = class
  private
    dsApp: TSQLite3Dataset;
    FNewApp: NewAppEvent;
    CurrField: TLiDBData;
    AppDataDir: string;
    DepDataDir: string;
    EOF: boolean;
    onnewapp_udata: Pointer;

    function GetAppField: LiAppInfo;
    function GetSQLiteVersion: string;
    function DBOkay: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open software database
    function Load(const rootmode: boolean = False): boolean;
    //** Get list of installed ipk apps
    function GetApplicationList(blacklist: TStringList = nil): boolean;
    //** Open filtered applications list (and set pointer to beginning)
    procedure OpenFilterAppList;
    //** Move one entry forward
    procedure NextField;
    //** Close (filter) connection
    procedure CloseFilter;
    //** Check if application is installed
    function AppExists(appid: string): boolean;
    //** Delete current app
    procedure AppDeleteCurrent;
    //** Add a new application
    procedure AppAddNew(app: LiAppInfo);
    //** Update version of app
    procedure AppUpdateVersion(appID: string; newv: string);
    //** Add new dependency to databse
    procedure DepAddNew(Name: string; version: string; origin: string;
      depnames: WideString);
    //** Check if dependency is already present
    function DepExists(Name, version: string): boolean;
    //** Register call to on new app found
    procedure RegOnNewApp(call: NewAppEvent; user_data: Pointer);
    //** Write changes to disk
    function Finalize: boolean;


    property EndReached: boolean read EOF;
    property DataField: TLiDBData read CurrField;
    property SQLiteVersion: string read GetSQLiteVersion;
    property AppConfDir: string read AppDataDir;
    property DepConfDir: string read DepDataDir;
    //** Event: Called if new application was found
    property OnNewApp: NewAppEvent read FNewApp write FNewApp;
  end;

implementation

constructor TListallerDB.Create;
begin
  dsApp := TSQLite3Dataset.Create(nil);
  onnewapp_udata := nil;
  EOF := true;
end;

destructor TListallerDB.Destroy;
begin
  dsApp.Close; //Close, if opened
  dsApp.Active := False;
  dsApp.Free;
  inherited;
end;

function TListallerDB.GetSQLiteVersion: string;
begin
  Result := dsApp.SqliteVersion;
end;

procedure TListallerDB.RegOnNewApp(call: NewAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    onnewapp_udata := user_data;
    FNewApp := call;
  end
  else
    perror('Invalid NewAppEvent pointer received!');
end;

function TListallerDB.DBOkay: boolean;
begin
  Result := False;
  if FileExists(dsApp.FileName) then
    if dsApp.TableExists('applications') then
      if dsApp.TableExists('dependencies') then
        Result := True;
end;

function TListallerDB.Load(const rootmode: boolean = False): boolean;
var
  rd: string;
begin
  Result := True;

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
    Result := False;
    exit;
  end;

  with dsApp do
  begin
    FileName := rd + 'software.db';
    if ((rootmode) and (not IsRoot) and (not FileExists(FileName))) then
      Result := False
    else
    begin
      //Create table for Applications
      TableName := 'applications';

      if not TableExists then
      begin
        with FieldDefs do
        begin
          Clear;
          Add('Name', ftString, 0, True);
          Add('PkName', ftString, 0, True);
          Add('AppId', ftString, 0, True);
          Add('Type', ftString, 0, True);
          Add('Description', ftString, 0, False);
          Add('Version', ftFloat, 0, True);
          Add('Publisher', ftString, 0, False);
          Add('IconName', ftString, 0, False);
          Add('Profile', ftString, 0, False);
          Add('Category', ftString, 0, True);
          Add('InstallDate', ftDateTime, 0, False);
          Add('Dependencies', ftMemo, 0, False);
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
          Add('Name', ftString, 0, True);
          Add('Version', ftString, 0, True);
          Add('Origin', ftString, 0, True);
          Add('DepNames', ftMemo, 0, True);
          Add('InstallDate', ftDateTime, 0, False);
        end;
        CreateTable;
      end;
      Active := True;
      //ApplyUpdates;
      Open;
    end;
  end;
  pinfo(rsDBOpened);
end;

function TListallerDB.GetApplicationList(blacklist: TStringList = nil): boolean;
var
  entry: LiAppInfo;
  p: ansistring;
begin
  Result := False;
  dsApp.TableName := 'applications';
  if not DBOkay then
    exit;

  dsApp.Close; //First close db
  dsApp.SQL := 'SELECT * FROM applications';
  dsApp.Open;
  dsApp.Filtered := True;
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
  h: string;

  function _(s: WideString): PChar;
  begin
    Result := PChar(s);
  end;

begin
  dsApp.TableName := 'applications';
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
  dsApp.TableName := 'applications';
  dsApp.Active := True;

  dsApp.SQL := 'SELECT * FROM Applications';
  dsApp.Open;
  dsApp.Filtered := True;
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

function TListallerDB.AppExists(appid: string): boolean;
begin
  Result := False;
  if not DBOkay then
    exit;
  dsApp.TableName := 'applications';
  dsApp.SQL := 'SELECT * FROM applications';
  dsApp.Open;
  dsApp.Filtered := True;
  dsApp.First;

  Result := False;
  while not dsApp.EOF do
  begin
    if (dsApp.FieldByName('AppId').AsString = appid) then
    begin
      Result := True;
      break;
    end
    else
      Result := False;
    dsApp.Next;
  end;
  dsApp.Close;
end;

procedure TListallerDB.AppDeleteCurrent;
begin
  dsApp.TableName := 'applications';
  dsApp.ExecuteDirect('DELETE FROM applications WHERE rowid=' + IntToStr(dsApp.RecNo));
  dsApp.ApplyUpdates;
end;

procedure TListallerDB.AppAddNew(app: LiAppInfo);
var
  g, h: string;
begin
  //Open database connection
  dsApp.TableName := 'applications';
  dsApp.Open;
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

procedure TListallerDB.AppUpdateVersion(appID: string; newv: string);
begin
  dsApp.TableName := 'applications';
  dsApp.SQL := 'SELECT * FROM applications';
  dsApp.Open;
  dsApp.Filtered := True;
  dsApp.Edit;
  dsApp.First;
  dsApp.ExecuteDirect('UPDATE applications SET Version = ''' + newv +
    ''' WHERE AppId = ''' + appID + '''');
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

procedure TListallerDB.DepAddNew(Name: string; version: string;
  origin: string; depnames: WideString);
begin
  //Open database connection
  dsApp.TableName := 'Dependencies';
  dsApp.Open;
  dsApp.Edit;
  dsApp.Insert;
  dsApp.ExecuteDirect('INSERT INTO dependencies VALUES (''' + Name +
    ''', ''' + Version + ''', ''' + origin + ''', ''' + depnames +
    ''', ''' + GetDateAsString + ''');');

  //Write changes
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

function TListallerDB.DepExists(Name, version: string): boolean;
begin
  Result := False;
  if not DBOkay then
    exit;
  dsApp.TableName := 'dependencies';
  dsApp.SQL := 'SELECT * FROM dependencies';
  dsApp.Open;
  dsApp.Filtered := True;
  dsApp.First;

  Result := False;
  while not dsApp.EOF do
  begin
    if (dsApp.FieldByName('Name').AsString = Name) and
      (dsApp.FieldByName('Version').AsString = Version) then
    begin
      Result := True;
      break;
    end
    else
      Result := False;
    dsApp.Next;
  end;
  dsApp.Close;
end;

function TListallerDB.Finalize: boolean;
begin
  Result := dsApp.ApplyUpdates;
end;

end.

