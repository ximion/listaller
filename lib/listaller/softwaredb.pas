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
unit softwaredb;

{$mode objfpc}

interface

uses
  DB, Classes, LiTypes, LiUtils, SysUtils, SQLite3DS, StrLocale;

type

  TLiDBData = record
    App: LiAppInfo;
  end;

  TSoftwareDB = class
  private
    dsApp: TSQLite3Dataset;
    FNewApp: NewAppEvent;
    CurrField: TLiDBData;
    AppDataDir: String;
    DepDataDir: String;
    EOF: Boolean;
    onnewapp_udata: Pointer;

    function GetAppField: LiAppInfo;
    function GetSQLiteVersion: String;
    function DBOkay: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open software database
    function Load(const forcesu: Boolean = false): Boolean;
    //** Get list of all installed apps
    function GetApplicationList(blacklist: TStringList = nil): Boolean;
    //** Open filtered applications list (and set pointer to beginning)
    procedure OpenFilterAppList;
    //** Move one entry forward
    procedure NextField;
    //** Close (filter) connection
    procedure Close;
    //** Check if application is installed
    function AppExisting(appname, pkgname: String): Boolean;
    //** Delete current app
    procedure AppDeleteCurrent;
    //** Add a new application
    procedure AppAddNew(app: LiAppInfo);
    //** Update version of app
    procedure AppUpdateVersion(pkgID: String; newv: String);
    //** Add new dependency to databse
    procedure DepAddNew(Name: String; version: String; origin: String;
      depnames: WideString);
    //** Check if dependency is already present
    function DepExisting(Name, version: String): Boolean;
    //** Register call to on new app found
    procedure RegOnNewApp(call: NewAppEvent;user_data: Pointer);


    property EndReached: Boolean read EOF;
    property DataField: TLiDBData read CurrField;
    property SQLiteVersion: String read GetSQLiteVersion;
    property AppConfDir: String read AppDataDir;
    property DepConfDir: String read DepDataDir;
    //** Event: Called if new application was found
    property OnNewApp: NewAppEvent read FNewApp write FNewApp;
  end;

implementation

constructor TSoftwareDB.Create;
begin
  dsApp := TSQLite3Dataset.Create(nil);
  onnewapp_udata := nil;
end;

destructor TSoftwareDB.Destroy;
begin
  dsApp.Close; //Close, if opened
  dsApp.Active := false;
  dsApp.Free;
  inherited;
end;

function TSoftwareDB.GetSQLiteVersion: String;
begin
  Result := dsApp.SqliteVersion;
end;

procedure TSoftwareDB.RegOnNewApp(call: NewAppEvent;user_data: Pointer);
begin
  if Assigned(call) then
  begin
   onnewapp_udata := user_data;
   FNewApp := call;
  end else
   perror('Invalid NewAppEvent pointer received!');
end;

function TSoftwareDB.DBOkay: Boolean;
begin
  Result := false;
  if FileExists(dsApp.FileName) then
    if dsApp.TableExists('Applications') then
      if dsApp.TableExists('Dependencies') then
        Result := true;
end;

function TSoftwareDB.Load(const forcesu: Boolean = false): Boolean;
var
  rd: String;
begin
  Result := true;

  if forcesu then
    rd := LI_CONFIG_DIR + LI_APPDB_PREF
  else
    rd := RegDir;

  AppDataDir := CleanFilePath(rd + '/apps/');
  DepDataDir := CleanFilePath(rd + '/deps/');
  ForceDirectories(rd);
  ForceDirectories(AppDataDir);
  ForceDirectories(DepDataDir);

  with dsApp do
  begin
    FileName := rd + 'software.db';
    if ((forcesu) and (not IsRoot) and (not FileExists(FileName))) then
      Result := false
    else
    begin
      //Create table for Applications
      TableName := 'Applications';

      if not TableExists then
      begin
        with FieldDefs do
        begin
          Clear;
          Add('Name', ftString, 0, true);
          Add('PkName', ftString, 0, true);
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
      TableName := 'Dependencies';
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
  pinfo(rsDBOpened);
end;

function TSoftwareDB.GetApplicationList(blacklist: TStringList = nil): Boolean;
var
  entry: LiAppInfo;
  p: AnsiString;
begin
  Result := false;
  dsApp.TableName := 'Applications';
  if not DBOkay then
    exit;

  dsApp.Close; //First close db
  dsApp.SQL := 'SELECT * FROM Applications';
  dsApp.Open;
  dsApp.Filtered := true;
  dsApp.First;

  while not dsApp.EOF do
  begin
    entry := GetAppField;
    entry.UId := entry.PkName;

    if Assigned(blacklist) then
      blacklist.Add(entry.Name);

    entry.Version := PChar(rsVersion + ': ' + entry.Version);
    if entry.Author <> '' then
      entry.Author := PChar(rsAuthor + ': ' + entry.Author);

    p := AppDataDir + LowerCase(entry.UId) + '/';

    //InstLst.Add(LowerCase(dsApp.FieldByName('ID').AsString));

    if entry.ShortDesc = '' then
      entry.ShortDesc := 'No description available';

    if FileExists(p + 'icon.png') then
      entry.IconName := PChar(p + 'icon.png');

    if Assigned(FNewApp) then
      FNewApp(entry.Name, @entry, nil);

    dsApp.Next;
  end;
  dsApp.Close;
end;

function TSoftwareDB.GetAppField: LiAppInfo;
var
  r: LiAppInfo;
  h: String;

  function _(s: WideString): PChar;
  begin
    Result := PChar(s);
  end;

begin
  dsApp.TableName := 'Applications';
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

  r.ShortDesc := _(dsApp.FieldByName('Description').AsString);
  r.Version := _(dsApp.FieldByName('Version').AsString);
  r.Author := _(dsApp.FieldByName('Publisher').AsString);
  r.IconName := _(dsApp.FieldByName('IconName').AsString);
  r.Profile := _(dsApp.FieldByName('Profile').AsString);
  h := dsApp.FieldByName('Category').AsString;

  if h = 'all' then
    r.Category := gtALL;
  if h = 'education' then
    r.Category := gtEDUCATION;
  if h = 'office' then
    r.Category := gtOFFICE;
  if h = 'development' then
    r.Category := gtDEVELOPMENT;
  if h = 'graphic' then
    r.Category := gtGRAPHIC;
  if h = 'network' then
    r.Category := gtNETWORK;
  if h = 'games' then
    r.Category := gtGAMES;
  if h = 'system' then
    r.Category := gtSYSTEM;
  if h = 'multimedia' then
    r.Category := gtMULTIMEDIA;
  if h = 'additional' then
    r.Category := gtADDITIONAL;
  if h = 'other' then
    r.Category := gtOTHER;

  r.InstallDate := dsApp.FieldByName('InstallDate').AsDateTime;
  r.Dependencies := PChar(dsApp.FieldByName('Dependencies').AsWideString);
  Result := r;
end;

procedure TSoftwareDB.OpenFilterAppList;
begin
  if not DBOkay then
    exit;
  dsApp.TableName := 'Applications';
  dsApp.Active := true;

  dsApp.SQL := 'SELECT * FROM Applications';
  dsApp.Open;
  dsApp.Filtered := true;
  dsApp.First;
  EOF := dsApp.EOF;
  CurrField.App := GetAppField;
end;

procedure TSoftwareDB.NextField;
begin
  dsApp.Next;
  EOF := dsApp.EOF;
  CurrField.App := GetAppField;
end;

procedure TSoftwareDB.Close;
begin
  dsApp.Close;
end;

function TSoftwareDB.AppExisting(appname, pkgname: String): Boolean;
begin
  Result := false;
  if not DBOkay then
    exit;
  dsApp.TableName := 'Applications';
  dsApp.SQL := 'SELECT * FROM Applications';
  dsApp.Open;
  dsApp.Filtered := true;
  dsApp.First;

  Result := false;
  while not dsApp.EOF do
  begin
    if (dsApp.FieldByName('Name').AsString = appName) then
      // and (dsApp.FieldByName('ID').AsString=aID) then
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

procedure TSoftwareDB.AppDeleteCurrent;
begin
  dsApp.TableName := 'Applications';
  dsApp.ExecuteDirect('DELETE FROM Applications WHERE rowid=' + IntToStr(dsApp.RecNo));
  dsApp.ApplyUpdates;
end;

procedure TSoftwareDB.AppAddNew(app: LiAppInfo);
var
  g, h: String;
begin
  //Open database connection
  dsApp.TableName := 'Applications';
  dsApp.Open;
  dsApp.Edit;

  if app.PkType = ptLinstall then
    h := 'linstall';
  if app.PkType = ptDLink then
    h := 'dlink';
  if app.PkType = ptContainer then
    h := 'container';

  //Set Category as string
  case app.Category of
    gtALL: g := 'All';
    gtEDUCATION: g := 'Education';
    gtOFFICE: g := 'Office';
    gtDEVELOPMENT: g := 'Development';
    gtGRAPHIC: g := 'Graphic';
    gtNETWORK: g := 'Network';
    gtGAMES: g := 'Games';
    gtSYSTEM: g := 'System';
    gtMULTIMEDIA: g := 'Multimedia';
    gtADDITIONAL: g := 'Additional';
    gtOTHER: g := 'Other';
  end;

  dsApp.Insert;
  dsApp.ExecuteDirect('INSERT INTO "Applications" VALUES (''' +
    app.Name + ''', ''' + app.PkName + ''', ''' + h + ''', ''' +
    app.ShortDesc + ''',''' + app.Version + ''',''' + app.Author +
    ''',''' + 'icon' + ExtractFileExt(app.IconName) + ''',''' +
    app.Profile + ''',''' + g + ''',''' + GetDateAsString + ''', ''' +
    app.Dependencies + ''');');

  //Write changes
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

procedure TSoftwareDB.AppUpdateVersion(pkgID: String; newv: String);
begin
  dsApp.TableName := 'Applications';
  dsApp.SQL := 'SELECT * FROM Applications';
  dsApp.Open;
  dsApp.Filtered := true;
  dsApp.Edit;
  dsApp.First;
  dsApp.ExecuteDirect('UPDATE AppInfo SET Version = ''' + newv +
    ''' WHERE PkName = ''' + pkgID + '''');
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

procedure TSoftwareDB.DepAddNew(Name: String; version: String;
  origin: String; depnames: WideString);
begin
  //Open database connection
  dsApp.TableName := 'Dependencies';
  dsApp.Open;
  dsApp.Edit;
  dsApp.Insert;
  dsApp.ExecuteDirect('INSERT INTO "Dependencies" VALUES (''' + Name +
    ''', ''' + Version + ''', ''' + origin + ''', ''' + depnames +
    ''', ''' + GetDateAsString + ''');');

  //Write changes
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

function TSoftwareDB.DepExisting(Name, version: String): Boolean;
begin
  Result := false;
  if not DBOkay then
    exit;
  dsApp.TableName := 'Dependencies';
  dsApp.SQL := 'SELECT * FROM Dependencies';
  dsApp.Open;
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

end.

