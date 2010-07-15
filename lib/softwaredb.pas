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
  DB, GLib2, Classes, LiTypes, LiUtils, SysUtils, SQLite3DS, StrLocale;

type

  TAppField = record
    AppName: String;
    PkName: String;
    LiType: TPkgType;
    Desc: String;
    Version: String;
    Publisher: String;
    IconName: String;
    Profile: String;
    Group: TGroupType;
    InstallDate: TDateTime;
    Dependencies: WideString;
  end;

  TLiDBData = record
    App: TAppField;
  end;

  TSoftwareDB = class
  private
    dsApp: TSQLite3Dataset;
    FNewApp: TAppEvent;
    CurrField: TLiDBData;
    EOF: Boolean;
    function GetAppField: TAppField;
    function GetSQLiteVersion: String;
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
    function ContainsApp(appname, pkgname: String): Boolean;
    //** Delete current app
    procedure AppDeleteCurrent;
    //** Add a new application
    procedure AppAddNew(app: TAppField);
    //** Update version of app
    procedure AppUpdateVersion(pkgID: String;newv: String);

    property EndReached: Boolean read EOF;
    property DataField: TLiDBData read CurrField;
    property SQLiteVersion: String read GetSQLiteVersion;
    //** Event: Called if new application was found
    property OnNewApp: TAppEvent read FNewApp write FNewApp;
  end;

implementation

constructor TSoftwareDB.Create;
begin
  dsApp := TSQLite3Dataset.Create(nil);
end;

destructor TSoftwareDB.Destroy;
begin
  dsApp.Close; //Close, if opened
  dsApp.Free;
  inherited;
end;

function TSoftwareDB.GetSQLiteVersion: String;
begin
  Result := dsApp.SqliteVersion;
end;

function TSoftwareDB.Load(const forcesu: Boolean = false): Boolean;
var
  rd: String;
begin
  Result := true;
  dsApp.Close;
  if forcesu then
    rd := LI_CONFIG_DIR + LI_APPDB_PREF
  else
    rd := RegDir;

  if not DirectoryExists(ExtractFilePath(rd)) then
    CreateDir(ExtractFilePath(rd));
  if not DirectoryExists(rd) then
    CreateDir(rd);

  with dsApp do
  begin
    FileName := rd + 'software.db';
    if ((forcesu) and (not IsRoot)) then
      Result := false
    else
      if not FileExists(FileName) then
      begin
        //Create table for Applications
        TableName := 'Applications';
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
          Add('Group', ftString, 0, true);
          Add('InstallDate', ftDateTime, 0, false);
          Add('Dependencies', ftMemo, 0, false);
        end;
        CreateTable;
        //Create table for Dependencies
        TableName := 'Dependencies';
        with FieldDefs do
        begin
          Clear;
          Add('Name', ftString, 0, true);
          Add('InstallDate', ftDateTime, 0, false);
        end;
        CreateTable;
      end;
  end;
  p_info(rsDBOpened);
end;

function TSoftwareDB.GetApplicationList(blacklist: TStringList = nil): Boolean;
var
  entry: TAppInfo;
  p, n: AnsiString;
begin
  n := ConfigDir;

  dsApp.Close; //First close db
  dsApp.SQL := 'SELECT * FROM Applications';
  dsApp.Open;
  dsApp.Filtered := true;
  dsApp.First;

  while not dsApp.EOF do
  begin
    dsApp.FieldByName('Name');
    entry.Name := PChar(dsApp.FieldByName('Name').AsString);

    if Assigned(blacklist) then
      blacklist.Add(entry.Name);

    entry.UId := PChar(dsApp.FieldByName('PkName').AsString);

    entry.Version := PChar(rsVersion + ': ' + dsApp.FieldByName('Version').AsString);
    entry.Author := PChar(rsAuthor + ': ' + dsApp.FieldByName('Publisher').AsString);
    if dsApp.FieldByName('Publisher').AsString = '' then
      entry.Author := '';
    p := RegDir + LowerCase(entry.UId) + '/';

    n := LowerCase(dsApp.FieldByName('AGroup').AsString);

    if n = 'all' then
      entry.Group := gtALL;
    if n = 'education' then
      entry.Group := gtEDUCATION;
    if n = 'office' then
      entry.Group := gtOFFICE;
    if n = 'development' then
      entry.Group := gtDEVELOPMENT;
    if n = 'graphic' then
      entry.Group := gtGRAPHIC;
    if n = 'network' then
      entry.Group := gtNETWORK;
    if n = 'games' then
      entry.Group := gtGAMES;
    if n = 'system' then
      entry.Group := gtSYSTEM;
    if n = 'multimedia' then
      entry.Group := gtMULTIMEDIA;
    if n = 'additional' then
      entry.Group := gtADDITIONAL;
    if n = 'other' then
      entry.Group := gtOTHER;

    // InstLst.Add(LowerCase(dsApp.FieldByName('ID').AsString));

    entry.ShortDesc := PChar(dsApp.FieldByName('Description').AsString);
    if entry.ShortDesc = '#' then
      entry.ShortDesc := 'No description available';

    if FileExists(p + 'icon.png') then
      entry.Icon := PChar(p + 'icon.png');

    if Assigned(FNewApp) then
      FNewApp(entry.Name, @entry);

    dsApp.Next;
  end;
  dsApp.Close;
end;

function TSoftwareDB.GetAppField: TAppField;
var
  r: TAppField;
  h: String;
begin
  r.AppName := dsApp.FieldByName('Name').AsString;
  r.PkName := dsApp.FieldByName('PkName').AsString;
  h := LowerCase(dsApp.FieldByName('Type').AsString);
  if h = 'linstall' then
    r.LiType := ptLinstall
  else
    if h = 'dlink' then
      r.LiType := ptDLink
    else
      if h = 'container' then
        r.LiType := ptContainer;
  r.Desc := dsApp.FieldByName('Description').AsString;
  r.Version := dsApp.FieldByName('Version').AsString;
  r.Publisher := dsApp.FieldByName('Publisher').AsString;
  r.IconName := dsApp.FieldByName('IconName').AsString;
  r.Profile := dsApp.FieldByName('Profile').AsString;
  h := dsApp.FieldByName('Group').AsString;

  if h = 'all' then
    r.Group := gtALL;
  if h = 'education' then
    r.Group := gtEDUCATION;
  if h = 'office' then
    r.Group := gtOFFICE;
  if h = 'development' then
    r.Group := gtDEVELOPMENT;
  if h = 'graphic' then
    r.Group := gtGRAPHIC;
  if h = 'network' then
    r.Group := gtNETWORK;
  if h = 'games' then
    r.Group := gtGAMES;
  if h = 'system' then
    r.Group := gtSYSTEM;
  if h = 'multimedia' then
    r.Group := gtMULTIMEDIA;
  if h = 'additional' then
    r.Group := gtADDITIONAL;
  if h = 'other' then
    r.Group := gtOTHER;

  r.InstallDate := dsApp.FieldByName('InstallDate').AsDateTime;
  r.Dependencies := dsApp.FieldByName('Dependencies').AsWideString;
  Result := r;
end;

procedure TSoftwareDB.OpenFilterAppList;
begin
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

function TSoftwareDB.ContainsApp(appname, pkgname: String): Boolean;
begin
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
  dsApp.ExecuteDirect('DELETE FROM Applications WHERE rowid=' + IntToStr(dsApp.RecNo));
  dsApp.ApplyUpdates;
end;

procedure TSoftwareDB.AppAddNew(app: TAppField);
var
  g, h: String;
begin
  //Open database connection
  dsApp.Open;
  dsApp.Edit;

  if app.LiType = ptLinstall then
    h := 'linstall';
  if app.LiType = ptDLink then
    h := 'dlink';
  if app.LiType = ptContainer then
    h := 'container';

  //Set group as string
  case app.Group of
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
    app.AppName + ''', ''' + app.PkName + ''', ''' + h + ''', ''' +
    app.Desc + ''',''' + app.Version + ''',''' + app.Publisher +
    ''',''' + 'icon' + ExtractFileExt(app.IconName) + ''',''' +
    app.Profile + ''',''' + g + ''',''' + GetDateAsString + ''', ''' +
    app.Dependencies + ''');');

  //Write changes
  dsApp.ApplyUpdates;
  dsApp.Close;
end;

procedure TSoftwareDB.AppUpdateVersion(pkgID: String; newv: String);
begin
  dsApp.SQL := 'SELECT * FROM Applications';
    dsApp.Open;
    dsApp.Filtered := true;
    dsApp.Edit;
    dsApp.First;
    dsApp.ExecuteDirect('UPDATE AppInfo SET Version = ''' + newv + ''' WHERE PkName = ''' + pkgID + '''');

   { while not dsApp.EOF do
    begin
     if (dsApp.FieldByName('Name').AsString=TUpdateInfo(ulist[uid]).AppName) and (dsApp.FieldByName('PkName').AsString=) then
     begin

      dsApp.FieldByName('Version').Value:=TUpdateInfo(ulist[uid]).NVersion;
      break;
     end;
    dsApp.Next;
    end;}
    dsApp.ApplyUpdates;
    dsApp.Close;
end;

end.

