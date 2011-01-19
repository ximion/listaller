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
//** Provide intelligent access to all application databases
unit softwaredb;

{$mode objfpc}{$H+}

interface

uses
  DB, Classes, LiTypes, LiUtils, SysUtils, StrLocale,
  AppInstallDB, ListallerDB, LiApp;

type
  TSoftwareDB = class
  private
    aiDB: TAppInstallDB;
    liDB: TListallerDB;
    FNewApp: LiAppEvent;
    curFilter: LiFilter;

    function GetSQLiteVersion: String;
    procedure SetNewAppEvent(event: LiAppEvent);
    function GetAppConfDir: String;
    function GetDepConfDir: String;
    function IsEOF: Boolean;
    function GetCurrentData: TLiDBData;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open software databases
    function Load(const rootmode: Boolean = false): Boolean;
    //** Make sure everything is written to disk
    procedure Finalize;
    //** Get list of applications
    function GetApplicationList(filter: LiFilter; filter_text: String;
      blacklist: TStringList = nil): Boolean;
    //** Check if application is installed
    function AppExists(appid: String): Boolean;
    //** Register a new application
    function AppAddNew(app: TLiAppItem): Boolean;
    //** Add new dependency to databse
    procedure DepAddNew(Name: String; version: String; origin: String;
      depnames: Widestring);
    //** Check if dependency is already present
    function DepExists(Name, version: String): Boolean;
    //** Register call to on new app found
    procedure RegOnNewApp(call: LiAppEvent; user_data: Pointer);
    //** Update version of app
    procedure AppUpdateVersion(appID: String; newv: String);
    ////
    //** Open filtered applications list (and set pointer to beginning)
    procedure OpenFilter(filter: LiFilter);
    //** Move one entry forward
    procedure NextField;
    //** Close (filter) connection
    procedure CloseFilter;
    //** Delete current field
    procedure DeleteCurrentField;

    property EndReached: Boolean read IsEOF;
    property CurrentDataField: TLiDBData read GetCurrentData;
    //** SQLite3 version used
    property SQLiteVersion: String read GetSQLiteVersion;
    //** Event: Called if new application was found
    property OnNewApp: LiAppEvent read FNewApp write SetNewAppEvent;
    property AppConfDir: String read GetAppConfDir;
    property DepConfDir: String read GetDepConfDir;
  end;

implementation

constructor TSoftwareDB.Create;
begin
  aiDB := TAppInstallDB.Create;
  liDB := TListallerDB.Create;
  curFilter := fAllApps;
end;

destructor TSoftwareDB.Destroy;
begin
  Finalize;
  aiDB.Free;
  liDB.Free;
  inherited;
end;

procedure TSoftwareDB.Finalize;
begin
  aiDB.Finalize;
  liDB.Finalize;
end;

function TSoftwareDB.GetSQLiteVersion: String;
begin
  Result := liDB.SQLiteVersion;
end;

procedure TSoftwareDB.SetNewAppEvent(event: LiAppEvent);
begin
  aiDB.OnNewApp := event;
  liDB.OnNewApp := event;
end;

function TSoftwareDB.IsEOF: Boolean;
begin
  Result := false;
  if curFilter <> fDeps then
  begin
    if aiDB.EndReached then
      liDB.OpenFilterAppList;
    if aiDB.EndReached and liDB.EndReached then
      Result := true;
  end
  else
    Result := liDB.EndReached;
end;

procedure TSoftwareDB.OpenFilter(filter: LiFilter);
begin
  curFilter := filter;
  if filter = fDeps then
  begin
    //TODO
  end
  else
  begin
    liDB.OpenFilterAppList;
    aiDB.OpenFilter;
  end;
end;

procedure TSoftwareDB.NextField;
begin
  if not aiDB.EndReached then
    aiDB.DeleteCurrentApp
  else if not liDB.EndReached then
    liDB.AppDeleteCurrent;
end;

procedure TSoftwareDB.DeleteCurrentField;
begin
  liDB.NextField;
  if curFilter <> fDeps then
    aiDB.NextField;
end;

procedure TSoftwareDB.CloseFilter;
begin
  liDB.CloseFilter;
  if curFilter <> fDeps then
    aiDB.CloseFilter;
end;

function TSoftwareDB.GetCurrentData: TLiDBData;
begin
  if not liDB.EndReached then
    Result := liDB.DataField
  else if not aiDB.EndReached then
    Result := aiDB.CurrentDataField
  else
    Result.App := nil;
end;

procedure TSoftwareDB.RegOnNewApp(call: LiAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    liDB.RegOnNewApp(call, user_data);
    aiDB.RegOnNewApp(call, user_data);
  end
  else
    perror('Invalid AppEvent pointer received!');
end;

function TSoftwareDB.GetAppConfDir: String;
begin
  // Return the current application data dir
  Result := liDB.AppConfDir;
end;

function TSoftwareDB.GetDepConfDir: String;
begin
  // Return the current dependeny data dir
  Result := liDB.DepConfDir;
end;

function TSoftwareDB.Load(const rootmode: Boolean = false): Boolean;
begin
  Result := aiDB.Load(rootmode);
  if Result then
    Result := liDB.Load(rootmode);
end;

function TSoftwareDB.GetApplicationList(filter: LiFilter; filter_text: String;
  blacklist: TStringList = nil): Boolean;
begin
  pdebug('Database filter_text is: '+filter_text);
  if (filter <> fAppIPK) then
    Result := aiDB.GetApplicationList(filter, filter_text, blacklist);
  if ((filter = fAllApps) or (filter = fAppIPK)) and (Result) and
    (filter <> fAppNative) then
    Result := liDB.GetApplicationList(filter_text, blacklist);
end;

function TSoftwareDB.AppExists(appid: String): Boolean;
begin
  Result := false;
  Result := liDB.AppExists(appid);
  if not Result then
    Result := aiDB.ContainsAppEntry(appid);
end;

function TSoftwareDB.AppAddNew(app: TLiAppItem): Boolean;
var
  ty: LiPkgType;
begin
  Result := true;
  ty := app.PkType;
  if (ty = ptNative) or (ty = ptExtern) then
    aiDB.AddApplication(app)
  else if (ty <> ptUnknown) then
    liDB.AppAddNew(app)
  else
    Result := false;
end;

procedure TSoftwareDB.DepAddNew(Name: String; version: String;
  origin: String; depnames: Widestring);
begin
  // Just forward it to Listaller DB
  liDB.DepAddNew(Name, version, origin, depnames);
end;

function TSoftwareDB.DepExists(Name, version: String): Boolean;
begin
  Result := liDB.DepExists(Name, version);
end;

procedure TSoftwareDB.AppUpdateVersion(appID: String; newv: String);
begin
  liDB.AppUpdateVersion(appID, newv);
end;

end.

