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
//** Provide intelligent access to all application databases
unit softwaredb;

{$mode objfpc}

interface

uses
  DB, Classes, LiTypes, LiUtils, SysUtils, SQLite3DS, StrLocale,
  AppInstallDB, ListallerDB;

type

  TSoftwareDB = class
  private
    aiDB: TAppInstallDB;
    liDB: TListallerDB;
    FNewApp: NewAppEvent;

    function GetSQLiteVersion: string;
    procedure SetNewAppEvent(event: NewAppEvent);
    function GetAppConfDir: String;
    function GetDepConfDir: String;
  public
    constructor Create;
    destructor Destroy; override;

    //** Open software databases
    function Load(const rootmode: boolean = False): Boolean;
    //** Make sure everything is written to disk
    procedure Finalize;
    //** Get list of applications
    function GetApplicationList(filter: LiAppFilter;
      blacklist: TStringList = nil): boolean;
    //** Check if application is installed
    function AppExists(appid: string): boolean;
    //** Register a new application
    function AppAddNew(app: LiAppInfo): boolean;
    //** Add new dependency to databse
    procedure DepAddNew(Name: string; version: string; origin: string;
      depnames: WideString);
    //** Check if dependency is already present
    function DepExists(Name, version: string): boolean;
    //** Register call to on new app found
    procedure RegOnNewApp(call: NewAppEvent; user_data: Pointer);

    //** SQLite3 version used
    property SQLiteVersion: string read GetSQLiteVersion;
    //** Event: Called if new application was found
    property OnNewApp: NewAppEvent read FNewApp write SetNewAppEvent;
    property AppConfDir: string read GetAppConfDir;
    property DepConfDir: string read GetDepConfDir;
  end;

implementation

constructor TSoftwareDB.Create;
begin
  aiDB := TAppInstallDB.Create;
  liDB := TListallerDB.Create;
end;

destructor TSoftwareDB.Destroy;
begin
  aiDB.Free;
  liDB.Free;
  inherited;
end;

procedure TSoftwareDB.Finalize;
begin
  aiDB.Finalize;
  liDB.Finalize;
end;

function TSoftwareDB.GetSQLiteVersion: string;
begin
  Result := liDB.SQLiteVersion;
end;

procedure TSoftwareDB.SetNewAppEvent(event: NewAppEvent);
begin
  aiDB.OnNewApp := event;
  liDB.OnNewApp := event;
end;

procedure TSoftwareDB.RegOnNewApp(call: NewAppEvent; user_data: Pointer);
begin
  if Assigned(call) then
  begin
    liDB.RegOnNewApp(call, user_data);
    aiDB.RegOnNewApp(call, user_data);
  end
  else
    perror('Invalid NewAppEvent pointer received!');
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

function TSoftwareDB.Load(const rootmode: boolean = False): boolean;
begin
  Result := aiDB.Load(rootmode);
  if Result then
    Result := liDB.Load(rootmode);
end;

function TSoftwareDB.GetApplicationList(filter: LiAppFilter;
  blacklist: TStringList = nil): boolean;
begin
  if (filter <> fIPK) then
    Result := aiDB.GetApplicationList(filter, blacklist);
  if ((filter = fAll) or (filter = fIPK)) and (Result) and (filter <> fNative) then
    Result := liDB.GetApplicationList(blacklist);
end;

function TSoftwareDB.AppExists(appid: string): boolean;
begin
  Result := False;
  Result := liDB.AppExists(appid);
  if not Result then
    Result := aiDB.ContainsAppEntry(appid);
end;

function TSoftwareDB.AppAddNew(app: LiAppInfo): boolean;
var
  ty: LiPkgType;
begin
  Result := True;
  ty := app.PkType;
  if (ty = ptNative) or (ty = ptExtern) then
    aiDB.AddApplication(app)
  else if (ty <> ptUnknown) then
    liDB.AppAddNew(app)
  else
    Result := False;
end;

procedure TSoftwareDB.DepAddNew(Name: string; version: string;
  origin: string; depnames: WideString);
begin
  // Just forward it to Listaller DB
  liDB.DepAddNew(Name, version, origin, depnames);
end;

function TSoftwareDB.DepExists(Name, version: string): boolean;
begin
  Result := liDB.DepExists(Name, version);
end;

end.

