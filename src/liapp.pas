(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 *  Matthias Klumpp
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
// Defines a Listaller application
unit liapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LiUtils, LiTypes;

type
  PLiAppItem = ^TLiAppItem;
  TLiAppItem = class (TPersistent)
  private
  protected
    FName: String;
    FId: String;
    FPkType: LiPkgType;
    FSummary: String;
    FVersion: String;
    FAuthor: String;
    FPublisher: String;
    FIconName: String;
    FCategories: String;
    FInstallDate: TDateTime;
    FDependencies: String;
  public
    constructor Create;
    destructor Destroy; override;

    property AName: String read FName write FName;
    property AId: String read FId write FId;
    property PkType: LiPkgType read FPkType write FPkType;
    property Summary: String read FSummary write FSummary;
    property Version: String read FVersion write FVersion;
    property Author: String read FAuthor write FAuthor;
    property Publisher: String read FPublisher write FPublisher;
    property IconName: String read FIconName write FiconName;
    property Categories: String read FCategories write FCategories;
    property TimeStamp: TDateTime read FInstalldate write FInstallDate;
    property Dependencies: String read FDependencies write FDependencies;
  end;

//** Build a fake package name for an application
function GenerateFakePackageName(appName: String): String;
//** Build a string to identify the application which can be used in filepaths
function GetAppIDString(ai: TLiAppItem): String;
//** Restore desktop file path from appID
function GetDesktopFileFromID(appID: String): String;
//** Create an appID from .desktop-file path
function GenerateAppID(desktopFile: String): String;

implementation

{ LiAppItem }

constructor TLiAppItem.Create;
begin
  FVersion := '?';
  FAuthor := '~';
  FId := '';
end;

destructor TLiAppItem.Destroy;
begin
  inherited;
end;

{ Helper functions }

function GenerateAppID(desktopFile: String): String;
begin
  Result := '';
  if trim(desktopFile) = '' then
    exit;
  Result := ExtractFilePath(desktopFile);
  Result := StrSubst(StrSubst(Result, '/usr/', ''), 'share/applications/', '');
  Result := ExcludeTrailingPathDelimiter(Result) +
    StrSubst(ExtractFileName(desktopFile), '.desktop', '');
end;

function GetDesktopFileFromID(appID: String): String;
begin
  Result := '';
  if trim(appID) = '' then
    exit;
  Result := StrSubst(appID, 'local/', 'local/share/applications/');
  if pos('local/share/applications/', Result) <= 0 then
    Result := 'share/applications/' + appID;
  Result := '/usr/' + Result;
  Result := Result + '.desktop';
end;

function GenerateFakePackageName(appName: String): String;
var
  s: String;

  procedure subst(a, b: String);
  begin
    s := StrSubst(s, a, b);
  end;

begin
  s := LowerCase(appName);
  subst('/', '');
  subst(';', '');
  subst(' ', '-');
  Result := s;
end;

function GetAppIDString(ai: LiApp.TLiAppItem): String;
begin
  Result := StrSubst(ai.AId, '/', '');
end;

end.

