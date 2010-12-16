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

  TLiAppItem = class
  private
    function GetAId: String;
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
    FDesktop: String;
  public
    constructor Create;
    destructor Destroy; override;

    //** Build a fake package name this application
    function FakePackageName: String;
    //** Create an appID from .desktop-file path
    function GenerateAppID: String;
    //** Generate ID string
    function AppIDString: String;
    procedure Assign(ai: TLiAppItem);
  published
    property AName: String read FName write FName;
    property AId: String read GetAId write FId;
    property PkType: LiPkgType read FPkType write FPkType;
    property Summary: String read FSummary write FSummary;
    property Version: String read FVersion write FVersion;
    property Author: String read FAuthor write FAuthor;
    property Publisher: String read FPublisher write FPublisher;
    property IconName: String read FIconName write FiconName;
    property Categories: String read FCategories write FCategories;
    property TimeStamp: TDateTime read FInstalldate write FInstallDate;
    property Dependencies: String read FDependencies write FDependencies;
    property DesktopFile: String read FDesktop write FDesktop;
  end;

//** Build a string to identify the application which can be used in filepaths
function GetAppIDString(ai: String): String;
//** Restore desktop file path from appID
function GetDesktopFileFromID(appID: String): String;

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

// Helper functions

function TLiAppItem.FakePackageName: String;
var
  s: String;

  procedure subst(a, b: String);
  begin
    s := StrSubst(s, a, b);
  end;

begin
  s := LowerCase(FName);
  subst('/', '');
  subst(';', '');
  subst(' ', '-');
  Result := s;
end;

function TLiAppItem.GenerateAppID: String;
begin
  Result := '';
  if trim(FDesktop) = '' then
  begin
    pwarning('Application ' + FName + ' has no valid .desktop file!');
    Result := LowerCase(FName + '-' + FVersion);
    exit;
  end;
  Result := ExtractFilePath(FDesktop);
  Result := StrSubst(StrSubst(Result, '/usr/', ''), 'share/applications/', '');
  Result := ExcludeTrailingPathDelimiter(Result) +
    StrSubst(ExtractFileName(FDesktop), '.desktop', '');
end;

function TLiAppItem.GetAId: String;
begin
  if trim(FId) = '' then
    FId := GenerateAppId;
  Result := FId;
end;

function TLiAppItem.AppIDString: String;
begin
  Result := GetAppIDString(FId);
end;

procedure TLiAppItem.Assign(ai: TLiAppItem);
begin
  FName := ai.AName;
  FId := ai.AId;
  FPkType := ai.PkType;
  FSummary := ai.Summary;
  FVersion := ai.version;
  FAuthor := ai.Author;
  FPublisher := ai.Publisher;
  FIconName := ai.IconName;
  FCategories := ai.Categories;
  FInstallDate := ai.TimeStamp;
  FDependencies := ai.Dependencies;
  FDesktop := ai.DesktopFile;
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

function GetAppIDString(ai: String): String;
begin
  Result := StrSubst(ai, '/', '');
end;

end.

