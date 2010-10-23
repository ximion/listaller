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
//** PackageKit appremove backend
unit backend_packagekit;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LiBackend, LiTypes, LiUtils, IniFiles, StrLocale, Process,
  PackageKit, PkTypes;

type
  TPackageKitBackend = class(TLiBackend)
  private
    dskFileName: String;
    pkg: String;
    pkit: TPackageKit;
    appInfo: LiAppInfo;
    function EmitRequest(s: String; ty: LiRqType): LiRqResult;
    //** Receive the PackageKit progress
    procedure PkitProgress(pos: Integer; xd: Pointer);
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(ai: LiAppInfo): Boolean; override;
    function CanBeUsed: Boolean; override;
    function Run: Boolean; override;
  end;

implementation

{ TPackageKitBackend }

constructor TPackageKitBackend.Create;
begin
  inherited;
  pkit := TPackageKit.Create;
  pkit.OnProgress := @PkitProgress;
end;

destructor TPackageKitBackend.Destroy;
begin
  pkit.Free;
  inherited;
end;

function TPackageKitBackend.Initialize(ai: LiAppInfo): Boolean;
begin
  dskFileName := GetDesktopFile(ai.removeID);
  appInfo := ai;
  Result := true;
end;

function TPackageKitBackend.EmitRequest(s: String; ty: LiRqType): LiRqResult;
begin
  //DUMMY
  pwarning('Request/Error handling not implemented in backend "PackageKit" (on TODO list :P)');
  pwarning(' Message was: ' + s);
end;

procedure TPackageKitBackend.PkitProgress(pos: Integer; xd: Pointer);
begin
  //User defindes pointer xd is always nil here
  EmitProgress(pos);
end;

function TPackageKitBackend.CanBeUsed: Boolean;
var
  tmp: TStringList;
  f, g: Widestring;
  i: Integer;
begin
  Result := true;
  EmitMessage(rsCallingPackageKitPKMonExecActions);
  EmitMessage(rsDetectingPackage);

  pkit.PkgNameFromFile(dskFileName, true);
  EmitProgress(20);

  // Now wait...
  while not pkit.Finished do ;

  if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
  begin
    EmitRequest(PAnsiChar(rsPKitProbPkMon + #10 + rsEMsg + #10 +
      pkit.LastErrorMessage),
      rqError);
    Result := false;
    exit;
  end;

  tmp := TStringList.Create;
  for i := 0 to pkit.RList.Count - 1 do
    tmp.Add(pkit.RList[i].PackageId);

  if (tmp.Count > 0) then
  begin
    f := tmp[0];

    EmitMessage(StrSubst(rsPackageDetected, '%s', f));
    EmitMessage(rsLookingForRevDeps);

    tmp.Clear;

    EmitProgress(18);
    pdebug('GetRequires()');
    pkit.GetRequires(f);

    EmitProgress(25);
    g := '';

    for i := 0 to tmp.Count - 1 do
    begin
      pdebug(tmp[i]);
      g := g + #10 + tmp[i];
    end;

    pdebug('Asking dependency question...');
    pkit.Free;
    if (StringReplace(g, ' ', '', [rfReplaceAll]) = '') or
      (EmitRequest(StringReplace(StringReplace(
      StringReplace(rsRMPkg, '%p', f, [rfReplaceAll]), '%a', appInfo.Name,
      [rfReplaceAll]), '%pl', PChar(g), [rfReplaceAll]), rqWarning) = rqsYes) then
      Result := true
    else
      Result := false;
  end;
  tmp.Free;
end;

function TPackageKitBackend.Run: Boolean;
begin
  EmitProgress(50);
  EmitMessage(StrSubst(rsRMAppC, '%a', appInfo.Name) + ' ...');
  pkit.RemovePkg(pkg);

  if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
  begin
    EmitRequest(rsRmError + #10 + rsEMsg + #10 + pkit.LastErrorMessage, rqError);
    Result := false;
    exit;
  end;

  EmitProgress(100);
  EmitMessage(rsDone);
end;

end.

