{ Copyright (C) 2008-2009 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this unit. If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains functions to use the installer part of libInstaller
unit installer;

{$MODE objfpc}{$H+}

interface

uses
Classes, SysUtils, liTypes;

type

TInstallPack = class
private
ins: Pointer;
ForcedActn: String;
procedure SetForced(s: String);
public
constructor Create;
destructor  Destroy;override;

procedure Initialize(pkname: String);
procedure SetMainChangeCall(call: TProgressCall);
procedure SetExtraChangeCall(call: TProgressCall);
procedure SetUserRequestCall(call: TRequestCall);
procedure SetMessageCall(call: TMessageCall);
procedure SetStepMessageCall(call: TMessageCall);
function  PkType: TPkgType;
procedure SetTestmode(b: Boolean);
function  GetDisallows: String;
function  GetSupDistris: String;
function  GetAppName: String;
function  GetAppVersion: String;
function  GetAppID: String;
procedure ReadLongDescription(lst: TStringList);
function  GetWizardImagePath: String;
procedure ReadLicense(lst: TStringList);
procedure ReadProfiles(lst:TStringList);
procedure ReadDeps(lst:TStringList);
function  GetAppIcon: String;
function  GetDesktopFiles: String;
function  ResolveDependencies: Boolean;
function  GetAppCMD: String;
function  GetFileList: String;
function  StartInstallation: Boolean;
procedure SetProfileID(i: Integer);
procedure SetRootMode(b: Boolean);
property Forced: String read ForcedActn write SetForced;
end;

function IsIPKAppInstalled(appname: String;appid: String): Boolean;

implementation

//Import library functions
function  li_setup_new: Pointer; cdecl;external libinst;
procedure li_setup_free(setup: Pointer);external libinst;
procedure li_setup_set_su_mode(setup: Pointer;b: Boolean);cdecl; external libinst;
function  li_setup_init(setup: Pointer;pkname: PChar): PChar;cdecl; external libinst;
function  li_setup_register_main_progress_call(setup: Pointer;call: TProgressCall): Boolean; cdecl; external libinst;
function  li_setup_register_extra_progress_call(setup: Pointer;call: TProgressCall): Boolean; cdecl; external libinst;
function  li_setup_pkgtype(setup: Pointer): TPkgType;cdecl; external libinst;
function  li_setup_disallows(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_supported_distributions(setup: Pointer): PChar; cdecl; external libinst;
function  li_setup_appname(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_appversion(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_pkgid(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_long_description(setup: Pointer; list: Pointer): Boolean; cdecl; external libinst;
function  li_setup_wizard_image_path(setup: Pointer): PChar; cdecl; external libinst;
function  li_setup_license(setup: Pointer; list: Pointer): Boolean; cdecl; external libinst;
function  li_setup_profiles_list(setup: Pointer; list: Pointer): Boolean; cdecl; external libinst;
function  li_setup_appicon(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_desktopfiles(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_resolve_dependencies(setup: Pointer; list: Pointer): Boolean; cdecl; external libinst;
function  li_setup_app_exec_command(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_profile_current_filelist(setup: Pointer): PChar;cdecl; external libinst;
function  li_setup_register_message_call(setup: Pointer;call: TMessageCall): Boolean; cdecl; external libinst name 'li_setup_register_message_call';
function  li_setup_register_step_message_call(setup: Pointer;call: TMessageCall): Boolean; cdecl; external libinst name 'li_setup_register_step_message_call';
function  li_setup_register_user_request_call(setup: Pointer;call: TRequestCall): Boolean; cdecl; external libinst name 'li_setup_register_user_request_call';
function  li_setup_start(setup: Pointer): Boolean;cdecl; external libinst name 'li_setup_start';
procedure li_setup_set_forced(setup: Pointer;str: PChar);cdecl; external libinst name 'li_setup_set_forced';
function  li_setup_dependencies(setup: Pointer; list: PStringList): Boolean; cdecl; external libinst name 'li_setup_dependencies';
function  li_setup_set_profileid(setup: Pointer;id: ShortInt): Boolean;cdecl;  external libinst name 'li_setup_set_profileid';
function  li_is_ipk_app_installed(appname: PChar;appid: PChar): Boolean;cdecl; external libinst name 'li_is_ipk_app_installed';
procedure li_testmode(st: Boolean);cdecl; external libinst name 'li_testmode';

{ TInstallPack }

constructor TInstallPack.Create;
begin
inherited Create;
ins := li_setup_new;
end;

destructor TInstallPack.Destroy;
begin
li_setup_free(@ins);
inherited Destroy;
end;

procedure TInstallPack.Initialize(pkname: String);
begin
li_setup_init(@ins,PChar(pkname))
end;

procedure TInstallPack.SetMainChangeCall(call: TProgressCall);
begin
li_setup_register_main_progress_call(@ins,call)
end;

procedure TInstallPack.SetExtraChangeCall(call: TProgressCall);
begin
li_setup_register_extra_progress_call(@ins,call)
end;

function TInstallPack.PkType: TPkgType;
begin
Result:=li_setup_pkgtype(@ins);
end;

procedure TInstallPack.SetTestmode(b: Boolean);
begin
li_testmode(b);
end;

function TInstallPack.GetDisallows: String;
begin
Result:=li_setup_disallows(@ins);
end;

function TInstallPack.GetSupDistris: String;
begin
Result:=li_setup_supported_distributions(@ins);
end;

function TInstallPack.GetAppName: String;
begin
Result:=li_setup_appname(@ins);
end;

function TInstallPack.GetAppVersion: String;
begin
Result:=li_setup_appversion(@ins);
end;

function TInstallPack.GetAppID: String;
begin
Result:=li_setup_pkgid(@ins);
end;

procedure TInstallPack.ReadLongDescription(lst: TStringList);
begin
li_setup_long_description(@ins,@lst)
end;

function TInstallPack.GetWizardImagePath: String;
begin
Result:=li_setup_wizard_image_path(@ins);
end;

procedure TInstallPack.ReadLicense(lst: TStringList);
begin
li_setup_license(@ins,@lst)
end;

procedure TInstallPack.ReadProfiles(lst: TStringList);
begin
li_setup_profiles_list(@ins,@lst);
end;

function TInstallPack.GetAppIcon: String;
begin
Result:=li_setup_appicon(@ins);
end;

function TInstallPack.GetDesktopFiles: String;
begin
Result:=li_setup_desktopfiles(@ins);
end;

function TInstallPack.ResolveDependencies: Boolean;
var tmp: TStringList;
begin
tmp:=TstringList.Create;
ReadDeps(tmp);
Result:=li_setup_resolve_dependencies(@ins,@tmp);
tmp.Free;
end;

function TInstallPack.GetAppCMD: String;
begin
Result:=li_setup_app_exec_command(@ins);
end;

function TInstallPack.GetFileList: String;
begin
Result:=li_setup_profile_current_filelist(@ins);
end;

procedure TInstallPack.SetUserRequestCall(call: TRequestCall);
begin
li_setup_register_user_request_call(@ins,call)
end;

procedure TInstallPack.SetMessageCall(call: TMessageCall);
begin
li_setup_register_message_call(@ins,call)
end;

procedure TInstallPack.SetStepMessageCall(call: TMessageCall);
begin
li_setup_register_step_message_call(@ins,call)
end;

procedure TInstallPack.ReadDeps(lst: TStringList);
begin
li_setup_dependencies(@ins,@lst);
end;

function TInstallPack.StartInstallation: Boolean;
begin
Result:=li_setup_start(@ins);
end;

procedure TInstallPack.SetRootMode(b: boolean);
begin
li_setup_set_su_mode(@ins,b);
end;

procedure TInstallPack.SetProfileID(i: Integer);
begin
li_setup_set_profileid(@ins,i);
end;

procedure TInstallPack.SetForced(s: String);
begin
ForcedActn:=s;
li_setup_set_forced(@ins,PChar(s));
end;

function IsIPKAppInstalled(appname: String;appid: String): Boolean;
begin
Result:=li_is_ipk_app_installed(PChar(appname), PChar(appid));
end;

end.

