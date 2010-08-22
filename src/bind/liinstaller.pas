{ Copyright (C) 2008-2010 Matthias Klumpp

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
//** LibInstaller functions to perform software installations
unit liinstaller;

{$MODE objfpc}{$H+}

interface

uses
Classes, SysUtils, LiTypes;

type

LiSetup = Pointer;

TInstallPack = class
private
ins: Pointer;
ForcedActn: String;
procedure SetForced(s: String);
public
constructor Create;
destructor  Destroy;override;

procedure Initialize(pkname: String);
procedure SetStatusChangeCall(call: TLiStatusChangeCall;const userdata: Pointer=nil);
procedure SetUserRequestCall(call: TRequestCall;const userdata: Pointer=nil);
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
function  GetAppCMD: String;
function  GetFileList: String;
function  StartInstallation: Boolean;
function  GetSignatureState: TPkgSigState;
procedure EnableUSource(b: Boolean);
procedure SetProfileID(i: Integer);
procedure SetRootMode(b: Boolean);
property Forced: String read ForcedActn write SetForced;
property RemoteObject: Pointer read ins;
end;

function IsIPKAppInstalled(appname: String;appid: String;sumode: Boolean): Boolean;

//Import library functions
function  li_setup_new: Pointer; cdecl;external libinst;
procedure li_setup_free(setup: Pointer);external libinst;
procedure li_setup_set_sumode(setup: Pointer;b: Boolean);cdecl;external libinst;
function  li_setup_init(setup: Pointer;pkname: PChar): PChar;cdecl;external libinst;
function  li_setup_get_pkgtype(setup: Pointer): TPkgType;cdecl;external libinst;
function  li_setup_get_disallows(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_supported_distributions(setup: Pointer): PChar; cdecl;external libinst;
function  li_setup_get_appname(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_appversion(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_pkgid(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_long_description(setup: Pointer; list: Pointer): Boolean;cdecl;external libinst;
function  li_setup_get_wizard_image_path(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_license(setup: Pointer; list: Pointer): Boolean;cdecl;external libinst;
function  li_setup_get_profiles_list(setup: Pointer; list: Pointer): Boolean;cdecl;external libinst;
function  li_setup_get_appicon(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_desktopfiles(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_app_exec_command(setup: Pointer): PChar;cdecl;external libinst;
function  li_setup_get_current_profile_filelist(setup: Pointer): PChar;cdecl;external libinst;
procedure li_setup_enable_usource_registering(setup: Pointer;b: Boolean);cdecl;external libinst;
function  li_setup_register_status_call(setup: Pointer;call: TLiStatusChangeCall;user_data: Pointer): Boolean;cdecl;external libinst;
function  li_setup_register_user_request_call(setup: Pointer;call: TRequestCall;user_data: Pointer): Boolean;cdecl;external libinst;
function  li_setup_execute(setup: Pointer): Boolean;cdecl;external libinst;
procedure li_setup_set_forced(setup: Pointer;str: PChar);cdecl;external libinst;
function  li_setup_get_dependencies(setup: Pointer; list: PStringList): Boolean;cdecl;external libinst;
function  li_setup_set_profileid(setup: Pointer;id: Smallint): Boolean;cdecl;external libinst;
function  li_setup_get_signature_state(setup: Pointer): TPkgSigState;cdecl;external libinst;
function  li_get_ipk_app_installed(appname: PChar;appid: PChar;sumode: Boolean): Boolean;cdecl;external libinst;
procedure li_set_testmode(st: Boolean);cdecl;external libinst;
procedure li_setup_exec_by_daemon(setup: Pointer;b: Boolean);cdecl;external libinst;

implementation

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

procedure TInstallPack.SetStatusChangeCall(call: TLiStatusChangeCall;const userdata: Pointer=nil);
begin
li_setup_register_status_call(@ins,call,userdata)
end;

function TInstallPack.PkType: TPkgType;
begin
Result:=li_setup_get_pkgtype(@ins);
end;

procedure TInstallPack.SetTestmode(b: Boolean);
begin
li_set_testmode(b);
end;

function TInstallPack.GetDisallows: String;
begin
Result:=li_setup_get_disallows(@ins);
end;

function TInstallPack.GetSupDistris: String;
begin
Result:=li_setup_get_supported_distributions(@ins);
end;

function TInstallPack.GetAppName: String;
begin
Result:=li_setup_get_appname(@ins);
end;

function TInstallPack.GetAppVersion: String;
begin
Result:=li_setup_get_appversion(@ins);
end;

function TInstallPack.GetAppID: String;
begin
Result:=li_setup_get_pkgid(@ins);
end;

procedure TInstallPack.ReadLongDescription(lst: TStringList);
begin
li_setup_get_long_description(@ins,@lst)
end;

function TInstallPack.GetWizardImagePath: String;
begin
Result:=li_setup_get_wizard_image_path(@ins);
end;

procedure TInstallPack.ReadLicense(lst: TStringList);
begin
li_setup_get_license(@ins,@lst)
end;

procedure TInstallPack.ReadProfiles(lst: TStringList);
begin
li_setup_get_profiles_list(@ins,@lst);
end;

function TInstallPack.GetAppIcon: String;
begin
Result:=li_setup_get_appicon(@ins);
end;

function TInstallPack.GetDesktopFiles: String;
begin
Result:=li_setup_get_desktopfiles(@ins);
end;

function TInstallPack.GetAppCMD: String;
begin
Result:=li_setup_get_app_exec_command(@ins);
end;

function TInstallPack.GetFileList: String;
begin
Result:=li_setup_get_current_profile_filelist(@ins);
end;

procedure TInstallPack.SetUserRequestCall(call: TRequestCall;const userdata: Pointer=nil);
begin
li_setup_register_user_request_call(@ins,call,userdata)
end;

procedure TInstallPack.ReadDeps(lst: TStringList);
begin
li_setup_get_dependencies(@ins,@lst);
end;

function TInstallPack.StartInstallation: Boolean;
begin
Result:=li_setup_execute(@ins);
end;

procedure TInstallPack.SetRootMode(b: Boolean);
begin
li_setup_set_sumode(@ins,b);
end;

procedure TInstallPack.EnableUSource(b: Boolean);
begin
li_setup_enable_usource_registering(@ins,b);
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

function TInstallPack.GetSignatureState: TPkgSigState;
begin
 Result:=li_setup_get_signature_state(@ins);
end;

function IsIPKAppInstalled(appname: String;appid: String;sumode: Boolean): Boolean;
begin
Result:=li_get_ipk_app_installed(PChar(appname), PChar(appid),sumode);
end;

end.

