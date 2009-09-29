{ Copyright (C) 2008-2009 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This library is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Listaller library for all IPK installation related processes
library libinstaller;

{$mode objfpc}{$H+}

uses
  Classes, ipkhandle, SysUtils, Controls, licommon, liTypes,
  management;


//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

function li_new_stringlist: Pointer; cdecl;
begin
 Result:=TStringList.Create;
end;

function li_free_stringlist(lst: PStringList): Boolean; cdecl;
begin
Result:=true;
try
 lst^.Free;
except
 Result:=false;
end;
end;

function li_stringlist_read_line(lst: PStringList;ln: Integer): PChar; cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=PChar(lst^[ln]);
 end else Result:='List index out of bounds.';
end;

function li_stringlist_write_line(lst: PStringList;ln: Integer;val: PChar): Boolean; cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=true;
  lst^[ln]:=val;
 end else Result:=false;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Installer part

//** Removes an application that was installed with an IPK package
function remove_ipk_installed_app(appname, appid: PChar;msgcall: TMessageEvent;poschange: TProgressCall;fastmode: Boolean): Boolean; cdecl;
begin
Result:=true;
try
 UninstallIPKApp(appname, appid,msgcall,poschange, fastmode, true)
except
 Result:=false;
end;
end;

//** Creates a new installation object
function ins_new_installation: Pointer; cdecl;
begin
 Result:=TInstallation.Create;
end;

//** Removes an TInstallation object
function ins_free_installation(setup: PInstallation): Boolean;
begin
try
 Result:=true;
 setup^.Free;
except
 Result:=false;
end;
end;

//** Initializes the setup
function ins_init_installation(setup: PInstallation;pkname: PChar): PChar; cdecl;
begin
 Result:='';
 if not Assigned(setup^.OnUserRequest) then
 begin
  writeLn('[WARNING] No user request callback is registered!');
 end;

 try
  setup^.Initialize(pkname);
 except
  Result:=PChar('Failed to initialize setup package '+ExtractFileName(pkname)+' !');
 end;
end;

//** Register progress changes (main)
function ins_register_main_prog_change_call(setup: PInstallation;call: TProgressCall): Boolean; cdecl;
begin
 Result:=true;
 try
   setup^.OnProgressMainChange:=call;
 except
  Result:=false;
 end;
end;

//** Register progress changes (extra)
function ins_register_extra_prog_change_call(setup: PInstallation;call: TProgressCall): Boolean; cdecl;
begin
 Result:=true;
 try
  setup^.OnProgressExtraChange:=call;
 except
  Result:=false;
 end;
end;

//** Message call
function ins_register_message_call(setup: PInstallation;call: TMessageEvent): Boolean; cdecl;
begin
 Result:=true;
 try
  setup^.OnMessage:=call;
 except
  Result:=false;
 end;
end;

//** Step message call
function ins_register_step_message_call(setup: PInstallation;call: TMessageEvent): Boolean; cdecl;
begin
 Result:=true;
 try
  setup^.OnStepMessage:=call;
 except
  Result:=false;
 end;
end;

//** User request message call
function ins_register_user_request_call(setup: PInstallation;call: TRequestEvent): Boolean; cdecl;
begin
 Result:=true;
 try
  setup^.OnUserRequest:=call;
 except
  Result:=false;
 end;
end;

//** Installation type
function ins_pkgtype(setup: PInstallation): TPkgType; cdecl;
begin
  Result:=setup^.pType;
end;

//** Set installation testmode
function li_testmode(st: Boolean): Boolean; cdecl;
begin
  Testmode:=st;
  Result:=true;
end;

//** Set to superuser mode
function li_set_su_mode(b: Boolean): Boolean; cdecl;
begin
  Root:=b;
  Result:=true;
  if Root then
  RegDir:='/etc/lipa/app-reg/'
  else
  RegDir:=SyblToPath('$INST')+'/app-reg/';
end;

//** Read disallows property
function ins_disallows(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.Disallows);
end;

//** Read supported Linux distributions
function ins_supported_distributions(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.Distris);
end;

//** Check if application is installed
function li_is_ipk_app_installed(appname: PChar;appid: PChar): Boolean; cdecl;
begin
  Result:=IsPackageInstalled(appname,appid);
end;

//** Resolve all dependencies
function ins_resolve_dependencies(setup: PInstallation): Boolean; cdecl;
begin
 Result:=setup^.ResolveDependencies;
end;

//** Readout application name
function ins_appname(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.AppName);
end;

//** Read appversion
function ins_appversion(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.AppVersion);
end;

//** Get package ID
function ins_pkgid(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.ID);
end;

//** Get description
function ins_long_description(setup: PInstallation; list: PStringList): Boolean; cdecl;
begin
try
 Result:=true;
 setup^.ReadDescription(list^);
except
 Result:=false;
end;
end;

//** Get wizard image patch
function ins_wizard_image_path(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.WizImage);
end;

//** Get license
function ins_license(setup: PInstallation; list: PStringList): Boolean; cdecl;
begin
try
 Result:=true;
 setup^.ReadLicense(list^);
except
 Result:=false;
end;
end;

//** Get profiles list
function ins_profiles_list(setup: PInstallation; list: PStringList): Boolean; cdecl;
begin
try
 Result:=true;
 setup^.ReadProfiles(list^);
except
 Result:=false;
end;
end;

//** Set current profile id
procedure ins_set_profileid(setup: PInstallation;id: ShortInt);cdecl;
begin
 setup^.SetCurProfile(id);
end;

//** Read appversion
function ins_appicon(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.AppIcon);
end;

//** Read desktopfiles
function ins_desktopfiles(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.DesktopFiles);
end;

//** Read appcmd
function ins_app_exec_command(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.CMDLn);
end;

//** Read path to file list
function ins_profile_current_filelist(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.IFileInfo);
end;

//** Starts the installation
function ins_start_installation(setup: PInstallation): Boolean; cdecl;
begin
  Result:=setup^.DoInstallation;
end;

//** Get dependencies
function ins_dependencies(setup: PInstallation; list: PStringList): Boolean; cdecl;
begin
try
 Result:=true;
 list^.Assign(setup^.ADeps);
except
 Result:=false;
end;
end;

////////////////////////////////////////////////////////////////////
//Manager part

//** Start loading list of applications
function mgr_load_applications(ty: GroupType): Boolean;cdecl;
begin
Result:=false;
if not Assigned(FReq) then begin writeLn('[ERROR] No user request callback was registered');exit;end;
try
 Result:=true;
 LoadEntries(ty);
except
 Result:=false;
end;
end;

//** Register message call
function mgr_register_message_call(call: TMessageEvent): Boolean; cdecl;
begin
 Result:=true;
 try
  management.FMsg:=call;
 except
  Result:=false;
 end;
end;

//** Register application event to catch found apps
function mgr_register_application_call(call: TAppEvent): Boolean;cdecl;
begin
 Result:=true;
 try
  management.FApp:=call;
 except
  Result:=false;
 end;
end;

//** Register event to recieve current progress
function mgr_register_progress_call(call: TProgressCall): Boolean;cdecl;
begin
 Result:=true;
 try
  management.FProg:=call;;
 except
  Result:=false;
 end;
end;

//** Register event to recieve user requests
function mgr_register_request_call(call: TRequestEvent): Boolean;cdecl;
begin
 Result:=true;
 try
  management.FReq:=call;;
 except
  Result:=false;
 end;
end;

//** Sets if aplications should work in root mode
function mgr_set_su_mode(md: Boolean): Boolean;cdecl;
begin
 Root:=md;
 Result:=true;
end;

//** Removes the application
function mgr_remove_application(obj: TAppInfo): Boolean;cdecl;
begin
 Result:=false;
if not Assigned(FProg) then begin writeLn('[ERROR] You need to register a progress callback!');exit;end;
if not Assigned(FReq) then begin writeLn('[ERROR] You need to register a user request callback!');exit;end;

 Result:=true;
 try
  UninstallApp(obj);
 except
  Result:=false;
 end;
end;

///////////////////////
exports
 //Stringlist functions
 li_new_stringlist,
 li_free_stringlist,
 li_stringlist_read_line,
 li_stringlist_write_line,

 //TInstallation related functions
 ins_new_installation,
 ins_free_installation,
 ins_init_installation,
 ins_register_main_prog_change_call,
 ins_register_extra_prog_change_call,
 ins_pkgtype,
 ins_disallows,
 ins_supported_distributions,
 ins_resolve_dependencies,
 ins_appname,
 ins_appversion,
 ins_pkgid,
 ins_long_description,
 ins_wizard_image_path,
 ins_license,
 ins_profiles_list,
 ins_appicon,
 ins_desktopfiles,
 ins_app_exec_command,
 ins_profile_current_filelist,
 ins_register_message_call,
 ins_register_step_message_call,
 ins_register_user_request_call,
 ins_start_installation,
 ins_dependencies,
 ins_set_profileid,

 //Management functions
 mgr_load_applications,
 mgr_register_message_call,
 mgr_register_application_call,
 mgr_register_progress_call,
 mgr_register_request_call,
 mgr_set_su_mode,
 mgr_remove_application,

 //Other functions
 remove_ipk_installed_app,
 li_testmode,
 li_set_su_mode,
 li_is_ipk_app_installed;

begin
end.

