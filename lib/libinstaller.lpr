{ Copyright (C) 2009-2010 Matthias Klumpp

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
//** Listaller library for all software management processes
library libinstaller;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, ipkInstall, SysUtils, Controls, liTypes, liUtils,
  liManageApp, liUpdateApp, glib2, softwaredb;


//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

function li_new_stringlist: GPointer;cdecl;
begin
 Result:=TStringList.Create;
end;

function li_free_stringlist(lst: PStringList): Boolean;cdecl;
begin
Result:=true;
try
 lst^.Free;
except
 Result:=false;
end;
end;

function li_stringlist_read_line(lst: PStringList;ln: Integer): PGChar;cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=PGChar(lst^[ln]);
 end else Result:='List index out of bounds.';
end;

function li_stringlist_write_line(lst: PStringList;ln: Integer;val: PGChar): GBoolean;cdecl;
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
function li_remove_ipk_installed_app(appname, appid: PGChar;statuscall: TLiStatusChangeCall;fastmode: GBoolean): GBoolean;cdecl;
begin
Result:=true;
try
 UninstallIPKApp(appname, appid, statuscall, fastmode, true)
except
 Result:=false;
end;
end;

//** Creates a new installation object
function li_setup_new: GPointer;cdecl;
begin
 Result:=TInstallation.Create;
end;

//** Removes an TInstallation object
procedure li_setup_free(setup: PInstallation);cdecl;
begin
 setup^.Free;
end;

//** Initializes the setup
function li_setup_init(setup: PInstallation;pkname: PChar): GBoolean;cdecl;
begin
 Result:=true;
 if not setup^.UserRequestRegistered then
 begin
  p_warning('No user request callback is registered!');
 end;

 //try
  setup^.Initialize(pkname);
  Result:=setup^.PkgOkay;
 {except
  setup^.PkgOkay;
  Result:=false;
 end; }
end;

//** Register callback on status change
function li_setup_register_status_call(setup: PInstallation;call: TLiStatusChangeCall;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
   setup^.RegOnStatusChange(call,user_data);
 except
  Result:=false;
 end;
end;

//** User request message call
function li_setup_register_user_request_call(setup: PInstallation;call: TRequestCall;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
  setup^.RegOnUsrRequest(call,user_data);
 except
  Result:=false;
 end;
end;

//** Installation type
function li_setup_get_pkgtype(setup: PInstallation): TPkgType;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=setup^.pType;
end;

//** Set installation testmode
procedure li_set_testmode(st: Boolean);cdecl;
begin
  Testmode:=st;
end;

//** Set actions which should be forced
procedure li_setup_set_forced(setup: PInstallation;str: PGChar);cdecl;
begin
  (*
   Possible strings:
   architecture = Ignore architecture
   dependencies = Do not process dependencies
  *)
  setup^.ForceActions:=str;
end;

//** Set TInstallation to superuser mode
procedure li_setup_set_sumode(setup: PInstallation;b: GBoolean);cdecl;
begin
 setup^.SuperuserMode:=b;
end;

//** Read disallows property
function li_setup_get_disallows(setup: PInstallation): PGChar;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.Disallows);
end;

//** Read supported Linux distributions
function li_setup_get_supported_distributions(setup: PInstallation): PGChar;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.Distris);
end;

//** Check if application is installed
function li_get_ipk_app_installed(appname: PGChar;appid: PGChar;sumode: Boolean): GBoolean;cdecl;
begin
  Result:=IsPackageInstalled(appname,appid,sumode);
end;

//** Readout application name
function li_setup_get_appname(setup: PInstallation): PGChar;cdecl;
begin
  // if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppName);
end;

//** Read appversion
function li_setup_get_appversion(setup: PInstallation): PGChar;cdecl;
begin
 // if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppVersion);
end;

//** Get package ID
function li_setup_get_pkgid(setup: PInstallation): PGChar;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppID);
end;

//** Get trust level of pkg signature
function li_setup_get_signature_state(setup: PInstallation): TPkgSigState;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=setup^.SignatureInfo;
end;

//** Get description
function li_setup_get_long_description(setup: PInstallation; list: PStringList): GBoolean;cdecl;
begin
 Result:=false;
 if not setup^.PkgOkay then exit;
try
 Result:=true;
 setup^.ReadDescription(list^);
except
 Result:=false;
end;
end;

//** Get wizard image patch
function li_setup_get_wizard_image_path(setup: PInstallation): PGChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.WizImage);
end;

//** Get license
function li_setup_get_license(setup: PInstallation; list: PStringList): GBoolean;cdecl;
begin
try
 Result:=true;
 setup^.ReadLicense(list^);
except
 Result:=false;
end;
end;

//** Get profiles list
function li_setup_get_profiles_list(setup: PInstallation; list: PStringList): GBoolean;cdecl;
begin
  Result:=false;
  if not setup^.PkgOkay then exit;
try
 Result:=true;
 setup^.ReadProfiles(list^);
except
 Result:=false;
end;
end;

//** Set current profile id
procedure li_setup_set_profileid(setup: PInstallation;id: GInt16);cdecl;
begin
 setup^.SetCurProfile(id);
end;

//** Set if update source should be registered
procedure li_setup_enable_usource_registering(setup: PInstallation;b: GBoolean);cdecl;
begin
 setup^.RegisterUpdateSource:=b;
end;

//** Read appversion
function li_setup_get_appicon(setup: PInstallation): PGChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppIcon);
end;

//** Read desktopfiles
function li_setup_get_desktopfiles(setup: PInstallation): PGChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.DesktopFiles);
end;

//** Read appcmd
function li_setup_get_app_exec_command(setup: PInstallation): PGChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.CMDLn);
end;

//** Read path to file list
function li_setup_get_current_profile_filelist(setup: PInstallation): PGChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.IFileInfo);
end;

//** Starts the installation
function li_setup_execute(setup: PInstallation): GBoolean;cdecl;
begin
  Result:=setup^.DoInstallation;
end;

//** Set daemon mode
procedure li_setup_exec_by_daemon(setup: PInstallation;b: GBoolean);cdecl;
begin
 setup^.DaemonMode:=b;
end;

//** Get dependencies
function li_setup_get_dependencies(setup: PInstallation; list: PStringList): GBoolean;cdecl;
var i: Integer;
begin
  Result:=false;
  if not setup^.PkgOkay then exit;
try
 Result:=true;
 for i:=0 to setup^.ADeps.Count-1 do
  list^.Add(setup^.ADeps[i]);
except
 Result:=false;
 p_error('setup:get_dependencies() failed!');
end;
end;

////////////////////////////////////////////////////////////////////
//Manager part

//** Creates a new TAppManager object
function li_mgr_new: GPointer;cdecl;
begin
 Result:=TAppManager.Create;
end;

//** Removes an TAppManager object
procedure li_mgr_free(mgr: PAppManager);cdecl;
begin
 mgr^.Free;
end;

//** Start loading list of applications
function li_mgr_load_apps(mgr: PAppManager): Boolean;cdecl;
begin
Result:=false;
try
if not mgr^.UserRequestRegistered then begin p_error('No user request callback was registered');exit;end;
 Result:=true;
 mgr^.LoadEntries;
except
 Result:=false;
end;
end;

//** Register call on status change for appmanager
function li_mgr_register_status_call(mgr: PAppManager;call: TLiStatusChangeCall;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
  mgr^.RegOnStatusChange(call,user_data);
 except
  Result:=false;
 end;
end;

//** Register application event to catch found apps
function li_mgr_register_app_call(mgr: PAppManager;call: TAppEvent): GBoolean;cdecl;
begin
 Result:=true;
 try
  mgr^.OnApplication:=call;
 except
  Result:=false;
 end;
end;

//** Register event to recieve user requests
function li_mgr_register_request_call(mgr: PAppManager;call: TRequestCall;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
  mgr^.RegOnRequest(call,user_data);
 except
  Result:=false;
 end;
end;

//** Sets if aplications should work in root mode
procedure li_mgr_set_sumode(mgr: PAppManager;md: GBoolean);cdecl;
begin
 mgr^.SuperuserMode:=md;
end;

//** Removes the application
function li_mgr_remove_app(mgr: PAppManager;obj: TAppInfo): GBoolean;cdecl;
begin
 Result:=false;
if not mgr^.UserRequestRegistered then begin p_error('You need to register a user request callback!');exit;end;
 Result:=true;
 try
  mgr^.UninstallApp(obj);
 except
  Result:=false;
 end;
end;

//** Check application dependencies
function li_mgr_check_apps(mgr: PAppManager;log: PStringList;root: GBoolean): GBoolean;cdecl;
procedure PerformCheck;
begin
 if not mgr^.CheckApps(log^,false,root) then
 begin
  Result:=false;
 end else Result:=true;
end;
begin
if Assigned(log^) then
  PerformCheck
else p_error('Check log != nil failed.');
end;

//** Fix application dependencies
function li_mgr_fix_apps(mgr: PAppManager;log: PStringList;root: GBoolean): GBoolean;cdecl;
procedure PerformCheck;
begin
 if not mgr^.CheckApps(log^,true,root) then
 begin
  Result:=false;
 end else Result:=true;
end;
begin
if Assigned(log^) then
  PerformCheck
else p_error('Check log != nil failed.');
end;

////////////////////////////////////////////////////////////////////
//Updater part

//** Creates a new TAppUpdater object
function li_updater_new: GPointer;cdecl;
begin
 Result:=TAppUpdater.Create;
end;

//** Removes an TAppUpdater object
procedure li_updater_free(upd: PAppUpdater);cdecl;
begin
 upd^.Free;
end;

//** Set superuser mode (or not)
procedure li_updater_set_sumode(upd: PAppUpdater;val: GBoolean);cdecl;
begin
 upd^.SetSumode(val);
end;

//** Register call on status change for updater
function li_updater_register_status_call(upd: PAppUpdater;call: TLiStatusChangeCall;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
  upd^.RegOnStatusChange(call,user_data);
 except
  Result:=false;
 end;
end;

//** Register event to recieve user requests
function li_updater_register_request_call(upd: PAppUpdater;call: TRequestCall;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
  upd^.RegOnRequest(call,user_data);
 except
  Result:=false;
 end;
end;

//** Register event for new updates
function li_updater_register_newupdate_call(upd: PAppUpdater;call: TNewUpdateEvent;user_data: GPointer): GBoolean;cdecl;
begin
 Result:=true;
 try
  upd^.RegOnNewUpdate(call,user_data);
 except
  on E: Exception do
  begin
   Result:=false;
   p_error(E.Message);
  end;
 end;
end;

//** Look for new updates
function li_updater_search_updates(upd: PAppUpdater): GBoolean;cdecl;
begin
 Result:=upd^.CheckUpdates;
end;

//** Fetch old version of application
function li_updater_updateid_oldversion(upd: PAppUpdater;uid: GInt32): PGChar;cdecl;
begin
 Result:=PChar(upd^.UpdateIDGetOldVersion(uid));
end;

//** Fetch new application version
function li_updater_updateid_newversion(upd: PAppUpdater;uid: GInt32): PGChar;cdecl;
begin
 Result:=PChar(upd^.UpdateIDGetNewVersion(uid));
end;

//** Execute update for given update ID
function li_updater_execute_update(upd: PAppUpdater;uid: GInt32): GBoolean;cdecl;
begin
 Result:=upd^.ExecuteUpdate(uid);
end;

//** Get registration dir
function li_regdir: PGChar;cdecl;
begin
 Result:=PChar(RegDir);
end;

///////////////////////
exports
 //Stringlist functions
 li_new_stringlist,
 li_free_stringlist,
 li_stringlist_read_line,
 li_stringlist_write_line,

 //TInstallation related functions
 li_setup_new,
 li_setup_free,
 li_setup_init,
 li_setup_set_sumode,
 li_setup_register_status_call,
 li_setup_get_pkgtype,
 li_setup_get_disallows,
 li_setup_get_supported_distributions,
 li_setup_get_appname,
 li_setup_get_appversion,
 li_setup_get_pkgid,
 li_setup_get_long_description,
 li_setup_enable_usource_registering,
 li_setup_get_wizard_image_path,
 li_setup_get_license,
 li_setup_get_profiles_list,
 li_setup_get_appicon,
 li_setup_get_desktopfiles,
 li_setup_get_app_exec_command,
 li_setup_get_signature_state,
 li_setup_get_current_profile_filelist,
 li_setup_register_user_request_call,
 li_setup_execute,
 li_setup_get_dependencies,
 li_setup_set_forced,
 li_setup_set_profileid,
 li_setup_exec_by_daemon,

 //Management functions
 li_mgr_new,
 li_mgr_free,
 li_mgr_load_apps,
 li_mgr_register_status_call,
 li_mgr_register_app_call,
 li_mgr_register_request_call,
 li_mgr_set_sumode,
 li_mgr_remove_app,
 li_mgr_check_apps,
 li_mgr_fix_apps,

 //Updater functions
 li_updater_new,
 li_updater_free,
 li_updater_set_sumode,
 li_updater_register_status_call,
 li_updater_register_request_call,
 li_updater_register_newupdate_call,
 li_updater_search_updates,
 li_updater_updateid_newversion,
 li_updater_updateid_oldversion,
 li_updater_execute_update,

 //Other functions
 li_regdir,
 li_remove_ipk_installed_app,
 li_set_testmode,
 li_get_ipk_app_installed;

{$R *.res}

begin
end.

