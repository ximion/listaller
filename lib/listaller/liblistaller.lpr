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
library liblistaller;

{$mode objfpc}{$H+}

//NOTE: We do not use a translatable GUI, so please use the -dNoGUI switch

uses
  CThreads, Classes, LiTypes, SysUtils, LiUtils, IPKInstall, LiDBusProc,
  LiManageApp, LiUpdateApp, listatusobj;

type
   PStringList = ^TStringList;
   PLiInstallation = ^TLiInstallation;
   PLiAppManager = ^TLiAppManager;
   PLiAppUpdater = ^TLiAppUpdater;

//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

{@BaseFunctions}

function li_new_stringlist: Pointer;cdecl;
begin
 Result := TStringList.Create;
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

function li_stringlist_read_line(lst: PStringList;ln: Integer): PChar;cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=PChar(lst^[ln]);
 end else Result:='List index out of bounds.';
end;

function li_stringlist_write_line(lst: PStringList;ln: Integer;val: PChar): Boolean;cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=true;
  lst^[ln]:=val;
 end else Result:=false;
end;

function li_stringlist_to_text(lst: PStringList): PChar;cdecl;
begin
  Result := PChar(lst^.Text);
end;

//** Get current pkg registration dir
function li_current_regdir: PChar;cdecl;
begin
 Result:=PChar(PkgRegDir);
end;

//** Get global registration dir
function li_global_regdir: PChar;cdecl;
begin
 Result:=PChar(RootPkgRegDir);
end;

//** Get Listaller version
function li_version: PChar;cdecl;
begin
 Result:=PChar(LiVersion);
end;

/////////////////////////////////////////////////////////////////////////////////////
//Installer part

{@Installer}

//** Creates a new installation object
function li_setup_new: Pointer;cdecl;
begin
 Result := TLiInstallation.Create;
end;

//** Removes an TInstallation object
procedure li_setup_free(setup: PLiInstallation);cdecl;
begin
 FReeAndNil(setup^);
end;

//** Initializes the setup
function li_setup_init(setup: PLiInstallation; const pkname: PChar): Boolean;cdecl;
begin
  Result:=true;
  setup^.Initialize(pkname);
  Result:=setup^.PkgOkay;
end;

//** Register callback on status change
function li_setup_register_status_call(setup: PLiInstallation; call: LiStateEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
   setup^.RegisterOnStatus(call, user_data);
 except
  Result:=false;
 end;
end;

//** User request message call
function li_setup_register_message_call(setup: PLiInstallation; call: LiMessageEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
  setup^.RegisterOnMessage(call,user_data);
 except
  Result:=false;
 end;
end;

//** Installation type
function li_setup_pkgtype(setup: PLiInstallation): LiPkgType;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=setup^.pType;
end;

//** Set installation testmode
procedure li_setup_set_testmode(setup: PLiInstallation;st: Boolean);cdecl;
begin
 setup^.TestMode := st;
end;

//** Check installation testmode
function li_setup_testmode(setup: PLiInstallation): Boolean;cdecl;
begin
 Result := setup^.TestMode;
end;

//** Set actions which should be forced
procedure li_setup_set_overrides(setup: PLiInstallation; const str: PChar);cdecl;
begin
  (*
   Possible strings:
   architecture = Ignore architecture
   dependencies = Do not process dependencies
  *)
  setup^.ForceActions:=str;
end;

//** Set installation to superuser mode
procedure li_setup_set_sumode(setup: PLiInstallation; b: Boolean);cdecl;
begin
 setup^.SuperuserMode:=b;
end;

//** True if installation is set to sumode
function li_setup_sumode(setup: PLiInstallation): Boolean;cdecl;
begin
 Result := setup^.SuperuserMode;
end;

//** Read disallows property
function li_setup_disallows(setup: PLiInstallation): PChar;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.Disallows);
end;

//** Read supported Linux distributions
function li_setup_supported_distros(setup: PLiInstallation): PChar;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.Distris);
end;

//** Check if application is installed
function li_ipk_app_is_installed(appname: PChar;appid: PChar;sumode: Boolean): Boolean;cdecl;
begin
  Result:=IsPackageInstalled(appname,appid,sumode);
end;

//** Readout application name
function li_setup_appname(setup: PLiInstallation): PChar;cdecl;
begin
  // if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppName);
end;

//** Read appversion
function li_setup_appversion(setup: PLiInstallation): PChar;cdecl;
begin
 // if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppVersion);
end;

//** Get package ID
function li_setup_pkgid(setup: PLiInstallation): PChar;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppID);
end;

//** Get trust level of pkg signature
function li_setup_signature_state(setup: PLiInstallation): PkgSignatureState;cdecl;
begin
  if not setup^.PkgOkay then exit;
  Result:=setup^.SignatureInfo;
end;

//** Get description
function li_setup_long_description(setup: PLiInstallation; list: PStringList): Boolean;cdecl;
begin
 Result := false;
 if not setup^.PkgOkay then exit;
try
 Result := true;
 setup^.ReadDescription(list^);
except
 Result := false;
end;
end;

//** Get description (as string)
function li_setup_long_description_as_string(setup: PLiInstallation): PChar;cdecl;
var
  tmp: TStringList;
begin
 if not setup^.PkgOkay then exit;
 tmp := TStringList.Create;
try
 setup^.ReadDescription(tmp);
 Result := PChar(tmp.Text);
finally
 tmp.Free;
 Result := '';
end;
end;

//** Get wizard image patch
function li_setup_wizard_image_path(setup: PLiInstallation): PChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.WizImage);
end;

//** Get license
function li_setup_license(setup: PLiInstallation; list: PStringList): Boolean;cdecl;
begin
try
 Result:=true;
 setup^.ReadLicense(list^);
except
 Result:=false;
end;
end;

//** Get profiles list
function li_setup_profiles_list(setup: PLiInstallation; list: PStringList): Boolean;cdecl;
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
procedure li_setup_set_profileid(setup: PLiInstallation;id: SmallInt);cdecl;
begin
 setup^.SetCurProfile(id);
end;

//** Set if update source should be registered
procedure li_setup_enable_usource_registering(setup: PLiInstallation;b: Boolean);cdecl;
begin
 setup^.RegisterUpdateSource:=b;
end;

//** Read appversion
function li_setup_appicon(setup: PLiInstallation): PChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.AppIcon);
end;

//** Read desktopfiles
function li_setup_desktopfiles(setup: PLiInstallation): PChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.DesktopFiles);
end;

//** Read appcmd
function li_setup_app_exec_command(setup: PLiInstallation): PChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.CMDLn);
end;

//** Read path to file list
function li_setup_current_profile_filelist(setup: PLiInstallation): PChar;cdecl;
begin
  Result:='';
  if not setup^.PkgOkay then exit;
  Result:=PChar(setup^.IFileInfo);
end;

//** Starts the installation
function li_setup_execute(setup: PLiInstallation): Boolean;cdecl;
begin
  Result:=setup^.DoInstallation;
end;

//** Set daemon mode
procedure li_setup_exec_by_daemon(setup: PLiInstallation;b: Boolean);cdecl;
begin
 setup^.DaemonMode:=b;
end;

//** Get dependencies
function li_setup_dependencies(setup: PLiInstallation; list: PStringList): Boolean;cdecl;
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
 perror('setup:get_dependencies() failed!');
end;
end;

////////////////////////////////////////////////////////////////////
//Manager part

{@Manager}

//** Creates a new TAppManager object
function li_mgr_new: Pointer;cdecl;
begin
 Result:=TLiAppManager.Create;
end;

//** Removes an TAppManager object
procedure li_mgr_free(mgr: PLiAppManager);cdecl;
begin
 FreeAndNil(mgr^);
end;

//** Start loading list of applications
procedure li_mgr_load_apps(mgr: PLiAppManager; filter: LiFilter);cdecl;
begin
  mgr^.LoadApplications(filter);
end;

//** Update AppInstall db by rescanning all data
function li_mgr_update_appdb(mgr: PLiAppManager): Boolean;cdecl;
begin
 Result:=false;
// try
  Result:=true;
  mgr^.UpdateAppDB;
// except
//  Result:=false;
// end;
end;

//** Register call on status change for appmanager
function li_mgr_register_status_call(mgr: PLiAppManager;call: LiStateEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
  mgr^.RegisterOnStatus(call,user_data);
 except
  Result:=false;
 end;
end;

//** Register application event to catch found apps
function li_mgr_register_app_call(mgr: PLiAppManager;call: LiNewAppEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result := false;
 Result:=true;
 try
  mgr^.RegOnNewApp(call, user_data);
 except
  Result:=false;
 end;
end;

//** Register event to recieve user requests
function li_mgr_register_message_call(mgr: PLiAppManager;call: LiMessageEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
  mgr^.RegisterOnMessage(call,user_data);
 except
  Result:=false;
 end;
end;

//** Sets if aplications should work in root mode
procedure li_mgr_set_sumode(mgr: PLiAppManager;md: Boolean);cdecl;
begin
 mgr^.SuperuserMode:=md;
end;

//** True if set to root mode
function li_mgr_sumode(mgr: PLiAppManager): Boolean;cdecl;
begin
 Result := mgr^.SuperuserMode;
end;

//** Removes the application
function li_mgr_remove_app(mgr: PLiAppManager;obj: LiAppInfo): Boolean;cdecl;
begin
 Result:=true;
 try
  mgr^.UninstallApp(obj);
 except
  Result:=false;
 end;
end;

//** Check application dependencies
function li_mgr_check_apps(mgr: PLiAppManager;log: PStringList;root: Boolean): Boolean;cdecl;
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
 else perror('Check log != nil failed.');
end;

//** Fix application dependencies
function li_mgr_fix_apps(mgr: PLiAppManager;log: PStringList;root: Boolean): Boolean;cdecl;
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
 else perror('Check log != nil failed.');
end;

////////////////////////////////////////////////////////////////////
//Updater part

{@Updater}

//** Creates a new TAppUpdater object
function li_updater_new: Pointer;cdecl;
begin
 Result := TLIAppUpdater.Create;
end;

//** Removes an TAppUpdater object
procedure li_updater_free(upd: PLiAppUpdater);cdecl;
begin
 upd^.Free;
end;

//** Set superuser mode (or not)
procedure li_updater_set_sumode(upd: PLiAppUpdater;val: Boolean);cdecl;
begin
 upd^.SetSumode(val);
end;

//** Register call on status change for updater
function li_updater_register_status_call(upd: PLiAppUpdater;call: LiStateEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
  upd^.RegisterOnStatus(call,user_data);
 except
  Result:=false;
 end;
end;

//** Register event to recieve user requests
function li_updater_register_message_call(upd: PLiAppUpdater;call: LiMessageEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
  upd^.RegisterOnMessage(call,user_data);
 except
  Result:=false;
 end;
end;

//** Register event for new updates
function li_updater_register_newupdate_call(upd: PLiAppUpdater;call: LiNewUpdateEvent;user_data: Pointer): Boolean;cdecl;
begin
 Result:=true;
 try
  upd^.RegOnNewUpdate(call,user_data);
 except
  on E: Exception do
  begin
   Result:=false;
   perror(E.Message);
  end;
 end;
end;

//** Look for new updates
function li_updater_search_updates(upd: PLiAppUpdater): Boolean;cdecl;
begin
 Result:=upd^.CheckUpdates;
end;

//** Fetch old version of application
function li_updater_updateid_oldversion(upd: PLiAppUpdater;uid: Longint): PChar;cdecl;
begin
 Result:=PChar(upd^.UpdateIDGetOldVersion(uid));
end;

//** Fetch new application version
function li_updater_updateid_newversion(upd: PLiAppUpdater;uid: Longint): PChar;cdecl;
begin
 Result:=PChar(upd^.UpdateIDGetNewVersion(uid));
end;

//** Execute update for given update ID
function li_updater_execute_update(upd: PLiAppUpdater;uid: Longint): Boolean;cdecl;
begin
 Result:=upd^.ExecuteUpdate(uid);
end;

///////////////////////
exports
 //Stringlist functions
 li_new_stringlist,
 li_free_stringlist,
 li_stringlist_read_line,
 li_stringlist_write_line,
 li_stringlist_to_text,

 //TInstallation related functions
 li_setup_new,
 li_setup_free,
 li_setup_init,
 li_setup_set_sumode,
 li_setup_sumode,
 li_setup_register_status_call,
 li_setup_register_message_call,
 li_setup_pkgtype,
 li_setup_disallows,
 li_setup_supported_distros,
 li_setup_appname,
 li_setup_appversion,
 li_setup_pkgid,
 li_setup_long_description,
 li_setup_long_description_as_string,
 li_setup_enable_usource_registering,
 li_setup_wizard_image_path,
 li_setup_license,
 li_setup_profiles_list,
 li_setup_appicon,
 li_setup_desktopfiles,
 li_setup_app_exec_command,
 li_setup_signature_state,
 li_setup_current_profile_filelist,
 li_setup_execute,
 li_setup_dependencies,
 li_setup_set_overrides,
 li_setup_set_profileid,
 li_setup_set_testmode,
 li_setup_testmode,
 li_setup_exec_by_daemon,

 //Management functions
 li_mgr_new,
 li_mgr_free,
 li_mgr_update_appdb,
 li_mgr_load_apps,
 li_mgr_register_status_call,
 li_mgr_register_message_call,
 li_mgr_register_app_call,
 li_mgr_set_sumode,
 li_mgr_sumode,
 li_mgr_remove_app,
 li_mgr_check_apps,
 li_mgr_fix_apps,

 //Updater functions
 li_updater_new,
 li_updater_free,
 li_updater_set_sumode,
 li_updater_register_status_call,
 li_updater_register_message_call,
 li_updater_register_newupdate_call,
 li_updater_search_updates,
 li_updater_updateid_newversion,
 li_updater_updateid_oldversion,
 li_updater_execute_update,

 //Other functions
 li_current_regdir,
 li_global_regdir,
 li_ipk_app_is_installed,
 li_version;

{$R *.res}

begin
end.
