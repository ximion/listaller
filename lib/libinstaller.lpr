{ libinstaller.lpr
  Copyright (C) Listaller Project 2008-2009

  libinstaller.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  libinstaller.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Listaller library for all database, management and installation related processes
library libinstaller;

{$mode objfpc}{$H+}

uses
  Classes, ipkhandle, SysUtils, Controls, licommon;

type
 PStringList = ^TStringList;

//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

function create_stringlist: Pointer; cdecl;
begin
 Result:=@TStringList.Create;
end;

function free_stringlist(lst: PStringList): Boolean; cdecl;
begin
Result:=true;
try
 lst^.Free;
except
 Result:=false;
end;
end;

function stringlist_read_line(lst: PStringList;ln: Integer): PChar; cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=PChar(lst^[ln]);
 end else Result:='List index out of bounds.';
end;

/////////////////////////////////////////////////////////////////////////////////////
//Exported functions

//** Removes an application that was installed with an IPK package
function remove_ipk_installed_app(appname, appid: PChar;log: Pointer;poschange: TProgressChange;fastmode: Boolean): Boolean; cdecl;
begin
Result:=true;
try
 UninstallIPKApp(appname, appid,TStringList(log),poschange, fastmode, true)
except
 Result:=false;
end;
end;

//** Creates a new installation object
function new_installation: Pointer; cdecl;
begin
 Result:=TInstallation.Create;
end;

//** Removes an TInstallation object
function free_installation(setup: PInstallation): Boolean;
begin
 setup^.Free;
end;

//** Initializes the setup
function init_installation(setup: PInstallation;pkname: PChar): PChar; cdecl;
begin
 Result:='';
 try
  setup^.Initialize(pkname);
 except
  Result:=PChar('Failed to initialize setup package '+ExtractFileName(pkname)+' !');
 end;
end;

//** Register progress changes (main)
function ins_register_main_prog_change_call(setup: PInstallation;call: TProgressChange): Boolean; cdecl;
begin
 Result:=true;
 if setup = nil then
 begin
  Result:=false;
  exit;
 end;
 try
   setup^.OnProgressMainChange:=call;
 except
  Result:=false;
 end;
end;

//** Register progress changes (extra)
function ins_register_extra_prog_change_call(setup: PInstallation;call: TProgressChange): Boolean; cdecl;
begin
 Result:=true;
 if setup = nil then
 begin
  Result:=false;
  exit;
 end;
 try
  setup^.OnProgressExtraChange:=call;
 except
  Result:=false;
 end;
end;

//** Installation type
function ins_pkgtype(setup: PInstallation): TListallerPackageType; cdecl;
begin
  Result:=setup^.pType;
end;

//** Set installation testmode
function set_testmode(st: Boolean): Boolean; cdecl;
begin
  Testmode:=st;
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
function is_ipk_app_installed(appname: PChar;appid: PChar): Boolean; cdecl;
begin
  Result:=IsPackageInstalled(appname,appid);
end;

//** Resolve all dependencies
function ins_resolve_dependencies(setup: PInstallation): Boolean; cdecl;
begin
 Result:=setup^.ResolveDependencies;
end;

//** Readout applocation name
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
function ins_appid(setup: PInstallation): PChar; cdecl;
begin
  Result:=PChar(setup^.AppID);
end;

//** Get description
function ins_long_description(setup: PInstallation; list: PStringList): Boolean; cdecl;
begin
try
 Result:=true;
 list^.LoadFromFile(setup^.DescFile);
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
 list^.LoadFromFile(setup^.LicenseFile);
except
 Result:=false;
end;
end;

exports
 //Stringlist functions
 create_stringlist,
 free_stringlist,
 stringlist_read_line,

 //TInstallation related functions
 new_installation,
 free_installation,
 init_installation,
 ins_register_main_prog_change_call,
 ins_register_extra_prog_change_call,
 ins_pkgtype,
 ins_disallows,
 ins_supported_distributions,
 ins_resolve_dependencies,
 ins_appname,
 ins_appversion,
 ins_appid,
 ins_long_description,
 ins_wizard_image_path,
 ins_license,

 //Other functions
 remove_ipk_installed_app,
 set_testmode,
 is_ipk_app_installed;

begin
end.

