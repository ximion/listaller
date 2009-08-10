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
 Result:=TStringList.Create;
end;

function free_stringlist(lst: Pointer): Boolean; cdecl;
begin
Result:=true;
try
 PStringList(lst)^.Free;
except
 Result:=false;
end;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Exported functions

function remove_ipk_installed_app(appname, appid: PChar;log: Pointer;poschange: TProgressChange;fastmode: Boolean): Boolean; cdecl;
begin
Result:=true;
try
 UninstallIPKApp(appname, appid,TStringList(log),poschange, fastmode, true)
except
 Result:=false;
end;
end;

function new_installation: Pointer; cdecl;
begin
 Result:=TInstallation.Create;
end;

function init_installation(setup: Pointer;pkname: PChar): PChar; cdecl;
begin
 Result:='';
 try
  PInstallation(setup)^.Initialize(pkname);
 except
  Result:=PChar('Failed to initialize setup package '+ExtractFileName(pkname)+' !');
 end;
end;

function register_inst_main_prog_change_call(setup: Pointer;call: TProgressChange): Boolean; cdecl;
begin
 Result:=true;
 if setup = nil then
 begin
  Result:=false;
  exit;
 end;
 try
 PInstallation(setup)^.OnProgressMainChange:=call;
 except
  Result:=false;
 end;
end;

function register_inst_extra_prog_change_call(setup: Pointer;call: TProgressChange): Boolean; cdecl;
begin
 Result:=true;
 if setup = nil then
 begin
  Result:=false;
  exit;
 end;
 try
 PInstallation(setup)^.OnProgressExtraChange:=call;
 except
  Result:=false;
 end;
end;

function inst_type(setup: Pointer): TListallerPackageType; cdecl;
begin
  Result:=PInstallation(setup)^.pType;
end;

function set_testmode(st: Boolean): Boolean; cdecl;
begin
  Testmode:=st;
end;

function inst_disallows(setup: Pointer): PChar; cdecl;
begin
  Result:=PChar(PInstallation(setup)^.Disallows);
end;

function inst_supported_distributions(setup: Pointer): PChar; cdecl;
begin
  Result:=PChar(PInstallation(setup)^.Distris);
end;

function is_ipk_app_installed(setup: Pointer;appname: PChar;appid: PChar): Boolean; cdecl;
begin
  Result:=PInstallation(setup)^.IsPackageInstalled(appname,appid);
end;

exports
 //Stringlist functions
 create_stringlist,
 free_stringlist,

 //** Removes an application that was installed with an IPK package
 remove_ipk_installed_app,
 //** Creates a new installation object
 new_installation,
 //** Initializes the setup
 init_installation,
 //** Register progress changes (main)
 register_inst_main_prog_change_call,
 //** Register progress changes (extra)
 register_inst_extra_prog_change_call,
 //** Installation type
 inst_type,
 //** Set installation testmode
 set_testmode,
 //** Read disallows property
 inst_disallows,
 //** Read supported Linux distributions
 inst_supported_distributions,
 //** Check if application is installed
 is_ipk_app_installed;

begin
end.

