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
//** LibInstaller functions to manage applications
unit liappmgr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LiTypes;

{@Begin:Manager}

function li_mgr_new: Pointer;cdecl;external liblistaller;
procedure li_mgr_free(mgr: PLiAppManager);cdecl;external liblistaller;
function li_mgr_load_apps(mgr: PLiAppManager): Boolean;cdecl;external liblistaller;
function li_mgr_register_status_call(mgr: PLiAppManager;call: StatusChangeEvent;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_mgr_register_app_call(mgr: PLiAppManager;call: NewAppEvent;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_mgr_register_request_call(mgr: PLiAppManager;call: UserRequestCall;user_data: Pointer): Boolean;cdecl;external liblistaller;
procedure li_mgr_set_sumode(mgr: PLiAppManager;md: Boolean);cdecl;external liblistaller;
function li_mgr_sumode(mgr: PLiAppManager): Boolean;cdecl;external liblistaller;
function li_mgr_remove_app(mgr: PLiAppManager;obj: LiAppInfo): Boolean;cdecl;external liblistaller;
function li_mgr_check_apps(mgr: PLiAppManager;log: PStringList;root: Boolean): Boolean;cdecl;external liblistaller;
function li_mgr_fix_apps(mgr: PLiAppManager;log: PStringList;root: Boolean): Boolean;cdecl;external liblistaller;

{@End:Manager}

implementation
end.
