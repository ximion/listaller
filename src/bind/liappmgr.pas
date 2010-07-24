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
  Classes, SysUtils, glib2, liTypes;

 function  li_mgr_new: GPointer;cdecl;external libinst;
 procedure li_mgr_free(mgr: GPointer);cdecl;external libinst;
 function  li_mgr_load_apps(mgr: GPointer): GBoolean;cdecl;external libinst;
 function  li_mgr_register_status_call(mgr: GPointer;call: TLiStatusChangeCall; user_data: GPointer): GBoolean;cdecl;external libinst;
 function  li_mgr_register_request_call(mgr: GPointer;call: TRequestCall;user_data: GPointer): TRqResult;cdecl;external libinst;
 function  li_mgr_register_app_call(mgr: GPointer;call: TAppEvent): GBoolean;cdecl;external libinst;
 procedure li_mgr_set_sumode(mgr: GPointer;md: GBoolean);cdecl;external libinst;
 function  li_mgr_remove_app(mgr: GPointer;obj: TAppInfo): GBoolean;cdecl;external libinst;
 function  li_remove_ipk_installed_app(appname, appid: PChar;scall: TLiStatusChangeCall;fastmode: GBoolean): GBoolean; cdecl; external libinst;
 function  li_mgr_check_apps(mgr: GPointer;log: PStringList;root: GBoolean): GBoolean;cdecl;external libinst;
 function  li_mgr_fix_apps(mgr: GPointer;log: PStringList;root: GBoolean): GBoolean;cdecl;external libinst;

implementation

end.

