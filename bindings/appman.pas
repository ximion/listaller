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
//** This unit contains libInstaller functions to manage applications
unit appman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, liTypes;

 function  li_mgr_new: Pointer;cdecl;external libinst name 'li_mgr_new';
 procedure li_mgr_free(mgr: Pointer);cdecl;external libinst name 'li_mgr_free';
 function  li_mgr_load_apps(mgr: Pointer): Boolean;cdecl;external libinst name 'li_mgr_load_apps';
 function  li_mgr_register_msg_call(mgr: Pointer;call: TMessageCall): Boolean;cdecl;external libinst name 'li_mgr_register_msg_call';
 function  li_mgr_register_progress_call(mgr: Pointer;call: TProgressCall): Boolean;cdecl;external libinst name 'li_mgr_register_progress_call';
 function  li_mgr_register_request_call(mgr: Pointer;call: TRequestCall): TRqResult;cdecl;external libinst name 'li_mgr_register_request_call';
 function  li_mgr_register_app_call(mgr: Pointer;call: TAppEvent): Boolean;cdecl;external libinst name 'li_mgr_register_app_call';
 procedure li_mgr_set_su_mode(mgr: Pointer;md: Boolean);cdecl;external libinst name 'li_mgr_set_su_mode';
 function  li_mgr_remove_app(mgr: Pointer;obj: TAppInfo): Boolean;cdecl;external libinst name 'li_mgr_remove_app';
 function  li_remove_ipk_installed_app(appname, appid: PChar;msgcall: TMessageCall;poschange: TProgressCall;fastmode: Boolean): Boolean; cdecl; external libinst name 'li_remove_ipk_installed_app';
 function  li_mgr_check_apps(mgr: Pointer;log: PStringList;root: Boolean): Boolean;cdecl;external libinst name 'li_mgr_check_apps';
 function  li_mgr_fix_apps(mgr: Pointer;log: PStringList;root: Boolean): Boolean;cdecl;external libinst name 'li_mgr_fix_apps';

implementation

end.

