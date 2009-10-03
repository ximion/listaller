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

 function li_mgr_load_apps: Boolean;cdecl;external libinst name 'li_mgr_load_apps';
 function li_mgr_register_msg_call(call: TMessageCall): Boolean;cdecl;external libinst name 'li_mgr_register_msg_call';
 function li_mgr_register_progress_call(call: TProgressCall): Boolean;cdecl;external libinst name 'li_mgr_register_progress_call';
 function li_mgr_register_request_call(call: TRequestCall): TRqResult;cdecl;external libinst name 'li_mgr_register_request_call';
 function li_mgr_register_app_call(call: TAppEvent): Boolean;cdecl;external libinst name 'li_mgr_register_app_call';
 function li_mgr_set_su_mode(md: Boolean): Boolean;cdecl;external libinst name 'li_mgr_set_su_mode';
 function li_remove_app(obj: TAppInfo): Boolean;cdecl;external libinst name 'li_mgr_remove_app';
 function li_remove_ipk_installed_app(appname, appid: PChar;msgcall: TMessageCall;poschange: TProgressCall;fastmode: Boolean): Boolean; cdecl; external libinst name 'li_remove_ipk_installed_app';
 function li_check_apps(log: PStringList;root: Boolean): Boolean;cdecl;external libinst name 'li_check_apps';
 function li_fix_apps(log: PStringList;root: Boolean): Boolean;cdecl;external libinst name 'li_fix_apps';

implementation

end.

