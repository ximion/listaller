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

 const amanlib='libinstaller.so';

 function load_applications(ty: GroupType): Boolean;cdecl;external amanlib name 'mgr_load_applications';
 function register_message_call(call: TMessageEvent): Boolean;cdecl;external amanlib name 'mgr_register_message_call';
 function register_progress_call(call: TProgressCall): Boolean;cdecl;external amanlib name 'mgr_register_progress_call';
 function register_request_call(call: TRequestEvent): TRqResult;cdecl;external amanlib name 'mgr_register_request_call';
 function register_application_call(call: TAppEvent): Boolean;cdecl;external amanlib name 'mgr_register_application_call';
 function set_su_mode(md: Boolean): Boolean;cdecl;external amanlib name 'mgr_set_su_mode';
 function remove_application(obj: TAppInfo): Boolean;cdecl;external amanlib name 'mgr_remove_application';

implementation

end.

