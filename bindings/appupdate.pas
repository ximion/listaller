{ Copyright (C) 2010 Matthias Klumpp

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
//** This unit contains libInstaller functions to perform software updates
unit appupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, liTypes;

  function  li_updater_new: Pointer;cdecl;external libinst;
  procedure li_updater_free(upd: Pointer);cdecl;external libinst;
  function  li_updater_register_status_call(upd: Pointer;call: TLiStatusChangeCall;user_data: Pointer): Boolean;cdecl;external libinst;
  function  li_updater_register_request_call(upd: Pointer;call: TRequestCall;user_data: Pointer): Boolean;cdecl;external libinst;
  function  li_updater_register_newupdate_call(upd: Pointer;call: TNewUpdateEvent;user_data: Pointer): Boolean;cdecl;external libinst;
  function  li_updater_search_updates(upd: Pointer): Boolean;cdecl;external libinst;
  function  li_updater_updateid_oldversion(upd: Pointer;uid: Integer): PChar;cdecl;external libinst;
  function  li_updater_updateid_newversion(upd: Pointer;uid: Integer): PChar;cdecl;external libinst;
  function  li_updater_execute_update(upd: Pointer;uid: Integer): Boolean;cdecl;external libinst;

implementation
end.

