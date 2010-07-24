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
//** LibInstaller functions to perform software updates
unit liappupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glib2, liTypes;

  function  li_updater_new: GPointer;cdecl;external libinst;
  procedure li_updater_free(upd: GPointer);cdecl;external libinst;
  procedure li_updater_set_sumode(upd: GPointer;val: GBoolean);cdecl;external libinst;
  function  li_updater_register_status_call(upd: GPointer;call: TLiStatusChangeCall;user_data: GPointer): GBoolean;cdecl;external libinst;
  function  li_updater_register_request_call(upd: GPointer;call: TRequestCall;user_data: GPointer): GBoolean;cdecl;external libinst;
  function  li_updater_register_newupdate_call(upd: GPointer;call: TNewUpdateEvent;user_data: GPointer): GBoolean;cdecl;external libinst;
  function  li_updater_search_updates(upd: GPointer): GBoolean;cdecl;external libinst;
  function  li_updater_updateid_oldversion(upd: GPointer;uid: GInt32): PGChar;cdecl;external libinst;
  function  li_updater_updateid_newversion(upd: GPointer;uid: GInt32): PGChar;cdecl;external libinst;
  function  li_updater_execute_update(upd: GPointer;uid: GInt32): GBoolean;cdecl;external libinst;

implementation
end.

