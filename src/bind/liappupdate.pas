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
  Classes, SysUtils, LiTypes;

{@Begin:Updater}

function li_updater_new: Pointer;cdecl;external liblistaller;
procedure li_updater_free(upd: PAppUpdater);cdecl;external liblistaller;
procedure li_updater_set_sumode(upd: PAppUpdater;val: Boolean);cdecl;external liblistaller;
function li_updater_register_status_call(upd: PAppUpdater;call: StatusChangeEvent;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_updater_register_request_call(upd: PAppUpdater;call: UserRequestCall;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_updater_register_newupdate_call(upd: PAppUpdater;call: NewUpdateEvent;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_updater_search_updates(upd: PAppUpdater): Boolean;cdecl;external liblistaller;
function li_updater_updateid_oldversion(upd: PAppUpdater;uid: Longint): PChar;cdecl;external liblistaller;
function li_updater_updateid_newversion(upd: PAppUpdater;uid: Longint): PChar;cdecl;external liblistaller;
function li_updater_execute_update(upd: PAppUpdater;uid: Longint): Boolean;cdecl;external liblistaller;
function li_regdir: PChar;cdecl;external liblistaller;

{@End:Updater}

implementation
end.

