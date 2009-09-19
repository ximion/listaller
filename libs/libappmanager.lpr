{ libappmanager.lpr
  Copyright (C) Listaller Project 2009

  libappmanager.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  libappmanager.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Listaller library for all application listing and uninstalling related processes
library libappmanager;

{$mode objfpc}{$H+}

uses
  Classes,
  management,
  liTypes;

//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

function mgr_create_stringlist: Pointer; cdecl;
begin
 Result:=TStringList.Create;
end;

function mgr_free_stringlist(lst: PStringList): Boolean; cdecl;
begin
Result:=true;
try
 lst^.Free;
except
 Result:=false;
end;
end;

function mgr_stringlist_read_line(lst: PStringList;ln: Integer): PChar; cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=PChar(lst^[ln]);
 end else Result:='List index out of bounds.';
end;

function mgr_stringlist_write_line(lst: PStringList;ln: Integer;val: PChar): Boolean; cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=true;
  lst^[ln]:=val;
 end else Result:=false;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Exported functions

//** Start loading list of applications
function mgr_load_applications(ty: GroupType): Boolean;cdecl;
begin
Result:=false;
if not Assigned(FReq) then begin writeLn('[ERROR] No user request callback was registered');exit;end;
try
 Result:=true;
 LoadEntries(ty);
except
 Result:=false;
end;
end;

//** Register message call
function mgr_register_message_call(call: TMessageEvent): Boolean; cdecl;
begin
 Result:=true;
 try
  management.FMsg:=call;
 except
  Result:=false;
 end;
end;

//** Register application event to catch found apps
function mgr_register_application_call(call: TAppEvent): Boolean;cdecl;
begin
 Result:=true;
 try
  management.FApp:=call;
 except
  Result:=false;
 end;
end;

//** Register event to recieve current progress
function mgr_register_progress_call(call: TProgressCall): Boolean;cdecl;
begin
 Result:=true;
 try
  management.FProg:=call;;
 except
  Result:=false;
 end;
end;

//** Register event to recieve user requests
function mgr_register_request_call(call: TRequestEvent): Boolean;cdecl;
begin
 Result:=true;
 try
  management.FReq:=call;;
 except
  Result:=false;
 end;
end;

//** Sets if aplications should work in root mode
function mgr_set_su_mode(md: Boolean): Boolean;cdecl;
begin
 Root:=md;
 Result:=true;
end;

//** Removes the application
function mgr_remove_application(obj: TAppInfo): Boolean;cdecl;
begin
 Result:=false;
if not Assigned(FProg) then begin writeLn('[ERROR] You need to register a progress callback!');exit;end;
if not Assigned(FReq) then begin writeLn('[ERROR] You need to register a user request callback!');exit;end;

 Result:=true;
 try
  UninstallApp(obj);
 except
  Result:=false;
 end;
end;

exports
 //Stringlist functions
 mgr_create_stringlist,
 mgr_free_stringlist,
 mgr_stringlist_read_line,
 mgr_stringlist_write_line,
 //
 mgr_load_applications,
 mgr_register_message_call,
 mgr_register_application_call,
 mgr_register_progress_call,
 mgr_register_request_call,
 mgr_set_su_mode,
 mgr_remove_application;

begin
end.

