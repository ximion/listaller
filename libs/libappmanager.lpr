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
//** Listaller library for all application listing and uninstallation related processes
library libappmanager;

{$mode objfpc}{$H+}

uses
  Classes,
  management;

type
 PStringList = ^TStringList;

//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

function create_stringlist: Pointer; cdecl;
begin
 Result:=TStringList.Create;
end;

function free_stringlist(lst: PStringList): Boolean; cdecl;
begin
Result:=true;
try
 lst^.Free;
except
 Result:=false;
end;
end;

function stringlist_read_line(lst: PStringList;ln: Integer): PChar; cdecl;
begin
 if (ln < lst^.Count)and(ln > -1) then
 begin
  Result:=PChar(lst^[ln]);
 end else Result:='List index out of bounds.';
end;

function stringlist_write_line(lst: PStringList;ln: Integer;val: PChar): Boolean; cdecl;
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
function load_applications(ty: GroupType): Boolean;cdecl;
begin
 LoadEntries(ty);
end;

//** Register message call
function register_message_call(call: TYMessageEvent): Boolean; cdecl;
begin
 Result:=true;
 try
  RegisterMessage(call);
 except
  Result:=false;
 end;
end;

//** Register application event to catch found apps
function register_application_call(call: TAppEvent): Boolean;cdecl;
begin
 Result:=true;
 try
  RegisterAppMessage(call);
 except
  Result:=false;
 end;
end;

//** Sets if aplications should work in root mode
function set_su_mode(md: Boolean): Boolean;cdecl;
begin
 Root:=md;
 Result:=true;
end;

exports
 //Stringlist functions
 create_stringlist,
 free_stringlist,
 stringlist_read_line,
 stringlist_write_line,
 //
 load_applications,
 register_message_call,
 register_application_call,
 set_su_mode;

begin
end.

