{ installer.pas
  Copyright (C) Listaller Project 2009

  installer.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  installer.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains functions to access libappmanager.so
unit appman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globdef;

type
//** Different message types
TMType    = (mtInfo,mtWarning);
//** Container for information about apps
TAppInfo = record
 Name: PChar;
 ShortDesc: PChar;
 Version: PChar;
 Author: PChar;
 Icon: PChar;
 UId: PChar;
end;

//** Application groups
GroupType = (gtALL,gtEDUCATION,gtOFFICE,gtDEVELOPMENT,gtGRAPHIC,gtNETWORK,gtGAMES,gtSYSTEM,gtMULTIMEDIA,gtADDITIONAL,gtOTHER);

//** Event to catch thrown application records
TAppEvent = function(name: PChar;obj: TAppInfo): Boolean;cdecl;

 const amanlib='libappmanager.so';

 function load_applications(ty: GroupType): Boolean;cdecl;external amanlib name 'load_applications';
 function register_message_call(call: TMessageEvent): Boolean;cdecl;external amanlib name 'register_message_call';
 function register_application_call(call: TAppEvent): Boolean;cdecl;external amanlib name 'register_application_call';
 function set_su_mode(md: Boolean): Boolean;cdecl;external amanlib name 'set_su_mode';
 function remove_application(obj: TAppInfo): Boolean;cdecl;external amanlib name 'remove_application';

implementation

end.

