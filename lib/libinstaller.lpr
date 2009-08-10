{ libinstaller.lpr
  Copyright (C) Listaller Project 2008-2009

  libinstaller.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  libinstaller.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Listaller library for all database, management and installation related processes
library libinstaller;

{$mode objfpc}{$H+}

uses
  Classes, ipkhandle, SysUtils, Controls;

//Events


//////////////////////////////////////////////////////////////////////////////////////
//Exported helper functions

function create_stringlist: Pointer; cdecl;
begin
 Result:=TStringList.Create;
end;

function free_stringlist(lst: Pointer): Boolean;
begin
Result:=true;
try
 TStringList(lst).Free;
except
 Result:=false;
end;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Exported functions


function remove_ipk_installed_app(appname, appid: PChar;log: Pointer;poschange: TPosChangeCall;fastmode: Boolean): Boolean; cdecl;
begin
Result:=true;
try
 UninstallIPKApp(appname, appid,TStringList(log),poschange, fastmode, true)
except
 Result:=false;
end;
end;

exports
 //Stringlist functions
 create_stringlist;
 free_stringlist;

 //** Removes an application that was installed with an IPK package
 remove_ipk_installed_app;

begin
end.

