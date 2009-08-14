{ globdef.pas
  Copyright (C) Listaller Project 2009

  globdef.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  globdef.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains types which are used by libinstaller and libappmanager
unit globdef;
 
{$MODE objfpc}{$H+}
 
interface
 
uses
  Classes, SysUtils;

type
 
 PStringList = ^TStringList;

 TListallerPackageType = (lptLinstall, lptDLink, lptContainer);

 TRqType   = (rqError,rqWarning,rqQuestion,rqInfo);
 TRqResult = (rsYes,rsNo,rsOK);
 TMType    = (mtInfo,mtWarning);

 TRequestEvent = function(mtype: TRqType;msg: PChar): TRqResult;cdecl;
 TMessageEvent = function(msg: String;imp: TMType): Boolean;cdecl;

 TProgressCall = function(pos: Longint): Boolean;cdecl;

implementation

end.
