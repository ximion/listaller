{ litypes.pas
  Copyright (C) Listaller Project 2009

  litypes.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  litypes.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains some basic types (especially for use with libs)
unit litypes;
 
{$MODE objfpc}{$H+}
 
interface
 
uses
  Classes, SysUtils;

type

 //** Pointer to TStringList
 PStringList = ^TStringList;

 //** Request types
 TRqType   = (rqError,rqWarning,rqQuestion,rqInfo);
 //** Request result types
 TRqResult = (rqsYes,rqsNo,rqsOK);
 //** Message types
 TMType    = (mtInfo,mtWarning);

 //** Callback for user request
 TRequestEvent = function(mtype: TRqType;msg: PChar): TRqResult;cdecl;
 //** Callback that submits a notification
 TMessageEvent = function(msg: String;imp: TMType): Boolean;cdecl;
 //** Called if a progress was changed
 TProgressCall = function(pos: Longint): Boolean;cdecl;

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
 GroupType = (gtALL,
              gtEDUCATION,
              gtOFFICE,
              gtDEVELOPMENT,
              gtGRAPHIC,
              gtNETWORK,
              gtGAMES,
              gtSYSTEM,
              gtMULTIMEDIA,
              gtADDITIONAL,
              gtOTHER,
              gtUNKNOWN);

 //** Event to catch thrown application records
 TAppEvent = function(name: PChar;obj: TAppInfo): Boolean;cdecl;

 //** Listaller package types
 TPkgType = (ptLinstall, ptDLink, ptContainer, ptUnknown);

implementation

end.
