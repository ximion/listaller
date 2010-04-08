{ Copyright (C) 2008-2010 Matthias Klumpp

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
//** This unit contains some basic types (especially for use with libs)
unit litypes;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  //** Name of the Listaller library
  libinst = 'libinstaller.so';

//** Version of Listaller API
  {$DEFINE APIVersion040}

type

  //** Pointer to TStringList
  PStringList = ^TStringList;

  //** Request types
  TRqType = (rqError, rqWarning, rqQuestion, rqInfo);
  //** Request result types
  TRqResult = (rqsYes, rqsNo, rqsOK);
  //** Type of a message (published)
  TMessageType = (mtStep, mtInfo);
  //** Result of a DBus action
  LiProcStatus = (prNone, prFailed, prAuthorized, prBlocked, prFinished, prError, prInfo);

  //** Things which can be changed
  LiStatusChange = (scMnProgress, scExProgress, scActionStatus, scMessage, scStepMessage);
  //** Data assigned to a status change
  TLiStatusData = record
    msg: PChar;
    exprogress: Integer;
    mnprogress: Integer;
    lastresult: LiProcStatus;
  end;

  //** Callback for change of status
  TLiStatusChangeCall = procedure(change: LiStatusChange; Data: TLiStatusData;
    user_data: Pointer); cdecl;
  //** Callback for user request
  TRequestCall = function(mtype: TRqType; msg: PChar; user_data: Pointer): TRqResult; cdecl;

  //** Called if progress was changed; only for internal use
  TProgressEvent = procedure(pos: Integer; user_data: Pointer) of object;

  //** Application groups
  TGroupType = (gtALL,
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

  //** Container for information about apps
  TAppInfo = record
    Name: PChar;
    ShortDesc: PChar;
    Version: PChar;
    Author: PChar;
    Icon: PChar;
    UId: PChar;
    Group: TGroupType;
  end;
  PAppInfo = ^TAppInfo;

  //** Event to catch thrown application records
  TAppEvent = function(Name: PChar; obj: PAppInfo): Boolean; cdecl;

  //** Shows information about new update
  TNewUpdateEvent = procedure(Name: PChar; id: Integer; user_data: Pointer); cdecl;

  //** Listaller package types
  TPkgType = (ptLinstall, ptDLink, ptContainer, ptUnknown);

  //** Package signature status
  TPkgSigState = (psNone, psTrusted, psUntrusted);


implementation

end.

