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
{$PACKRECORDS C}

interface

uses
  Classes, SysUtils;

const
  //** Name of the Listaller library
  liblistaller = 'liblistaller.so';

//** Version of Listaller API
  {$DEFINE APIVersion040}

type
  //** Pointer to TStringList
  StringList = Pointer;

  //** Pointer to an AppManager
  LiAppManager = Pointer;
  //** Pointer to Installation
  LiInstallation = Pointer;
  //** Pointer to AppUpdater
  LiAppUpdater = Pointer;
  //** Pointer to application object
  LiAppItem = Pointer;

  //** Status of current job
  LI_STATUS = (LIS_None, LIS_Started, LIS_Stage, LIS_Authorized,
    LIS_Blocked, LIS_Progress,
    LIS_Failed, LIS_Successful, LIS_Finished);

  //** Request result types
  LI_REQUEST_RES = (LIRQS_Yes, LIRQS_No, LIRQS_OK);

  //** Different status
  LI_MESSAGE = (LIM_None, LIM_Info, LIM_Warning, LIM_Question_YesNo,
    LIM_Question_AbortContinue);

  //** Data assigned to a status change
  LiStatusData = record
    text: PChar;
    exprogress: Longint;
    mnprogress: Longint;
  end;

  //** Callback for change of status
  LiStateEvent = procedure(status: LI_STATUS; details: LiStatusData;
    user_data: Pointer); cdecl;
  //** Callback for messages & user requests
  LiMessageEvent = function(mtype: LI_MESSAGE; const text: PChar;
    user_data: Pointer): LI_REQUEST_RES; cdecl;

  //** Called if progress was changed; only for internal use
  TProgressEvent = procedure(pos: Integer; user_data: Pointer) of object;

  //** Application groups
  AppCategory = (gtALL,
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

  //** Listaller package types
  LiPkgType = (ptUnknown, ptNative, ptExtern, ptLinstall, ptDLink, ptContainer);

  //** Filter for apps
  LiFilter = (fAllApps, fAppNative, fAppIPK, fAppExtern, fDeps);

  //** Event to catch thrown application records
  LiAppEvent = procedure(item: LiAppItem; user_data: Pointer); cdecl;

  //** Shows information about new update
  LiNewUpdateEvent = procedure(Name: PChar; id: Integer; user_data: Pointer); cdecl;

  //** Package signature status
  PkgSignatureState = (psNone, psTrusted, psUntrusted);


implementation

end.

