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
//{$packrecords c}

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
  PStringList = ^TStringList;

  //** Pointer to an AppManager
  PLiAppManager = Pointer;
  //** Pointer to Installation
  PLiInstallation = Pointer;
  //** Pointer to AppUpdater
  PLiAppUpdater = Pointer;

  //** Request types
  TRqType = (rqError, rqWarning, rqQuestion, rqInfo);
  //** Request result types
  TRqResult = (rqsYes, rqsNo, rqsOK);
  //** Type of a message (published)
  TMessageType = (mtStep, mtInfo);
  //** Status of current job
  LiProcStatus = (prNone, prFailed, prAuthorized, prBlocked, prFinished,
    prError, prInfo, prStarted);

  //** Things which can be changed
  LiStatusChange = (scNone, scMnProgress, scExProgress, scStatus,
    scMessage, scStepMessage);
  //** Data assigned to a status change
  TLiStatusData = record
    msg: PChar;
    exprogress: Longint;
    mnprogress: Longint;
    lastresult: LiProcStatus;
    change: LiStatusChange;
  end;

  //** Callback for change of status
  StatusChangeEvent = procedure(change: LiStatusChange; data: TLiStatusData;
    user_data: Pointer); cdecl;
  //** Callback for user request
  UserRequestCall = function(mtype: TRqType; msg: PChar;
    user_data: Pointer): TRqResult; cdecl;

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
  PkgType = (ptLinstall, ptDLink, ptContainer, ptUnknown);

  //** Container for information about apps
  AppInfo = record
    Name: PChar;
    PkName: PChar;
    PkType: PkgType;
    ShortDesc: PChar;
    Version: PChar;
    Author: PChar;
    IconName: PChar;
    Profile: PChar;
    UId: PChar;
    Category: AppCategory;
    InstallDate: TDateTime;
    Dependencies: PChar;
  end;

  PAppInfo = ^AppInfo;

  //** Event to catch thrown application records
  NewAppEvent = procedure(name: PChar; obj: PAppInfo; user_data: Pointer); cdecl;

  //** Shows information about new update
  NewUpdateEvent = procedure(name: PChar; id: Integer; user_data: Pointer); cdecl;

  //** Package signature status
  PkgSignatureState = (psNone, psTrusted, psUntrusted);


implementation

end.

