/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2010 Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under the GNU General Public License Version 3
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#if !defined (__LISTALLER_H_INSIDE__)
#error "Only <listaller.h> can be included directly."
#endif

#ifndef __LI_TYPES
#define __LI_TYPES

#include <glib.h>
#include <stdio.h>


typedef enum {
      rqError,
      rqWarning,
      rqQuestion,
      rqInfo
} TRqType;

typedef enum {
      rqsYes,
      rqsNo,
      rqsOK
} TRqResult;

typedef enum {
      mtStep,
      mtInfo
} TMessageType;

typedef enum {
      prNone,
      prFailed,
      prAuthorized,
      prBlocked,
      prFinished,
      prError,
      prInfo,
      prStarted
} LiProcStatus;

typedef enum {
      scNone,
      scMnProgress,
      scExProgress,
      scStatus,
      scMessage,
      scStepMessage
} LiStatusChange;

struct TLiStatusData
{
      gchar *msg;
      gint32 exprogress;
      gint32 mnprogress;
      LiProcStatus lastresult;
      LiStatusChange change;
};

typedef void (*TLiStatusChangeCall) {(LiStatusChange change,TLiStatusData Data,gpointer user_data);}

typedef TRqResult (*TRequestCall) {(TRqType mtype,char *msg,gpointer user_data);}

typedef void (*TProgressEvent) {(int pos,gpointer user_data);}

typedef enum {
      gtALL,
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
      gtUNKNOWN
} AppCategory;

typedef enum {
      ptLinstall,
      ptDLink,
      ptContainer,
      ptUnknown
} TPkgType;

struct TAppInfo
{
      gchar *Name;
      gchar *PkName;
      TPkgType PkType;
      gchar *ShortDesc;
      gchar *Version;
      gchar *Author;
      gchar *IconName;
      gchar *Profile;
      gchar *UId;
      AppCategory Category;
      TDateTime InstallDate;
      WideString Dependencies;
};

typedef GBoolean (*TAppEvent) {(char *Name,appinfo *obj);}

typedef void (*TNewUpdateEvent) {(char *Name,int id,gpointer user_data);}

typedef enum {
      psNone,
      psTrusted,
      psUntrusted
} TPkgSigState;


#endif /* __LI_TYPES */
