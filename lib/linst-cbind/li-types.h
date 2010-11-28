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

#include <stdlib.h>
#include <stdio.h>

typedef const void* StringList;

typedef const void* LiAppManager;

typedef const void* LiInstallation;

typedef const void* LiAppUpdater;

typedef enum {
      LIS_None,
      LIS_Stage,
      LIS_Started,
      LIS_Progress,
      LIS_Failed,
      LIS_Authorized,
      LIS_Blocked,
      LIS_Finished
} LI_STATUS;

typedef enum {
      LIRQS_Yes,
      LIRQS_No,
      LIRQS_OK
} LI_REQUEST_RES;

typedef enum {
      LIM_None,
      LIM_Info,
      LIM_Warning,
      LIM_Question_YesNo,
      LIM_Question_AbortContinue
} LI_MESSAGE;

typedef struct {
      char *text;
      int exprogress;
      int mnprogress;
} LiStatusData;

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
      ptNative,
      ptExtern,
      ptLinstall,
      ptDLink,
      ptContainer,
      ptUnknown
} LiPkgType;

typedef enum {
      fAllApps,
      fAppNative,
      fAppIPK,
      fAppExtern,
      fDeps
} LiFilter;

typedef struct {
      char *Name;
      char *AppId;
      LiPkgType PkType;
      char *Summary;
      char *Version;
      char *Author;
      char *IconName;
      char *Categories;
      double InstallDate;
      char *Dependencies;
      char *PkName;
      char *Profile;
} LiAppInfo;

typedef enum {
      raID,
      raDETAILS
} LiResolveAction;

typedef enum {
      psNone,
      psTrusted,
      psUntrusted
} PkgSignatureState;


typedef void (*LiStateEvent) (LI_STATUS status,LiStatusData details,void* user_data);

typedef LI_REQUEST_RES (*LiMessageEvent) (LI_MESSAGE mtype,const char *text,void* user_data);

typedef void (*TProgressEvent) (int pos,void* user_data);

typedef void (*LiAppEvent) (LiAppInfo *item,LiResolveAction action,void* user_data);

typedef void (*LiNewUpdateEvent) (char *name,int id,void* user_data);



#endif /* __LI_TYPES */
