
(* Part of Listaller's PackageKit bindings
 *
 * (c) 2010 Matthias Klumpp
 *
 * Licensed under the same license as the original header.
 * see copyright notice below for more information.
 *)

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{ -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * Copyright (C) 2009 Richard Hughes <richard@hughsie.com>
 *
 * Licensed under the GNU Lesser General Public License Version 2.1
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
  }
{$ifndef __PK_PROGRESS_H}
{$define __PK_PROGRESS_H}
(* error 
#define PK_TYPE_PROGRESS		(pk_progress_get_type ())
in define line 35 *)
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   


    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    { was #define dname def_expr }


    { padding for future expansion  }


function pk_progress_get_type:GType;cdecl;external pklib2;
function pk_progress_new:PPkProgress;cdecl;external pklib2;
procedure pk_progress_test(user_data:gpointer);cdecl;external pklib2;
    type
      PPkProgressType = Pointer;
      PkProgressType = (PK_PROGRESS_TYPE_PACKAGE_ID,PK_PROGRESS_TYPE_TRANSACTION_ID,
        PK_PROGRESS_TYPE_PERCENTAGE,PK_PROGRESS_TYPE_SUBPERCENTAGE,
        PK_PROGRESS_TYPE_ALLOW_CANCEL,PK_PROGRESS_TYPE_STATUS,
        PK_PROGRESS_TYPE_ROLE,PK_PROGRESS_TYPE_CALLER_ACTIVE,
        PK_PROGRESS_TYPE_ELAPSED_TIME,PK_PROGRESS_TYPE_REMAINING_TIME,
        PK_PROGRESS_TYPE_SPEED,PK_PROGRESS_TYPE_UID,
        PK_PROGRESS_TYPE_PACKAGE,PK_PROGRESS_TYPE_INVALID
        );

      PkProgressCallback = procedure (progress:PPkProgress; _type:PkProgressType; user_data:gpointer);cdecl;
(* Const before type ignored *)

function pk_progress_set_package_id(progress:PPkProgress; package_id:Pgchar):gboolean;cdecl;external pklib2;
(* Const before type ignored *)
function pk_progress_set_transaction_id(progress:PPkProgress; package_id:Pgchar):gboolean;cdecl;external pklib2;
function pk_progress_set_percentage(progress:PPkProgress; percentage:gint):gboolean;cdecl;external pklib2;
function pk_progress_set_subpercentage(progress:PPkProgress; subpercentage:gint):gboolean;cdecl;external pklib2;
function pk_progress_set_status(progress:PPkProgress; status:PkStatusEnum):gboolean;cdecl;external pklib2;
function pk_progress_set_role(progress:PPkProgress; role:PkRoleEnum):gboolean;cdecl;external pklib2;
function pk_progress_set_allow_cancel(progress:PPkProgress; allow_cancel:gboolean):gboolean;cdecl;external pklib2;
function pk_progress_set_caller_active(progress:PPkProgress; caller_active:gboolean):gboolean;cdecl;external pklib2;
function pk_progress_set_elapsed_time(progress:PPkProgress; elapsed_time:guint):gboolean;cdecl;external pklib2;
function pk_progress_set_remaining_time(progress:PPkProgress; remaining_time:guint):gboolean;cdecl;external pklib2;
function pk_progress_set_speed(progress:PPkProgress; speed:guint):gboolean;cdecl;external pklib2;
function pk_progress_set_uid(progress:PPkProgress; uid:guint):gboolean;cdecl;external pklib2;
function pk_progress_set_package(progress:PPkProgress; package:PPkPackage):gboolean;cdecl;external pklib2;
{$endif}
    { __PK_PROGRESS_H  }
