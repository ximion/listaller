
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
{$ifndef __PK_PACKAGE_SACK_H}
{$define __PK_PACKAGE_SACK_H}
(* error 
#define PK_TYPE_PACKAGE_SACK		(pk_package_sack_get_type ())
in define line 36 *)
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
        { return type might be wrong }

    type

    { signals  }
    { padding for future expansion  }


       PPkPackageSackSortType = Pointer;
       PkPackageSackSortType = (PK_PACKAGE_SACK_SORT_TYPE_NAME,PK_PACKAGE_SACK_SORT_TYPE_INFO,
         PK_PACKAGE_SACK_SORT_TYPE_PACKAGE_ID,
         PK_PACKAGE_SACK_SORT_TYPE_SUMMARY,
         PK_PACKAGE_SACK_SORT_TYPE_LAST);

function pk_package_sack_get_type:GType;cdecl;external pklib2;
function pk_package_sack_new:PPkPackageSack;cdecl;external pklib2;
procedure pk_package_sack_test(user_data:gpointer);cdecl;external pklib2;
    type

       PkPackageSackFilterFunc = function (package:PPkPackage; user_data:gpointer):gboolean;cdecl;
    { managing the array  }

procedure pk_package_sack_clear(sack:PPkPackageSack);cdecl;external pklib2;
function pk_package_sack_get_ids(sack:PPkPackageSack):PPGChar;cdecl;external pklib2;
function pk_package_sack_get_array(sack:PPkPackageSack):PGPtrArray;cdecl;external pklib2;
procedure pk_package_sack_sort(sack:PPkPackageSack; _type:PkPackageSackSortType);cdecl;external pklib2;
function pk_package_sack_add_package(sack:PPkPackageSack; package:PPkPackage):gboolean;cdecl;external pklib2;
(* Const before type ignored *)
function pk_package_sack_add_package_by_id(sack:PPkPackageSack; package_id:Pgchar; error:PPGError):gboolean;cdecl;external pklib2;
function pk_package_sack_remove_package(sack:PPkPackageSack; package:PPkPackage):gboolean;cdecl;external pklib2;
(* Const before type ignored *)
function pk_package_sack_remove_package_by_id(sack:PPkPackageSack; package_id:Pgchar):gboolean;cdecl;external pklib2;
function pk_package_sack_remove_by_filter(sack:PPkPackageSack; filter_cb:PkPackageSackFilterFunc; user_data:gpointer):gboolean;cdecl;external pklib2;
(* Const before type ignored *)
function pk_package_sack_find_by_id(sack:PPkPackageSack; package_id:Pgchar):PPkPackage;cdecl;external pklib2;
function pk_package_sack_filter_by_info(sack:PPkPackageSack; info:PkInfoEnum):PPkPackageSack;cdecl;external pklib2;
function pk_package_sack_filter(sack:PPkPackageSack; filter_cb:PkPackageSackFilterFunc; user_data:gpointer):PPkPackageSack;cdecl;external pklib2;
function pk_package_sack_get_total_bytes(sack:PPkPackageSack):guint64;cdecl;external pklib2;
function pk_package_sack_merge_generic_finish(sack:PPkPackageSack; res:PGAsyncResult; error:PPGError):gboolean;cdecl;external pklib2;
    { merging in data to the array using Resolve()  }
procedure pk_package_sack_resolve_async(sack:PPkPackageSack; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; callback:GAsyncReadyCallback; 
                user_data:gpointer);cdecl;external pklib2;
    { merging in data to the array using Details()  }
procedure pk_package_sack_get_details_async(sack:PPkPackageSack; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; callback:GAsyncReadyCallback; 
                user_data:gpointer);cdecl;external pklib2;
    { merging in data to the array using UpdateDetail()  }
procedure pk_package_sack_get_update_detail_async(sack:PPkPackageSack; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; callback:GAsyncReadyCallback; 
                user_data:gpointer);cdecl;external pklib2;
{$endif}
    { __PK_PACKAGE_SACK_H  }
