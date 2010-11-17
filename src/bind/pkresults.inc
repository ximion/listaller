
(* Part of Listaller's PackageKit bindings
 *
 * (c) 2010 Matthias Klumpp
 *
 * Licensed under the same license as the original header.
 * see copyright notice below for more information.
 *
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
{$ifndef __PK_RESULTS_H}
{$define __PK_RESULTS_H}
(* error 
#define PK_TYPE_RESULTS		(pk_results_get_type ())
in define line 48 *)
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


    { padding for future expansion  }


function pk_results_get_type:GType;cdecl;external pklib2;
function pk_results_new:PPkResults;cdecl;external pklib2;
procedure pk_results_test(user_data:gpointer);cdecl;external pklib2;
    { set  }
function pk_results_set_exit_code(results:PPkResults; exit_enum:PkExitEnum):gboolean;cdecl;external pklib2;
function pk_results_set_error_code(results:PPkResults; item:PPkError):gboolean;cdecl;external pklib2;
    { add  }
function pk_results_add_package(results:PPkResults; item:PPkPackage):gboolean;cdecl;external pklib2;
function pk_results_add_details(results:PPkResults; item:PPkDetails):gboolean;cdecl;external pklib2;
function pk_results_add_update_detail(results:PPkResults; item:PPkUpdateDetail):gboolean;cdecl;external pklib2;
function pk_results_add_category(results:PPkResults; item:PPkCategory):gboolean;cdecl;external pklib2;
function pk_results_add_distro_upgrade(results:PPkResults; item:PPkDistroUpgrade):gboolean;cdecl;external pklib2;
function pk_results_add_require_restart(results:PPkResults; item:PPkRequireRestart):gboolean;cdecl;external pklib2;
function pk_results_add_transaction(results:PPkResults; item:PPkTransactionPast):gboolean;cdecl;external pklib2;
function pk_results_add_files(results:PPkResults; item:PPkFiles):gboolean;cdecl;external pklib2;
function pk_results_add_repo_signature_required(results:PPkResults; item:PPkRepoSignatureRequired):gboolean;cdecl;external pklib2;
function pk_results_add_eula_required(results:PPkResults; item:PPkEulaRequired):gboolean;cdecl;external pklib2;
function pk_results_add_media_change_required(results:PPkResults; item:PPkMediaChangeRequired):gboolean;cdecl;external pklib2;
function pk_results_add_repo_detail(results:PPkResults; item:PPkRepoDetail):gboolean;cdecl;external pklib2;
function pk_results_add_message(results:PPkResults; item:PPkMessage):gboolean;cdecl;external pklib2;
    { get single data  }
function pk_results_get_exit_code(results:PPkResults):PkExitEnum;cdecl;external pklib2;
function pk_results_get_package_sack(results:PPkResults):PPkPackageSack;cdecl;external pklib2;
function pk_results_get_error_code(results:PPkResults):PPkError;cdecl;external pklib2;
function pk_results_get_require_restart_worst(results:PPkResults):PkRestartEnum;cdecl;external pklib2;
    { get array objects  }
function pk_results_get_package_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_details_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_update_detail_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_category_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_distro_upgrade_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_require_restart_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_transaction_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_files_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_repo_signature_required_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_eula_required_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_media_change_required_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_repo_detail_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
function pk_results_get_message_array(results:PPkResults):PGPtrArray;cdecl;external pklib2;
{$endif}
    { __PK_RESULTS_H  }
