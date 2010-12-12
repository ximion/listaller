
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
{$ifndef __PK_CLIENT_H}
{$define __PK_CLIENT_H}
(* error 
#define PK_TYPE_CLIENT		(pk_client_get_type ())
in define line 39 *)
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

    { was #define dname def_expr }
        { return type might be wrong }

    {*
     * PkClientError:
     * @PK_CLIENT_ERROR_FAILED: the transaction failed for an unknown reason
     * @PK_CLIENT_ERROR_NO_TID: the transaction id was not pre-allocated (internal error)
     * @PK_CLIENT_ERROR_ALREADY_TID: the transaction id has already been used (internal error)
     * @PK_CLIENT_ERROR_ROLE_UNKNOWN: the role was not set (internal error)
     * @PK_CLIENT_ERROR_INVALID_INPUT: the package_id is invalid
     * @PK_CLIENT_ERROR_INVALID_FILE: the file is invalid
     * @PK_CLIENT_ERROR_NOT_SUPPORTED: the action is not supported
     * @PK_CLIENT_ERROR_DECLINED_SIMULATION: the simulation was declined by the user
     *
     * Errors that can be thrown
      }
    { we define this so we can punt the PkErrorEnums here  }
    type

       PPkClientError = Pointer;
       PkClientError = (PK_CLIENT_ERROR_FAILED,PK_CLIENT_ERROR_FAILED_AUTH,
         PK_CLIENT_ERROR_NO_TID,PK_CLIENT_ERROR_ALREADY_TID,
         PK_CLIENT_ERROR_ROLE_UNKNOWN,PK_CLIENT_ERROR_CANNOT_START_DAEMON,
         PK_CLIENT_ERROR_INVALID_INPUT,PK_CLIENT_ERROR_INVALID_FILE,
         PK_CLIENT_ERROR_NOT_SUPPORTED,PK_CLIENT_ERROR_DECLINED_SIMULATION,
         PK_CLIENT_ERROR_LAST);

    { signals  }
    { padding for future expansion  }


function pk_client_error_quark:GQuark;cdecl;external pklib2;
function pk_client_get_type:GType;cdecl;external pklib2;
function pk_client_new:PPkClient;cdecl;external pklib2;
procedure pk_client_test(user_data:gpointer);cdecl;external pklib2;
    { get transaction results  }
function pk_client_generic_finish(client:PPkClient; res:PGAsyncResult; error:PPGError):PPkResults;cdecl;external pklib2;
procedure pk_client_resolve_async(client:PPkClient; filters:PkBitfield; packages:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_search_names_async(client:PPkClient; filters:PkBitfield; values:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_search_details_async(client:PPkClient; filters:PkBitfield; values:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_search_groups_async(client:PPkClient; filters:PkBitfield; values:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_search_files_async(client:PPkClient; filters:PkBitfield; values:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_details_async(client:PPkClient; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_update_detail_async(client:PPkClient; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
procedure pk_client_download_packages_async(client:PPkClient; package_ids:PPgchar; directory:Pgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_updates_async(client:PPkClient; filters:PkBitfield; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_update_system_async(client:PPkClient; only_trusted:gboolean; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_depends_async(client:PPkClient; filters:PkBitfield; package_ids:PPgchar; recursive:gboolean; cancellable:PGCancellable; 
                progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_packages_async(client:PPkClient; filters:PkBitfield; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_requires_async(client:PPkClient; filters:PkBitfield; package_ids:PPgchar; recursive:gboolean; cancellable:PGCancellable; 
                progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_what_provides_async(client:PPkClient; filters:PkBitfield; provides:PkProvidesEnum; values:PPgchar; cancellable:PGCancellable; 
                progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_distro_upgrades_async(client:PPkClient; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; 
                user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_files_async(client:PPkClient; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_categories_async(client:PPkClient; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; 
                user_data:gpointer);cdecl;external pklib2;
procedure pk_client_remove_packages_async(client:PPkClient; package_ids:PPgchar; allow_deps:gboolean; autoremove:gboolean; cancellable:PGCancellable; 
                progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_refresh_cache_async(client:PPkClient; force:gboolean; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_install_packages_async(client:PPkClient; only_trusted:gboolean; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
(* Const before type ignored *)
procedure pk_client_install_signature_async(client:PPkClient; _type:PkSigTypeEnum; key_id:Pgchar; package_id:Pgchar; cancellable:PGCancellable; 
                progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_update_packages_async(client:PPkClient; only_trusted:gboolean; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_install_files_async(client:PPkClient; only_trusted:gboolean; files:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
procedure pk_client_accept_eula_async(client:PPkClient; eula_id:Pgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
procedure pk_client_rollback_async(client:PPkClient; transaction_id:Pgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_get_repo_list_async(client:PPkClient; filters:PkBitfield; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
procedure pk_client_repo_enable_async(client:PPkClient; repo_id:Pgchar; enabled:gboolean; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
procedure pk_client_repo_set_data_async(client:PPkClient; repo_id:Pgchar; parameter:Pgchar; value:Pgchar; cancellable:PGCancellable; 
                progress_callback:PkProgressCallback; progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_simulate_install_files_async(client:PPkClient; files:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_simulate_install_packages_async(client:PPkClient; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_simulate_remove_packages_async(client:PPkClient; package_ids:PPgchar; autoremove:gboolean; cancellable:PGCancellable; progress_callback:PkProgressCallback; 
                progress_user_data:gpointer; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
procedure pk_client_simulate_update_packages_async(client:PPkClient; package_ids:PPgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
(* Const before type ignored *)
procedure pk_client_adopt_async(client:PPkClient; transaction_id:Pgchar; cancellable:PGCancellable; progress_callback:PkProgressCallback; progress_user_data:gpointer; 
                callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
function pk_client_get_progress_finish(client:PPkClient; res:PGAsyncResult; error:PPGError):PPkProgress;cdecl;external pklib2;
(* Const before type ignored *)
procedure pk_client_get_progress_async(client:PPkClient; transaction_id:Pgchar; cancellable:PGCancellable; callback_ready:GAsyncReadyCallback; user_data:gpointer);cdecl;external pklib2;
    { getters and setters  }
(* Const before type ignored *)
procedure pk_client_set_locale(client:PPkClient; locale:Pgchar);cdecl;external pklib2;
(* Const before type ignored *)
function pk_client_get_locale(client:PPkClient):Pgchar;cdecl;external pklib2;
procedure pk_client_set_background(client:PPkClient; background:gboolean);cdecl;external pklib2;
function pk_client_get_background(client:PPkClient):gboolean;cdecl;external pklib2;
procedure pk_client_set_interactive(client:PPkClient; interactive:gboolean);cdecl;external pklib2;
function pk_client_get_interactive(client:PPkClient):gboolean;cdecl;external pklib2;
function pk_client_get_idle(client:PPkClient):gboolean;cdecl;external pklib2;
procedure pk_client_set_cache_age(client:PPkClient; cache_age:guint);cdecl;external pklib2;
{$endif}
    { __PK_CLIENT_H  }
