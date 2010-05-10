{ Copyright (C) 2009-2010 Matthias Klumpp

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
//PkClient interface bindings

const
  pklib2 = 'libpackagekit-glib2.so';

//Bitfield
function pk_filter_bitfield_from_string(roles: PChar): GuInt64; cdecl;external pklib2 Name 'pk_filter_bitfield_from_text';
//Package obj conversion
function pk_package_obj_to_string(obj: GPointer): PChar; cdecl; external pklib2;
function pk_package_obj_get_id(obj: GPointer): PPkPackageID; cdecl; external pklib2;
//Actions
function pk_client_new: Pointer; cdecl; external pklib2;
procedure pk_client_resolve_async(client: PPkClient; filters: GuInt64;
                                           packages: PPChar; cancellable: PGObject;
                                           progress_callback: TPkProgressCallback;
                                           progress_user_data: GPointer;
                                           callback_ready: GAsyncReadyCallback;
                                           user_data: GPointer);
                                           cdecl; external pklib2;
procedure pk_client_install_packages_async(client: PPkClient; only_trusted: GBoolean;
                                                   package_ids: PPGChar;
                                                   cancellable: PGObject;
                                                   progress_callback:
                                                   TPkProgressCallback; progress_user_data: GPointer;
                                                   callback_ready: GAsyncReadyCallback;
                                                   user_data: GPointer);
                                                   cdecl; external pklib2;
procedure pk_client_get_requires_async(client: PPkClient; filters: GuInt64;
                                               package_ids: PPGChar; recursive: GBoolean;
                                               cancellable: PGObject;
                                               progress_callback: TPkProgressCallback;
                                               progress_user_data: GPointer;
                                               callback_ready: GAsyncReadyCallback;
                                               user_data: GPointer);
                                               cdecl; external pklib2;
procedure pk_client_remove_packages_async(client: PPkClient; package_ids: PPGChar;
                                                  allow_deps: GBoolean;
                                                  autoremove: GBoolean; cancellable:
                                                  PGObject; progress_callback:
                                                  TPkProgressCallback; progress_user_data: GPointer;
                                                  callback_ready: GAsyncReadyCallback;
                                                  user_data: GPointer);
                                                  cdecl; external pklib2;
procedure pk_client_search_files_async(client: PPkClient; filters: PkBitfield;
                                               values: PPGChar; cancellable:
                                               PGCancellable; progress_callback:
                                               TPKProgressCallback; progress_user_data: GPointer;
                                               callback_ready: GAsyncReadyCallback;
                                               user_data: GPointer);
                                               cdecl; external pklib2;
procedure pk_client_install_files_async(client: PPkClient; only_trusted: GBoolean;
                                                files: PPGChar; cancellable: PGObject;
                                                progress_callback: TPkProgressCallback;
                                                progress_user_data: GPointer;
                                                callback_ready: GAsyncReadyCallback;
                                                user_data: GPointer);
                                                cdecl; external pklib2;
procedure pk_client_get_details_async(client: PPkClient; package_ids: PPGChar;
                                              cancellable: PGCancellable;
                                              progress_callback: TPkProgressCallback;
                                              progress_user_data: GPointer;
                                              callback_ready: GAsyncReadyCallback;
                                              user_data: GPointer); cdecl; external pklib2;
function pk_client_generic_finish(client: PPkClient; res: Pointer;error: PPGError): Pointer; cdecl; external pklib2;
function pk_results_get_exit_code(results: PPkResults): PkExitEnum;cdecl; external pklib2;
function pk_results_get_error_code(results: PPkResults): PPkError; cdecl; external pklib2;
function pk_results_get_details_array(results: PPkResults): PGPtrArray;cdecl; external pklib2;
function pk_client_error_quark(): GQuark; cdecl; external pklib2;
function pk_client_get_type(): GType; cdecl; external pklib2;

function pk_results_get_package_sack(results: PPkResults): PPkPackageSack;cdecl;external pklib2;
function pk_package_sack_find_by_id(sack: PPkPackageSack;package_id: PGChar): PPkPackage;cdecl;external pklib2;

