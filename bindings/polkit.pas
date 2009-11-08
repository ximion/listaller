{ Copyright (C) 2009 Matthias Klumpp

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
//** Some basic PoilcyKit GLib functions
unit polkit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glib2, gext;

type
 PPolkitAuthorizationResult = Pointer;
 PPolkitAuthority = Pointer;
 PPolkitSubject = Pointer;
 PPolkitDetails = Pointer;

const
 libpolkit = 'libpolkit-gobject-1.so';
 POLKIT_CHECK_AUTHORIZATION_FLAGS_NONE = $0000;
 POLKIT_CHECK_AUTHORIZATION_FLAGS_ALLOW_USER_INTERACTION = $0001;

function  polkit_authority_get: PPolkitAuthority;cdecl;external libpolkit;
function  polkit_authority_check_authorization_finish(authority: PPolkitAuthority;
                                                      res: PGAsyncResult;
                                                      error: PPGError): PPolkitAuthorizationResult;cdecl;external libpolkit;
function  polkit_authorization_result_get_is_authorized(result: PPolkitAuthorizationResult): GBoolean;cdecl;external libpolkit;
function  polkit_authorization_result_get_is_challenge(result: PPolkitAuthorizationResult): GBoolean;cdecl;external libpolkit;
procedure polkit_authority_check_authorization(authority: PPolkitAuthority;
                                               subject: PPolkitSubject;action_id: PGChar;
                                               details: PPolkitDetails;
                                               flags: Integer;
                                               cancellable: PGCancellable;
                                               callback: TGAsyncReadyCallback;
                                               user_data: GPointer);cdecl;external libpolkit;

function  polkit_system_bus_name_new(name: PGChar): PPolkitSubject;cdecl;external libpolkit;
function  polkit_system_bus_name_get_process_sync(system_bus_name: Pointer;
                                                                  cancellable: PGCancellable;
                                                                  error: PPGError): PPolkitSubject;cdecl;external libpolkit;

implementation
end.

