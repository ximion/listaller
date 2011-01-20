
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
{$ifndef __PK_ERROR_CODE_H}
{$define __PK_ERROR_CODE_H}
(* error 
#define PK_TYPE_ERROR_CODE		(pk_error_get_type ())
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


    { padding for future expansion  }


function pk_error_get_type:GType;cdecl;external pklib2;
function pk_error_new:PPkError;cdecl;external pklib2;
function pk_error_get_code(error_code:PPkError):PkErrorEnum;cdecl;external pklib2;
(* Const before type ignored *)
function pk_error_get_details(error_code:PPkError):Pgchar;cdecl;external pklib2;
{$endif}
    { __PK_ERROR_CODE_H  }
