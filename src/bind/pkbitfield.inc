
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
 * Copyright (C) 2007-2008 Richard Hughes <richard@hughsie.com>
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
{$ifndef __PK_BITFIELD_H}
{$define __PK_BITFIELD_H}
(* error 
typedef guint64 PkBitfield;
in declaration at line 33 *)

    { convenience functions as it's easy to forget the bitwise operators  }
(* error 
#define pk_bitfield_add(bitfield,enum)		do { ((bitfield) |= (pk_bitfield_value(enum))); } while (0)
in declaration at line 37 *)
(* error 
#define pk_bitfield_add(bitfield,enum)		do { ((bitfield) |= (pk_bitfield_value(enum))); } while (0)
in declaration at line 38 *)
(* error 
#define pk_bitfield_remove(bitfield,enum)	do { ((bitfield) &= ~(pk_bitfield_value(enum))); } while (0)
in declaration at line 39 *)
(* error 
#define pk_bitfield_invert(bitfield,enum)	do { ((bitfield) ^= (pk_bitfield_value(enum))); } while (0)
in define line 40 *)
(* error 
#define pk_bitfield_value(enum)			((PkBitfield) 1 << (enum))
in define line 41 *)

function pk_bitfield_contain_priority(values:PkBitfield; value:gint; args:array of const):gint;cdecl;external pklib2;
function pk_bitfield_contain_priority(values:PkBitfield; value:gint):gint;cdecl;external pklib2;
function pk_bitfield_from_enums(value:gint; args:array of const):PkBitfield;cdecl;external pklib2;
function pk_bitfield_from_enums(value:gint):PkBitfield;cdecl;external pklib2;
(* Const before type ignored *)
function pk_role_bitfield_from_string(roles:Pgchar):PkBitfield;cdecl;external pklib2;
function pk_role_bitfield_to_string(roles:PkBitfield):Pgchar;cdecl;external pklib2;
(* Const before type ignored *)
function pk_group_bitfield_from_string(groups:Pgchar):PkBitfield;cdecl;external pklib2;
function pk_group_bitfield_to_string(groups:PkBitfield):Pgchar;cdecl;external pklib2;
(* Const before type ignored *)
function pk_filter_bitfield_from_string(filters:Pgchar):PkBitfield;cdecl;external pklib2;
function pk_filter_bitfield_to_string(filters:PkBitfield):Pgchar;cdecl;external pklib2;
procedure pk_bitfield_test(user_data:gpointer);cdecl;external pklib2;
{$endif}
    { __PK_BITFIELD_H  }
