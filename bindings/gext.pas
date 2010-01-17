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
//** Contains some additional GLib functions
unit gext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, glib2;

type

//** A GQuark type
GQuark = Integer;

PGAsyncResult = Pointer;
PGCancellable = PGObject;

TGAsyncReadyCallback = procedure(source_object: PGObject;res: Pointer;user_data: GPointer);
PGAsyncReadyCallback = ^TGAsyncReadyCallback;

//** Intialize the GType
procedure InitializeGType;

//GLib-GCancellable
function  g_cancellable_new: Pointer;cdecl;external gliblib name 'g_cancellable_new';
function  g_cancellable_is_cancelled(cancellable: Pointer): GBoolean;cdecl;external gliblib;
function  g_cancellable_set_error_if_cancelled(cancellable: Pointer;error: PPGError): GBoolean;cdecl;external gliblib;
procedure g_cancellable_cancel(cancellable: PGCancellable);cdecl;external gliblib;

implementation

procedure InitializeGType;
begin
 //Needed for use with Qt4
 {$IFNDEF LCLGTK2}
  g_type_init();
 {$ENDIF}
end;

initialization
 InitializeGType;

end.

