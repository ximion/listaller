{ Copyright (C) 2008-2009 Matthias Klumpp

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
//** Find desktop metadata about a package (PackageKit binding)
unit pkdesktop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, packagekit, glib2;

type
TGFunc = procedure(data: GPointer;user_data: GPointer);

procedure g_ptr_array_foreach(ar: Pointer;func: TGFunc;user_data: GPointer);cdecl;external gliblib name 'g_ptr_array_foreach';
function pk_desktop_new: Pointer;cdecl;external pklib2 name 'pk_desktop_new';
function pk_desktop_open_database(desktop: Pointer;error: PPGError): GBoolean;cdecl;external pklib2 name 'pk_desktop_open_database';
function pk_desktop_get_files_for_package(desktop: Pointer;pkg: PGChar;error: PPGError): Pointer;cdecl;external pklib2 name 'pk_desktop_get_files_for_package';
function pk_desktop_get_package_for_file(desktop: Pointer;filename: GChar;error: PPGError): GChar;cdecl;external; pklib2 name 'pk_desktop_get_package_for_file';

implementation

end.

