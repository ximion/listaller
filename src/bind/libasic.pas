{ Copyright (C) 2010 Matthias Klumpp

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
//** LibInstaller basic functions
unit libasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LiTypes;

{@Begin:BaseFunctions}

function li_new_stringlist: Pointer;cdecl;external liblistaller;
function li_free_stringlist(lst: PStringList): Boolean;cdecl;external liblistaller;
function li_stringlist_read_line(lst: PStringList;ln: Integer): PChar;cdecl;external liblistaller;
function li_stringlist_write_line(lst: PStringList;ln: Integer;val: PChar): Boolean;cdecl;external liblistaller;

{@End:BaseFunctions}

implementation
end.
