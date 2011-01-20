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
//** LibInstaller LiAppItem related functions
unit liappitem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LiTypes;

{@Begin:AppItem}

function li_appitem_new(): LiAppItem; cdecl;external liblistaller;
function li_appitem_new_from_appid(appID: PChar): LiAppItem; cdecl;external liblistaller;
function li_appitem_name(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_id(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_version(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_summary(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_author(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_publisher(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_iconname(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_categories(item: LiAppItem): PChar; cdecl;external liblistaller;
function li_appitem_timestamp(item: LiAppItem): Double; cdecl;external liblistaller;
function li_appitem_dependencies(item: LiAppItem): PChar; cdecl;external liblistaller;

{@End:AppItem}

implementation
end.
