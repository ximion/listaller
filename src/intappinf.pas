{ Copyright (C) 2010 Listaller Project

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** Small helper object to store application information
unit intappinf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, liTypes, Graphics;

type
 TIntAppInfo = class
  private
   bmp: TBitmap;
   FName: String;
   FSDesc: String;
   FAuthor: String;
   FGroup: TGroupType;
  public
   constructor Create;
   destructor  Destroy;override;

   property Name: String read FName write FName;
   property SDesc: String read FSDesc write FSDesc;
   property Author: String read FAuthor write FAuthor;
   property Group: TGroupType read FGroup write FGroup;
   property Icon: TBitmap read bmp write bmp;
 end;


implementation

{ TIntAppInfo }

constructor TIntAppInfo.Create;
begin
 inherited;
 bmp:=TBitmap.Create;
end;

destructor TIntAppInfo.Destroy;
begin
 bmp.Free;
 inherited;
end;

end.

