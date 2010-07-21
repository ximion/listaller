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
//** Small list object to list information about applications (used by GUI applist)
unit appitem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LiTypes, Graphics, Contnrs;

type
 TAppInfoItem = class
  private
   bmp: TBitmap;
   FName: String;
   FIconPath: String;
   FSDesc: String;
   FAuthor: String;
   FCat: AppCategory;
   FVersion: String;
   FUId: String;
  public
   constructor Create;
   destructor  Destroy;override;

   property Name: String read FName write FName;
   property SDesc: String read FSDesc write FSDesc;
   property Author: String read FAuthor write FAuthor;
   property Category: AppCategory read FCat write FCat;
   property Icon: TBitmap read bmp write bmp;
   property Version: String read FVersion write FVersion;
   property UId: String read FUId write FUId;
   property IconPath: String read FIconPath write FIconPath;
 end;

 TAppItemList = class(TObjectList)
  protected
    function getItem(Index: Integer): TAppInfoItem; virtual;
    procedure setItem(Index: Integer; aItem: TAppInfoItem);virtual;
  public
    function Add(aItem: TAppInfoItem): Integer;virtual;
    function Remove(aItem: TAppInfoItem): Integer;virtual;
    function IndexOf(aItem: TAppInfoItem): Integer;virtual;
    procedure Insert(Index: Integer; aItem: TAppInfoItem);virtual;
    function First: TAppInfoItem;virtual;
    function Last: TAppInfoItem;virtual;
    property Items[index: Integer]: TAppInfoItem read getItem write setItem;default;
  end;

implementation

{ TAppInfoItem }

constructor TAppInfoItem.Create;
begin
 bmp:=TBitmap.Create;
end;

destructor TAppInfoItem.Destroy;
begin
 bmp.Free;
 inherited;
end;

{ TAppItemList }

function TAppItemList.getItem(Index: Integer): TAppInfoItem;
begin
    Result := TAppInfoItem(inherited Items[Index]);
end;

procedure TAppItemList.setItem(Index: Integer; aItem: TAppInfoItem);
begin
 inherited Items[Index] := aItem;
end;

function TAppItemList.Add(aItem: TAppInfoItem): Integer;
begin
 Result := inherited Add(aItem);
end;

function TAppItemList.First: TAppInfoItem;
begin
 Result := TAppInfoItem(inherited First());
end;

function TAppItemList.IndexOf(aItem: TAppInfoItem): Integer;
begin
 Result := inherited IndexOf(aItem);
end;

procedure TAppItemList.Insert(Index: Integer; aItem: TAppInfoItem);
begin
 inherited Insert(Index, aItem);
end;

function TAppItemList.Last: TAppInfoItem;
begin
 Result := TAppInfoItem(inherited Last());
end;

function TAppItemList.Remove(aItem: TAppInfoItem): Integer;
begin
 Result := inherited Remove(aItem);
end;

end.

