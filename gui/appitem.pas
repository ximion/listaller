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
  Classes, SysUtils, Graphics, Contnrs, LiApp;

type
  TAppListItem = class
  private
    bmp: TBitmap;
    FApp: TLiAppItem;
  public
    constructor Create;
    destructor Destroy; override;

    property AppInfo: TLiAppItem read FApp write FApp;
    property Icon: TBitmap read bmp write bmp;
  end;

  TAppItemList = class(TObjectList)
  protected
    function getItem(Index: Integer): TAppListItem; virtual;
    procedure setItem(Index: Integer; aItem: TAppListItem); virtual;
  public
    function Add(aItem: TAppListItem): Integer; virtual;
    function Remove(aItem: TAppListItem): Integer; virtual;
    function IndexOf(aItem: TAppListItem): Integer; virtual;
    procedure Insert(Index: Integer; aItem: TAppListItem); virtual;
    function First: TAppListItem; virtual;
    function Last: TAppListItem; virtual;
    property Items[index: Integer]: TAppListItem read getItem write setItem; default;
  end;

implementation

{ TAppListItem }

constructor TAppListItem.Create;
begin
  bmp := TBitmap.Create;
  FApp := TLiAppItem.Create;
end;

destructor TAppListItem.Destroy;
begin
  fApp.Free;
  bmp.Free;
  inherited;
end;

{ TAppItemList }

function TAppItemList.getItem(Index: Integer): TAppListItem;
begin
  Result := TAppListItem(inherited Items[Index]);
end;

procedure TAppItemList.setItem(Index: Integer; aItem: TAppListItem);
begin
  inherited Items[Index] := aItem;
end;

function TAppItemList.Add(aItem: TAppListItem): Integer;
begin
  Result := inherited Add(aItem);
end;

function TAppItemList.First: TAppListItem;
begin
  Result := TAppListItem(inherited First());
end;

function TAppItemList.IndexOf(aItem: TAppListItem): Integer;
begin
  Result := inherited IndexOf(aItem);
end;

procedure TAppItemList.Insert(Index: Integer; aItem: TAppListItem);
begin
  inherited Insert(Index, aItem);
end;

function TAppItemList.Last: TAppListItem;
begin
  Result := TAppListItem(inherited Last());
end;

function TAppItemList.Remove(aItem: TAppListItem): Integer;
begin
  Result := inherited Remove(aItem);
end;

end.

