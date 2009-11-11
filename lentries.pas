{ Copyright (C) 2008-2009 Listaller Project

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
//** The global-used Listaller list-entry classes
unit lentries;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, StdCtrls, ExtCtrls, Buttons, Controls, iconloader,
 trstrings, LiBasic, liTypes;

type
//** One entry of Listaller's visual software lists
TListEntry = class(TPanel) //Helper class / a software entry
protected
AppLabel: TLabel;
DescLabel: TLabel;
Vlabel: TLabel;
MnLabel: TLabel;
Graphic: TImage;
ID: Integer;
sAI: TAppInfo;
private
 procedure SetAppName(s: String);
 function GetAppName: String;
 procedure SetAppDesc(s: String);
 function GetAppDesc: String;
 procedure SetAppMaintainer(s: String);
 function GetAppMaintainer: String;
 procedure SetAppVersion(s: String);
 function GetAppVersion: String;
public
UnButton: TBitBtn;
//** Name of the application
property AppName: String read GetAppName write SetAppName;
//** Description of the application
property AppDesc: String read GetAppDesc write SetAppDesc;
//** Name of the maintainer(s)
property AppMn: String read GetAppMaintainer write SetAppMaintainer;
//** Version of the application
property AppVersion: String read GetAppVersion write SetAppVersion;
//** Software removal ID
property appInfo: TAppInfo read sAI write sAI;
//** Load all data from AppInfo record
procedure LoadFromAppInfo(ai: TAppInfo);
//** Set an image for the entry
procedure SetImage(AImage: String);
//** Constructor
constructor Create(AOwner: TComponent); override;
//** Destructor
destructor Destroy; override;
end;

implementation

{ TListEntry }
constructor TListEntry.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
id:=-1;
self.Align:=alTop;
self.Height:=80;
self.BevelOuter:=BVNone;
self.BevelInner:=bvLowered;

AppLabel:=TLabel.Create(nil);
with AppLabel do
begin
Parent:=Self;
AutoSize:=true;
Caption:='<appname>';
Font.Size:=16;
Anchors:=[akLeft,akTop];
Top:=10;
Left:=84;
end;

mnLabel:=TLabel.Create(nil);
with mnLabel do
begin
Parent:=Self;
AutoSize:=true;
Anchors:=[akLeft,akTop];
Top:=42;
Left:=96;
Caption:='<creator>';
end;

DescLabel:=TLabel.Create(nil);
with DescLabel do
begin
Parent:=Self;
AutoSize:=true;
Anchors:=[akLeft,akTop];
Top:=30;
Left:=90;
Caption:='<description>';
end;

vLabel:=TLabel.Create(nil);
with vLabel do
begin
Parent:=Self;
AutoSize:=true;
Anchors:=[akBottom,akRight];
Top:=self.Height-55;
Left:=self.Width-125;

Caption:='';
end;

Graphic:=TImage.Create(nil);
with Graphic do
begin
Parent:=self;
Width:=64;
Height:=64;
Top:=8;
Left:=8;
Center:=true;
Anchors:=[akLeft];
Stretch:=true;
Proportional:=true;
Picture.LoadFromFile(GetDataFile('graphics/spackage.png'));
end;

UnButton:=TBitBtn.Create(nil);
UnButton.Parent:=self;
with UnButton do
begin
Height:=24;
Width:=120;
Caption:=rsUninstall;
Anchors:=[akBottom,akRight];
Top:=self.Height-40;
Left:=self.Width-132;
LoadStockPixmap(STOCK_DELETE,ICON_SIZE_BUTTON,Glyph);
end;
end;

destructor TListEntry.Destroy;
begin
AppLabel.Free;
UnButton.free;
Graphic.free;
mnLabel.Free;
DescLabel.Free;
vLabel.Free;

StrDispose(sAI.Name);
StrDispose(sAI.Author);
StrDispose(sAI.UId);
StrDispose(sAI.ShortDesc);
StrDispose(sAI.Icon);
StrDispose(sAI.Version);
inherited Destroy;
end;

procedure TListEntry.SetAppName(s: String);
begin
 AppLabel.Caption:=s;
end;

function TListEntry.GetAppName: String;
begin
 Result:=AppLabel.Caption;
end;

procedure TListEntry.SetAppDesc(s: String);
begin
 DescLabel.Caption:=s;
end;

function TListEntry.GetAppDesc: String;
begin
 Result:=DescLabel.Caption;
end;

procedure TListEntry.SetAppMaintainer(s: String);
begin
 MNLabel.Caption:=s;
end;

function TListEntry.GetAppMaintainer: String;
begin
 Result:=MNLabel.Caption;
end;

procedure TListEntry.SetAppVersion(s: String);
begin
 VLabel.Caption:=s;
end;

function TListEntry.GetAppVersion: String;
begin
 Result:=VLabel.Caption;
end;

procedure TListEntry.SetImage(AImage: String);
begin
Graphic.Picture.LoadFromFile(AImage);
end;

procedure TListEntry.LoadFromAppInfo(ai: TAppInfo);
begin
 AppName:=ai.Name;
 AppDesc:=ai.ShortDesc;
 AppVersion:=ai.Version;
 AppMn:=ai.Author;
 sAI.Name:=StrAlloc(StrLen(ai.Name)+1);
 StrCopy(sAI.Name, ai.Name);
 sAI.Author:=StrAlloc(StrLen(ai.Author)+1);
 StrCopy(sAI.Author, ai.Author);
 sAI.Icon:=StrAlloc(StrLen(ai.Icon)+1);
 StrCopy(sAI.Icon, ai.Icon);
 sAI.ShortDesc:=StrAlloc(StrLen(ai.ShortDesc)+1);
 StrCopy(sAI.ShortDesc, ai.ShortDesc);
 sAI.UId:=StrAlloc(StrLen(ai.UId)+1);
 StrCopy(sAI.UId, ai.UId);
 sAI.Version:=StrAlloc(StrLen(ai.Version)+1);
 StrCopy(sAI.Version, ai.Version);
 sAI.Group:=ai.Group;

 if FileExists(ai.Icon) then
   SetImage(ai.Icon);
end;

end.
