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
//** Contains a nice list class to display installed apps
//** This is a rewrite of TListEntry
unit applist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, StdCtrls, Graphics, Controls,
  LCLType, Buttons, IconLoader, liBasic, IntAppInf, liTypes,
  ExtCtrls, Contnrs;

type

 { TAppListView }

 TAppListView = class(TCustomListBox)
 private
  btn: TBitBtn;
  aList: TObjectList;
  procedure AppListViewDrawItem(Control: TWinControl; Index: Integer;
     ARect: TRect; State: TOwnerDrawState);
  procedure CustomDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
            State: TOwnerDrawState);
 protected
 public
  constructor Create(TheOwner: TComponent); override;
  destructor  Destroy; override;

  procedure ItemFromAppInfo(ai: TAppInfo);
 end;

implementation

{ TAppListView }

constructor TAppListView.Create(TheOwner: TComponent);
var pic: TPicture;
begin
 inherited;
 Style:=lbOwnerDrawVariable;
 ItemHeight:=48;
 OnDrawItem:=@AppListViewDrawItem;

 aList:=TObjectList.Create(true);

 btn:=TBitBtn.Create(self);
 btn.Parent:=self;
 btn.Width:=40;
 btn.Height:=40;
 btn.Visible:=false;
 LoadStockPixmap(STOCK_DELETE,ICON_SIZE_BUTTON,btn.Glyph);
end;

destructor TAppListView.Destroy;
begin
 btn.Free;
 aList.Free;
 inherited;
end;

procedure TAppListView.AppListViewDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  CustomDrawItem(Control, Index, ARect, State);
end;

procedure TAppListView.CustomDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  bmp: TBitmap;
  TopDif: Integer;
begin
  bmp := TBitmap.Create;
  try
      //bmp.Transparent := True;
      if (Index>0) and (Index<Items.Count) then
       bmp.Assign(TIntAppInfo(aList[Index]).Icon);

      if odSelected in State then
      begin
        Canvas.Font.Color:=clHotLight;
        btn.Visible:=true;
        btn.Top:=Rect.Top+4;
        btn.Left:=Width-btn.Width-28;
      end else
        Canvas.Font.Color:=clWindowText;

      Canvas.Font.Height:=12;
      Canvas.TextRect(Rect, Rect.Left+bmp.Width+6,Rect.Top+4, TIntAppInfo(aList[Index]).Name);
      TopDif := (ItemHeight div 2) - (Canvas.TextHeight('A') div 2);
      Canvas.Font.Height:=9;
      Canvas.TextRect(Rect, Rect.Left+bmp.Width+6+10,Rect.Top+TopDif+2, TIntAppInfo(aList[Index]).SDesc);
      Canvas.Draw(Rect.Left, Rect.Top, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TAppListView.ItemFromAppInfo(ai: TAppInfo);
var new: TIntAppInfo;pic: TImage;
begin
 Items.Add(' ');

 new:=TIntAppInfo.Create;
 new.Name:=ai.Name;
 new.Author:=ai.Author;
 new.SDesc:=ai.ShortDesc;
{new.UId:=StrAlloc(StrLen(ai.UId)+1);
 StrCopy(new.UId, ai.UId);
 new.Version:=StrAlloc(StrLen(ai.Version)+1);
 StrCopy(new.Version, ai.Version);}
 new.Group:=ai.Group;

 aList.Add(new);

 try
 pic:=TImage.Create(nil);
 if FileExists(ai.Icon) then
   pic.Picture.LoadFromFile(ai.Icon)
 else
  pic.Picture.LoadFromFile(GetDataFile('graphics/spackage.png'));

  pic.Stretch:=true;
  pic.Proportional:=true;
  pic.Picture.Bitmap.Width:=48;
  pic.Picture.Bitmap.Height:=48;
  new.Icon.Assign(pic.Picture.Bitmap);
  pic.Free;
 except
 writeLn('Error while loading "'+ai.Icon+'"!');
 end;
end;

end.

