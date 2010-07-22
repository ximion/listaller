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
  AppItem, Buttons, Classes, LCLType, LiUtils, LiTypes, ComCtrls, Controls,
  Graphics, StdCtrls, SysUtils, IconLoader;

type
  TAListRmButtonClick = procedure(Sender: TObject; Index: Integer;
    item: TAppInfoItem) of object;
  TAListItemSelect = procedure(Sender: TObject; item: TAppInfoItem) of object;

  TAppListView = class(TCustomListBox)
  private
    btn: TBitBtn;
    aList: TAppItemList;
    procedure AListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure AListCustomDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure AListOnRmBtnClick(Sender: TObject);
    procedure AListSelectionChange(Sender: TObject; User: Boolean);
  protected
    FOnRmClick: TAListRmButtonClick;
    FOnItemSelect: TAListItemSelect;
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(TheOwner: TComponent; const ownItems: Boolean = true);
    destructor Destroy; override;

    procedure ItemFromAppInfo(ai: TAppInfo);
    procedure AddItem(item: TAppInfoItem);
    procedure ClearList;
    property OnRmButtonClick: TAListRmButtonClick read FOnRmClick write FOnRmClick;
    property OnItemSelect: TAListItemSelect read FOnItemSelect write FOnItemSelect;
    property AppItems: TAppItemList read aList write aList;
  end;

implementation

{ TAppListView }

constructor TAppListView.Create(TheOwner: TComponent; const ownItems: Boolean = true);
begin
  inherited Create(TheOwner);

  Style := lbOwnerDrawVariable;
  ItemHeight := 48;
  OnSelectionChange := @AListSelectionChange;
  OnDrawItem := @AListDrawItem;

  aList := TAppItemList.Create(ownItems);

  btn := TBitBtn.Create(self);
  btn.Parent := self;
  btn.Tag := -1;
  btn.Width := 40;
  btn.Height := 40;
  btn.Visible := false;
  btn.OnClick := @AListOnRmBtnClick;

  LoadStockPixmap(STOCK_DELETE, ICON_SIZE_BUTTON, btn.Glyph);
end;

destructor TAppListView.Destroy;
begin
  btn.Free;
  aList.Free;
  inherited;
end;

procedure TAppListView.AListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  AListCustomDrawItem(Control, Index, ARect, State);
end;

procedure TAppListView.AListSelectionChange(Sender: TObject; User: Boolean);
begin
  if Assigned(FOnItemSelect) then
    FOnItemSelect(self, aList[self.ItemIndex]);
end;

procedure TAppListView.AListCustomDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  bmp: TBitmap;
  TopDif: Integer;
begin
  if btn.Tag>0 then
    if ((ItemRect(btn.Tag).Top)>(Top+Height-6))or ((ItemRect(btn.Tag).Top)<(Top+4)) then
      btn.Visible := false;

  bmp := TBitmap.Create;
  try
    //bmp.Transparent := True;
    if (Index>=0) and (Index<Items.Count) then
      bmp.Assign(aList[Index].Icon);

    {$IFDEF LCLQt}
     if odSelected in State then
       Canvas.Brush.Color := clHighlight
     else
       Canvas.Brush.Color := clWhite;
     Canvas.FillRect(Rect);
    {$ENDIF}

    if odSelected in State then
    begin
      // Canvas.Font.Color := clHotLight;
      btn.Visible := true;
      btn.Top := Rect.Top+6;
      btn.Left := Width-btn.Width-26;
      btn.Tag := Index;
    end
    else
      Canvas.Font.Color := clWindowText;

    Canvas.Font.Height := 12;
    Canvas.TextRect(Rect, Rect.Left+bmp.Width+6, Rect.Top+4, aList[Index].Name);
    TopDif := (ItemHeight div 2) - (Canvas.TextHeight('A') div 2);
    Canvas.Font.Height := 9;
    Canvas.TextRect(Rect, Rect.Left+bmp.Width+6, Rect.Top+TopDif+2, aList[Index].SDesc);
      {if aList[Index].SDesc<>'' then
       TopDif := TopDif+(Canvas.TextHeight('A') div 2);
      Canvas.TextRect(Rect, Rect.Left+bmp.Width+6+10,Rect.Top+TopDif+2, aList[Index].Version);
      }
    Canvas.Draw(Rect.Left, Rect.Top, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TAppListView.ItemFromAppInfo(ai: TAppInfo);
var
  new: TAppInfoItem;
  pic: TPicture;
  bmp: TBitmap;
  rect: TRect;
const
  size = 48;
begin
  Items.Add(' ');

  new := TAppInfoItem.Create;
  new.Name := ai.Name;
  new.Author := ai.Author;
  new.SDesc := ai.ShortDesc;
  new.UId := ai.UId;
  new.Version := ai.Version;
  new.Category := ai.Category;

  aList.Add(new);

  pic := TPicture.Create;
  try
    if FileExists(ai.IconName) then
      new.IconPath := ai.IconName
    else
      new.IconPath := GetDataFile('graphics/spackage.png');


    pic.LoadFromFile(new.IconPath);
  except
    writeLn('Error while loading "'+ai.IconName+'"!');
    new.IconPath := GetDataFile('graphics/spackage.png');
  end;

  pic.LoadFromFile(new.IconPath);

  bmp := TBitmap.Create;
  rect.Top := 0;
  rect.Left := 0;
  rect.Bottom := size;
  rect.Right := size;
  bmp.Width := size;
  bmp.Height := size;
  bmp.Canvas.Brush.Color := clNone;
  bmp.Transparent := true;
  bmp.Canvas.FillRect(0, 0, size, size);
  {$IFNDEF LCLQt}
   bmp.Mask(clNone);
   bmp.Masked := true;
  {$ENDIF}
  bmp.Canvas.StretchDraw(rect, pic.Graphic);
  pic.Free;
  new.Icon.Assign(bmp);
  bmp.Free;
end;

procedure TAppListView.AListOnRmBtnClick(Sender: TObject);
begin
  FOnRmClick(Sender, btn.Tag, aList[btn.Tag]);
end;

procedure TAppListView.SetVisible(Value: Boolean);
begin
  inherited;
  if not Value then
    btn.Visible := false;
end;

procedure TAppListView.AddItem(item: TAppInfoItem);
begin
  Items.Add('');
  aList.Add(item);
end;

procedure TAppListView.ClearList;
begin
  Items.Clear;
  aList.Clear;
end;

end.

