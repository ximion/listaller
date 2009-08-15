{ iconloader.pas
  Copyright (C) Listaller Project 2008-2009

  iconloader.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  iconloader.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains an icon-load procedure to find the right icons for Qt4 and GTK2 GUI
unit iconloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType
  {$IFDEF LCLGTK2}
  ,gtkdef, gtk2, gdk2pixbuf, gtk2int, gdk2
  {$ENDIF};

//** Loads a stock icon (native on GTK2, if Qt4 is used aditional images are needed) @returns Handle to th bitmap
procedure LoadStockPixmap(StockId: PChar; IconSize: integer;bmp: TBitmap);

const
//Stock constants
 STOCK_QUIT='stock-quit';
 STOCK_GO_FORWARD='stock-go-forward';
 STOCK_GO_BACK='stock-go-back';
 STOCK_DIALOG_WARNING='stock-dialog-warning';
 STOCK_CLOSE='stock-close';
 STOCK_DELETE='stock-delete';
 STOCK_EXECUTE='stock-execute';
 STOCK_APPLY='stock-apply';
 STOCK_DIALOG_INFO='stock-dialog-info';
 STOCK_REFRESH='stock-refresh';
 STOCK_OPEN='stock-open';
 STOCK_PROJECT_OPEN='stock-project-open';
 ICON_SIZE_BUTTON=4;
 ICON_SIZE_SMALL_TOOLBAR=2;
 ICON_SIZE_LARGE_TOOLBAR=3;
 ICON_SIZE_MENU=1;
 //
 //** Directory with KDE4 icons
 KDE_ICON_DIR='/usr/share/icons/default.kde4/';

implementation

procedure LoadStockPixmap(StockId: PChar; IconSize: integer; bmp: TBitmap);
{$IFDEF LCLGTK2}
function Gtk2LoadStockPixmap(StockId: PChar; IconSize: integer): HBitmap;
var
  StockWindow: PGtkWidget;
  Pixmap: PGDIObject;
  Pixbuf: PGDKPixbuf;
begin
  Result := 0;
  // If not a default icon then stop
  if gtk_icon_factory_lookup_default(StockId) = nil then Exit;
  StockWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  Pixbuf := gtk_widget_render_icon(StockWindow, StockId, IconSize, nil);
  gtk_widget_destroy(StockWindow);
  // Check if icon was assigned
  if PtrUInt(Pixbuf) = 0 then Exit;
  Pixmap := Gtk2WidgetSet.NewGDIObject(gdiBitmap);
  with Pixmap^ do
  begin
    GDIBitmapType := gbPixmap;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    gdk_pixbuf_render_pixmap_and_mask(Pixbuf, GDIPixmapObject.Image,
      GDIPixmapObject.Mask, 192);
  end;
  gdk_pixbuf_unref(Pixbuf);
  Result := HBitmap(PtrUInt(Pixmap));
end;
var cicon: String;BH: HBitmap;
begin
{$ENDIF}
{$IFDEF LCLGTK2}
 if StockId = STOCK_PROJECT_OPEN then StockId:=STOCK_OPEN;
 cicon:=StringReplace(StockId,'stock','gtk',[rfReplaceAll]);
 BH:=Gtk2LoadStockPixmap(PChar(cicon),IconSize);
 if BH <> 0 then
    bmp.Handle:=BH;
{$ELSE}
var cicon: String;pic: TPicture;s: String;
begin
if not DirectoryExists(KDE_ICON_DIR+'22x22/') then
 if not DirectoryExists('/usr/share/icons/default.kde/22x22') then
  if not DirectoryExists('/usr/share/icons/oxygen/22x22') then
   if not DirectoryExists('/usr/share/icons/hicolor/22x22') then exit;
 if StockId = STOCK_QUIT then cicon:='process-stop.png';
 if StockId = STOCK_GO_FORWARD then cicon:='arrow-right.png';
 if StockId = STOCK_GO_BACK then cicon:='arrow-left.png';
 if StockId = STOCK_DIALOG_WARNING then cicon:='../status/dialog-warning.png';
 if StockId = STOCK_CLOSE then cicon:='window-close.png';
 if StockId = STOCK_DELETE then cicon:='edit-delete.png';
 if StockId = STOCK_EXECUTE then cicon:='fork.png';
 if StockId = STOCK_APPLY then cicon:='dialog-ok-apply.png';
 if StockId = STOCK_DIALOG_INFO then cicon:='help-hint.png';
 if StockId = STOCK_REFRESH then cicon:='view-refresh.png';
 if StockId = STOCK_PROJECT_OPEN then cicon:='project-open.png';
 if StockId = STOCK_OPEN then cicon:='../places/folder.png';

 if IconSize=4 then
  s:=KDE_ICON_DIR+'16x16/actions/';
 if IconSize=3 then
  s:=KDE_ICON_DIR+'32x32/actions/';
 if IconSize=2 then
  s:=KDE_ICON_DIR+'22x22/actions/';
 if IconSize=1 then
  s:=KDE_ICON_DIR+'16x16/actions/';
 if not FileExists(s) then exit;
 pic:=TPicture.Create;
 pic.LoadFromFile(s+cicon);
 bmp.Clear;
 bmp.LoadFromBitmapHandles(pic.Bitmap.Handle,pic.Bitmap.MaskHandle);
 pic.Free;
{$ENDIF}
end;

end.

