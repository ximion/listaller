{ xtypefm.pas
  Copyright (C) Listaller Project 2008-2009

  pkgconvertdisp.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  xtypefm.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
//** This unit provides an formular that lets the user choose between program runlevels (root / not as root / in app-testmode)
unit xtypefm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  process, ExtCtrls, utilities, LCLType, Buttons, distri, gtk2, trstrings;

type

  { TimdFrm }

  TimdFrm = class(TForm)
    btnTest: TBitBtn;
    btnHome: TBitBtn;
    btnInstallAll: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    procedure btnHomeClick(Sender: TObject);
    procedure btnInstallAllClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  imdFrm: TimdFrm;

implementation

{ TimdFrm }

procedure TimdFrm.FormCreate(Sender: TObject);
var BH: HBitmap;s: String;
begin
if Image1.Visible then begin
  BH:=Gtk2LoadStockPixmap(GTK_STOCK_DIALOG_WARNING,GTK_ICON_SIZE_SMALL_TOOLBAR);
   if BH <> 0 then
     begin
       Image1.Picture.Bitmap.Handle:=BH;
     end;
     end;

//Check PackageKit version
try
  s:=CmdResult('pkcon --version');
  if StrToInt(StringReplace(s,'.','',[rfReplaceAll]))<44 then begin
        Label2.Caption:=StringReplace(StringReplace(strPackageKitWarning,'%cp',s,[rfReplaceAll]),'%np','0.4.4',[rfReplaceAll]);
        Label2.Visible:=true;
        Image2.Visible:=true;
        Image2.Picture.Bitmap.Handle:=Gtk2LoadStockPixmap(GTK_STOCK_DIALOG_WARNING,GTK_ICON_SIZE_BUTTON);
     end;

  except
    Image2.Picture.Bitmap.Handle:=Gtk2LoadStockPixmap(GTK_STOCK_DIALOG_WARNING,GTK_ICON_SIZE_BUTTON);
  end;

end;

procedure TimdFrm.FormShow(Sender: TObject);
begin
end;

procedure TimdFrm.btnInstallAllClick(Sender: TObject);
var t: TProcess;DInfo: TDistroInfo;
begin
DInfo:=GetDistro;
if not IsRoot then begin
       t:=TProcess.Create(nil);
         if DInfo.Desktop='KDE' then
          if FileExists('/usr/bin/kdesu') then
            t.CommandLine := 'kdesu '+Application.ExeName+' '+paramstr(1)
           else
            t.CommandLine := 'kdesudo '+Application.ExeName+' '+paramstr(1)
         else
         if DInfo.DName='Ubuntu' then
          if FileExists('/usr/bin/gksudo') then
            t.CommandLine := 'gksudo '+Application.ExeName+' '+paramstr(1)
          else
            t.CommandLine := 'gksu '+Application.ExeName+' '+paramstr(1)
         else
          t.CommandLine := 'gnomesu '+Application.ExeName+' '+paramstr(1);
        t.Options:=[];
        t.Execute;
        t.Free;
        self.Free;
        halt(0); //Terminate program
        exit;
  end;
end;

procedure TimdFrm.btnTestClick(Sender: TObject);
begin
  Testmode:=true;
  btnHome.Tag:=2;
  close;
end;

procedure TimdFrm.btnHomeClick(Sender: TObject);
begin
  btnHome.Tag:=2;
  close;
end;

procedure TimdFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
if btnHome.Tag <= 0 then begin Application.Terminate;halt(0);end;
end;

initialization
  {$I xtypefm.lrs}

end.

