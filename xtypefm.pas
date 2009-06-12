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
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit provides an formular that lets the user choose between program runlevels (root / not as root / in app-testmode)
unit xtypefm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  process, ExtCtrls, LiCommon, LCLType, Buttons, distri, trstrings, ComCtrls,
  packagekit;

type

  { TimdFrm }

  TimdFrm = class(TForm)
    btnTest: TBitBtn;
    btnHome: TBitBtn;
    btnInstallAll: TBitBtn;
    PkWarnImg: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    PkILabel: TLabel;
    procedure btnHomeClick(Sender: TObject);
    procedure btnInstallAllClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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
var s: String;pkit: TPackageKit;
begin
 //Set translation strings
 Caption:=strSelInstMode;
 Label1.Caption:=strWantToDoQ;
 PkILabel.Caption:=strSpkWarning;
 btnInstallAll.Caption:=strInstallEveryone;
 btnTest.Caption:=strTestApp;
 btnHome.Caption:=strInstallHome;

//Check PackageKit version
try
  pkit:=TPackageKit.Create;
  s:=pkit.Version;
  pkit.Free;
  writeLn('Detected PackageKit: '+s);
  s:=copy(s,1,5);
  if StrToInt(StringReplace(s,'.','',[rfReplaceAll]))<44 then begin
        Label2.Caption:=StringReplace(StringReplace(strPackageKitWarning,'%cp',s,[rfReplaceAll]),'%np','0.4.4',[rfReplaceAll]);
        Label2.Visible:=true;
        Image2.Visible:=true;
        LoadStockPixmap(STOCK_DIALOG_WARNING,ICON_SIZE_BUTTON,Image2.Picture.Bitmap);
     end;

  except
    LoadStockPixmap(STOCK_DIALOG_WARNING,ICON_SIZE_BUTTON,Image2.Picture.Bitmap);
  end;
end;

procedure TimdFrm.FormShow(Sender: TObject);
begin
//Workaround for really strange LCL bug:
//If PkILabel is invisible, the whole layout of the Form is destroyed
PkiLabel.Caption:='';
if PkWarnImg.Visible then
begin
  LoadStockPixmap(STOCK_DIALOG_WARNING,ICON_SIZE_SMALL_TOOLBAR,PkWarnImg.Picture.Bitmap);
  PkILabel.Caption:=strSpkWarning;
end;
end;

procedure TimdFrm.btnInstallAllClick(Sender: TObject);
var DInfo: TDistroInfo;
begin
DInfo:=GetDistro;
if not IsRoot then begin
if FileExists(paramstr(1)) then
 ExecuteAsRoot(Application.ExeName+' '+paramstr(1),'Enter your password to install the application for everyone.',GetDataFile('graphics/mime-ipk.png'))
 else
 ExecuteAsRoot(Application.ExeName,'Enter your password to run the application with advanced privileges.','/usr/share/pixmaps/listaller.png');

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

procedure TimdFrm.FormActivate(Sender: TObject);
begin
  Refresh;
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

