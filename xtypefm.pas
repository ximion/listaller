{ Copyright (C) 2008-2009 Matthias Klumpp

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
//** This unit provides an formular that lets the user choose between program runlevels (root / not as root / in app-testmode)
unit xtypefm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  process, ExtCtrls, LiCommon, LCLType, Buttons, distri, trstrings, ComCtrls,
  packagekit, iconloader;

type

  { TIMdFrm }

  TIMdFrm = class(TForm)
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
  IMdFrm: TIMdFrm;

implementation

{ TIMdFrm }

procedure TIMdFrm.FormCreate(Sender: TObject);
var s: String;pkit: TPackageKit;
begin
 //Set translation strings
 Caption:=rsSelInstMode;
 PkILabel.Caption:=rsSpkWarning;
 btnInstallAll.Caption:=rsInstallEveryone;
 btnTest.Caption:=rsTestApp;
 btnHome.Caption:=rsInstallHome;
 Label1.Caption:=rsWantToDoQ;
 PkILabel.Caption:=rsSpkWarning;

//Check PackageKit version
try
  pkit:=TPackageKit.Create;
  s:=pkit.Version;
  pkit.Free;
  writeLn('Detected PackageKit: '+s);
  // s:=copy(s,1,5);
  if StrToInt(StringReplace(s,'.','',[rfReplaceAll]))<46 then
  begin
        Label2.Caption:=StringReplace(StringReplace(rsPackageKitWarning,'%cp',s,[rfReplaceAll]),'%np','0.4.6',[rfReplaceAll]);
        Label2.Visible:=true;
        Image2.Visible:=true;
        LoadStockPixmap(STOCK_DIALOG_WARNING,ICON_SIZE_BUTTON,Image2.Picture.Bitmap);
  end;

  except
    LoadStockPixmap(STOCK_DIALOG_WARNING,ICON_SIZE_BUTTON,Image2.Picture.Bitmap);
  end;
end;

procedure TIMdFrm.FormShow(Sender: TObject);
begin
if PkWarnImg.Visible then
begin
  LoadStockPixmap(STOCK_DIALOG_WARNING,ICON_SIZE_SMALL_TOOLBAR,PkWarnImg.Picture.Bitmap);
  PkiLabel.Visible:=true;
end;
end;

procedure TIMdFrm.btnInstallAllClick(Sender: TObject);
var DInfo: TDistroInfo;
begin
DInfo:=GetDistro;
if not IsRoot then begin
if FileExists(paramstr(1)) then
 ExecuteAsRoot(Application.ExeName+' '+paramstr(1), rsRootPassQAppEveryone,
   GetDataFile('graphics/mime-ipk.png'))
 else
 ExecuteAsRoot(Application.ExeName, rsRootPassAdvancedPriv, '/usr/share/'
   +'pixmaps/listaller.png');

 self.Free;
 halt(0); //Terminate program
 exit;
  end;
end;

procedure TIMdFrm.btnTestClick(Sender: TObject);
begin
  Testmode:=true;
  btnHome.Tag:=2;
  close;
end;

procedure TIMdFrm.FormActivate(Sender: TObject);
begin
  Refresh;
end;

procedure TIMdFrm.btnHomeClick(Sender: TObject);
begin
  btnHome.Tag:=2;
  close;
end;

procedure TIMdFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
if btnHome.Tag <= 0 then begin Application.Terminate;halt(0);end;
end;

initialization
  {$I xtypefm.lrs}

end.

