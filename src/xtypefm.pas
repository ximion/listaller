{ Copyright (C) 2008-2010 Matthias Klumpp

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
//** This unit provides an form to choose between program runlevels (root / not as root / in app-testmode)
unit xtypefm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Process, ExtCtrls, liUtils, LCLType, Buttons, Distri, strLocale, ComCtrls,
  IconLoader, liTypes;

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
    LoadProgress: TProgressBar;
    procedure btnHomeClick(Sender: TObject);
    procedure btnInstallAllClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FShow: Boolean;
    sig: TPkgSigState;
  public
    { public declarations }
    procedure EnterLoadingState;
    procedure LeaveLoadingState;

    procedure SetSigState(sigstate: TPkgSigState);
  end; 

// Publish procedure so it can be used by igobase
//** Receive progress change signal on package initialization
procedure PkgInitProgressChange(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;

var
  //** True if superuser mode is enabled
  Superuser: Boolean=false;

implementation

{$R xtypefm.lfm}

uses SigInfoDisp;

{ TIMdFrm }

procedure TIMdFrm.FormCreate(Sender: TObject);
begin
 FShow:=false;
 //Set translation strings
 Caption:=rsSelInstMode;
 PkILabel.Caption:=rsSpkWarning;
 btnInstallAll.Caption:=rsInstallEveryone;
 btnTest.Caption:=rsTestApp;
 btnHome.Caption:=rsInstallHome;
 Label1.Caption:=rsWantToDoQ;
 PkILabel.Caption:=rsSpkWarning;
 LoadProgress.Visible:=false;

 //Show development version warning
 if (pos('exp',LiVersion)>0)or(pos('dev',LiVersion)>0) then
 begin
     Label2.Caption:=rsDevVersion;
     Label2.Visible:=true;
     Image2.Visible:=true;
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

procedure PkgInitProgressChange(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;
begin
 if change=scExProgress then
 begin
  TIMdFrm(user_data).LoadProgress.Position:=data.exprogress;
 end;
 Application.ProcessMessages;
end;

procedure TIMdFrm.btnInstallAllClick(Sender: TObject);
var DInfo: TDistroInfo;
begin
 Superuser:=true;
 btnHome.Tag:=2;
 close;

{DInfo:=GetDistro;
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
  end; }
end;

procedure TIMdFrm.btnTestClick(Sender: TObject);
begin
  Testmode:=true;
  btnHome.Tag:=2;
  close;
end;

procedure TIMdFrm.FormActivate(Sender: TObject);
var secInfo: TSigInfoFrm;
begin
  Refresh;

  if FShow then
begin
 FShow:=false;
 if (sig=psNone)or(sig=psUntrusted) then
 begin
  secInfo:=TSigInfoFrm.Create(nil);
  if sig=psNone then
   secInfo.LblPkgSigned.Caption:=rsPkgUnsigned
  else
   secInfo.LblPkgSigned.Caption:=rsPkgUntrusted;

  secInfo.ShowModal;
  secInfo.Free;
 end;
end;
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

procedure TIMdFrm.EnterLoadingState;
begin
 PkILabel.Visible:=false;
 PkWarnImg.Visible:=false;
 LoadProgress.Visible:=true;
 btnInstallAll.Enabled:=false;
 btnTest.Enabled:=false;
 btnHome.Enabled:=false;
 Show;
end;

procedure TIMdFrm.LeaveLoadingState;
begin
 PkILabel.Visible:=true;
 PkWarnImg.Visible:=true;
 LoadProgress.Visible:=false;
 btnInstallAll.Enabled:=true;
 btnTest.Enabled:=true;
 btnHome.Enabled:=true;
 Hide;
end;

procedure TIMdFrm.SetSigState(sigstate: TPkgSigState);
begin
 FShow:=true;
 sig:=sigstate;
end;

end.

