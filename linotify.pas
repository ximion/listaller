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
//** Listaller's notify try icon: Shows notifications about the PackageDB status
unit linotify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, LiCommon, Process, Distri, TRStrings, appman,
  LCLType, LiTranslator;

type

  { TForm1 }

  TForm1 = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
var p: TProcess;DInfo: TDistroInfo;
begin
 if FileExists(ExtractFilePath(Application.ExeName)+'listallmgr') then
 begin
  p:=TProcess.Create(nil);
  p.Options:=[];
  p.CommandLine:=ExtractFilePath(Application.ExeName)+'listallmgr';
  p.Execute;
  p.Free;
 end else
 begin
  writeLn(Format(rsUnableFind, ['listallMgr!']));
  ShowMessage(rsListallerMgrNotFound);
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrayIcon.Visible:=true;
  MenuItem1.Caption:=rsCheckApps;
  MenuItem2.Caption:=rsCheckForUpd;
  MenuItem4.Caption:=rsLaunchLiMgr;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var rep: TStringList;

procedure LogQ;
begin
if Application.MessageBox(PAnsiChar(rsViewLogQ), 'ViewLog', MB_YESNO)=IDYES then
  ShowMessage(rep.Text);
end;

procedure PerformCheck;
begin
//??? Reimplement / base on libappmanager

{rep:=TStringList.Create;
 if not CheckApps(rep) then
 begin
 if Application.MessageBox(PAnsiChar(rsBrokenDepsFixQ), 'FixDeps', MB_YESNO)=IDYES then
   CheckApps(rep,true);
 LogQ;
 end else LogQ;  }

end;

begin

if Application.MessageBox(PAnsiChar(rsCheckAppDepsQ), PChar(rsCheckDepsQ), MB_YESNO)=
  IDYES then
begin
 RegDir:=SyblToPath('$INST')+'/app-reg/';
 if Application.MessageBox(PAnsiChar(rsCheckRootAppsQ), PChar(rsCheckDepsQ), MB_YESNO)=IDYES then
 begin
  PerformCheck;
  RegDir:='/etc/lipa/app-reg/';
  PerformCheck;
 end else PerformCheck;

end;

end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var p: TProcess;
begin
 if FileExists(ExtractFilePath(Application.ExeName)+'liupdate -q') then
 begin
  p:=TProcess.Create(nil);
  p.Options:=[];
  p.CommandLine:=ExtractFilePath(Application.ExeName)+'liupdate -q';
  p.Execute;
  p.Free;
 end else
 begin
  writeLn(Format(rsUnableFind,['Listaller Updater!']));
  ShowMessage(rsCouldntFindUpdater);
 end;
end;

initialization
  {$I linotify.lrs}

end.

