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
//** This unit contains the code to manage installed packages
unit manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Inifiles, StdCtrls, process, LCLType, Buttons, ExtCtrls, Distri, LEntries,
  Uninstall, trStrings, FileUtil, CheckLst, xTypeFm, AppMan, LiCommon, liTypes,
  Contnrs, AboutBox, GetText, PackageKit, Spin, iconLoader;

type

  { TMnFrm }

  TMnFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    AboutBtn: TButton;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Button1: TButton;
    MBar: TProgressBar;
    StatusLabel: TLabel;
    RmUpdSrcBtn: TBitBtn;
    UpdCheckBtn: TBitBtn;
    CatButton: TSpeedButton;
    CBox: TComboBox;
    CbShowPkMon: TCheckBox;
    UListBox: TCheckListBox;
    UsILabel: TLabel;
    PageControl1: TPageControl;
    SysRepoSheet: TTabSheet;
    UpdRepoSheet: TTabSheet;
    WarnDistCb: TCheckBox;
    AutoDepLdCb: TCheckBox;
    EnableProxyCb: TCheckBox;
    Edit1: TEdit;
    edtFTPProxy: TLabeledEdit;
    edtPasswd: TLabeledEdit;
    edtUsername: TLabeledEdit;
    FilterEdt: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    ImageList1: TImageList;
    InstallButton: TSpeedButton;
    InstAppButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Notebook1: TNotebook;
    OpenDialog1: TOpenDialog;
    LeftBar: TPanel;
    InstalledAppsPage: TPage;
    CatalogPage: TPage;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    RepoPage: TPage;
    ConfigPage: TPage;
    SettingsButton: TSpeedButton;
    RepoButton: TSpeedButton;
    SWBox: TScrollBox;
    Process1: TProcess;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCatClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBoxChange(Sender: TObject);
    procedure AutoDepLdCbChange(Sender: TObject);
    procedure CbShowPkMonChange(Sender: TObject);
    procedure EnableProxyCbChange(Sender: TObject);
    procedure RmUpdSrcBtnClick(Sender: TObject);
    procedure UListBoxClick(Sender: TObject);
    procedure UpdCheckBtnClick(Sender: TObject);
    procedure WarnDistCbChange(Sender: TObject);
    procedure FilterEdtEnter(Sender: TObject);
    procedure FilterEdtExit(Sender: TObject);
    procedure FilterEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InstAppButtonClick(Sender: TObject);
    procedure RepoButtonClick(Sender: TObject);
  private
    { private declarations }
    blst: TStringList;
    procedure UninstallClick(Sender: TObject);
  public
    { public declarations }
    DInfo: TDistroInfo;
    //** Visual package list
    AList: TObjectList;
    //** Information about the application that should be uninstalled
    uApp: TAppInfo;
    //** Pointer to our AppManager object
    amgr: Pointer;
  end;

  //** Fill ImageList with category icons
  procedure FillImageList(IList: TImageList);

var
 //** Main formular instance
  MnFrm:   TMnFrm;
 //** List of installed application names
  instLst: TStringList;

//Published for use in uninstall.pas
procedure OnMessage(msg: String;imp: TMType);cdecl;

implementation

uses pkgconvertdisp, swcatalog;

{ TListEntry }

procedure TMnFrm.UninstallClick(Sender: TObject);
begin
uApp:=TAppInfo(TListEntry((Sender as TBitBtn).Parent).appInfo);
RMForm.ShowModal;
end;

function OnNewAppFound(name: PChar;obj: TAppInfo): Boolean;cdecl;
var entry: TListEntry;
begin
with MnFrm do begin
 entry:=TListEntry.Create(MnFrm);
 entry.Parent:=SWBox;
 entry.LoadFromAppInfo(TAppInfo(obj));
 entry.UnButton.OnClick:=@UninstallClick;
 AList.Add(entry);
end;
Application.ProcessMessages;
end;

procedure OnMessage(msg: String;imp: TMType);cdecl;
begin
 if imp=mtInfo then MnFrm.StatusLabel.Caption:=msg;
 if imp=mtWarning then ShowMessage(msg);
end;

function OnUserRequest(mtype: TRqType;msg: PChar): TRqResult;cdecl;
begin
 ShowMessage(msg);
end;

{ TMnFrm }

procedure RemoveDuplicates(s: TStrings);
var
  iLow, iHigh: integer;
begin
  for iLow := 0 to s.Count - 2 do
    for iHigh := Pred(s.Count) downto Succ(iLow) do
      if s[iLow] = s[iHigh] then
        s.Delete(iHigh);
end;

var fAct: Boolean;
procedure TMnFrm.FormShow(Sender: TObject);
begin
fAct:=true;
end;

procedure TMnFrm.InstAppButtonClick(Sender: TObject);
begin
  Notebook1.ActivePageComponent:=InstalledAppsPage;
  CatButton.Down:=false;
  SettingsButton.Down:=false;
  RepoButton.Down:=false;
  InstAppButton.Down:=true;
end;

procedure TMnFrm.RepoButtonClick(Sender: TObject);
begin
  Notebook1.ActivePageComponent:=RepoPage;
  CatButton.Down:=false;
  SettingsButton.Down:=false;
  RepoButton.Down:=true;
  InstAppButton.Down:=false;
end;

procedure FillImageList(IList: TImageList);
var tm: TPicture;bmp: TBitmap;a: String;
begin
  //Add images to list
  tm:=TPicture.Create;
  a:=GetDataFile('graphics/categories/');

  bmp := TBitmap.Create;
  bmp.width := 24;
  bmp.height := 24;
  bmp.TransparentColor:=clWhite;
  bmp.Transparent:=true;

  with IList do begin
  tm.LoadFromFile(a+'all.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'science.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'office.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'development.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'graphics.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'internet.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'games.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'system.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'multimedia.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  tm.LoadFromFile(a+'other.png');
  bmp.canvas.draw(0,0,tm.Graphic);
  Add(bmp,nil);
  end;
  tm.Free;
  bmp.Free;
end;

procedure TMnFrm.btnInstallClick(Sender: TObject);
var p: TProcess;pkit: TPackageKit;
begin
  if OpenDialog1.Execute then
  if FileExists(OpenDialog1.Filename) then
  begin
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.ipk')
  or (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.zip') then
  begin
  Process1.CommandLine := ExtractFilePath(Application.ExeName)+'listallgo '+OpenDialog1.Filename;
  Process1.Execute;
  MnFrm.Hide;
  while Process1.Running do Application.ProcessMessages;
  MnFrm.Show;
  end else
  begin
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.deb') then
  if DInfo.PackageSystem='DEB' then
  begin
  //Open DEB-File
   p:=TProcess.Create(nil);
   p.Options:=[poWaitOnExit,poNewConsole];
   Application.ProcessMessages;
   p.CommandLine:='xdg-open '+''''+OpenDialog1.FileName+'''';
   p.Execute;
   p.Free;
   exit;
  end else
   if Application.MessageBox(PAnsiChar(StringReplace(StringReplace(rsConvertPkg,'%x','DEB',[rfReplaceAll]),'%y','RPM',[rfReplaceAll])),
                              PAnsiChar(rsConvertPkgQ),MB_YESNO)=IDYES then
   begin
   with ConvDisp do
   begin
   if not FileExists('/usr/bin/alien') then
    if Application.MessageBox(PChar(rsListallerAlien),PChar(rsInstPkgQ),MB_YESNO)=IDYES then
    begin
      ShowMessage(rsplWait);
      pkit:=TPackageKit.Create;
      pkit.InstallPkg('alien');
      while not pkit.PkFinished do Application.ProcessMessages;

      if pkit.PkFinishCode>0 then
      begin
       ShowMessage(StringReplace(rsPkgInstFail,'%p','alien',[rfreplaceAll]));
       pkit.Free;
       exit;
       end;
      pkit.Free;
     end else exit;
   Application.ProcessMessages;
   Caption:=StringReplace(rsConvTitle,'%p','DEB',[rfReplaceAll]);
   Process1.CommandLine:='alien --to-rpm -v -i --scripts '+''''+OpenDialog1.FileName+'''';
   GetOutPutTimer.Enabled:=true;
   Process1.Execute;
   ShowModal;
   end;
   exit;
   end;
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.rpm') then
  if DInfo.PackageSystem='RPM' then
  begin
   //Open RPM-File
   p:=TProcess.Create(nil);
   p.Options:=[poWaitOnExit,poNewConsole];
   Application.ProcessMessages;
   p.CommandLine:='xdg-open '+''''+OpenDialog1.FileName+'''';
   p.Execute;
   p.Free;
   exit;
  end else
   if Application.MessageBox(PAnsiChar(StringReplace(StringReplace(rsConvertPkg,'%x','RPM',[rfReplaceAll]),'%y','DEB',[rfReplaceAll])),
                              PAnsiChar(rsConvertPkgQ),MB_YESNO)=IDYES then
   begin
   with ConvDisp do
   begin

   if not FileExists('/usr/bin/alien') then
    if Application.MessageBox(PChar(rsListallerAlien),PChar(rsInstPkgQ),MB_YESNO)=IDYES then
    begin
      ShowMessage(rsplWait);
      pkit.InstallPkg('alien');
      while not pkit.PkFinished do Application.ProcessMessages;

      if pkit.PkFinishCode>0 then
      begin
       ShowMessage(StringReplace(rsPkgInstFail,'%p','alien',[rfreplaceAll]));
       pkit.Free;
       exit;
       end;
      pkit.Free;
     end else exit;

   Application.ProcessMessages;
   Caption:=StringReplace(rsConvTitle,'%p','RPM',[rfReplaceAll]);
   Process1.CommandLine:='alien --to-deb -v -i --scripts '+''''+OpenDialog1.FileName+'''';
   GetOutPutTimer.Enabled:=true;
   Process1.Execute;
   ShowModal;
   end;
   exit;
   end;
   
   end;
  end;
end;

procedure TMnFrm.BitBtn1Click(Sender: TObject);
begin
  SCForm.ShowModal;
end;

procedure TMnFrm.BitBtn6Click(Sender: TObject);
var p: TProcess;
begin
  p:=TProcess.Create(nil);
  p.Options:=[poUsePipes];
  if DInfo.DBase='KDE' then
  begin
     if FileExists('/usr/bin/kpackagekit') then
      p.CommandLine:='/usr/bin/kpackagekit --settings'
     else
      p.CommandLine:='/usr/bin/gpk-repo';
  end else
  begin
     if FileExists('/usr/bin/gpk-repo') then
      p.CommandLine:='/usr/bin/gpk-repo'
     else
      p.CommandLine:='/usr/bin/kpackagekit';
  end;
  Notebook1.Enabled:=false;
  LeftBar.Enabled:=false;
  MBar.Visible:=true;
  try
   p.Execute;
  except
   ShowMessage(rsNoGUIPkgManFound);
   p.Free;
   exit;
  end;

  while p.Running do Application.ProcessMessages;
  p.Free;
  Notebook1.Enabled:=true;
  LeftBar.Enabled:=true;
  MBar.Visible:=false;
end;

procedure TMnFrm.BitBtn2Click(Sender: TObject);
var p: TProcess;
begin
  p:=TProcess.Create(nil);
  p.Options:=[poUsePipes];
  if DInfo.DBase='KDE' then
  begin
   if (DInfo.DName='Ubuntu') then
    if FileExists('/usr/bin/qappinstall') then
     p.CommandLine:='/usr/bin/qappinstall'
    else
     p.CommandLine:='/usr/bin/kpackagekit'
   else
     if FileExists('/usr/bin/kpackagekit') then
      p.CommandLine:='/usr/bin/kpackagekit'
     else
      p.CommandLine:='/usr/bin/gpk-application';
  end else
  begin
    if (DInfo.DName='Ubuntu') then
    if FileExists('/usr/bin/gnome-app-install') then
     p.CommandLine:='/usr/bin/gnome-app-install'
    else
     p.CommandLine:='/usr/bin/gpk-application'
   else
     if FileExists('/usr/bin/gpk-application') then
      p.CommandLine:='/usr/bin/gpk-application'
     else
      p.CommandLine:='/usr/bin/kpackagekit';
  end;
  if not FileExists(p.CommandLine) then
  begin
   ShowMessage(rsNoGUIPkgManFound);
   p.Free;
   exit;
  end;
  Notebook1.Enabled:=false;
  LeftBar.Enabled:=false;
  MBar.Visible:=true;
  p.Execute;
  while p.Running do Application.ProcessMessages;
  p.Free;
  Notebook1.Enabled:=true;
  LeftBar.Enabled:=true;
  MBar.Visible:=false;
end;

procedure TMnFrm.btnSettingsClick(Sender: TObject);
var cnf:TIniFile;
begin
  Notebook1.ActivePageComponent:=ConfigPage;
  CatButton.Down:=false;
  SettingsButton.Down:=true;
  RepoButton.Down:=false;
  InstAppButton.Down:=false;
  //Now load the configuration

  cnf:=TIniFile.Create(ConfigDir+'config.cnf');
  EnableProxyCb.Checked:=cnf.ReadBool('Proxy','UseProxy',false);
  Edit1.Text:=cnf.ReadString('Proxy','Server','');
  SpinEdit1.Value:=cnf.ReadInteger('Proxy','Port',0);
  CbShowPkMon.Checked:=cnf.ReadBool('MainConf','ShowPkMon',false);
  cnf.free;
   if Edit1.Text='' then
   begin
   if (mnFrm.DInfo.DBase='GNOME')and(FileExists('/usr/bin/gconftool-2')) then
   begin
    if CmdResult('gconftool-2 -g /system/http_proxy/use_http_proxy')='true' then EnableProxyCb.Checked:=true
    else EnableProxyCb.Checked:=false;
    Edit1.Text:=CmdResult('gconftool-2 -g /system/http_proxy/host');
    SpinEdit1.Value:=StrToInt(CmdResult('gconftool-2 -g /system/http_proxy/port'));
   end;
   end;
end;

procedure TMnFrm.btnCatClick(Sender: TObject);
begin
  Notebook1.ActivePageComponent:=CatalogPage;
  CatButton.Down:=true;
  SettingsButton.Down:=false;
  RepoButton.Down:=false;
  InstAppButton.Down:=false;
end;

procedure TMnFrm.AboutBtnClick(Sender: TObject);
var abbox: TFmAbout;
begin
 abbox:=TFmAbout.Create(self);
 abbox.ShowModal;
 abbox.free;
end;

procedure TMnFrm.Button1Click(Sender: TObject);
var p: TProcess;
begin
if not FileExists(ExtractFilePath(paramstr(0))+'litray') then
begin
ShowMessage(rsNotFoundliTray);
exit;
end;
  p:=TProcess.Create(nil);
  p.Options:=[];
  p.CommandLine:=ExtractFilePath(paramstr(0))+'litray';
  p.Execute;
  p.Free;
end;

procedure TMnFrm.CBoxChange(Sender: TObject);
var gt: TGroupType;i: Integer;
begin
  CBox.Enabled:=false;
  SWBox.Enabled:=false;
  Application.ProcessMessages;
  case CBox.ItemIndex of
  0: gt:=gtALL;
  1: gt:=gtEDUCATION;
  2: gt:=gtOFFICE;
  3: gt:=gtDEVELOPMENT;
  4: gt:=gtGRAPHIC;
  5: gt:=gtNETWORK;
  6: gt:=gtGAMES;
  7: gt:=gtSYSTEM;
  8: gt:=gtMULTIMEDIA;
  9: gt:=gtADDITIONAL;
  10: gt:=gtOTHER;
  end;
  MBar.Visible:=true;
  StatusLabel.Caption:=rsFiltering;

  if gt=gtALL then
  begin
  for i:=0 to AList.Count-1 do
   TListEntry(AList[i]).Visible:=true;
  end else
  for i:=0 to AList.Count-1 do
  begin
   TListEntry(AList[i]).Visible:=true;
   Application.ProcessMessages;
   if TListEntry(AList[i]).appInfo.Group<>gt then
      TListEntry(AList[i]).Visible:=false;
  end;

  StatusLabel.Caption:=rsReady;
  SWBox.Enabled:=true;
  MBar.Visible:=false;
  CBox.Enabled:=true;
end;

procedure TMnFrm.AutoDepLdCbChange(Sender: TObject);
var h: String;ini: TIniFile;
begin
  h:=ConfigDir;
  ini:=TIniFile.Create(h+'config.cnf');
  ini.WriteBool('MainConf','AutoDepLoad',(Sender as TCheckBox).Checked);
  ini.Free;
end;

procedure TMnFrm.CbShowPkMonChange(Sender: TObject);
var h: String;ini: TIniFile;
begin
  h:=ConfigDir;
  ini:=TIniFile.Create(h+'config.cnf');
  ini.WriteBool('MainConf','ShowPkMon',(Sender as TCheckBox).Checked);
  ini.Free;
end;

procedure TMnFrm.EnableProxyCbChange(Sender: TObject);
var p: String;cnf: TIniFile;
begin
  if (Sender as TCheckBox).Checked then begin
  Label3.Enabled:=true;
  Edit1.Enabled:=true;
  SpinEdit1.Enabled:=true;
  edtUsername.Enabled:=true;
  edtPasswd.Enabled:=true;
  edtFTPProxy.Enabled:=true;
  spinEdit2.Enabled:=true;
  Label4.Enabled:=true;
  Label5.Enabled:=true;
  end else begin
  Label3.Enabled:=false;
  Edit1.Enabled:=false;
  SpinEdit1.Enabled:=false;
  Label4.Enabled:=false;
  edtUsername.Enabled:=false;
  edtPasswd.Enabled:=false;
  edtFTPProxy.Enabled:=false;
  spinEdit2.Enabled:=false;
  Label5.Enabled:=false;
  end;
  p:=ConfigDir;
  cnf:=TIniFile.Create(p+'config.cnf');
  cnf.WriteBool('Proxy','UseProxy',(Sender as TCheckBox).Checked);
  cnf.Free;
end;

procedure TMnFrm.RmUpdSrcBtnClick(Sender: TObject);
var uconf: TStringList;
begin
if UListBox.ItemIndex>-1 then
begin
 if Application.MessageBox(PChar(rsRmSrcQ),PChar(rsRmSrcQC),MB_YESNO)=IDYES then
 begin
  uconf:=tStringList.Create;
  uconf.LoadFromFile(RegDir+'updates.list');
  uconf.Delete(UListBox.ItemIndex+1);
  uconf.SaveToFile(RegDir+'updates.list');
  uconf.Free;
  UListBox.Items.Delete(UListBox.ItemIndex);
  ShowMessage(rsSourceDeleted);
 end;
end else ShowMessage(rsPleaseSelectListItem);
end;

procedure TMnFrm.UListBoxClick(Sender: TObject);
var uconf: TStringList;
    h: String;
begin
 uconf:=TStringList.Create;
 uconf.LoadFromFile(RegDir+'updates.list');
 h:=uconf[UListBox.ItemIndex+1];
 if UListBox.Checked[UListBox.ItemIndex] then
  h[1]:='-' else h[1]:='#';
 uconf[UListBox.ItemIndex+1]:=h;
 uconf.SaveToFile(RegDir+'updates.list');
 uconf.Free;
end;

procedure TMnFrm.UpdCheckBtnClick(Sender: TObject);
begin
if FileExists(ExtractFilePath(Application.ExeName)+'liupdate') then
begin
  Process1.CommandLine:=ExtractFilePath(Application.ExeName)+'liupdate';
  Process1.Execute;
end else ShowMessage(rsLiUpdateAccessFailed);
end;

procedure TMnFrm.WarnDistCbChange(Sender: TObject);
var h: String;ini: TIniFile;
begin
  h:=ConfigDir;
  ini:=TIniFile.Create(h+'config.cnf');
  ini.WriteBool('MainConf','DistroWarning',(Sender as TCheckBox).Checked);
  ini.Free;
end;

procedure TMnFrm.FilterEdtEnter(Sender: TObject);
begin
  if FilterEdt.Text=rsFilter then
   FilterEdt.Text:='';
end;

procedure TMnFrm.FilterEdtExit(Sender: TObject);
begin
  if (StringReplace(FilterEdt.Text,' ','',[rfReplaceAll])='') then
   FilterEdt.Text:=rsFilter;
end;

procedure TMnFrm.FilterEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: Integer;
begin
  if Key = VK_RETURN then
  begin
     SWBox.HorzScrollbar.Position:=0;
     SWBox.Enabled:=false;
     FilterEdt.Enabled:=false;
     CBox.Enabled:=false;
     Application.ProcessMessages;

     if ((FilterEdt.Text=' ') or (FilterEdt.Text='*')or (FilterEdt.Text='')) then
     begin
     for i:=0 to AList.Count-1 do
     TListEntry(AList[i]).Visible:=true;
     end else
     begin
     Application.ProcessMessages;
     StatusLabel.Caption:=rsFiltering;
     for i:=0 to AList.Count-1 do
     begin
      TListEntry(AList[i]).Visible:=true;
       Application.ProcessMessages;
        if ((pos(LowerCase(FilterEdt.Text),LowerCase(TListEntry(AList[i]).AppName))<=0)
        or (pos(LowerCase(FilterEdt.Text),LowerCase(TListEntry(AList[i]).AppDesc))<=0))
         and (LowerCase(FilterEdt.Text)<>LowerCase(TListEntry(AList[i]).AppName)) then
         TListEntry(AList[i]).Visible:=false;
         end;
       end;
StatusLabel.Caption:=rsReady;
FilterEdt.Enabled:=true;
SWBox.Enabled:=true;
CBox.Enabled:=true;
end;
end;

procedure TMnFrm.FormActivate(Sender: TObject);
begin
  if fAct then
  begin fAct:=false;
  MBar.Visible:=true;
  li_mgr_load_apps(@amgr);
  MBar.Visible:=false;
  end;
end;

procedure TMnFrm.FormCreate(Sender: TObject);
var xFrm: TimdFrm;i: Integer;tmp: TStringList;
begin

SWBox.DoubleBuffered:=true;
DoubleBuffered:=true;
DInfo:=GetDistro;

amgr:=li_mgr_new; //Create new app manager

 if not DirectoryExists(RegDir) then SysUtils.CreateDir(RegDir);
  
 uApp.UId:='';

 AList:=TObjectList.Create(true); //Create object-list to store AppInfo-Panels

 if not IsRoot then
 begin
 xFrm:=TimdFrm.Create(nil);

 //Set reg-dir
 RegDir:=SyblToPath('$INST/app-reg/');


 with xFrm do
 begin
  Caption:=rsSelMgrMode;
  btnTest.Visible:=false;
  CatButton.Caption:=rsSWCatalogue;
  btnInstallAll.Caption:=rsDispRootApps;
  btnHome.Caption:=rsDispOnlyMyApps;
  Refresh;
  ShowModal;
 end;
xFrm.Free;
end else
RegDir:='/etc/lipa/app-reg/';

if not DirectoryExists(RegDir) then CreateDir(RegDir);

//Load update source settings
  PageControl1.ActivePageIndex:=0;

  tmp:=TStringList.Create;
  if not FileExists(RegDir+'updates.list') then
  begin
   tmp.Add('Listaller UpdateSources-pk0.8');
   tmp.SaveToFile(RegDir+'updates.list');
  end;
  tmp.LoadFromFile(RegDir+'updates.list');
  for i:=1 to tmp.Count-1 do begin
  UListBox.items.Add(copy(tmp[i],pos(' <',tmp[i])+2,length(tmp[i])-pos(' <',tmp[i])-2)+' ('+copy(tmp[i],3,pos(' <',tmp[i])-3)+')');
  UListBox.Checked[UListBox.Items.Count-1]:=tmp[i][1]='-';
  end;

 //Translate
 Caption:=rsSoftwareManager;
 CatButton.Caption:=rsSWCatalogue;
 Label1.Caption:=rsShow;
 Label3.Caption:=rsNoAppsFound;
 AboutBtn.Caption:=rsAboutListaller;
 BitBtn1.Caption:=rsBrowseLiCatalog;
 BitBtn2.Caption:=rsOpenDirsiCatalog;
 InstAppButton.Caption:=rsInstalledApps;
 InstallButton.Caption:=rsInstallPkg;
 CatButton.Caption:=rsBrowseCatalog;
 RepoButton.Caption:=rsRepositories;
 SettingsButton.Caption:=rsSettings;
 FilterEdt.Text:=rsFilter;
 //Translate config page
 edtUsername.Caption:=rsUsername+':';
 edtPasswd.Caption:=rsPassword+':';
 EnableProxyCb.Caption:=rsEnableProxy;
 GroupBox1.Caption:=rsProxySettings;
 AutoDepLdCb.Caption:=rsAutoLoadDep;
 CbShowPkMon.Caption:=rsShowPkMon;
 Button1.Caption:=rsStartLiTray;
 //Translate repo page(s)
 UpdRepoSheet.Caption:=rsUpdSources;
 RmUpdSrcBtn.Caption:=rsDelSrc;
 UpdCheckBtn.Caption:=rsCheckForUpd;
 UsILabel.Caption:=rsListofSrc;
 BitBtn6.Caption:=rsChangePkgManSettings;

with CBox do
begin
 Items[0]:=rsAll;
 Items[1]:=rsEducation;
 Items[2]:=rsOffice;
 Items[3]:=rsDevelopment;
 Items[4]:=rsGraphic;
 Items[5]:=rsNetwork;
 Items[6]:=rsGames;
 Items[7]:=rsSystem;
 Items[8]:=rsMultimedia;
 Items[9]:=rsAddidional;
 Items[10]:=rsOther;
end;
 Image1.Picture.LoadFromFile(GetDataFile('graphics/header.png'));
 Application.ShowMainForm:=true;
 instLst:=TStringList.Create;
 blst:=TStringList.Create; //Create Blacklist

 //Create uninstall panel
Application.CreateForm(TRMForm, RMForm);

{//Option check
if (Application.HasOption('u','uninstall'))and(IsRoot) then
begin
 if paramstr(2)[1]='/' then ProcessDesktopFile(paramstr(2),'all')
 else IdList.Add(paramstr(2));

 uId:=0;
 RMForm.ShowModal;
 Application.Terminate;
 halt(0);
end;     }

//Register callback to be notified if new app was found
li_mgr_register_app_call(@amgr,@OnNewAppFound);

//Register callback to be notified if a message was thrown
li_mgr_register_msg_call(@amgr,@OnMessage);

//Register request call
li_mgr_register_request_call(@amgr,@OnUserRequest);

li_mgr_set_su_mode(@amgr,IsRoot);

InstAppButton.Down:=true;

Notebook1.ActivePageComponent:=InstalledAppsPage;
 WriteLn('GUI loaded.');
end;

procedure TMnFrm.FormDestroy(Sender: TObject);
procedure WriteConfig;
 var p: String;cnf: TIniFile;
begin
  p:=ConfigDir;
  cnf:=TIniFile.Create(p+'config.cnf');
  cnf.WriteString('Proxy','hServer',Edit1.Text);
  cnf.WriteInteger('Proxy','hPort',SpinEdit1.Value);
  //
  cnf.WriteString('Proxy','Username',edtUsername.Caption);
  cnf.WriteString('Proxy','Password',edtPasswd.Caption);

  cnf.WriteString('Proxy','fServer',edtFTPProxy.Caption);
  cnf.WriteInteger('Proxy','fPort',SpinEdit2.Value);
  cnf.Free;
end;
begin
  //Write configuration which was not applied yet
  WriteConfig();
  li_mgr_free(@aMgr); //Free appmanager
  if Assigned(blst) then blst.Free;       //Free blacklist
  if Assigned(InstLst) then InstLst.Free; //Free list of installed apps
  if Assigned(AList) then AList.Free;     //Free AppPanel store
end;

initialization
  {$I manager.lrs}

end.

{
//Create GIFThread for Throbber animation
gif:=TGifThread.Create(true);
gif.FileName:=GetDataFile('graphics/throbber.gif');
ThrobberBox.Width:=gif.Width;
ThrobberBox.Height:=gif.Height;
ThrobberBox.Top:=(InstalledAppsPage.Height div 2)-(ThrobberBox.Height div 2);
ThrobberBox.Left:=(InstalledAppsPage.Width div 2)-(ThrobberBox.Width div 2);
gif.Initialize(ThrobberBox.Canvas);
}
