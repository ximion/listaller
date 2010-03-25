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
//** This unit contains the code to manage installed packages
unit manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Inifiles, StdCtrls, Process, LCLType, Buttons, ExtCtrls, Distri, AppList,
  Uninstall, trStrings, FileUtil, CheckLst, AppMan, LiBasic, liTypes, AboutBox,
  PackageKit, Spin, Menus, iconLoader, LiCommon, AppItem;

type

  { TMnFrm }

  TMnFrm = class(TForm)
    AppDescLbl: TLabel;
    AppImage: TImage;
    AppInfoPanel: TPanel;
    AppNameLbl: TLabel;
    AppVersionLbl: TLabel;
    BitBtn2: TBitBtn;
    AboutBtn: TButton;
    AppViewControl: TPageControl;
    BtnPanel: TPanel;
    CatButton: TSpeedButton;
    InstAppButton: TSpeedButton;
    MBar: TProgressBar;
    MenuItem2: TMenuItem;
    RepoButton: TSpeedButton;
    SettingsButton: TSpeedButton;
    SpacerPan: TPanel;
    SWBox: TPanel;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Button1: TButton;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MItemInstallPkg: TMenuItem;
    Splitter1: TSplitter;
    StatusLabel: TLabel;
    RmUpdSrcBtn: TBitBtn;
    SWBoxSU: TPanel;
    MyAppSheet: TTabSheet;
    SysAppSheet: TTabSheet;
    UnButton: TBitBtn;
    UpdCheckBtn: TBitBtn;
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
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
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
    procedure appListItemSelect(Sender: TObject; item: TAppInfoItem);
    procedure AppViewControlChange(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCatClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBoxChange(Sender: TObject);
    procedure AutoDepLdCbChange(Sender: TObject);
    procedure CbShowPkMonChange(Sender: TObject);
    procedure EnableProxyCbChange(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MItemInstallPkgClick(Sender: TObject);
    procedure MyAppSheetShow(Sender: TObject);
    procedure RmUpdSrcBtnClick(Sender: TObject);
    procedure SysAppSheetShow(Sender: TObject);
    procedure UListBoxClick(Sender: TObject);
    procedure UnButtonClick(Sender: TObject);
    procedure UpdCheckBtnClick(Sender: TObject);
    procedure WarnDistCbChange(Sender: TObject);
    procedure FilterEdtEnter(Sender: TObject);
    procedure FilterEdtExit(Sender: TObject);
    procedure FilterEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InstAppButtonClick(Sender: TObject);
    procedure RepoButtonClick(Sender: TObject);
  private
    { private declarations }

    blst: TStringList;
    //** Selected application item
    activeRmItem: TAppInfoItem;
    //** Visual application list
    appList: TAppListView;
    //** Visual application list for shared apps
    appListSU: TAppListView;
    //** Visual applist for search results
    appResList: TAppListView;
    procedure UninstallClick(Sender: TObject; Index: Integer;
              item: TAppInfoItem);
    function CreateNewUserAppList: TAppListView;
    function CreateNewSuAppList: TAppListView;
  public
    { public declarations }
    DInfo: TDistroInfo;
    //** Information about the application that should be uninstalled
    uApp: TAppInfo;
    //** Pointer to our AppManager object
    amgr: Pointer;
    //** Reference to current active list
    currAppList: TAppListView;
    //** Reload the current application list
    procedure ReloadAppList(const force: Boolean=false);
  end;

  //** Fill ImageList with category icons
  procedure FillImageList(IList: TImageList);

var
 //** Main formular instance
 MnFrm:   TMnFrm;
 //** List of installed application names
 instLst: TStringList;
 //** Show Su-Apps (or not)
 SUApps: Boolean=false;

//Published for use in uninstall.pas
procedure OnMgrStatus(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;

//NOTE:
//IMPORTANT: RegDir should be moved to libInstaller!

implementation

uses pkgconvertdisp;

{ TMainForm }

procedure TMnFrm.UninstallClick(Sender: TObject; Index: Integer;
  item: TAppInfoItem);
begin
 //Set the AppInfo of the to-be-removed app
 with uApp do
 begin
  UId:=PChar(item.UId);
  Name:=PChar(item.Name);
  Version:=PChar(item.Version);
  Author:=PChar(item.Author);
 end;
//Start removing by showing the uninstall form
RMForm.ShowModal;
end;

function OnNewAppFound(name: PChar;obj: PAppInfo): Boolean;cdecl;
begin
Result:=true;
with MnFrm do
begin
 currAppList.ItemFromAppInfo(TAppInfo(obj^));
end;
Application.ProcessMessages;
end;

procedure OnMgrStatus(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;
begin
case change of
 scStepmessage: MnFrm.StatusLabel.Caption:=data.msg;
 scMessage    : begin
                     MnFrm.StatusLabel.Caption:=data.msg;
                      //p_info(data.msg);
                end;
end;
end;

function OnUserRequest(mtype: TRqType;msg: PChar;data: Pointer): TRqResult;cdecl;
begin
Result:=rqsOK;
case mtype of
rqError: begin Application.MessageBox(msg,PChar(rsError),MB_OK+MB_IconError);
               RMForm.RmProcStatus:=prFailed;
         end;
rqWarning:  if Application.MessageBox(msg,PChar(rsWarning),MB_YesNo+MB_IconWarning)= idYes then
             Result:=rqsYes else Result:=rqsNo;
rqQuestion: if Application.MessageBox(msg,PChar(rsQuestion),MB_YesNo+MB_IconQuestion)= idYes then
             Result:=rqsYes else Result:=rqsNo;
rqInfo: ShowMessage(msg);
end;
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

function TMnFrm.CreateNewSuAppList: TAppListView;
begin
 //New applist for shared apps
 Result:=TAppListView.Create(self); //New application list
 with Result do
 begin
  Parent:=SWBoxSU;
  Align:=alClient;
  OnRmButtonClick:=@UninstallClick;
  OnItemSelect:=@appListItemSelect;
 end;
end;

function TMnFrm.CreateNewUserAppList: TAppListView;
begin
 //New applist for user-apps
 Result:=TAppListView.Create(self); //New application list
 with Result do
 begin
  Parent:=SWBox;
  Align:=alClient;
  OnRmButtonClick:=@UninstallClick;
  OnItemSelect:=@appListItemSelect;
 end;
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

procedure TMnFrm.ReloadAppList(const force: Boolean=false);
begin
if currAppList=appList then
begin
 SUApps:=false;
 li_mgr_set_sumode(@aMgr,SuApps);
 if (appList.Count<=0)or(force) then
 begin
 appList.ClearList;
{ if appList is TAppListView then appList.Free;
 appList:=CreateNewUserAppList;}
 li_mgr_load_apps(@MnFrm.amgr);
 end;
end else
if currAppList=appListSU then
begin
 SUApps:=true;
 li_mgr_set_sumode(@aMgr,SuApps);
 if (appListSU.Count<=0)or(force) then
 begin
  appListSU.ClearList;
  {if appListSU is TAppListView then appListSU.Free;
 appListSU:=CreateNewSuAppList;}
  li_mgr_load_apps(@MnFrm.amgr);
 end;
end;
end;

procedure TMnFrm.appListItemSelect(Sender: TObject; item: TAppInfoItem);
begin
 if FileExists(item.IconPath) then
  AppImage.Picture.LoadFromFile(item.IconPath);
 AppNameLbl.Caption:=item.Name;
 AppDescLbl.Caption:=item.SDesc;
 if item.Version<>'' then
  AppVersionLbl.Caption:=item.Version
 else
  AppVersionLbl.Caption:='Version: unknown';
 AppInfoPanel.Visible:=true;
 activeRmItem:=item;
end;

procedure TMnFrm.AppViewControlChange(Sender: TObject);
begin

end;

procedure TMnFrm.BitBtn6Click(Sender: TObject);
var p: TProcess;
begin
  p:=TProcess.Create(nil);
  p.Options:=[poUsePipes];
  if DInfo.DBase='KDE' then
  begin
     if FileExists(FindBinary('kpackagekit')) then
      p.CommandLine:=FindBinary('kpackagekit')+' --settings'
     else
      p.CommandLine:=FindBinary('gpk-repo');
  end else
  begin
     if FileExists(FindBinary('gpk-repo')) then
      p.CommandLine:=FindBinary('gpk-repo')
     else
      p.CommandLine:=FindBinary('kpackagekit');
  end;
  Notebook1.Enabled:=false;
  BtnPanel.Enabled:=false;
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
  BtnPanel.Enabled:=true;
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
    if FileExists(FindBinary('qappinstall')) then
     p.CommandLine:=FindBinary('qappinstall')
    else
     p.CommandLine:=FindBinary('kpackagekit')
   else
     if FileExists(FindBinary('kpackagekit')) then
      p.CommandLine:=FindBinary('kpackagekit')
     else
      p.CommandLine:=FindBinary('gpk-application');
  end else
  begin
    if (DInfo.DName='Ubuntu') then
    if FileExists(FindBinary('software-center')) then
     p.CommandLine:=FindBinary('software-center')
    else
     p.CommandLine:=FindBinary('gpk-application')
   else
     if FileExists(FindBinary('gpk-application')) then
      p.CommandLine:=FindBinary('gpk-application')
     else
      p.CommandLine:=FindBinary('kpackagekit');
  end;
  if not FileExists(p.CommandLine) then
  begin
   ShowMessage(rsNoGUIPkgManFound);
   p.Free;
   exit;
  end;
  Notebook1.Enabled:=false;
  BtnPanel.Enabled:=false;
  MBar.Visible:=true;
  p.Execute;
  while p.Running do Application.ProcessMessages;
  p.Free;
  Notebook1.Enabled:=true;
  BtnPanel.Enabled:=true;
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
   appList.Visible:=true;
   appResList.ClearList;
   appResList.Visible:=false;
   AppInfoPanel.Visible:=false;
  end else
  begin
  for i:=0 to appList.Count-1 do
  begin
   Application.ProcessMessages;
   if appList.AppItems[i].Group=gt then
      appResList.AddItem(appList.AppItems[i]);
  end;
   appResList.Visible:=true;
   AppInfoPanel.Visible:=false;
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
  if (Sender as TCheckBox).Checked then
  begin
  Edit1.Enabled:=true;
  SpinEdit1.Enabled:=true;
  edtUsername.Enabled:=true;
  edtPasswd.Enabled:=true;
  edtFTPProxy.Enabled:=true;
  spinEdit2.Enabled:=true;
  Label4.Enabled:=true;
  Label5.Enabled:=true;
  end else
  begin
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

procedure TMnFrm.MenuItem2Click(Sender: TObject);
var rep: TStringList;
    root: Boolean;
    appcheckmgr: Pointer;

procedure LogQ;
begin
if Application.MessageBox(PAnsiChar(rsViewLogQ), 'ViewLog', MB_YESNO+MB_IconQuestion)=IDYES then
  ShowMessage(rep.Text);
end;

procedure PerformCheck;
begin
Notebook1.Visible:=false;
BtnPanel.Enabled:=false;
MBar.Visible:=true;
if not li_mgr_check_apps(@appcheckmgr,@rep,root) then
begin
 if Application.MessageBox(PAnsiChar(rsBrokenDepsFixQ), 'FixDeps', MB_YESNO+MB_IconQuestion)=IDYES then
   li_mgr_fix_apps(@appcheckmgr,@rep,root);
end;
Notebook1.Visible:=true;
BtnPanel.Enabled:=true;
MBar.Visible:=false;
end;

begin
root:=false;
if Application.MessageBox(PAnsiChar(rsCheckAppDepsQ), PChar(rsCheckDepsQ), MB_YESNO+MB_IconQuestion)=
  IDYES then
begin
 appcheckmgr:=li_mgr_new; //New appmanager
 if Application.MessageBox(PAnsiChar(rsCheckRootAppsQ), PChar(rsCheckDepsQ), MB_YESNO+MB_IconQuestion)=IDYES then
 begin
  rep:=TStringList.Create;
  PerformCheck;
  root:=true;
  PerformCheck;
 end else PerformCheck;
 LogQ;
 li_mgr_free(@appcheckmgr);
end;
end;

procedure TMnFrm.MItemInstallPkgClick(Sender: TObject);
var p: TProcess;pkit: TPackageKit;
begin
  if OpenDialog1.Execute then
  if FileExists(OpenDialog1.Filename) then
  begin
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.ipk')
  or (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.xz') then
  begin
  p:=TProcess.Create(nil);
  p.Options:=[];
  p.CommandLine := ExtractFilePath(Application.ExeName)+'listallgo '+OpenDialog1.Filename;
  p.Execute;
  MnFrm.Hide;
  while p.Running do Application.ProcessMessages;
  p.Free;
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

//No throbber needed at time. Maybe later.
{function TMnFrm.NewThrobber: TGifThread;
begin
  //Create GIFThread for Throbber animation
  Result:=TGifThread.Create(true);
  Result.FileName:=GetDataFile('graphics/throbber.gif');
  ThrobberBox.Width:=Result.Width;
  ThrobberBox.Height:=Result.Height;
  ThrobberBox.Top:=(AppViewControl.Height div 2)-(ThrobberBox.Height div 2);
  ThrobberBox.Left:=(AppViewControl.Width div 2)-(ThrobberBox.Width div 2);
  Result.Initialize(ThrobberBox.Canvas);
end; }

procedure TMnFrm.MyAppSheetShow(Sender: TObject);
begin
 AppInfoPanel.Visible:=false;
 currAppList:=appList;
 SWBox.Visible:=false;
 MBar.Visible:=true;

 ReloadAppList;
 SWBox.Visible:=true;
 MBar.Visible:=false;
end;

procedure TMnFrm.RmUpdSrcBtnClick(Sender: TObject);
var uconf: TStringList;
begin
if UListBox.ItemIndex>-1 then
begin
 if Application.MessageBox(PChar(rsRmSrcQ),PChar(rsRmSrcQC),MB_YESNO+MB_IconWarning)=IDYES then
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

procedure TMnFrm.SysAppSheetShow(Sender: TObject);
begin
 AppInfoPanel.Visible:=false;
 currAppList:=appListSU;
 MBar.Visible:=true;
 SWBoxSU.Visible:=false;
 ReloadAppList;
 SWBoxSU.Visible:=true;
 MBar.Visible:=false;
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

procedure TMnFrm.UnButtonClick(Sender: TObject);
begin
  UninstallClick(Sender,-1,activeRmItem);
end;

procedure TMnFrm.UpdCheckBtnClick(Sender: TObject);
var p: TProcess;
begin
if FileExists(ExtractFilePath(Application.ExeName)+'liupdate') then
begin
  p:=TProcess.Create(nil);
  p.Options:=[];
  p.CommandLine:=ExtractFilePath(Application.ExeName)+'liupdate';
  p.Execute;
  p.Free;
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
     SWBox.Enabled:=false;
     FilterEdt.Enabled:=false;
     CBox.Enabled:=false;
     Application.ProcessMessages;

     if ((FilterEdt.Text=' ')
     or (FilterEdt.Text='*')
     or (FilterEdt.Text=rsFilter)
     or (FilterEdt.Text='')) then
     begin
      appResList.ClearList;
      appResList.Visible:=false;
     end else
     begin
     Application.ProcessMessages;
     StatusLabel.Caption:=rsFiltering;
     appResList.ClearList;
     appResList.Parent:=currAppList.Parent;
     for i:=0 to currAppList.Count-1 do
     begin
       Application.ProcessMessages;
        if ((pos(LowerCase(FilterEdt.Text),LowerCase(currAppList.AppItems[i].Name))>0)
        or (pos(LowerCase(FilterEdt.Text),LowerCase(currAppList.AppItems[i].SDesc))>0))
         and (LowerCase(FilterEdt.Text)<>LowerCase(currAppList.AppItems[i].Name)) then
         begin
          appResList.AddItem(currAppList.AppItems[i]);
         end;
     end;
      appResList.Visible:=true;
     end;

     AppInfoPanel.Visible:=false;
StatusLabel.Caption:=rsReady;
FilterEdt.Enabled:=true;
SWBox.Enabled:=true;
CBox.Enabled:=true;
FilterEdt.SetFocus;
end;
end;

procedure TMnFrm.FormCreate(Sender: TObject);
var i: Integer;tmp: TStringList;
begin
SWBox.DoubleBuffered:=true;
DoubleBuffered:=true;
DInfo:=GetDistro;

amgr:=li_mgr_new; //Create new app manager

 if not DirectoryExists(RegDir) then SysUtils.CreateDir(RegDir);
  
 uApp.UId:='';

 SuApps:=IsRoot;

 //Set reg-dir
 RegDir:=SyblToPath('$INST/app-reg/');


li_mgr_set_sumode(@aMgr,SuApps);

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
  UListBox.items.Add(copy(tmp[i],pos(' (',tmp[i])+2,length(tmp[i])-pos(' (',tmp[i])-2)+' ('+copy(tmp[i],3,pos(' (',tmp[i])-3)+')');
  UListBox.Checked[UListBox.Items.Count-1]:=tmp[i][1]='-';
  end;

 //New applist for user-apps
 appList:=CreateNewUserAppList;

 //New applist for shared apps
 appListSU:=CreateNewSuAppList;

 //Applist to display search results
 appResList:=TAppListView.Create(self,false);
 with appResList do
 begin
  Parent:=SWBox;
  Align:=alClient;
  OnRmButtonClick:=@UninstallClick;
  OnItemSelect:=@appListItemSelect;
  Visible:=false;
 end;

 if SuApps then
 begin
  AppViewControl.ActivePage:=SysAppSheet;
  currAppList:=appListSU;
 end else
 begin
  AppViewControl.ActivePage:=MyAppSheet;
  currAppList:=appList;
 end;

 AppInfoPanel.Caption:='';
 AppInfoPanel.Visible:=false;
 LoadStockPixmap(STOCK_DELETE,ICON_SIZE_BUTTON,UnButton.Glyph);


 //Translate
 Caption:=rsSoftwareManager;
 CatButton.Caption:=rsPackageLists;
 Label1.Caption:=rsShow;
 AboutBtn.Caption:=rsAboutListaller;
 BitBtn2.Caption:=rsOpenDirsiCatalog;
 InstAppButton.Caption:=rsApplications;//rsInstalledApps;
 MItemInstallPkg.Caption:=rsInstallPkg;
 RepoButton.Caption:=rsRepositories;
 SettingsButton.Caption:=rsSettings;
 FilterEdt.Text:=rsFilter;
 MyAppSheet.Caption:=rsMyApps;
 SysAppSheet.Caption:=rsSharedApps;
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
li_mgr_register_status_call(@amgr,@OnMgrStatus,nil);

//Register request call
li_mgr_register_request_call(@amgr,@OnUserRequest,nil);

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
  if Assigned(appResList) then appResList.Free; //Free app reult list
  if Assigned(appList) then appList.Free; //Free application list
  if Assigned(appListSU) then appListSU.Free; //Free application SU list
end;

initialization
  {$I manager.lrs}

end.
