{ manager.pas
  Copyright (C) Listaller Project 2008-2009

  manager.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  manager.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program.  If not, see <http://www.gnu.org/licenses/>}
//** This unit contains the code to manage installed packages
unit manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Inifiles, StdCtrls, process, LCLType, Buttons, ExtCtrls, distri, common,
  uninstall, trstrings, gettext, FileUtil, xtypefm, ipkhandle, gifanimator,
  packagekit;

type

  { TMnFrm }

  TMnFrm = class(TForm)
    btnInstall: TBitBtn;
    btnSettings: TBitBtn;
    btnCat: TBitBtn;
    CBox: TComboBox;
    edtFilter: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    ThrobberBox: TPaintBox;
    Process1: TProcess;
    StatusBar1: TStatusBar;
    SWBox: TScrollBox;
    procedure btnInstallClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCatClick(Sender: TObject);
    procedure CBoxChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    blst: TStringList;
    procedure UninstallClick(Sender: TObject);
  public
    { public declarations }
    DInfo: TDistroInfo;
    //** List of package id's
    IdList: TStringList;
    //** Visual package list
    AList: Array of TListEntry;
    //** Length of AList
    ListLength: Integer;
    //** Current id of package that should be uninstalled
    uID: Integer;
    //** Process .desktop-file and add info to list @param fname Name of the .desktop file  @param tp Category name
    procedure ProcessDesktopFile(fname: String; tp: String);
    //** Load software list entries
    procedure LoadEntries;
  end;

  //** Fill ImageList with category icons
  procedure FillImageList(IList: TImageList);

var
 //** Main formular instance
  MnFrm:   TMnFrm;
 //** List of installed application names
  instLst: TStringList;

var
//** In this directory package information will be stored
 RegDir: String;

implementation

uses settings, pkgconvertdisp, swcatalog;

{ TListEntry }

procedure TMnFrm.UninstallClick(Sender: TObject);
begin
uID:=(Sender as TBitBtn).Tag;
RMForm.ShowModal;
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

procedure TMnFrm.ProcessDesktopFile(fname: String; tp: String);
var d: TIniFile;
begin
d:=TIniFile.Create(fname);
       Application.ProcessMessages;
       StatusBar1.Panels[0].Text:=strLoading+'  '+ExtractFileName(fname);

       if (not IsRoot)and(d.ReadString('Desktop Entry','Exec','')[1]<>'/')
       then
       else
       if (LowerCase(d.ReadString('Desktop Entry','NoDisplay','false'))<>'true')
       and (pos('yast',LowerCase(fname))<=0)
       and(LowerCase(d.ReadString('Desktop Entry','Hidden','false'))<>'true')
       and(not IsInList(d.ReadString('Desktop Entry','Name',''),blst))
       and((pos(tp,LowerCase(d.ReadString('Desktop Entry','Categories','')))>0)or(tp='all'))
      // and(pos('system',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('core',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('.hidden',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
      // and(pos('base',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('wine',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('wine',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(d.ReadString('Desktop Entry','X-KDE-ParentApp','#')='#')
       and(pos('screensaver',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('setting',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
      // and(pos('utility',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(d.ReadString('Desktop Entry','OnlyShowIn','')='')
       and(d.ReadString('Desktop Entry','X-AllowRemove','true')='true')
       then begin
       SetLength(AList,ListLength+1);
       Inc(ListLength);
       AList[ListLength-1]:=TListEntry.Create(MnFrm);
       AList[ListLength-1].UnButton.OnClick:=@UnInstallClick;
       AList[ListLength-1].Parent:=SWBox;
       AList[ListLength-1].UnButton.Tag:=ListLength-1;

       if pos('apkg-remove',LowerCase(d.ReadString('Desktop Entry','Actions','')))>0 then
       IdList.Add('!'+d.ReadString('Desktop Action Apkg-Remove','Exec',''))
       else
       IdList.Add(fname);

       with AList[ListLength-1] do begin
       aID:=IDList.Count-1;

       if d.ReadString('Desktop Entry','Name['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','<error>') <> '<error>' then
        AppName:=d.ReadString('Desktop Entry','Name['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','<error>')
        else
         AppName:=d.ReadString('Desktop Entry','Name','<error>');

         AppName:=StringReplace(AppName,'&','&&',[rfReplaceAll]);

         instLst.Add(Lowercase(d.ReadString('Desktop Entry','Name','<error>')));

        if d.ReadString('Desktop Entry','Comment['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','')<>'' then
        AppDesc:=d.ReadString('Desktop Entry','Comment['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','')
        else
        AppDesc:=d.ReadString('Desktop Entry','Comment','');

        AppMn:=strAuthor+': '+d.ReadString('Desktop Entry','X-Publisher','<error>');
        if AppMn=strAuthor+': '+'<error>' then
        AppMn:='';
        AppVersion:='';
        if d.ReadString('Desktop Entry','X-AppVersion','')<>'' then
        AppVersion:=strVersion+': '+d.ReadString('Desktop Entry','X-AppVersion','');

        //Load the icons
        if (LowerCase(ExtractFileExt(d.ReadString('Desktop Entry','Icon','')))<>'.tiff') then begin
        if (d.ReadString('Desktop Entry','Icon','')<>'')
        and(d.ReadString('Desktop Entry','Icon','')[1]<>'/') then begin
        try
        if FileExists('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
            SetImage('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') else
        if FileExists('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')) then
            SetImage('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')) else
        if FileExists('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
            SetImage('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') else
        if FileExists('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon','')) then
            SetImage('/usr/share/icons/hicolor/48x48/apps/'+d.ReadString('Desktop Entry','Icon',''));
        //
        if FileExists('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.xpm')
        and (ExtractFileExt(d.ReadString('Desktop Entry','Icon',''))='.xpm')then
            SetImage('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.xpm')
        else if FileExists('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.xpm') then
             SetImage('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.xpm');
        if (FileExists('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.png'))
        and (ExtractFileExt(d.ReadString('Desktop Entry','Icon',''))='.png')then
            SetImage('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.png')
        else if FileExists('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                SetImage('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.png');
        except writeLn('ERROR: Unable to load icon!');ShowMessage(StringReplace(strCannotLoadIcon,'%a',AppName,[rfReplaceAll]));end;
        { This code is EXPERIMENTAL!}
        //Load KDE4 Icons
          //GetEnvironmentVariable('KDEDIRS')

        if FileExists('/usr/share/icons/default.kde/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                SetImage('/usr/share/icons/default.kde/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png')
        else
        if FileExists('/usr/lib/kde4/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                SetImage('/usr/lib/kde4/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png');
        end else begin
         if (FileExists(d.ReadString('Desktop Entry','Icon','')))
         and(LowerCase(ExtractFileExt(d.ReadString('Desktop Entry','Icon','')))<>'.svg') then
            SetImage(d.ReadString('Desktop Entry','Icon',''));
        end;
       end;
        //Reset control's positions
        SetPositions;
        Application.ProcessMessages;
        end;

        end;
       d.Free;
end;

procedure TMnFrm.LoadEntries;
var ireg,ini: TIniFile;tmp,xtmp: TStringList;i,j,k: Integer;p,n: String;tp: String;
    gif: TGifThread;
begin
j:=0;
while ListLength>0 do begin
AList[ListLength-1].Free;
Dec(ListLength);
end;
btnCat.Enabled:=false;
btnInstall.Enabled:=false;
edtFilter.Enabled:=false;
StatusBar1.Panels[0].Text:=strLoading;

//Create GIFThread for Throbber animation
gif:=TGifThread.Create(true);
gif.FileName:=GetDataFile('graphics/throbber.gif');
ThrobberBox.Width:=gif.Width;
ThrobberBox.Height:=gif.Height;
ThrobberBox.Top:=(Height div 2)-(ThrobberBox.Height div 2)+16;
ThrobberBox.Left:=(Width div 2)-(ThrobberBox.Width div 2)+20;
gif.Initialize(ThrobberBox.Canvas);


edtFilter.Text:='';

SwBox.Visible:=false;
if blst.Count<4 then begin
blst.Clear;
blst.LoadFromFile('/etc/lipa/blacklist');
blst.Delete(0);
end;

//Set original names
case CBox.Itemindex of
0: tp:='all';
1: tp:='education';
2: tp:='office';
3: tp:='development';
4: tp:='graphic';
5: tp:='network';
6: tp:='games';
7: tp:='system';
8: tp:='multimedia';
9: tp:='additional';
10: tp:='other';
end;

if not DirectoryExists(RegDir) then begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

ireg:=TInifile.Create(RegDir+'appreg.lst');
tmp:=TStringList.Create;
xtmp:=TStringList.Create;
ireg.ReadSections(xtmp);
IdList.Clear;

if tp='all' then tmp.Assign(xtmp)
else begin
for i:=0 to xtmp.Count-1 do begin

if pos(LowerCase(ireg.ReadString(xtmp[i],'Group','other')),tp)>0 then
tmp.Add(xtmp[i]);
end;
end;
xtmp.free;

k:=0;

for i:=0 to tmp.Count-1 do begin
k:=ListLength+1;
SetLength(AList,k);
ListLength:=k;
Dec(k);
AList[k]:=TListEntry.Create(MnFrm);
AList[k].UnButton.OnClick:=@UnInstallClick;
AList[k].Parent:=SWBox;
AList[k].aId:=k;
AList[k].UnButton.Tag:=k;

AList[k].AppName:=(copy(tmp[i],0,pos('~',tmp[i])-1));

blst.Add(AList[k].AppName);
IdList.Add(copy(tmp[i],pos('~',tmp[i]),length(tmp[i])));

AList[k].AppVersion:=strVersion+': '+(ireg.ReadString(tmp[i],'Version','0.0'));
AList[k].AppMn:=strAuthor+': '+(ireg.ReadString(tmp[i],'Author','#'));
if ireg.ReadString(tmp[i],'Author','#')='#' then AList[k].AppMn:='';
p:=RegDir+AList[k].AppName+'-'+copy(tmp[i],pos('~',tmp[i])+1,length(tmp[i]))+'/';

InstLst.Add(LowerCase(ireg.ReadString(tmp[i],'idName',AList[k].AppName)));
aList[k].AppDesc:=(ireg.ReadString(tmp[i],'SDesc','No description given'));
if AList[k].AppDesc='#' then AList[k].AppDesc:='No description given';

if FileExists(p+'icon.png') then
AList[k].SetImage(p+'icon.png');

Inc(k);
Application.ProcessMessages;
end;
ireg.Free;
tmp.Free;


{if (CBox.ItemIndex=0) or (CBox.ItemIndex=10) then begin
tmp:=TStringList.Create;
xtmp:=TStringList.Create;

j:=0;
for i:=0 to xtmp.Count-1 do begin
try
ReadXMLFile(Doc, xtmp[i]);
xnode:=Doc.FindNode('product');
 SetLength(AList,ListLength+1);
 Inc(ListLength);
 AList[ListLength-1]:=TListEntry.Create(MnFrm);
 AList[ListLength-1].Parent:=SWBox;
 AList[ListLength-1].AppLabel.Caption:=xnode.Attributes.GetNamedItem('desc').NodeValue;
 instLst.Add(LowerCase(xnode.Attributes.GetNamedItem('desc').NodeValue));
 blst.Add(AList[ListLength-1].AppLabel.Caption);
xnode:=Doc.DocumentElement.FindNode('component');
 AList[ListLength-1].Vlabel.Caption:=strVersion+': '+xnode.Attributes.GetNamedItem('version').NodeValue;
IdList.Add(xtmp[i]);
//Unsupported
AList[ListLength-1].MnLabel.Visible:=false;
AList[ListLength-1].DescLabel.Visible:=false;
AList[Listlength-1].id:=IDList.Count-1;
AList[ListLength-1].SetPositions;
Application.ProcessMessages;
except
j:=101;
end;
end;

tmp.free;
xtmp.Free;

end; //End Autopackage  }


n:=ConfigDir;
ini:=TIniFile.Create(n+'config.cnf');

//Search for other applications that are installed on this system...
tmp:=TStringList.Create;
xtmp:=TStringList.Create;

if IsRoot then //Only if user is root
begin
tmp.Assign(FindAllFiles('/usr/share/applications/','*.desktop',true));
xtmp.Assign(FindAllFiles('/usr/local/share/applications/','*.desktop',true));
end else tmp.Assign(FindAllFiles(GetEnvironmentVariable('HOME')+'/.local/share/applications','*.desktop',false));

for i:=0 to xtmp.Count-1 do tmp.Add(xtmp[i]);
xtmp.Free;
if tp='games' then tp:='game';
if tp='multimedia' then tp:='audiovideo';
for i:=0 to tmp.Count-1 do begin
       ProcessDesktopFile(tmp[i],tp);
       end;
       tmp.Free;
ini.Free;

//Check LOKI-success:
if j>100 then
StatusBar1.Panels[0].Text:=strLOKIError;

StatusBar1.Panels[0].Text:=strReady; //Loading list finished!

btnCat.Enabled:=true;

btnInstall.Enabled:=true;
edtFilter.Enabled:=true;
SwBox.Visible:=true;
If Assigned(gif) Then gif.Terminate;
gif := Nil;

for i:=0 to ListLength-1 do AList[i].SetPositions;
end;

var fAct: Boolean;
procedure TMnFrm.FormShow(Sender: TObject);
begin
fAct:=true;
btnCat.Left:=btnInstall.Left+btnInstall.Width+14;
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
  if FileExists(OpenDialog1.Filename) then begin
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.ipk')
  or (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.zip') then
  begin
  Process1.CommandLine := ExtractFilePath(Application.ExeName)+'listallgo '+OpenDialog1.Filename;
  Process1.Execute;
  MnFrm.Hide;
  while Process1.Running do Application.ProcessMessages;
  MnFrm.Show;
  end else begin
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.deb') then
  if DInfo.PackageSystem='DEB' then begin
  //Open DEB-File
   p:=TProcess.Create(nil);
   p.Options:=[poWaitOnExit,poNewConsole];
   Application.ProcessMessages;
   p.CommandLine:='xdg-open '+''''+OpenDialog1.FileName+'''';
   p.Execute;
   p.Free;
   exit;
  end else
   if Application.MessageBox(PAnsiChar(StringReplace(StringReplace(strConvertPkg,'%x','DEB',[rfReplaceAll]),'%y','RPM',[rfReplaceAll])),
                              PAnsiChar(strConvertPkgQ),MB_YESNO)=IDYES then begin
   with ConvDisp do begin
   if not FileExists('/usr/bin/alien') then
    if Application.MessageBox(PChar(strListallerAlien),PChar(strInstPkgQ),MB_YESNO)=IDYES then begin
      ShowMessage(strplWait);
      pkit:=TPackageKit.Create;
      if not pkit.InstallPkg('alien') then begin
       ShowMessage(StringReplace(strPkgInstFail,'%p','alien',[rfreplaceAll]));
       pkit.Free;
       exit;
       end;
      pkit.Free;
     end else exit;
   Application.ProcessMessages;
   Caption:=StringReplace(strConvTitle,'%p','DEB',[rfReplaceAll]);
   Process1.CommandLine:='alien --to-rpm -v -i --scripts '+''''+OpenDialog1.FileName+'''';
   GetOutPutTimer.Enabled:=true;
   Process1.Execute;
   ShowModal;
   end;
   exit;
   end;
  if (LowerCase(ExtractFileExt(OpenDialog1.FileName))='.rpm') then
  if DInfo.PackageSystem='RPM' then begin
   //Open RPM-File
   p:=TProcess.Create(nil);
   p.Options:=[poWaitOnExit,poNewConsole];
   Application.ProcessMessages;
   p.CommandLine:='xdg-open '+''''+OpenDialog1.FileName+'''';
   p.Execute;
   p.Free;
   exit;
  end else
   if Application.MessageBox(PAnsiChar(StringReplace(StringReplace(strConvertPkg,'%x','RPM',[rfReplaceAll]),'%y','DEB',[rfReplaceAll])),
                              PAnsiChar(strConvertPkgQ),MB_YESNO)=IDYES then begin
   with ConvDisp do begin

   if not FileExists('/usr/bin/alien') then
    if Application.MessageBox(PChar(strListallerAlien),PChar(strInstPkgQ),MB_YESNO)=IDYES then begin
      ShowMessage(strplWait);
      pkit:=TPackageKit.Create;
      if not pkit.InstallPkg('alien') then begin
       ShowMessage(StringReplace(strPkgInstFail,'%p','alien',[rfreplaceAll]));
       pkit.Free;
       exit;
       end;
      pkit.Free;
     end else exit;

   Application.ProcessMessages;
   Caption:=StringReplace(strConvTitle,'%p','RPM',[rfReplaceAll]);
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

procedure TMnFrm.BitBtn2Click(Sender: TObject);
begin
RMForm.ShowModal;
end;

procedure TMnFrm.btnSettingsClick(Sender: TObject);
begin
  FmConfig.ShowModal;
end;

procedure TMnFrm.btnCatClick(Sender: TObject);
begin
  SCForm.ShowModal;
end;

procedure TMnFrm.CBoxChange(Sender: TObject);
begin
  CBox.Enabled:=false;
  LoadEntries;
  CBox.Enabled:=true;
end;

procedure TMnFrm.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: Integer;
begin
  if Key = VK_RETURN then begin
     if ((edtFilter.Text=' ') or (edtFilter.Text='*')or (edtFilter.Text='')) then begin
     for i:=0 to ListLength-1 do
     AList[i].Visible:=true;
     end else begin
     Application.ProcessMessages;
     StatusBar1.Panels[0].Text:=strFiltering;
     for i:=0 to ListLength-1 do begin
      AList[i].Visible:=true;
       Application.ProcessMessages;
        if ((pos(LowerCase(edtFilter.Text),LowerCase(AList[i].AppName))<=0)
        or (pos(LowerCase(edtFilter.Text),LowerCase(AList[i].AppDesc))<=0))
         and (LowerCase(edtFilter.Text)<>LowerCase(AList[i].AppName)) then
         AList[i].Visible:=false;
         end;
       end;
StatusBar1.Panels[0].Text:=strReady;
end;
end;

procedure TMnFrm.FormActivate(Sender: TObject);
begin
  if fAct then begin fAct:=false;btnCat.Left:=btnInstall.Left+btnInstall.Width+12;LoadEntries;end;
end;

procedure TMnFrm.FormCreate(Sender: TObject);
var xFrm: TimdFrm;
begin
if FileExists(paramstr(1)) then begin
  Process1.Options:=[];
  Process1.CommandLine := ExtractFilePath(Application.ExeName)+'listallgo '+paramstr(1);
  Process1.Execute;
  Application.Terminate;
  exit;
end;

SWBox.DoubleBuffered:=true;
DoubleBuffered:=true;
DInfo:=GetDistro;

 if not DirectoryExists(RegDir) then SysUtils.CreateDir(RegDir);
  
 uID:=-1;
 ListLength:=0;

 IdList:=TStringList.Create;

 Caption:=strSoftwareManager;
 btnInstall.Caption:=strInstNew;
 btnSettings.Caption:=strShowSettings;
 btnCat.Caption:=strSWCatalogue;
 Label1.Caption:=strShow;
 Label2.Caption:=strFilter;
 Label3.Caption:=strNoAppsFound;

 if not IsRoot then begin
 xFrm:=TimdFrm.Create(nil);

 //Set reg-dir
 RegDir:=SyblToPath('$INST/app-reg/');


 with xFrm do begin
  Caption:=strSelMgrMode;
  btnTest.Visible:=false;
  Image1.Visible:=false;
  Label13.Visible:=false;
  btnCat.Caption:=strSWCatalogue;
  btnInstallAll.Caption:=strDispRootApps;
  btnHome.Caption:=strDispOnlyMyApps;
  Refresh;
  ShowModal;
 end;
xFrm.Free;
end else
RegDir:='/etc/lipa/app-reg/';

if not DirectoryExists(RegDir) then CreateDir(RegDir);

with CBox do begin
 Items[0]:=strAll;
 Items[1]:=strEducation;
 Items[2]:=strOffice;
 Items[3]:=strDevelopment;
 Items[4]:=strGraphic;
 Items[5]:=strNetwork;
 Items[6]:=strGames;
 Items[7]:=strSystem;
 Items[8]:=strMultimedia;
 Items[9]:=strAddidional;
 Items[10]:=strOther;
end;
 Image1.Picture.LoadFromFile(GetDataFile('graphics/header.png'));
 Application.ShowMainForm:=true;
 instLst:=TStringList.Create;
 blst:=TStringList.Create; //Create Blacklist

 //Create uninstall panel
Application.CreateForm(TRMForm, RMForm);
//Option check
if (Application.HasOption('u','uninstall'))and(IsRoot) then
begin
 if paramstr(2)[1]='/' then ProcessDesktopFile(paramstr(2),'all')
 else IdList.Add(paramstr(2));

 uId:=0;
 RMForm.ShowModal;
 Application.Terminate;
 halt(0);
end;

 WriteLn('GUI loaded.');
end;

procedure TMnFrm.FormDestroy(Sender: TObject);
var i: Integer;
begin
  if Assigned(blst) then blst.Free; //Free blacklist
  if Assigned(IdList) then IdList.Free;
  if Assigned(InstLst) then InstLst.Free;
  for i:=0 to ListLength-1 do AList[i].Free;
end;

procedure TMnFrm.FormResize(Sender: TObject);
var i: Integer;
begin
  for i:=0 to ListLength-1 do
  AList[i].SetPositions;
end;

initialization
  {$I manager.lrs}

end.

