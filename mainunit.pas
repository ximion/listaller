{ mainunit.pas
  Copyright (C) Listaller Project 2008-2009

  mainunit.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  mainunit.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains the code for the graphical installation of standard IPK-packages
unit mainunit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, AbUnZper, AbArcTyp, StdCtrls, IniFiles, FileUtil, ExtCtrls,
  process, Buttons, LCLType, MD5, LCLIntf, distri, utilities, HTTPSend,
  blcksock, ftpsend, trstrings, translations, gettext, gtk2, gtkint, gtkdef, gtkproc,
  XMLRead, DOM, SynEdit, xtypefm, ipkhandle;

type
    
  { TIWizFrm }

  //** The installer wizard window
  TIWizFrm = class(TForm)
    AbortBtn1: TBitBtn;
    Button1: TBitBtn;
    btn_sendinput: TButton;
    Button5: TBitBtn;
    CheckBox1: TCheckBox;
    CbExecApp: TCheckBox;
    Edit1: TEdit;
    ExProgress: TProgressBar;
    FinBtn1: TBitBtn;
    GetOutPutTimer: TIdleTimer;
    GroupBox1: TGroupBox;
    Image2: TImage;
    InfoMemo: TMemo;
    InsProgress: TProgressBar;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    LblTestMode: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    LicMemo: TMemo;
    Notebook1: TNotebook;
    OpenDialog1: TOpenDialog;
    IMPage: TPage;
    ModeGroup: TRadioGroup;
    WPage: TPage;
    DPage: TPage;
    LPage: TPage;
    IPage: TPage;
    FinPage: TPage;
    Panel1: TPanel;
    Process1: TProcess;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure AbortBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FinBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetOutputTimerTimer(Sender: TObject);
    procedure btn_sendinputClick(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
  private
    { private declarations }
    RmApp: Boolean;
    AbortIns: Boolean;
    procedure HookSock(Sender: TObject; Reason:THookSocketReason; const Value: string);
    procedure StartInstallation;
  public
    { public declarations }
    //** Information about the application that should be installed
    IAppName,IAppVersion, IAppCMD, IAuthor, DescFile,ShDesc, LicenseFile: String;
    //** Information about the current package
    PkgName, pID, idName, AType: String;
    //** Dependency list
    Dependencies: TStringList;
    //**Profiles list
    Profiles: TStringList;
    //** File information
    FFileInfo: String;
    //** Current MD5 Hash
    MDHash: String;
    //** Update source
    USource: String;
    //** Path of the package icon
    IconPath: String;
    //** Execute external applications that are linked in the IPK-file
    ExecA, ExecB, ExecX: String;
  end; 

var
  IWizFrm: TIWizFrm;
  FDir:  String;
  //** Distribution information
  DInfo: TDistroInfo;
  //** Set if application installs shared files
  ContSFiles: Boolean=false;
  
const
  //** Working directory of Listaller
  lp='/tmp/';

implementation

uses DGUnit;

{ TIWizFrm }

procedure TIWizFrm.Button1Click(Sender: TObject);
begin
case Notebook1.PageIndex of
5: exit;
4: exit;
3: begin
    FFileInfo:='/stuff/fileinfo-'+copy(Profiles[ModeGroup.ItemIndex],pos(' #',Profiles[ModeGroup.ItemIndex])+2,length(Profiles[ModeGroup.ItemIndex]))+'.id';
    Button5.Visible:=false;
    Button1.Visible:=false;
    NoteBook1.PageIndex:=4;
    StartInstallation;
    exit;
  end;
2: begin
        if FFileInfo<>'*' then begin
           Button5.Visible:=false;
           Button1.Visible:=false;
           NoteBook1.PageIndex:=4;
           StartInstallation;
           exit;
        end;
        Button1.Caption:=strInstallNow;
        NoteBook1.PageIndex:=3;
   end;
1: begin
 if (LicenseFile='')and(FFileInfo<>'*') then begin
  Button5.Visible:=false;
  Button1.Visible:=false;
  NoteBook1.PageIndex:=3;
  StartInstallation;
  exit;
 end;

     if FFileInfo<>'*' then begin
        Button1.Caption:=strInstallNow;
     end;

  if RadioButton2.Checked then
  Button1.Enabled:=false
  else
  Button1.Enabled:=true;

   if (LicenseFile='') then begin
    Button1.Caption:=strInstallNow;
    Notebook1.PageIndex:=3 end else
    Notebook1.PageIndex:=2;
  end;
0: begin
 if (DescFile='')and(LicenseFile='')and(FFileInfo<>'*') then begin
  Button5.Visible:=false;
  Button1.Visible:=false;
  NoteBook1.PageIndex:=3;
  StartInstallation;
  exit;
 end;
 if (DescFile='') then begin
  Button1.Caption:=strInstallNow;
 if RadioButton2.Enabled then
  Button1.Enabled:=false;
  NoteBook1.PageIndex:=2;
  Button5.Visible:=true;
  exit;
 end;
 if (DescFile<>'')and(LicenseFile='') then begin
  NoteBook1.PageIndex:=1;
  Button5.Visible:=true;
  Button1.Caption:=strInstallNow;
  exit;
 end;
  NoteBook1.PageIndex:=1;
  Button5.Visible:=true;
  end;
end;
end;

procedure TIWizFrm.AbortBtn1Click(Sender: TObject);
begin
if NoteBook1.PageIndex=5 then begin
  Label10.Caption:=strInstAborted;
  Label11.Caption:=StringReplace(strAppNInstall,'%a',IAppName,[rfReplaceAll]);
  AbortIns:=true;
end else
  Application.Terminate;
end;

procedure TIWizFrm.Button5Click(Sender: TObject);
begin
case NoteBook1.PageIndex of
1: begin
  Button1.Caption:=strNext;
  Notebook1.PageIndex:=0;
  Button5.Visible:=false;
  Button1.Enabled:=true;
  end;
2: begin
  Button1.Caption:=strNext;
 { Button1.Width:=83;
  Button1.Left:=566; }
  if DescFile='' then
  Notebook1.PageIndex:=0
  else
  Notebook1.PageIndex:=1;
  Button1.Enabled:=true;
  end;
3: begin
        Button1.Caption:=strNext;
        if LicenseFile='' then
        Notebook1.PageIndex:=1
        else
        Notebook1.PageIndex:=2;
        Button1.Enabled:=true;
   end;
4: exit;
end;
end;

procedure TIWizFrm.CheckBox1Change(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then begin
  ListBox1.Visible:=true;
  Infomemo.Visible:=true;
  end else begin
  ListBox1.Visible:=false;
  Infomemo.Visible:=false;
  end;
end;

procedure TIWizFrm.FinBtn1Click(Sender: TObject);
begin
  FinBtn1.Enabled:=false;
  Label16.Caption:='Clean workdir...';
  Label16.Visible:=true;
  FinPage.Refresh;
  DeleteDirectory(lp+ExtractFileName(paramstr(1)),false);

  if (IAppCMD<>'#')and(CbExecApp.Checked) then begin
  Process1.CommandLine:=IAppCMD;
  Process1.Execute;
  end;

  Application.Terminate;
end;

function IsCommandRunning(cmd:String):Boolean;
var t:TProcess;
s:TStringList;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.CommandLine:='ps -A'+cmd;
 t.Options:=[poUsePipes,poWaitonexit];
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   Result:=Pos(cmd,s.Text)>0;
  finally
  s.free;
  end;
 finally
 t.Free;
 end;
end;

function FindChildNode(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i].FirstChild;break;exit;end else
Result:=nil;
end;
end;

function FindChildNodeX(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i];break;exit;end else
Result:=nil;
end;
end;

procedure TIWizFrm.FormCreate(Sender: TObject);
var z: TAbUnZipper;ar: TIniFile;i: Integer;x: TStringList;t:TProcess;
    BH: Array[0..4] of HBitmap;MH: Array[0..1] of HBitmap;n,pkgtype: String;
    Doc: TXMLDocument;xnode: TDOMNode;
    PODirectory, Lang, FallbackLang: String;
    imForm: TimdFrm;
begin
if not DirectoryExists(RegDir) then begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

//Load language pack
PODirectory:=ExtractFilePath(Application.ExeName)+'lang/';
GetLanguageIDs(Lang, FallbackLang);
translations.TranslateUnitResourceStrings('LCLStrConsts', PODirectory + 'lclstrconsts-%s.po', Lang, FallbackLang);
translations.TranslateUnitResourceStrings('trstrings', PODirectory + 'listaller-%s.po', Lang, FallbackLang);
writeLn('Language pack loaded.');

if not FileExists(Paramstr(1)) then begin
  ShowMessage(strRunParam);
  halt(1);
  exit;
end;

  DInfo.DName:='';
  Dinfo:=GetDistro;

//Load GTK2 icons
BH[0]:=Gtk2LoadStockPixmap(GTK_STOCK_QUIT,GTK_ICON_SIZE_BUTTON);
if BH[0] <> 0 then
begin
 FinBtn1.Glyph.Handle:=BH[0];
end;

BH[1]:=LoadStockPixmap(idButtonAbort,MH[1]);
if BH[1] <> 0 then
begin
 AbortBtn1.Glyph.Handle:=BH[1];
 AbortBtn1.Glyph.MaskHandle:=MH[1];
end;

BH[3]:=Gtk2LoadStockPixmap(GTK_STOCK_GO_FORWARD,GTK_ICON_SIZE_BUTTON);
if BH[3]<>0 then begin
Button1.Glyph.Handle:=BH[3];
end;

BH[4]:=Gtk2LoadStockPixmap(GTK_STOCK_GO_BACK,GTK_ICON_SIZE_BUTTON);
if BH[4]<>0 then begin
Button5.Glyph.Handle:=BH[4];
end;

//Set translation strings (1)
  Label1.Caption:=strWelcome;
  Label3.Caption:=strnToStart;
  Label4.Caption:=strprogDesc;
  Label5.Caption:=strLicense;
  Label8.Caption:=strPleaseRead;
  Label6.Caption:=strRunning;
  Label7.Caption:=strplWait;
  Label10.Caption:=strComplete;
  Label12.Caption:=strprFinish;
  FinBtn1.Caption:=strFinish;
  AbortBtn1.Caption:=strAbort;
  Button5.Caption:=strBack;
  Button1.Caption:=strNext;
  RadioButton1.Caption:=strIagree;
  RadioButton2.Caption:=strInagree;
  CheckBox1.Caption:=strDispLog;
  
  NoteBook1.PageIndex:=0;
  NoteBook1.ShowTabs:=false;
  
  if not DirectoryExists(RegDir) then SysUtils.CreateDir(RegDir);
  
  //Check distribution
  if DInfo.DName='' then begin
   ShowMessage(strLDnSupported+#13+strInClose+#13+notifyDevs);
   halt;
   exit;
  end;

writeLn('Initialized.');
writeLn('Loading IPK package...');
//Begin loading package
Application.ShowMainForm:=true;
RmApp:=false;
z:=TAbUnZipper.Create(nil);
FDir:=lp+ExtractFileName(paramstr(1))+'/';
if not DirectoryExists(lp) then
CreateDir(lp);
if not DirectoryExists(FDir) then
CreateDir(FDIR);

try
z.FileName:=paramstr(1);
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp+ExtractFileName(paramstr(1));

z.ExtractFiles('arcinfo.pin');
except
z.Free;
ShowMessage(strExtractError+#13+strPkgDM+#13+strABLoad);
Application.Terminate;
exit;
end;

PkgName:=ExtractFileName(paramstr(1));

ReadXMLFile(Doc,lp+PkgName+'/arcinfo.pin'); //Read XML configuration
xnode:=Doc.FindNode('package');
pkgtype:=xnode.Attributes.GetNamedItem('type').NodeValue;

if pkgtype='linstall' then
if not IsRoot then begin
  imForm:=TimdFrm.Create(self);
  with imForm do
  begin
    btnTest.Enabled:=true;
    btnHome.Enabled:=true;
    if FindChildNode(xnode,'disallow') <> nil then begin
  if (pos('iotest',LowerCase(FindChildNode(xnode,'disallow').NodeValue))>0) then
    btnTest.Enabled:=false;
  if (pos('iolocal',LowerCase(FindChildNode(xnode,'disallow').NodeValue))>0) then
    btnHome.Enabled:=false;
  if (pos('iobase',LowerCase(FindChildNode(xnode,'disallow').NodeValue))>0) then
    btnInstallAll.Enabled:=false;
    end;
  Label13.Caption:=strSWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

if pkgtype='dlink' then
if not IsRoot then begin
  imForm:=TimdFrm.Create(self);
  with imForm do
  begin
  btnTest.Enabled:=false;
  btnHome.Enabled:=false;
  Label13.Caption:=strSWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

if pkgtype='container' then
if not IsRoot then begin
  imForm:=TimdFrm.Create(self);
  with imForm do
  begin
  btnTest.Enabled:=false;
 if FindChildNode(xnode,'disallow') <> nil then begin
  if (pos('iolocal',LowerCase(FindChildNode(xnode,'disallow').NodeValue))<=0) then
    btnHome.Enabled:=false;
  if (pos('iobase',LowerCase(FindChildNode(xnode,'disallow').NodeValue))<=0) then
    btnInstallAll.Enabled:=false;
    end;
  Label13.Caption:=strSWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

Application.ProcessMessages;

if pkgtype='' then pkgtype:='linstall';
if LowerCase(pkgtype)='linstall' then begin

writeLn('Package type is "linstall"');

 t:=TProcess.Create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:='uname -m';
 t.Execute;
 x:=TStringList.Create;
 x.LoadFromStream(t.OutPut);
 n:=x[0];
 x.Free;
 t.Free;

if (n='i686')
or (n='i586')
or (n='i486')
then n:='i386';

if (pos(n,LowerCase(FindChildNode(xnode,'architecture').NodeValue))<=0)
and (pos('all',LowerCase(FindChildNode(xnode,'architecture').NodeValue))<=0) then begin
ShowMessage(strInvArchitecture);
z.Free;
Application.Terminate;
exit;
end;
writeLn('Architecture: '+n);
pID:=xnode.FindNode('id').FirstChild.NodeValue;
idName:='';
if xnode.FindNode('idName')<> nil then
idName:=xnode.FindNode('idName').FirstChild.NodeValue;
writeLn('Package idname: '+idName);

//Find profiles
i:=1;
Profiles:=TStringList.Create;
repeat
if FindChildNode(xnode,'profile'+IntToStr(i))<>nil then begin
Profiles.Add(FindChildNode(xnode,'profile'+IntToStr(i)).NodeValue+' #'+FindChildNodeX(xnode,'profile'+IntToStr(i)).Attributes.GetNamedItem('id').NodeValue);
writeLn('Found installation profile '+Profiles[Profiles.Count-1]);
z.ExtractFiles('fileinfo-'+FindChildNodeX(xnode,'profile'+IntToStr(i)).Attributes.GetNamedItem('id').NodeValue+'.id');
Inc(i);
end;
until FindChildNode(xnode,'profile'+IntToStr(i))=nil;

Application.ShowMainForm:=true;
xnode:=Doc.DocumentElement.FindNode('application');

IAppName:=xnode.Attributes.GetNamedItem('name').NodeValue;

if idName='' then
idName:=xnode.Attributes.GetNamedItem('name').NodeValue;

if FindChildNode(xnode,'version')<>nil then
IAppVersion:=FindChildNode(xnode,'version').NodeValue;

if FindChildNode(xnode,'description')<>nil then begin
DescFile:=FindChildNode(xnode,'description').NodeValue;
z.ExtractFiles(ExtractFileName(DescFile));
end;
if FindChildNode(xnode,'license')<>nil then begin
LicenseFile:=FindChildNode(xnode,'license').NodeValue;
z.ExtractFiles(ExtractFileName(LicenseFile));
end;

if FindChildNode(xnode,'icon')<>nil then begin
IconPath:=FindChildNode(xnode,'icon').NodeValue;
z.ExtractFiles(ExtractFileName(IconPath));
end;
if FindChildNode(xnode,'updsource')<>nil then
USource:=FindChildNode(xnode,'updsource').NodeValue
else
USource:='#';
if FindChildNode(xnode,'author')<>nil then
IAuthor:=FindChildNode(xnode,'author').NodeValue
else
IAuthor:='#';
if FindChildNode(xnode,'appcmd')<>nil then
IAppCMD:=FindChildNode(xnode,'appcmd').NodeValue
else begin
CbExecApp.Visible:=false;
IAppCMD:='#';
end;

if IAppCMD <> '#' then
writeLn('Application command is '+IAppCMD);

if (IAppCMD='#')and (Testmode) then
begin
ShowMessage(strActionNotPossiblePkg);
 z.Free;
 Application.Terminate;
 exit;
end;

IAppCMD:=SyblToPath(IAppCMD);

if length(pID)<>17 then begin
ShowMessage(strIDInvalid);
 z.Free;
 Application.Terminate;
 exit;
 end;

//Load Description
for i:=0 to xnode.ChildNodes.Count-1 do begin
if LowerCase(xnode.ChildNodes.Item[i].NodeName)='sdesc' then begin
xnode:=xnode.ChildNodes.Item[i];break;end;
end;
ShDesc:='#';
if xnode <> nil then begin
if FindChildNode(xnode,Copy(GetEnvironmentVariable('LANG'), 1, 2))<>nil then
ShDesc:=FindChildNode(xnode,Copy(GetEnvironmentVariable('LANG'), 1, 2)).NodeValue
else
ShDesc:=xnode.Attributes.GetNamedItem('std').NodeValue;
end;

xnode:=Doc.DocumentElement.FindNode('application');

AType:=FindChildNode(xnode,'group').NodeValue;
if AType='' then AType:='other';

if (pos(LowerCase(DInfo.DName),LowerCase(FindChildNode(xnode,'dsupport').NodeValue))<=0)
and (LowerCase(FindChildNode(xnode,'dsupport').NodeValue)<>'all') then begin
if Application.MessageBox(PChar(PAnsiChar(strnSupported)+#13+PAnsiChar(strInstAnyway)),'Distro-Error',MB_YESNO)= IDNO then
 begin
 z.Free;
 Application.Terminate;
 exit;
 end;
end;

//Load Wizard-Image
Image2.Picture.LoadFromFile(ExtractFilePath(Application.Exename)+'graphics/wizardimage.png');
if (xnode.FindNode('wizimage')<>nil) and (PAnsiChar(xnode.FindNode('wizimage').NodeValue)[0] = '/') then begin
z.ExtractFiles(ExtractFileName(xnode.FindNode('wizimage').NodeValue));
if fileexists(lp+PkgName+xnode.FindNode('wizimage').NodeValue) then
Image2.Picture.LoadFromFile(lp+PkgName+xnode.FindNode('wizimage').NodeValue);
end;

z.ExtractFiles('preinst');
z.ExtractFiles('postinst');
z.ExtractFiles('prerm');
ExecA:='<disabled>';
ExecB:='<disabled>';
ExecX:='<disabled>';
if FileExists(z.BaseDirectory+'/preinst') then ExecA:=z.BaseDirectory+'/preinst';
if FileExists(z.BaseDirectory+'/postinst') then ExecB:=z.BaseDirectory+'/postinst';
if FileExists(z.BaseDirectory+'/prerm') then ExecX:=z.BaseDirectory+'/prerm';

ar:=TIniFile.Create(RegDir+'appreg.lst');
x:=TStringList.Create;
ar.ReadSections(x);
for i:=0 to x.Count-1 do begin

if (copy(x[i],1,pos('<',x[i])-1)=IAppName) and (ar.ReadString(x[i],'Version*','0.0')=IAppVersion)
and (idName=ar.ReadString(x[i],'idName','???')) then

if Application.MessageBox(PChar(PAnsiChar(strAlreadyInst)+#13+PAnsiChar(strInstallAgain)),PChar(strReInstall),MB_YESNO)= IDNO then
begin
x.Free;
ar.Free;
Application.Terminate;
exit;
end else RmApp:=true;

end;
x.Free;
ar.Free;

//Load Description
if LowerCase(DescFile)<>'' then
Memo1.Lines.LoadFromFile(lp+PkgName+DescFile);

//Load License
if LowerCase(LicenseFile)<>'' then
LicMemo.Lines.LoadFromFile(lp+PkgName+LicenseFile);

if (LicenseFile='')and(DescFile='')then
begin
  Button1.Caption:=strInstallNow;
  Button1.Left:=648-Button1.Width;
end;

//Load Dependencies

Dependencies:=TStringList.Create;

ListBox1.Items.Add('Distribution: '+DInfo.DName);
ListBox1.Items.Add('Version: '+DInfo.Release);
ListBox1.Items.Add('PackageSystem: '+DInfo.PackageSystem);

xnode:=Doc.FindNode('package'); //Set xnode to the package tree

if xnode.FindNode('Dep'+DInfo.DName)<>nil then begin  //Check if there are specific packages available

if (xnode.FindNode('Dep'+DInfo.DName).Attributes.GetNamedItem('releases')<>nil)
and (pos(DInfo.Release,xnode.FindNode('Dep'+DInfo.DName).Attributes.GetNamedItem('releases').NodeValue)<= 0)
then begin
if Application.MessageBox(PChar(PAnsiChar(strInvalidDVersion)+#13+PAnsiChar(strInstAnyway)),'Distro-Error',MB_YESNO)= IDNO then begin
Application.Terminate;
exit;
 end;
end;
xnode:=xnode.FindNode('Dep'+DInfo.DName);
for i:=0 to xnode.ChildNodes.Count-1 do
Dependencies.Add(xnode.ChildNodes.Item[i].FirstChild.NodeValue);
end else begin
if xnode.FindNode('Dep'+DInfo.PackageSystem)<>nil then begin
xnode:=xnode.FindNode('Dep'+DInfo.PackageSystem);
for i:=0 to xnode.ChildNodes.Count-1 do
Dependencies.Add(xnode.ChildNodes.Item[i].FirstChild.NodeValue);
 end;
end;

for i:=0 to Dependencies.Count-1 do
if Dependencies[i][1]='.' then z.ExtractFiles(ExtractFileName(Dependencies[i]));

z.Free;

Label2.Caption:=StringReplace(strWelcomeTo,'%a',IAppName,[rfReplaceAll]);
if Testmode then begin
IWizFrm.Caption:=StringReplace(strInstOf,'%a',IAppName,[rfReplaceAll])+' ['+strTestMode+']';
LblTestMode.Caption:=strTestMode+'!';
LblTestMode.Visible:=true;
end else
IWizFrm.Caption:=StringReplace(strInstOf,'%a',IAppName,[rfReplaceAll]);

Button1.Enabled:=true;

writeLn('Profiles count is '+IntToStr(Profiles.Count));
if Profiles.Count<0 then begin
ShowMessage(strPkgInval+#13'Message: No profiles and no file list found!');
Profiles.Free;
halt;
exit;
end;

//Set profiles to RadioGroup:
for i:=0 to Profiles.Count-1 do
ModeGroup.Items.Add(copy(Profiles[i],0,pos(' #',Profiles[i])));
ModeGroup.ItemIndex:=0;

If Profiles.Count=1 then FFileInfo:='/stuff/fileinfo-'+copy(Profiles[0],pos(' #',Profiles[0])+2,length(Profiles[0]))+'.id'
else FFileInfo:='*';

IWizFrm.Show;
end else //Handle other IPK types
if LowerCase(pkgtype)='dlink' then begin
writeLn('Package type is "dlink"');
IWizFrm.Hide;
IWizFrm.Visible:=false;
DGForm:=TDGForm.Create(nil);
z:=TAbUnZipper.Create(nil);
z.FileName:=paramstr(1);
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp+ExtractFileName(paramstr(1));

xnode:=Doc.DocumentElement.FindNode('application');

DescFile:=FindChildNode(xnode,'description').NodeValue;

IAppName:=xnode.Attributes.GetNamedItem('name').NodeValue;
IAppVersion:=FindChildNode(xnode,'version').NodeValue;

if FindChildNode(xnode,'author')<>nil then
IAuthor:=FindChildNode(xnode,'author').NodeValue
else
IAuthor:='#';

AType:=FindChildNode(xnode,'group').NodeValue;
z.ExtractFiles(ExtractFileName(DescFile));

if FindChildNode(xnode,'icon')<>nil then begin
DGForm.IIconPath:=FindChildNode(xnode,'icon').NodeValue;
z.ExtractFiles(ExtractFileName(DGForm.IIconPath));
end;

if FindChildNode(xnode,'desktopfiles')<>nil then
DGForm.IDesktopFiles:=FindChildNode(xnode,'desktopfiles').NodeValue;

z.Free;

//Load Description
for i:=0 to xnode.ChildNodes.Count-1 do begin
if LowerCase(xnode.ChildNodes.Item[i].NodeName)='sdesc' then begin
xnode:=xnode.ChildNodes.Item[i];break;end;
end;
ShDesc:='#';
if xnode <> nil then begin
if FindChildNode(xnode,Copy(GetEnvironmentVariable('LANG'), 1, 2))<>nil then
ShDesc:=FindChildNode(xnode,Copy(GetEnvironmentVariable('LANG'), 1, 2)).NodeValue
else
ShDesc:=xnode.Attributes.GetNamedItem('std').NodeValue;
end;

with DGForm do begin
Label1.Caption:=StringReplace(strInstOf,'%a',IAppName,[rfReplaceAll]);
Label2.Caption:=strWillDLFiles;
Caption:=Label1.Caption;
Memo1.Lines.LoadFromFile(lp+ExtractFileName(paramstr(1))+'/'+DescFile);
LicMemo.Clear;
LicMemo.Lines.Add(strPkgDownload);

//Load dependencies
xnode:=Doc.FindNode('package');


if xnode.FindNode('Dep'+DInfo.DName)<>nil then begin //Check if there are specific packages available for the distribution
xnode:=xnode.FindNode('Dep'+DInfo.DName);

for i:=0 to xnode.ChildNodes.Count-1 do begin
n:=xnode.ChildNodes.Item[i].FirstChild.NodeValue;
if pos(' <',n)>0 then
Memo2.Lines.Add(copy(n,pos(' <',n)+2,length(n)-pos(' <',n)-2)+' - '+copy(n,1,pos(' <',n)-1))
else Memo2.Lines.Add(n);
end;
end else
if Application.MessageBox(PChar(strNoLDSources),PChar(strUseCompPQ),MB_YESNO)=IDYES then begin

if xnode.FindNode('Dep'+DInfo.PackageSystem)=nil then begin ShowMessage(strNoComp+#13+strInClose);Application.Terminate;exit;end;
xnode:=xnode.FindNode('Dep'+DInfo.PackageSystem);
for i:=0 to xnode.ChildNodes.Count-1 do begin
n:=xnode.ChildNodes.Item[i].FirstChild.NodeValue;
if pos(' <',n)>0 then
Memo2.Lines.Add(copy(n,pos(' <',n)+2,length(n)-pos(' <',n)-2)+' - '+copy(n,1,pos(' <',n)-1))
else Memo2.Lines.Add(n);
end;
 end;
end;
DGForm.Show;

end else
if LowerCase(pkgtype)='container' then begin
writeLn('Package type is "container"');
IWizFrm.Hide;
IWizFrm.Visible:=false;
z:=TAbUnZipper.Create(nil);
z.FileName:=paramstr(1);
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp;
xnode:=Doc.DocumentElement.FindNode('application');
z.ExtractFiles(ExtractFileName(FindChildNode(xnode,'package').NodeValue));
for i:=0 to FindChildNode(xnode,'package').ChildNodes.Count-1 do
z.ExtractFiles(ExtractFileName(FindChildNode(xnode,'package').ChildNodes[i].NodeValue));

z.Free;
 t:=tprocess.create(nil);
 t.CommandLine:='chmod 755 ''/tmp/'+FindChildNode(xnode,'package').NodeValue+'''';
 t.Options:=[poUsePipes,poWaitonexit];
 t.Execute;
 t.Options:=[poUsePipes,poWaitonexit,poNewConsole];
 if LowerCase(ExtractFileExt(FindChildNode(xnode,'package').NodeValue))='.package' then begin
 if FileExists('/usr/bin/package') then
 t.Options:=[poUsePipes,poWaitonexit];
 
 t.CommandLine:='/tmp/'+FindChildNode(xnode,'package').NodeValue;
 t.Execute;
 t.Free;
 end else begin
 if (FindChildNode(xnode,'InTerminal')<>nil)and(LowerCase(FindChildNode(xnode,'InTerminal').NodeValue)='true') then
 t.Options:=[poUsePipes, poWaitOnExit, poNewConsole]
 else t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:='/tmp/'+FindChildNode(xnode,'package').NodeValue;
 t.Execute;
 t.Free;
 end;
 
DeleteFile('/tmp/'+FindChildNode(xnode,'package').NodeValue);
Application.Terminate;
exit;
end;

end;

procedure TIWizFrm.FormDestroy(Sender: TObject);
begin
  //Free instances
  if Assigned(Dependencies) then Dependencies.Free;
  if Assigned(Profiles) then Profiles.Free;
end;

procedure TIWizFrm.FormShow(Sender: TObject);
begin
{LicRichView:=TRichView.Create(nil);
RVStyle1:=TRVStyle.Create(nil);
LicRichview.Top:=SynEdit1.Top;
LicRichView.Left:=SynEdit1.Left;
LicRichView.Visible:=true;
SynEdit1.Visible:=false;
LicRichView.Parent:=LicPage;
LicRichView.Width:=SynEdit1.Width;
LicRichView.height:=SynEdit1.Height;
LicRichView.Style:=RVStyle1;
LicRichview.lines.LoadFromFile(lp+PkgName+LicenseFile);}
end;

procedure TIWizFrm.GetOutputTimerTimer(Sender: TObject);
var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess(Process: TProcess;
    OutputMemo: TMemo);
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead:LongInt;
  begin
  
    if Process.Running then
    begin

      BytesAvailable := Process.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
       begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        OutputMemo.Text := OutputMemo.Text + copy(Buffer,1, BytesRead);
        writeLn(OutputMemo.Text + copy(Buffer,1, BytesRead));
        Application.ProcessMessages;
        BytesAvailable := Process.OutPut.NumBytesAvailable;
        NoMoreOutput := false;
      end;
      if BytesRead>0 then
        OutputMemo.SelStart := Length(OutputMemo.Text);
    end;
    
  end;
begin
if Process1.CommandLine<>'' then begin
  repeat
    NoMoreOutput := true;
    Application.ProcessMessages;
    DoStuffForProcess(Process1, InfoMemo);
  until noMoreOutput;
if (Process1.ExitStatus>0) then begin
    GetOutputTimer.Enabled:=false;
    writeLn('Connection to backend broken.');
    writeLn('Cannot resolve dependencies');
    ShowMessage(strCouldntSolve+#13+StringReplace(strViewLog,'%p',ExtractFilePath(Application.ExeName),[rfReplaceAll])+#13+'Code: '+IntToStr(Process1.ExitStatus));
    InfoMemo.Lines.SaveTofile('/tmp/install-'+IAppName+'.log');
    halt;
    exit;
  end;
  end;
end;

var HTTP: THTTPSend;FTP: TFTPSend; //<- Has to be global
procedure TIWizFrm.HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
ExProgress.Visible:=true;
Application.ProcessMessages;
//HTTP
if Http.DownloadSize>100 then begin
ExProgress.Max:=HTTP.DownloadSize;
ExProgress.Position:=HTTP.Document.Size;
exit;
end;
//FTP
ExProgress.Position:=FTP.DSock.RecvCounter;
end;

procedure TIWizFrm.StartInstallation;
var
i,j: Integer;
fi,ndirs, s, appfiles: TStringList;
dest,h: String; // h is an helper variable - used for various actions
ar,dsk, cnf: TIniFile; // Archive setting, configuration
z: TAbUnZipper; // Zipper
setcm: Boolean;
t:TProcess; // Helper process with pipes
begin
while IPage.Visible=false do Application.ProcessMessages;

Edit1.Visible:=true;
btn_sendinput.Visible:=true;
AbortBtn1.Enabled:=false;
Button1.Enabled:=false;
Button5.Enabled:=false;
AbortIns:=false;
if FFileInfo='' then begin
ShowMessage(strPKGError+#13'Message: No file information that is assigned to this profile was not found!'+#13+strAppClose);
Application.Terminate;
exit;
end;

AbortBtn1.Enabled:=true;
Label9.Caption:=strStep1;

fi:=TStringList.Create;
fi.LoadFromFile(lp+PkgName+FFileInfo);

if (fi.Count mod 3)<>0 then begin
ShowMessage(strPKGError+#13'Message: File list is unlogical!'+#13+strAppClose);
fi.Free;
Application.Terminate;
exit;
end;

//Check if fileinfo contains shared files
for i:=0 to (fi.Count div 3)-1 do begin
if IsSharedFile(lp+PkgName+fi[i*3]) then ContSFiles:=true;
end;

InsProgress.Max:=((Dependencies.Count+(fi.Count div 3))*10)+16;
Application.ProcessMessages;

if Testmode then
begin
 Process1.CommandLine := 'rm -rf /tmp/litest';
 Process1.Execute;
end;

//Check if PackageKit trackmode is enabled:
cnf:=TIniFile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('MainConf','ShowPkMon',false) then begin
t:=TProcess.Create(nil);
t.CommandLine:='pkmon';
t.Options:=[poNewConsole];
t.Execute;
t.free;
end;
cnf.free;

//Execute programs/scripts
if ExecA<>'<disabled>' then begin
    Process1.CommandLine := 'chmod 777 '+ExecA;
    Process1.Execute;
while Process1.Running do Application.ProcessMessages;
    Process1.CommandLine := ExecA;
    Process1.Execute;
while Process1.Running do Application.ProcessMessages;
end;

if Dependencies.Count>0 then begin
for I:=0 to Dependencies.Count-1 do begin  //Download & install dependencies
if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then begin
cnf:=TInifile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('MainConf','AutoDepLoad',true)=false then
if Application.MessageBox(PAnsiChar(StringReplace(strWDLDep,'%l',Dependencies[i],[rfReplaceAll])+#13+strWAllow),'DepDownload',MB_YESNO)= IDNO then begin
ShowMessage(strLiCloseANI);
cnf.Free;
Application.Terminate;
exit;
end;

//Create HTTP object
HTTP := THTTPSend.Create;
HTTP.Sock.OnStatus:=HookSock;
HTTP.UserAgent:='Listaller-GET';
//Create FTP object
FTP := TFTPSend.Create;
FTP.DSock.Onstatus:=HookSock;
if cnf.ReadBool('Proxy','UseProxy',false) then begin
//Set HTTP
HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted
//Not needed
{if DInfo.Desktop='GNOME' then begin
HTTP.ProxyPass:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_user');
HTTP.ProxyUser:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_password');
 end;
//Set FTP
FTP.:=cnf.ReadString('Proxy','fPort','');
HTTP.ProxyHost:=cnf.ReadString('Proxy','fServer','');
HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
HTTP.ProxyPass:=cnf.ReadString('Proxy','Password','');  }
end;
cnf.Free;

GetOutPutTimer.Enabled:=false;
try
    ExProgress.Visible:=true;
    ExProgress.Position:=0;
    InfoMemo.Lines.Add(strGetDependencyFrom+' '+Dependencies[i]+'.');
    InfoMemo.Lines.Add(strPlWait2);
 if pos('http://',LowerCase(Dependencies[i]))>0 then begin
  try
    HTTP.HTTPMethod('GET', copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    HTTP.Document.SaveToFile('/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));
  except
  ShowMessage(strDepDLProblem);
  Application.Terminate;
  exit;
  end;
 end else begin

with FTP do begin
    TargetHost := GetServerName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));

  try
    DirectFileName := '/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    DirectFile:=True;
    if not Login then ShowMessage('Couldn''t login on the FTP-Server!');
    ChangeWorkingDir(GetServerPath(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));

    IWizFrm.ExProgress.Max:=FileSize(ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));

    RetrieveFile(ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)), false);
    Logout;
  except
   ShowMessage(strDepDLProblem);
   Application.Terminate;
   exit;
  end;
  end;
end;
 
finally
HTTP.Free;
FTP:=nil;
FTP.Free;
end;

//Add package-name

if (DInfo.PackageSystem='DEB')and(pos(' <',Dependencies[i])<=0) then begin
    t:=tprocess.create(nil);
    t.CommandLine:='dpkg --info /tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    t.Options:=[poUsePipes,poWaitonexit];
    try
    t.Execute;
    s:=tstringlist.Create;
    try
    s.LoadFromStream(t.Output);
    for j:=0 to s.Count-1 do
    if pos('Package: ',s[j])>0 then break;
    Dependencies[i]:=Dependencies[i]+' <'+copy(s[j],11,length(s[j]))+'>';
    finally
    s.free;
    end;
   finally
    t.Free;
    end;
end else begin
if (pos(' <',Dependencies[i])<=0) then begin
    t:=tprocess.create(nil);
    t.CommandLine:='rpm -qip /tmp/'+ExtractFileName(Dependencies[i]);
    t.Options:=[poUsePipes,poWaitonexit];
    try
    t.Execute;
    s:=tstringlist.Create;
    try
    s.LoadFromStream(t.Output);
    for j:=0 to s.Count-1 do
    if pos('Name ',s[j])>0 then break;
    Dependencies[i]:=Dependencies[i]+' <'+copy(s[j],15,pos(' ',copy(s[j],15,length(s[j])))-1)+'>';
    finally
    s.free;
    end;
   finally
    t.Free;
    end;
    
    end;
end;

InfoMemo.Lines.Add('Done.');
GetOutPutTimer.Enabled:=true;
end;

ExProgress.Visible:=false;
sleep(18); //Wait...
if (Dependencies[i][1]='/')or(pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then begin

    InfoMemo.Lines.Add('--');
    Infomemo.Lines.Add('DepInstall: '+Dependencies[i]+' (using PackageKit +x)');
    InfoMemo.Lines.Add('-');

    t:=TProcess.Create(nil);
    t.Options:=[poUsePipes,poWaitonexit];

  if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then  begin
    t.CommandLine := pkit+'--is-installed '+copy(Dependencies[i],pos(' <',Dependencies[i])+2,length(Dependencies[i])-1);
    t.Execute;

    if t.ExitStatus = 0 then
    Process1.CommandLine := pkit+'--install-local /tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])));
  end else
    Process1.CommandLine := pkit+'--install-local '+lp+PkgName+Dependencies[i];
    if t.ExitStatus = 0 then
    Process1.Execute;

    t.Free;
    Application.ProcessMessages;
    while Process1.Running do Application.ProcessMessages;
    InsProgress.Position:=InsProgress.Position+5;

    //Check if the package was really installed
    if Process1.ExitStatus>0 then
    begin
    ShowMessage(strCouldntSolve+#13+StringReplace(strViewLog,'%p',ExtractFilePath(Application.ExeName),[rfReplaceAll]));
    t.Free;
    InfoMemo.Lines.SaveTofile('/tmp/install-'+IAppName+'.log');
    Application.Terminate;
    exit;
    end;

    InsProgress.Position:=InsProgress.Position+5;

while Process1.Running do Application.ProcessMessages;

 //If only a file name given install them with distri-tool
end else begin
     InfoMemo.Lines.Add('--');
     Infomemo.Lines.Add('DepInstall: '+Dependencies[i]+' (using PackageKit)');
     InfoMemo.Lines.Add('-');

     GetOutPutTimer.Enabled:=true;

     t:=TProcess.Create(nil);
     t.Options:=[poUsePipes,poWaitonexit];

     t.CommandLine := pkit+'--is-installed '+Dependencies[i];
     t.Execute;

     if t.ExitStatus = 0 then begin
     Process1.CommandLine := pkit+'--install '+Dependencies[i];
     Process1.Execute;

    //Check if the package was really installed
  if Process1.ExitStatus>0 then
    begin
     writeLn('Package '+Dependencies[i]+' can not be installed.');
     ShowMessage(strCouldntSolve+#13+StringReplace(strViewLog,'%p',ExtractFilePath(Application.ExeName),[rfReplaceAll]));
     t.Free;
     InfoMemo.Lines.SaveTofile('/tmp/install-'+IAppName+'.log');
     Application.Terminate;
     exit;
    end;

    end; //Eof <> 1

    t.Free;

    while Process1.Running do Application.ProcessMessages;
    InsProgress.Position:=InsProgress.Position+10;
  end;
  end; //End of Dependecies.Count term
end; //End of dependency-check

GetOutPutTimer.Enabled:=false;

Edit1.Visible:=false;
btn_sendinput.Visible:=false;
sleep(10);

ExProgress.Visible:=false;
ExProgress.Enabled:=false;
AbortBtn1.Enabled:=false;
Application.ProcessMessages;

//Eventually delete old application
if RmApp then begin
ExProgress.Visible:=true;
ExProgress.Position:=UnInstallIPKApp(IAppName,idName,InfoMemo.Lines,true);
ExProgress.Visible:=false;
end;
appfiles:=TStringList.Create;
Label9.Caption:=strStep2;
z:=TAbUnZipper.Create(nil);
z.FileName:=paramstr(1);
ndirs:=TStringList.Create;
j:=0;
if not DirectoryExists(SyblToPath('$INST')) then SysUtils.CreateDir(SyblToPath('$INST'));
for i:=0 to fi.Count-1 do begin
Application.ProcessMessages;
if i mod 3 = 0 then begin

if (pos(' <'+LowerCase(DInfo.DName)+'-only>',LowerCase(fi[i]))>0)
or (pos('-only',LowerCase(fi[i]))<=0) then begin

dest:=SyblToPath(fi[i+2]);

if not DirectoryExists(dest) then begin
SysUtils.CreateDir(dest);
ndirs.Add(dest);
end;
h:=dest;
while not DirectoryExists(dest) do begin
CreateDir(h);
if DirectoryExists(h) then h:=Dest
else h:=ExtractFilePath(ExcludeTrailingBackslash(h));
end;
//h is now used for the file-path
h:=DeleteModifiers(fi[i]);


FDir:=lp+ExtractFileName(paramstr(1))+'/';
if not DirectoryExists(FDir) then
CreateDir(FDIR);

try
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp+ExtractFileName(paramstr(1));

z.ExtractFiles(ExtractFileName(h));
Application.ProcessMessages;
except
ShowMessage(strExtractError);
z.Free;
halt;
end;

InfoMemo.Lines.Add('Copy file '+ExtractFileName(h)+' to '+dest+' ...');
Application.ProcessMessages;

if fi[i+1] <> MDPrint((MD5.MD5File(DeleteModifiers(lp+PkgName+h),1024))) then begin
ShowMessage(strHashError);
InfoMemo.Lines.SaveTofile('/tmp/install-'+IAppName+'.log');
Application.Terminate;
exit;
end;

Inc(j);

FileCopy(DeleteModifiers(lp+PkgName+h),dest+'/'+ExtractFileName(DeleteModifiers(h)));
if(pos('.desktop',LowerCase(ExtractFileName(h)))>0) then
begin
dsk:=TIniFile.Create(dest+'/'+ExtractFileName(h));
dsk.WriteString('Desktop Entry','AppVersion',IAppVersion);
dsk.WriteString('Desktop Entry','ExternalInst','true');
dsk.WriteString('Desktop Entry','Maintainer',IAuthor);
if dsk.ValueExists('Desktop Entry','Icon') then
dsk.WriteString('Desktop Entry','Icon',SyblToPath(dsk.ReadString('Desktop Entry','Icon','*')));
if dsk.ValueExists('Desktop Entry','Exec') then
dsk.WriteString('Desktop Entry','Exec',SyblToPath(dsk.ReadString('Desktop Entry','Exec','*')));
dsk.Free;
end;

if(pos('<setvars>',LowerCase(ExtractFileName(h)))>0) then
begin
s:=TStringList.Create;
s.LoadFromFile(dest+'/'+ExtractFileName(h));
for j:=0 to s.Count-1 do
s[j]:=SyblToPath(s[j]);
s.SaveToFile(dest+'/'+ExtractFileName(h));
s.Free;
end;


appfiles.Add(dest+'/'+ExtractFileName(fi[i]));
InfoMemo.Lines.Add('Okay.');

InsProgress.Position:=InsProgress.Position+10;

  end;

 end;
end;

Label9.Caption:=strStep3;
//Check if every single file needs its own command to get the required rights (It's faster if only every folder recieves the rights)
setcm:=false;
for i:=0 to (fi.Count div 3)-1 do
if pos(' <chmod:',fi[i*3])>0 then setcm:=true;


if setcm then begin //Rechte einzeln setzen
for i:=0 to fi.Count-1 do begin
if i mod 3 = 0 then begin
h:=fi[i];

if pos(' <chmod:',h)>0 then begin
Process1.CommandLine := 'chmod '+copy(h,pos(' <chmod:',h)+8,3)+SyblToPath(fi[i+1])+'/'+ExtractFileName(DeleteModifiers(fi[i]));
Process1.Execute;
end else begin
Process1.CommandLine := 'chmod 755 '+SyblToPath(fi[i+1])+'/'+ExtractFileName(DeleteModifiers(fi[i]));
Process1.Execute;
end;

while Process1.Running do Application.ProcessMessages;
InfoMemo.Lines.Add('Rights assigned to '+DeleteModifiers(ExtractFileName(SyblToPath(fi[i]))));
 end;
end;
end else begin //Rechte ordnerweise setzen
for i:=0 to ndirs.Count-1 do begin
Application.ProcessMessages;
Process1.CommandLine := 'chmod 755 -R '+SyblToPath(ndirs[i]);
Process1.Execute;
Application.ProcessMessages;
InfoMemo.Lines.Add('Rights assigned to folder '+ExtractFileName(SyblToPath(ndirs[i])));
 end;
end; //Ende setcm

InsProgress.Position:=InsProgress.Position+6;

fi.Free;
Label9.Caption:=strStep4;

if not DirectoryExists(RegDir+IAppName+'-'+idName) then SysUtils.CreateDir(RegDir+IAppName+'-'+idName);
FileCopy(lp+PkgName+'/arcinfo.pin',RegDir+IAppName+'-'+idName+'/proginfo.pin');

appfiles.SaveToFile(RegDir+IAppName+'-'+idName+'/appfiles.list');
appfiles.Free;

if IconPath[1]='/' then
FileCopy(lp+PkgName+IconPath,RegDir+IAppName+'-'+idName+'/icon'+ExtractFileExt(IconPath));

ndirs.SaveToFile(RegDir+IAppName+'-'+idName+'/AppDirs.list');

if ExecX<>'<disabled>' then
FileCopy(ExecX,RegDir+IAppName+'-'+idName+'/prerm');

ndirs.Free;

ar:=TInifile.Create(RegDir+'appreg.lst');
ar.WriteString(IAppName+'~'+idName,'Version*',IAppVersion);
ar.WriteString(IAppName+'~'+idName,'Version',IAppVersion);
ar.WriteString(IAppName+'~'+idName,'Author',IAuthor);
ar.WriteString(IAppName+'~'+idName,'Package',pID);
ar.WriteString(IAppName+'~'+idName,'C-Dir',RegDir+IAppName+' '+IAppVersion);
ar.WriteBool(IAppName+'~'+idName,'ContSFiles',ContSFiles);
ar.WriteString(IAppName+'~'+idName,'SDesc',ShDesc);
ar.WriteString(IAppName+'~'+idName,'Group',AType);
ar.Free;
ar:=TInifile.Create(RegDir+IAppName+'-'+idName+'/proginfo.pin');
for i:=0 to Dependencies.Count-1 do
ar.WriteString('DepOS','ID'+IntToStr(i+1),Dependencies[i]);
ar.Free;

InsProgress.Position:=InsProgress.Position+5;
//Execute Program/Script
if ExecB<>'<disabled>' then begin
    Process1.CommandLine := 'chmod 777 '+ExecB;
    Process1.Execute;
while Process1.Running do Application.ProcessMessages;
    Process1.CommandLine := ExecB;
    Process1.Execute;
while Process1.Running do Application.ProcessMessages;
end;

if USource<>'#' then begin
fi:=TStringList.Create;
if not FileExists(RegDir+'updates.list') then begin
fi.Add('Listaller UpdateSources-V0.8');
fi.SaveToFile(RegDir+'updates.list');
end;
fi.LoadFromFile(RegDir+'updates.list');
for i:=1 to fi.Count-1 do
if pos(USource,fi[i])>0 then break;
if i=fi.Count then begin
if Application.MessageBox(PChar(strAddUpdSrc+#13+
copy(USource,pos(' <',USource)+2,length(USource)-pos(' <',USource)-2)+' ('+copy(uSource,3,pos(' <',USource)-3)+')'+#13+
PAnsiChar(strQAddUpdSrc)),'Add update-source',MB_YESNO)= IDYES then begin

fi.Add('- '+USource);
fi.SaveToFile(RegDir+'updates.list');
 end;
 end;
 fi.Free;
end;

InsProgress.Position:=InsProgress.Position+5;

Label9.Caption:=strFinished;
sleep(600);
InfoMemo.Lines.SaveTofile('/tmp/install-'+IAppName+'.log');

if not Testmode then begin
NoteBook1.PageIndex:=5;

Label11.Caption:=StringReplace(strWasInstalled,'%a',IAppName,[rfReplaceAll]);
FinBtn1.Visible:=true;
AbortBtn1.Visible:=false;
FinPage.Refresh;
end else begin
  Process1.CommandLine:=IAppCMD;
  Process1.Options:=[poWaitOnExit];
  Hide;
  Application.ProcessMessages;
  Process1.Execute;
  ShowMessage(strTestFinished);
   Process1.CommandLine := 'rm -rf /tmp/litest';
   Process1.Execute;
  Application.Terminate;
 end;
end;

procedure TIWizFrm.btn_sendinputClick(Sender: TObject);
var InputString: String;
begin
if Process1.Running then begin
  InputString:=Edit1.text;
  Edit1.SelectAll;
  Process1.Input.Write(InputString[1], length(InputString));
  end else ShowMessage('Error - Process not running!');
end;

procedure TIWizFrm.RadioButton1Change(Sender: TObject);
begin
  with Sender as TRadioButton do begin
  if Checked then Button1.Enabled:=true
  else Button1.Enabled:=false;
  end;
end;

procedure TIWizFrm.RadioButton2Change(Sender: TObject);
begin

end;

initialization
  {$I mainunit.lrs}

end.

