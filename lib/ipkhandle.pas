{ ipkhandle.pas
  Copyright (C) Listaller Project 2008-2009

  ipkhandle.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  ipkhandle.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Functions to handle IPK packages
unit ipkhandle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LiCommon, Process, trstrings, packagekit,
  sqlite3ds, db, AbUnZper, AbArcTyp, XMLRead, DOM, distri, HTTPSend, FTPSend,
  MD5;

type
 TProgressChange = function(max: Longint;pos: Longint): Boolean; cdecl;
 TVisSwitchEvent = function(vis: Boolean): Boolean; cdecl;
 TMessageEvent = function(msg: String): Boolean; cdecl;

 PInstallation = ^TInstallation;

 //** Everything which is needed for an installation
 TInstallation = class
 private
  //Basic information about the package and the new application
  IAppName,IAppVersion, IAppCMD, IAuthor, FDescFile,ShDesc, FLicenseFile: String;
  IIconPath, IDesktopFiles: String;
  PkgName, PkgPath, pID, idName, AType: String;
  FWizImage: String;
  //Package database connection
  dsApp: TSQLite3Dataset;
  //Current active profile
  CurProfile: String;
  //List of application's dependencies
  Dependencies: TStringList;
  //Package profiles
  PkProfiles: TStringList;
  //Information about new files
  FFileInfo: String;
  //Name of the update source which should be registered
  USource: String;
  //Path of the package icon
  IconPath: String;
  //Execute external applications that are linked in the IPK-file
  ExecA, ExecB, ExecX: String;
  //Overwrite all files? (needed for patches)
  FOverwrite: Boolean;
  // True if IPK package is a patch
  FPatch: Boolean;
  //Current setup type
  pkType: TListallerPackageType;
  //Disallow-line (every action the package disallows, unformated)
  FDisallow: String;
  //Only set if old package should be removed
  RmApp: Boolean;
  //All supported distribution of this package
  FSupportedDistris: String;
  //Reference to an existing HTTPSend object
  HTTP: THTTPSend;
  //Reference to an existing FTPSend object
  FTP: TFTPSend;
  //Progress message relais
  FProgChange1: TProgressChange;
  FProgChange2: TProgressChange;
  FVisible1: TVisSwitchEvent;
  FVisible2: TVisSwitchEvent;
  FErrorMsg: TMessageEvent;
  FWarnMsg:  TMessageEvent;
  FTermRequest:  TMessageEvent;
  FSMessage:  TMessageEvent;
  //Executed if Linstallation should be done
  function RunNormalInstallation(proc: TProcess;ln: TStrings): Boolean;
  //Runs a DLink installation
  function RunDLinkInstallation(proc: TProcess;ln: TStrings): Boolean;
 protected
  function FindChildNode(dn: TDOMNode; n: String): TDOMNode;
  function FindChildNodeX(dn: TDOMNode; n: String): TDOMNode;
  //Set/Get methods for progress indication
  procedure SetMainProg(pos: integer;max: Integer);
  procedure SetExtraProg(pos: integer;max: Integer);
  procedure SetMainPosVisibility(vis: Boolean);
  procedure SetExtraPosVisibility(vis: Boolean);
  procedure SendErrorMsg(msg: String);
  procedure SendTermQuestion(qs: String);
  procedure SendStateMsg(msg: String);
 public
  //** Constructor
  constructor Create;
  //** Destructor
  destructor Destroy; override;
  //** Loads an IPK package @param fname Path to IPK file
  function Initialize(fname: String): Boolean;
  {** Executes the installation
      @param proc TProcess to catch the output of external tools
      @ ln TStrings to recieve a logfile
      @returns Sucess of operation}
  function DoInstallation(proc: TProcess;ln: TStrings): Boolean;
  //** Gets the maximal steps the installation needs
  function GetMaxInstSteps: Integer;
  //** Function to solve all dependencies on libraries the package has
  function ResolveDependencies: Boolean;
  //** Name of the application
  property AppName: String read IAppName;
  //** Version of the to-be-installed application
  property AppVersion: String read IAppVersion;
  //** ID of the application
  property AppID: String read idName;
  //** Listaller package type
  property pType: TListallerPackageType read pkType;
  //** Unformatted disallows string
  property Disallows: String read FDisallow;
  //** All distributions the package supports
  property Distris: String read FSupportedDistris;
  //** Absolute path to the description file
  property DescFile: String read FDescFile;
  //** Path to an wizard image
  property WizImage: String read FWizImage;
  //** Path to an license file
  property LicenseFile: String read FLicenseFile;
  //** List of all profiles the package has
  property Profiles: TStringList read PkProfiles;
  //** Path to an icon of the applications/setup
  property AppIcon: String read IIconPath;
  //** All registerd .desktop files
  property DesktopFiles: String read IDesktopFiles;
  //** List of all dependencies
  property ADeps: TStringList read Dependencies write Dependencies;
  //** Commend line to execute the new application
  property CMDLn: String read IAppCMD;
  //** Possibility to assign HTTPSend object
  property HTTPSend: THTTPSend read HTTP write HTTP;
  //** Possibility to assign FTPSend object
  property FTPSend: TFTPSend read FTP write FTP;
  //** Path to the current file information (that fits the profile)
  property IFileInfo: String read FFileInfo write FFileInfo;
  //** Name of the current profile
  property CurrentProfile: String read CurProfile write CurProfile;
  //Progress events
  property OnProgressMainChange: TProgressChange read FProgChange1 write FProgChange1;
  property OnProgressExtraChange: TProgressChange read FProgChange2 write FProgChange2;
  property OnMainVisibleChange: TVisSwitchEvent read FVisible1 write FVisible1;
  property OnExtraVisibleChange: TVisSwitchEvent read FVisible2 write FVisible2;
  //Message events
  property OnError: TMessageEvent read FErrorMsg write FErrorMsg;
  property OnTermQuestion: TMessageEvent read FTermRequest write FTermRequest;
  property OnStateMessage: TMessageEvent read FSMessage write FSMessage;
end;

{** Removes an IPK application
     @param AppName Name of the application, that should be uninstalled
     @param AppID ID of the application
     @param Log TStrings to get the log output
     @param progress Event handler for operation progress (set nil if not needed)
     @param fast Does a quick uninstallation if is true (Set to "False" by default)
     @param RmDeps Remove dependencies if true (Set to "True" by default)}
 procedure UninstallIPKApp(AppName, AppID: String; var Log: TStrings;progress: TProgressChange; fast: Boolean=false; RmDeps:Boolean=true);

 {** Checks dependencies of all installed apps
     @param report Report of the executed actions
     @param fix True if all found issues should be fixed right now
     @returns True if everything is okay, False if dependencies are missing}
 function CheckApps(report: TStringList;const fix: Boolean=false;const forceroot: Boolean=false): Boolean;

 //** Checks if package is installed
 function IsPackageInstalled(aName: String;aID: String): Boolean;

 var
  //** Path to package registration
  RegDir: String;
  //** @deprecated Set if application installs shared files
  ContSFiles: Boolean=false;

  Testmode: Boolean;

  //Notice: The "Testmode" variable is declared in LiCommon.pas

implementation

procedure TInstallation.SetMainProg(pos: integer;max: Integer);
begin
 if Assigned(FProgChange1) then FProgChange1(max,pos);
end;

procedure TInstallation.SetExtraProg(pos: integer;max: Integer);
begin
 if Assigned(FProgChange2) then FProgChange2(max,pos);
end;

procedure TInstallation.SetMainPosVisibility(vis: Boolean);
begin
 if Assigned(FVisible1) then FVisible1(vis);
end;

procedure TInstallation.SetExtraPosVisibility(vis: Boolean);
begin
 if Assigned(FVisible2) then FVisible2(vis);
end;

procedure TInstallation.SendErrorMsg(msg: String);
begin
 if Assigned(FErrorMsg) then FErrorMsg(msg);
end;

procedure TInstallation.SendTermQuestion(qs: String);
begin
 if Assigned(FTermRequest) then FTermRequest(qs);
end;

procedure TInstallation.SendStateMsg(msg: String);
begin
 if Assigned(FSMessage) then FSMessage(msg);
end;

constructor TInstallation.Create;
begin
 inherited Create;
 writeLn(rsOpeningDB);
 dsApp:= TSQLite3Dataset.Create(nil);
 if IsRoot then
  RegDir:='/etc/lipa/app-reg/'
 else
  RegDir:=SyblToPath('$INST')+'/app-reg/';
end;

destructor TInstallation.Destroy;
begin
 inherited Destroy;
 dsApp.Free;
 writeLn('Database connection closed.');
 if Assigned(Dependencies) then Dependencies.Free;
end;

function TInstallation.FindChildNode(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i].FirstChild;break;exit;end else
Result:=nil;
end;
end;

function TInstallation.FindChildNodeX(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i];break;exit;end else
Result:=nil;
end;
end;

function IsPackageInstalled(aname: String;aid: String): Boolean;
var dsApp: TSQLite3Dataset;
begin
dsApp:=TSQLite3Dataset.Create(nil);

with dsApp do
 begin
   FileName:=RegDir+'applications.db';
   TableName:='AppInfo';
   if not FileExists(FileName) then
   begin
   with FieldDefs do
     begin
       Clear;
       Add('Name',ftString,0,true);
       Add('ID',ftString,0,true);
       Add('Type',ftString,0,true);
       Add('Description',ftString,0,False);
       Add('Version',ftFloat,0,true);
       Add('Publisher',ftString,0,False);
       Add('Icon',ftString,0,False);
       Add('Profile',ftString,0,False);
       Add('AGroup',ftString,0,true);
       Add('InstallDate',ftDateTime,0,False);
       Add('Dependencies',ftMemo,0,False);
     end;
   CreateTable;
 end;
end;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered := true;
dsApp.First;

Result:=false;
while not dsApp.EOF do
begin
 if (dsApp.FieldByName('Name').AsString=aName) and (dsApp.FieldByName('ID').AsString=aID) then
begin
Result:=true;
break;
end else Result:=false;
 dsApp.Next;
end;

dsApp.Close;
dsApp.Free;
end;

procedure RemoveDuplicates(sl: TStringList);
var
i,j:Integer;
begin
i := 0;
  while i <= sl.Count-1 do
  begin
    for j := i+1 to sl.Count-1 do
    begin
      if sl.Strings[i] = sl.Strings[j] then
      begin
         dec(i);
         sl.Delete(j);
        break;
      end;
    end;
    inc(i);
  end;
end;

function TInstallation.ResolveDependencies: Boolean;
var i: Integer;p: TProcess;h: String;tmp: TStringList;pkit: TPackageKit;mnpos,max: Integer;
begin
 Result:=true;
  if (Dependencies.Count>0) and (Dependencies[0]='*getlibs*') then
  begin
    Dependencies.Delete(0);
    mnpos:=0;
    SetMainPosVisibility(true);
    max:=Dependencies.Count*2;
    SetMainProg(mnpos,max);
    p:=TProcess.Create(nil);
    ShowPKMon();
    p.Options:=[poUsePipes,poWaitOnExit];
    tmp:=TStringList.Create;
    for i:=0 to Dependencies.Count-1 do
    begin
     mnpos:=mnpos+1;
     SetMainProg(mnpos,max);
     pkit:=TPackageKit.Create;
     h:=pkit.PkgNameFromNIFile(Dependencies[i]);
     pkit.Free;
     if h='Failed!' then begin SendErrorMsg(StringReplace(rsDepNotFound,'%l',Dependencies[i],[rfReplaceAll])+#13+rsInClose);tmp.Free;Result:=false;exit;end;
     if h='PackageKit problem.' then begin SendErrorMsg(rsPKitProbPkMon);tmp.Free;Result:=false;exit;end;
     tmp.Add(h);
     h:='';
     mnpos:=mnpos+1;
     SetMainProg(mnpos,max);
    end;
    RemoveDuplicates(tmp);
    Dependencies.Assign(tmp);
    tmp.Free;
    SetMainPosVisibility(false);
  end;
end;

function TInstallation.Initialize(fname: String): Boolean;
var z: TAbUnZipper;
    Doc: TXMLDocument;xnode: TDOMNode;
    FDir,n: String;
    i: Integer;
    DInfo: TDistroInfo;
    p: TProcess;
begin
 Result:=true;
with dsApp do
 begin
   FileName:=RegDir+'applications.db';
   TableName:='AppInfo';
   if not FileExists(FileName) then
   begin
   with FieldDefs do
     begin
       Clear;
       Add('Name',ftString,0,true);
       Add('ID',ftString,0,true);
       Add('Type',ftString,0,true);
       Add('Description',ftString,0,False);
       Add('Version',ftFloat,0,true);
       Add('Publisher',ftString,0,False);
       Add('Icon',ftString,0,False);
       Add('Profile',ftString,0,False);
       Add('AGroup',ftString,0,true);
       Add('InstallDate',ftDateTime,0,False);
       Add('Dependencies',ftMemo,0,False);
     end;
   CreateTable;
 end;
end;
dsApp.Active:=true;
writeLn('SQLite version: '+dsApp.SqliteVersion);



writeLn('Loading IPK package...');
//Begin loading package
RmApp:=false;
z:=TAbUnZipper.Create(nil);
FDir:=lp+ExtractFileName(fname)+'/';
if not DirectoryExists(lp) then
CreateDir(lp);
if not DirectoryExists(FDir) then
CreateDir(FDIR);

try
z.FileName:=fname;
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp+ExtractFileName(fname);

z.ExtractFiles('arcinfo.pin');
except
z.Free;
SendErrorMsg(rsExtractError+#13+rsPkgDM+#13+rsABLoad);
Result:=false;
exit;
end;

PkgName:=ExtractFileName(fname);
PkgPath:=fname;

ReadXMLFile(Doc,lp+PkgName+'/arcinfo.pin'); //Read XML configuration
xnode:=Doc.FindNode('package');
n:=xnode.Attributes.GetNamedItem('type').NodeValue;
n:=LowerCase(n);
if n='linstall' then pkType:=lptLinstall;
if n='' then pkType:=lptLinstall;
if n='dlink' then pkType:=lptDLink;
if n='container' then pkType:=lptContainer;

if (xnode.Attributes.GetNamedItem('patch')<>nil)
and(xnode.Attributes.GetNamedItem('patch').NodeValue='true')
then begin
FPatch:=true;
writeLn('WARNING: This package patches another application on your machine!');
end else FPatch:=false;

if FindChildNode(xnode,'disallow')<>nil then
FDisallow:=LowerCase(FindChildNode(xnode,'disallow').NodeValue)
else
FDisallow:='';

if pkType=lptLinstall then
begin

writeLn('Package type is "linstall"');

n:=GetSystemArchitecture;

if (pos('iofilecheck',FDisallow)>0) then begin
FOverwrite:=true;
writeLn('WARNING: This package will overwrite existing files without request!');
end else FOverwrite:=false;

writeLn('Architecture: '+n);
writeLn('Package-Arch: '+FindChildNode(xnode,'architecture').NodeValue);
if (pos(n,LowerCase(FindChildNode(xnode,'architecture').NodeValue))<=0)
and (LowerCase(FindChildNode(xnode,'architecture').NodeValue)<>'all') then begin
SendErrorMsg(rsInvArchitecture);
z.Free;
Result:=false;
exit;
end;

pID:=xnode.FindNode('id').FirstChild.NodeValue;
idName:='';
if xnode.FindNode('idName')<> nil then
idName:=xnode.FindNode('idName').FirstChild.NodeValue;
writeLn('Package idName: '+idName);

//Find profiles
i:=1;
PkProfiles:=TStringList.Create;
repeat
if FindChildNode(xnode,'profile'+IntToStr(i))<>nil then begin
PkProfiles.Add(FindChildNode(xnode,'profile'+IntToStr(i)).NodeValue+' #'+FindChildNodeX(xnode,'profile'+IntToStr(i)).Attributes.GetNamedItem('id').NodeValue);
writeLn('Found installation profile '+PkProfiles[PkProfiles.Count-1]);
z.ExtractFiles('fileinfo-'+FindChildNodeX(xnode,'profile'+IntToStr(i)).Attributes.GetNamedItem('id').NodeValue+'.id');
Inc(i);
end;
until FindChildNode(xnode,'profile'+IntToStr(i))=nil;

xnode:=Doc.DocumentElement.FindNode('application');

IAppName:=xnode.Attributes.GetNamedItem('name').NodeValue;

if idName='' then
idName:=xnode.Attributes.GetNamedItem('name').NodeValue;

if FindChildNode(xnode,'version')<>nil then
IAppVersion:=FindChildNode(xnode,'version').NodeValue;

if FindChildNode(xnode,'description')<>nil then begin
FDescFile:=FindChildNode(xnode,'description').NodeValue;
z.ExtractFiles(ExtractFileName(FDescFile));
end;
if FindChildNode(xnode,'license')<>nil then begin
FLicenseFile:=FindChildNode(xnode,'license').NodeValue;
z.ExtractFiles(ExtractFileName(FLicenseFile));
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
else
IAppCMD:='#';

if IAppCMD <> '#' then
writeLn('Application main exec command is '+IAppCMD);

if (IAppCMD='#')and (Testmode) then
begin
 SendErrorMsg(rsActionNotPossiblePkg);
 z.Free;
 Result:=false;
 exit;
end;

IAppCMD:=SyblToPath(IAppCMD);

if length(pID)<>17 then begin
 SendErrorMsg(rsIDInvalid);
 z.Free;
 Result:=false;
 exit;
 end;

//Load Description
for i:=0 to xnode.ChildNodes.Count-1 do begin
if LowerCase(xnode.ChildNodes.Item[i].NodeName)='sdesc' then begin
xnode:=xnode.ChildNodes.Item[i];break;end;
end;
ShDesc:='#';
if xnode <> nil then begin
if FindChildNode(xnode,GetLangID)<>nil then
ShDesc:=FindChildNode(xnode,GetLangID).NodeValue
else
ShDesc:=xnode.Attributes.GetNamedItem('std').NodeValue;
end;

xnode:=Doc.DocumentElement.FindNode('application');


AType:=FindChildNode(xnode,'group').NodeValue;
if AType='' then AType:='other';

if FindChildNode(xnode,'icon')<>nil then
begin
IIconPath:=FindChildNode(xnode,'icon').NodeValue;
z.ExtractFiles(ExtractFileName(IIconPath));
IIconPath:=lp+PkgName+IIconPath;
end;

//Only executed to make sure that "RmApp" property is set
if not Testmode then
IsPackageInstalled(IAppName,idName);

if FindChildNode(xnode,'dsupport')<> nil then
FSupportedDistris:=LowerCase(FindChildNode(xnode,'dsupport').NodeValue);


//Load Wizard-Image
FWizImage:=GetDataFile('graphics/wizardimage.png');
if (xnode.FindNode('wizimage')<>nil) and (PAnsiChar(xnode.FindNode('wizimage').NodeValue)[0] = '/') then begin
z.ExtractFiles(ExtractFileName(xnode.FindNode('wizimage').NodeValue));
if FileExists(lp+PkgName+xnode.FindNode('wizimage').NodeValue) then
FWizImage:=lp+PkgName+xnode.FindNode('wizimage').NodeValue;
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

//Load Description
if LowerCase(DescFile)<>'' then
FDescFile:=lp+PkgName+DescFile;

//Load License
if LowerCase(LicenseFile)<>'' then
FLicenseFile:=lp+PkgName+FLicenseFile;

//Load Dependencies
Dependencies:=TStringList.Create;

xnode:=Doc.FindNode('package'); //Set xnode to the package tree
DInfo:=GetDistro;

if xnode.FindNode('dependencies')<>nil then
begin
 xnode:=xnode.FindNode('dependencies');
 if xnode.FindNode('pkcatalog')<>nil then
 begin
  z.ExtractFiles(ExtractFileName(xnode.FindNode('pkcatalog').NodeValue));
  Dependencies.Add('cat:'+lp+PkgName+xnode.FindNode('pkcatalog').NodeValue)
 end else
 begin
 Dependencies.Add('*getlibs*');
 for i:=0 to xnode.ChildNodes.Count-1 do
  Dependencies.Add(xnode.ChildNodes.Item[i].FirstChild.NodeValue);
 end;
end else
begin
if xnode.FindNode('Dep'+DInfo.DName)<>nil then
begin  //Check if there are specific packages available

if (xnode.FindNode('Dep'+DInfo.DName).Attributes.GetNamedItem('releases')<>nil)
and (pos(DInfo.Release,xnode.FindNode('Dep'+DInfo.DName).Attributes.GetNamedItem('releases').NodeValue)<= 0)
then
begin
 SendTermQuestion(rsInvalidDVersion);
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

end;

for i:=0 to Dependencies.Count-1 do
if Dependencies[i][1]='.' then z.ExtractFiles(ExtractFileName(Dependencies[i]));

z.Free;

writeLn('Profiles count is '+IntToStr(PkProfiles.Count));
if PkProfiles.Count<0 then begin
SendErrorMsg(rsPkgInval+#13'Message: No profiles and no file list found!');
Profiles.Free;
Result:=false;
exit;
end;

if PkProfiles.Count=1 then FFileInfo:='/stuff/fileinfo-'+copy(PkProfiles[0],pos(' #',PkProfiles[0])+2,length(PkProfiles[0]))+'.id'
else FFileInfo:='*';

if IsPackageInstalled(AppName,AppID) then RmApp:=true;

end else //Handle other IPK types
if pkType=lptDLink then begin
writeLn('Package type is "dlink"');

z.BaseDirectory:=lp+ExtractFileName(paramstr(1));

xnode:=Doc.DocumentElement.FindNode('application');

FDescFile:=FindChildNode(xnode,'description').NodeValue;

IAppName:=xnode.Attributes.GetNamedItem('name').NodeValue;
IAppVersion:=FindChildNode(xnode,'version').NodeValue;

if FindChildNode(xnode,'icon')<>nil then
begin
IIconPath:=FindChildNode(xnode,'icon').NodeValue;
z.ExtractFiles(ExtractFileName(IIconPath));
IIconPath:=lp+PkgName+IIconPath;
end;

if FindChildNode(xnode,'author')<>nil then
IAuthor:=FindChildNode(xnode,'author').NodeValue
else
IAuthor:='#';

AType:=FindChildNode(xnode,'group').NodeValue;
z.ExtractFiles(ExtractFileName(FDescFile));

if FindChildNode(xnode,'desktopfiles')<>nil then
IDesktopFiles:=FindChildNode(xnode,'desktopfiles').NodeValue;

z.Free;

//Load Description
for i:=0 to xnode.ChildNodes.Count-1 do begin
if LowerCase(xnode.ChildNodes.Item[i].NodeName)='sdesc' then
begin
 xnode:=xnode.ChildNodes.Item[i];
 break;
end;
end;
ShDesc:='#';
if xnode <> nil then begin
if FindChildNode(xnode,GetLangID)<>nil then
ShDesc:=FindChildNode(xnode,GetLangID).NodeValue
else
ShDesc:=xnode.Attributes.GetNamedItem('std').NodeValue;
end;

Dependencies:=TStringList.Create;
xnode:=Doc.FindNode('package');

if xnode.FindNode('Dep'+DInfo.DName)<>nil then //Check if there are specific packages available for the distribution
begin
xnode:=xnode.FindNode('Dep'+DInfo.DName);

for i:=0 to xnode.ChildNodes.Count-1 do
begin
n:=xnode.ChildNodes.Item[i].FirstChild.NodeValue;
if pos(' <',n)>0 then
Dependencies.Add(copy(n,pos(' <',n)+2,length(n)-pos(' <',n)-2)+' - '+copy(n,1,pos(' <',n)-1))
else Dependencies.Add(n);
end;
end else
begin
SendTermQuestion(rsNoLDSources);

if xnode.FindNode('Dep'+DInfo.PackageSystem)=nil then
begin
 SendErrorMsg(rsNoComp+#13+rsInClose);
 Result:=false;
 exit;
end;

xnode:=xnode.FindNode('Dep'+DInfo.PackageSystem);
for i:=0 to xnode.ChildNodes.Count-1 do
begin
n:=xnode.ChildNodes.Item[i].FirstChild.NodeValue;
if pos(' <',n)>0 then
Dependencies.Add(copy(n,pos(' <',n)+2,length(n)-pos(' <',n)-2)+' - '+copy(n,1,pos(' <',n)-1))
else Dependencies.Add(n);
end;
end;

end else
if pkType=lptContainer then
begin
writeLn('Package type is "container"');
z:=TAbUnZipper.Create(nil);
z.FileName:=PkgPath;
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp;
xnode:=Doc.DocumentElement.FindNode('application');
z.ExtractFiles(ExtractFileName(FindChildNode(xnode,'package').NodeValue));
for i:=0 to FindChildNode(xnode,'package').ChildNodes.Count-1 do
z.ExtractFiles(ExtractFileName(FindChildNode(xnode,'package').ChildNodes[i].NodeValue));

z.Free;
 p:=TProcess.create(nil);
 p.CommandLine:='chmod 755 ''/tmp/'+FindChildNode(xnode,'package').NodeValue+'''';
 p.Options:=[poUsePipes,poWaitonexit];
 p.Execute;
 p.Options:=[poUsePipes,poWaitonexit,poNewConsole];
 if LowerCase(ExtractFileExt(FindChildNode(xnode,'package').NodeValue))='.package' then begin
 if FileExists('/usr/bin/package') then
 p.Options:=[poUsePipes,poWaitonexit];

 p.CommandLine:='/tmp/'+FindChildNode(xnode,'package').NodeValue;
 p.Execute;
 p.Free;
 end else begin
 if (FindChildNode(xnode,'InTerminal')<>nil)and(LowerCase(FindChildNode(xnode,'InTerminal').NodeValue)='true') then
 p.Options:=[poUsePipes, poWaitOnExit, poNewConsole]
 else p.Options:=[poUsePipes, poWaitOnExit];
 p.CommandLine:='/tmp/'+FindChildNode(xnode,'package').NodeValue;
 p.Execute;
 p.Free;
 end;

DeleteFile('/tmp/'+FindChildNode(xnode,'package').NodeValue);
end;
end;

function TInstallation.GetMaxInstSteps: Integer;
var fi: TStringList;
begin
Result:=0;
fi:=TStringList.Create;
fi.LoadFromFile(lp+PkgName+FFileInfo);
Result:=((Dependencies.Count+(fi.Count div 3))*10)+16;
fi.Free;
end;

function TInstallation.RunNormalInstallation(proc: TProcess;ln: TStrings): Boolean;
var
i,j: Integer;
fi,ndirs, s, appfiles: TStringList;
dest,h, FDir: String; // h is an helper variable - used for various actions
dsk, cnf: TIniFile; // Configuration files etc.
z: TAbUnZipper; // Zipper
setcm: Boolean;
p:TProcess; // Helper process with pipes
pkit: TPackageKit; //PackageKit object
DInfo: TDistroInfo; //Distribution information
mnpos,expos,max,emax: Integer; //Current positions of operation
begin
fi:=TStringList.Create;
fi.LoadFromFile(lp+PkgName+FFileInfo);

DInfo:=GetDistro; //Load DistroInfo

if (fi.Count mod 3)<>0 then begin
SendErrorMsg(rsPKGError+#13'Message: File list is unlogical!'+#13+rsAppClose);
fi.Free;
Result:=false;
exit;
end;

Result:=true;
mnpos:=0;
expos:=0;

//Check if fileinfo contains shared files
for i:=0 to (fi.Count div 3)-1 do begin
if IsSharedFile(lp+PkgName+fi[i*3]) then ContSFiles:=true;
end;

max:=GetMaxInstSteps;
SetMainProg(mnpos,max);

if Testmode then
begin
 proc.CommandLine := 'rm -rf /tmp/litest';
 proc.Execute;
end;

//Check if PackageKit trackmode is enabled:
cnf:=TIniFile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('MainConf','ShowPkMon',false) then begin
p:=TProcess.Create(nil);
p.CommandLine:='pkmon';
p.Options:=[poNewConsole];
p.Execute;
p.free;
end;
cnf.free;

SetExtraPosVisibility(false);
//Execute programs/scripts
if ExecA<>'<disabled>' then
begin
    Proc.CommandLine := 'chmod 777 '+ExecA;
    Proc.Execute;
 //while proc.Running do Application.ProcessMessages;
    Proc.CommandLine := ExecA;
    Proc.Execute;
 //while proc.Running do Application.ProcessMessages;
end;

pkit:=TPackageKit.Create;

if Dependencies.Count>0 then
begin
//Check if we should install a software catalog
if pos('cat:',Dependencies[0])>0 then
begin
 ln.Add('Installing package catalog...');
 pkit.InstallLocalPkg(copy(Dependencies[0],5,length(Dependencies[0])));

 if not pkit.OperationSucessfull then
 begin
  SendErrorMsg(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+IAppName+'.log',[rfReplaceAll]));
  p.Free;
  pkit.Free;
  Result:=false;
  ln.SaveTofile('/tmp/install-'+IAppName+'.log');
  exit;
 end;
 Dependencies[0]:='cat:'+ExtractFileName(copy(Dependencies[0],5,length(Dependencies[0])));

end else
begin
for I:=0 to Dependencies.Count-1 do
begin  //Download & install dependencies
if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
begin

cnf:=TInifile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('MainConf','AutoDepLoad',true)=false then
SendTermQuestion(StringReplace(rsWDLDep,'%l',Dependencies[i],[rfReplaceAll])+#13+rsWAllow);

cnf.Free;

    ln.Add(rsGetDependencyFrom+' '+Dependencies[i]+'.');
    ln.Add(rsPlWait2);
 if pos('http://',LowerCase(Dependencies[i]))>0 then
 begin
  try
    HTTP.HTTPMethod('GET', copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    HTTP.Document.SaveToFile('/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));
  except
  SendErrorMsg(rsDepDLProblem);
  Result:=false;
  exit;
  end;
 end else begin

with FTP do begin
    TargetHost := GetServerName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));

  try
    DirectFileName := '/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    DirectFile:=True;
    if not Login then
    begin
      SendErrorMsg('Couldn''t login on the FTP-Server!');
      Result:=false;
      exit;
    end;
    ChangeWorkingDir(GetServerPath(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));

    emax:=FileSize(ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));
    SetExtraProg(0,emax);

    RetrieveFile(ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)), false);
    Logout;
  except
   SendErrorMsg(rsDepDLProblem);
   Result:=false;
   exit;
  end;
  end;
end;

//Add package-name

if (DInfo.PackageSystem='DEB')and(pos(' <',Dependencies[i])<=0) then begin
    p:=tprocess.create(nil);
    p.CommandLine:='dpkg --info /tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    p.Options:=[poUsePipes,poWaitonexit];
    try
    p.Execute;
    s:=tstringlist.Create;
    try
    s.LoadFromStream(p.Output);
    for j:=0 to s.Count-1 do
    if pos('Package: ',s[j])>0 then break;
    Dependencies[i]:=Dependencies[i]+' <'+copy(s[j],11,length(s[j]))+'>';
    finally
    s.free;
    end;
   finally
    p.Free;
    end;
end else begin
if (pos(' <',Dependencies[i])<=0) then begin
    p:=TProcess.create(nil);
    p.CommandLine:='rpm -qip /tmp/'+ExtractFileName(Dependencies[i]);
    p.Options:=[poUsePipes,poWaitonexit];
    try
    p.Execute;
    s:=tstringlist.Create;
    try
    s.LoadFromStream(p.Output);
    for j:=0 to s.Count-1 do
    if pos('Name ',s[j])>0 then break;
    Dependencies[i]:=Dependencies[i]+' <'+copy(s[j],15,pos(' ',copy(s[j],15,length(s[j])))-1)+'>';
    finally
    s.free;
    end;
   finally
    p.Free;
    end;

    end;
end;

ln.Add('Done.');
end;

SetExtraPosVisibility(false);
sleep(18); //Wait...

if (Dependencies[i][1]='/')or(pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
begin

    ln.Add('--');
    ln.Add('DepInstall: '+Dependencies[i]+' (using PackageKit +x)');
    ln.Add('-');

  if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
  begin
    if not pkit.IsInstalled(copy(Dependencies[i],pos(' <',Dependencies[i])+2,length(Dependencies[i])-1)) then
     pkit.InstallLocalPkg('/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i]))));
  end else
    pkit.InstallLocalPkg(lp+PkgName+Dependencies[i]);

    mnpos:=mnpos+5;
    SetMainProg(mnpos,max);

    //Check if the package was really installed
    if not pkit.OperationSucessfull then
    begin
    SendErrorMsg(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+IAppName+'.log',[rfReplaceAll]));
    p.Free;
    Result:=false;
    ln.SaveTofile('/tmp/install-'+IAppName+'.log');
    exit;
    end;

    mnpos:=mnpos+5;
    SetMainProg(mnpos,max);

  //while proc.Running do Application.ProcessMessages;

 //If only a file name given install them with distri-tool
end else begin
     ln.Add('--');
     ln.Add('DepInstall: '+Dependencies[i]+' (using PackageKit)');
     ln.Add('-');

     if not pkit.IsInstalled(Dependencies[i]) then begin
      pkit.InstallPkg(Dependencies[i]);

    //Check if the package was really installed
  if not pkit.OperationSucessfull then
    begin
     writeLn('Package '+Dependencies[i]+' can not be installed.');
     SendErrorMsg(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+IAppName+'.log',[rfReplaceAll]));
     ln.SaveTofile('/tmp/install-'+IAppName+'.log');
     Result:=false;
     exit;
    end;

    end; //Eof <> 1

    mnpos:=mnpos+10;
    SetMainProg(mnpos,max);
  end;
   end; //End of PKCatalog end-else
  end; //End of Dependecies.Count term
end; //End of dependency-check

pkit.Free;

SetExtraPosVisibility(false);

//Delete old application installation if necessary
if (RmApp)and(not Testmode) then
begin
SetExtraPosVisibility(true);
//??? Important: Enable this after testing
//UnInstallIPKApp(IAppName,idName,ln,FPos2,true);
SetExtraPosVisibility(false);
end;

appfiles:=TStringList.Create;
SendStateMsg(rsStep2);
z:=TAbUnZipper.Create(nil);
z.FileName:=PkgPath;
ndirs:=TStringList.Create;
j:=0;
if not DirectoryExists(SyblToPath('$INST')) then SysUtils.CreateDir(SyblToPath('$INST'));
for i:=0 to fi.Count-1 do
begin
if i mod 3 = 0 then
begin

if (pos(' <'+LowerCase(DInfo.DName)+'-only>',LowerCase(fi[i]))>0)
or (pos('-only',LowerCase(fi[i]))<=0) then
begin

dest:=SyblToPath(fi[i+2]);

if not DirectoryExists(dest) then
begin
SysUtils.CreateDir(dest);
ndirs.Add(dest);
end;
h:=dest;
while not DirectoryExists(dest) do
begin
CreateDir(h);
if DirectoryExists(h) then h:=Dest
else h:=ExtractFilePath(ExcludeTrailingBackslash(h));
end;
//h is now used for the file-path
h:=DeleteModifiers(fi[i]);


FDir:=lp+ExtractFileName(PkgPath)+'/';
if not DirectoryExists(FDir) then
CreateDir(FDIR);

try
z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
z.BaseDirectory:=lp+ExtractFileName(PkgPath);

z.ExtractFiles(ExtractFileName(h));
except
SendErrorMsg(rsExtractError);
z.Free;
halt;
end;

ln.Add('Copy file '+ExtractFileName(h)+' to '+dest+' ...');

if fi[i+1] <> MDPrint((MD5.MD5File(DeleteModifiers(lp+PkgName+h),1024))) then begin
SendErrorMsg(rsHashError);
exit;
end;

Inc(j);

if FOverwrite then
FileCopy(DeleteModifiers(lp+PkgName+h),dest+'/'+ExtractFileName(DeleteModifiers(h)))
else
if (not FileExists(dest+'/'+ExtractFileName(DeleteModifiers(h)))) then
 FileCopy(DeleteModifiers(lp+PkgName+h),dest+'/'+ExtractFileName(DeleteModifiers(h)))
else
begin
  SendErrorMsg(StringReplace(rsCnOverride,'%f',dest+'/'+ExtractFileName(DeleteModifiers(h)),[rfReplaceAll])+#10+rsInClose);
  exit;
end;

if(pos('.desktop',LowerCase(ExtractFileName(h)))>0) then
begin
dsk:=TIniFile.Create(dest+'/'+ExtractFileName(h));
dsk.WriteString('Desktop Entry','X-AppVersion',IAppVersion);
dsk.WriteString('Desktop Entry','X-AllowRemove','true');
dsk.WriteString('Desktop Entry','X-Publisher',IAuthor);
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
ln.Add('Okay.');

mnpos:=mnpos+10;
SetMainProg(mnpos,max);

  end;

 end;
end;

SendStateMsg(rsStep3);
//Check if every single file needs its own command to get the required rights (It's faster if only every folder recieves the rights)
setcm:=false;
for i:=0 to (fi.Count div 3)-1 do
if pos(' <chmod:',fi[i*3])>0 then setcm:=true;


if setcm then
begin //Rechte einzeln setzen
for i:=0 to fi.Count-1 do
begin
if i mod 3 = 0 then
begin
h:=fi[i];

if pos(' <chmod:',h)>0 then begin
proc.CommandLine := 'chmod '+copy(h,pos(' <chmod:',h)+8,3)+SyblToPath(fi[i+1])+'/'+ExtractFileName(DeleteModifiers(fi[i]));
proc.Execute;
end else
begin
proc.CommandLine := 'chmod 755 '+SyblToPath(fi[i+1])+'/'+ExtractFileName(DeleteModifiers(fi[i]));
proc.Execute;
end;

 //while proc.Running do Application.ProcessMessages;
ln.Add('Rights assigned to '+DeleteModifiers(ExtractFileName(SyblToPath(fi[i]))));
 end;
end;
end else
begin //Rechte ordnerweise setzen
for i:=0 to ndirs.Count-1 do
begin
proc.CommandLine := 'chmod 755 -R '+SyblToPath(ndirs[i]);
proc.Execute;
ln.Add('Rights assigned to folder '+ExtractFileName(SyblToPath(ndirs[i])));
 end;
end; //Ende setcm

mnpos:=mnpos+6;
SetMainProg(mnpos,max);;

fi.Free;
SendStateMsg(rsStep4);

if not FPatch then
begin

if not DirectoryExists(RegDir+LowerCase(IAppName+'-'+idName)) then SysUtils.CreateDir(RegDir+LowerCase(IAppName+'-'+idName));
FileCopy(lp+PkgName+'/arcinfo.pin',RegDir+LowerCase(IAppName+'-'+idName)+'/proginfo.pin');

//Save list of installed files
appfiles.SaveToFile(RegDir+LowerCase(IAppName+'-'+idName)+'/appfiles.list');
appfiles.Free;

//Open database connection
dsApp.Open;
dsApp.Edit;

if pkType=lptLinstall then h:='linstall';
if pkType=lptDLink then h:='dlink';
if pkType=lptContainer then h:='containerF';

dsApp.Insert;
dsApp.ExecuteDirect('INSERT INTO "AppInfo" VALUES ('''+IAppName+''', '''+
        idName+''', '''+h+''', '''+ShDesc+''','''+IAppVersion+''','''+IAuthor+''','''+'icon'+ExtractFileExt(IconPath)+''','''+CurProfile+''','''+
        AType+''','''+GetDateAsString+''', '''+Dependencies.Text+''');');

//Write changes
dsApp.ApplyUpdates;
dsApp.Close;

if IconPath[1]='/' then
FileCopy(lp+PkgName+IconPath,RegDir+LowerCase(IAppName+'-'+idName)+'/icon'+ExtractFileExt(IconPath));

ndirs.SaveToFile(RegDir+LowerCase(IAppName+'-'+idName)+'/appdirs.list');

if ExecX<>'<disabled>' then
FileCopy(ExecX,RegDir+LowerCase(IAppName+'-'+idName)+'/prerm');

ndirs.Free;

end; //End of Patch check

mnpos:=mnpos+5;
SetMainProg(mnpos,max);
//Execute Program/Script
if ExecB<>'<disabled>' then begin
    proc.CommandLine := 'chmod 777 '+ExecB;
    Proc.Execute;
 //while Proc.Running do Application.ProcessMessages;
    Proc.CommandLine := ExecB;
    Proc.Execute;
 //while Proc.Running do Application.ProcessMessages;
end;

if USource<>'#' then
begin
fi:=TStringList.Create;
if not FileExists(RegDir+'updates.list') then
begin
fi.Add('Listaller UpdateSources-V0.8');
fi.SaveToFile(RegDir+'updates.list');
end;
fi.LoadFromFile(RegDir+'updates.list');
for i:=1 to fi.Count-1 do
if pos(USource,fi[i])>0 then break;
if i=fi.Count then
begin
// ??? Needs a proper solution: Command-line tools shouldn't display visual dialogs (and need X11)
{if Application.MessageBox(PAnsiChar(strAddUpdSrc+#13+
copy(USource,pos(' <',USource)+2,length(USource)-pos(' <',USource)-2)+' ('+copy(uSource,3,pos(' <',USource)-3)+')'+#13+
PAnsiChar(strQAddUpdSrc)),'Add update-source',MB_YESNO)= IDYES then
begin   }
fi.Add('- '+USource);
fi.SaveToFile(RegDir+'updates.list');
 //end;
 end;
 fi.Free;
end;

mnpos:=mnpos+5;
SetMainProg(mnpos,max);;

SendStateMsg(rsFinished);
sleep(600);
ln.SaveTofile('/tmp/install-'+IAppName+'.log');
end;

function TInstallation.RunDLinkInstallation(proc: TProcess;ln: TStrings): Boolean;
var i: Integer;cnf,ar: TIniFile;pkit: TPackageKit;mnpos,max,emax: Integer;
begin
max:=Dependencies.Count*6000;
mnpos:=0;
SetMainProg(mnpos,max);
HTTP.UserAgent:='Listaller-GET';


//Set Proxy-Settings
cnf:=TInifile.Create(ConfigDir+'config.cnf');
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
 end;}
end;

ShowPKMon();

pkit:=TPackageKit.Create;

  for i:=1 to Dependencies.Count-1 do
  begin
  if pos('://',Dependencies[i])<=0 then
  begin
   ln.Add('Looking for '+Dependencies[i]);
  if not pkit.IsInstalled(Dependencies[i]) then
  begin
   ln.Add('Installing '+Dependencies[i]+'...');

   pkit.InstallPkg(Dependencies[i]);
  end;
  end else begin
   ln.Add('Looking for '+copy(Dependencies[i],1,pos(' -',Dependencies[i])-1));
  if not pkit.IsInstalled(copy(Dependencies[i],1,pos(' -',Dependencies[i])-1)) then
  begin
   ln.Add('Downloading package...');

if pos('http://',LowerCase(Dependencies[i]))>0 then
begin
 try
  HTTP.HTTPMethod('GET', copy(Dependencies[i],pos(' -',Dependencies[i])+2,length(Dependencies[i])-pos(' -',Dependencies[i])+2));
  HTTP.Document.SaveToFile('/tmp/'+ExtractFileName(Dependencies[i]));
 except
   SendErrorMsg(rsDepDlProblem);;
   exit;
  end;

  end else
  begin
  with FTP do
  begin
    TargetHost := GetServerName(Dependencies[i]);
  try
    DirectFileName := '/tmp/'+ExtractFileName(Dependencies[i]);
    DirectFile:=True;
    if not Login then SendErrorMsg(rsFTPfailed);
    ChangeWorkingDir(GetServerPath(Dependencies[i]));

    emax:=FileSize(ExtractFileName(Dependencies[i]));
    SetExtraProg(0,emax);

    RetrieveFile(ExtractFileName(Dependencies[i]), false);
    Logout;
  except
   SendErrorMsg(rsDepDlProblem);
   Result:=false;
   exit;
  end;
  end;
end;

  ln.Add('Installing '+copy(Dependencies[i],1,pos(' -',Dependencies[i])-1)+'...');
  pkit.InstallLocalPkg('/tmp/'+ExtractFileName(Dependencies[i]));
end;

  mnpos:=mnpos+6000;
  SetMainProg(mnpos,max);
end;

end;

pkit.Free;


while Length(IDesktopFiles)>1 do begin
if pos(';',IDesktopFiles)>0 then
ar:=TInifile.Create('/usr/share/applications/'+copy(IDesktopFiles,0,pos(';',IDesktopFiles)-1))
else ar:=TInifile.Create('/usr/share/applications/'+IDesktopFiles);
ar.WriteString('Desktop Entry','X-AppVersion',IAppVersion);
ar.WriteString('Desktop Entry','X-Publisher',IAuthor);
if pos(';',IDesktopFiles)>0 then
IDesktopFiles:=copy(IDesktopFiles,pos(';',IDesktopFiles)+1,length(IDesktopFiles))
else IDesktopFiles:='';
ar.Free;
end;

{for i:=1 to Dependencies.Count-1 do begin
if pos('://',Dependencies[i])<=0 then
ar.WriteString('DepOS','ID'+IntToStr(i),Dependencies[i])
else
ar.WriteString('DepOS','ID'+IntToStr(i),copy(Dependencies[i],1,pos(' -',Dependencies[i])-1));
end; }

SendStateMsg(rsSuccess);
end;

function TInstallation.DoInstallation(proc: TProcess;ln: TStrings): Boolean;
begin
 if pkType=lptLinstall then Result:=RunNormalInstallation(proc,ln)
 else
  if pkType=lptDLink then Result:=RunDLinkInstallation(proc,ln)
  else
  begin
   ln.Add('Could not detect package type!');
   ln.Add('TInstallation failure.');
   Result:=false;
  end;
end;

/////////////////////////////////////////////////////
////////////////////////////////////////////////////
///////////////////////////////////////////////////
procedure UninstallIPKApp(AppName,AppID: String; var Log: TStrings;progress: TProgressChange; fast:Boolean=false; RmDeps:Boolean=true);
var tmp,tmp2,s,slist: TStringList;p,f: String;i,j: Integer;k: Boolean;upd: String;
    proc: TProcess;dlink: Boolean;t: TProcess;
    pkit: TPackageKit;
    dsApp: TSQLite3Dataset;
    FPos: TProgressChange;
    mnprog: Integer;
procedure SetPosition(prog: Integer;max: Integer);
begin
if Assigned(FPos) then FPos(max,prog);
end;
begin
p:=RegDir+LowerCase(AppName+'-'+AppID)+'/';
FPos:=progress;
mnprog:=0;
SetPosition(0,100);
//InfoMemo.Lines.Add('Begin uninstallation...');

{
//ExProgress.Max:=((tmp.Count div 2)*10)+4;
reg:=TIniFile.Create(p+'proginfo.pin');
upd:=reg.ReadString('Application','UpdSource','#');
reg.Free;    }

writeLn('- IPK uninstallation -');

writeLn('Opening database...');
dsApp:= TSQLite3Dataset.Create(nil);
with dsApp do
 begin
   FileName:=RegDir+'applications.db';
   TableName:='AppInfo';
   if not FileExists(FileName) then
   begin
   with FieldDefs do
     begin
       Clear;
       Add('Name',ftString,0,true);
       Add('ID',ftString,0,true);
       Add('Type',ftString,0,true);
       Add('Description',ftString,0,False);
       Add('Version',ftFloat,0,true);
       Add('Publisher',ftString,0,False);
       Add('Icon',ftString,0,False);
       Add('Profile',ftString,0,False);
       Add('AGroup',ftString,0,true);
       Add('InstallDate',ftDateTime,0,False);
       Add('Dependencies',ftMemo,0,False);
     end;
   CreateTable;
 end;
end;
dsApp.Active:=true;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Edit;
dsApp.Open;
dsApp.Filtered:=true;
while not dsApp.EOF do
begin
 if (dsApp.FieldByName('Name').AsString=AppName) and (dsApp.FieldByName('ID').AsString=AppID) then
 begin

 if LowerCase(dsApp.FieldByName('Type').AsString) = 'dlink'
 then dlink:=true
 else dlink:=false;

if not dlink then
begin
tmp:=TStringList.Create;
tmp.LoadFromfile(p+'appfiles.list');
//UProgress.Max:=((tmp.Count)*10)+4;
end;

if not fast then
begin
if FileExists(p+'prerm') then
begin
 Log.Add('PreRM-Script found.');
 writeLn('PreRM-Script found.');
 t:=TProcess.Create(nil);
 t.Options:=[poUsePipes,poWaitonexit];
 t.CommandLine:='chmod 775 '''+p+'prerm''';
 t.Execute;
 Log.Add('Executing prerm...');
 writeLn('Executing prerm...');
 t.CommandLine:=''''+p+'prerm''';
 t.Execute;
 t.Free;
 Log.Add('Done.');
 writeLn('Done.');
end;

///////////////////////////////////////
if RmDeps then
begin
Log.Add(rsRMUnsdDeps);
writeLn(rsRMUnsdDeps);
tmp2:=TStringList.Create;
tmp2.Text:=dsApp.FieldByName('Dependencies').AsString;

if tmp2.Count>-1 then
begin
for i:=0 to tmp2.Count-1 do
begin
f:=tmp2[i];
//Skip catalog based packages - impossible to detect unneeded dependencies
if pos('cat:',f)>0 then break;

if (LowerCase(f)<>'libc6') then
begin
//Check if another package requires this package
t:=TProcess.Create(nil);
if pos('>',f)>0 then
Log.Add(f+' # '+copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2))
else Log.Add(f);
writeLn(Log[Log.Count-1]);

pkit:=TPackageKit.Create;
s:=tstringlist.Create;
if pos('>',f)>0 then
 pkit.GetRequires(copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2),s)
else pkit.GetRequires(f,s);

   if s.Count <=1 then
   begin
   if pos('>',f)>0 then
   pkit.RemovePkg(copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2))
   else pkit.RemovePkg(f);
   //GetOutPutTimer.Enabled:=true;

   Log.Add('Removing '+f+'...');
   writeLn('Removing '+f+'...');
  end;

 s.free;
 pkit.Free;
  end;
 end; //End of tmp2-find loop

end else begin Log.Add('No installed deps found!');writeLn('No installed deps found!');end;

tmp2.Free;
 end; //End of remove-deps request
end; //End of "fast"-request
//////////////////////////////////////////////////////

if not dlink then
begin
slist:=TStringList.Create;

{if reg.ReadBool(AppName,'ContSFiles',false) then
begin
tmp2:=TStringList.Create;
reg.ReadSections(tmp2);

for i:=0 to tmp2.Count-1 do begin
if reg.ReadBool(tmp2[i],'ContSFiles',false) then begin
s:=TStringList.Create;

s.LoadFromFile(p+'/appfiles.list');
for j:=0 to s.Count-1 do
if pos(' <s>',s[j])>0 then slist.Add(DeleteModifiers(s[j]));
s.Free;
  end;
tmp2.Free;
 end;
end; //End of shared-test  }

//Undo Mime-registration (if necessary)
for i:=0 to tmp.Count-1 do
begin
if pos( '<mime>',tmp[i])>0 then
begin
Log.Add('Uninstalling MIME-Type...');
writeLn('Uninstalling MIME-Type...');
t:=TProcess.Create(nil);
if (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i])))='.png')
or (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i])))='.xpm') then
begin
t.CommandLine:='xdg-icon-resource uninstall '+SysUtils.ChangeFileExt(ExtractFileName(DeleteModifiers(tmp[i])),'');
t.Execute
end else
begin
t.CommandLine:='xdg-mime uninstall '+DeleteModifiers(f+'/'+ExtractFileName(tmp[i]));
t.Execute;
end;
t.Free;
end;
end;

Log.Add('Removing files...');
writeLn('Removing files...');
//Uninstall application
for i:=0 to tmp.Count-1 do
begin

f:=SyblToPath(tmp[i]);

f:=DeleteModifiers(f);

k:=false;
for j:=0 to slist.Count-1 do
if f = slist[j] then k:=true;

if not k then
DeleteFile(f);

mnprog:=mnprog+10;
SetPosition(mnprog,100);
end;

//InfoMemo.Lines.Add('Direcory remove...');
writeLn('Removing empty dirs...');
tmp.LoadFromFile(p+'appdirs.list');
proc:=TProcess.Create(nil);
for i:=0 to tmp.Count-1 do
begin
  proc.CommandLine :='rm -rf '+tmp[i];
  proc.Execute;
 //while proc.Running do Application.ProcessMessages;
end;
proc.Free;

if upd<>'#' then begin
tmp.LoadFromFile(RegDir+'updates.list');
//InfoMemo.Lines.Add('Removing update-source...');
for i:=1 to tmp.Count-1 do
if pos(upd,tmp[i])>0 then begin tmp.Delete(i);break;end;
tmp.SaveToFile(RegDir+'updates.list');
tmp.Free;
end;

end;

end;
dsApp.Next;
end;

if mnprog>0 then
begin
mnprog:=mnprog+2;
SetPosition(mnprog,100);
writeLn('Unregistering...');

dsApp.ExecuteDirect('DELETE FROM AppInfo WHERE rowid='+IntToStr(dsApp.RecNo));
dsApp.ApplyUpdates;
dsApp.Close;
dsApp.Free;
writeLN('Database connection closed.');

proc:=TProcess.Create(nil);
proc.Options:=[poWaitOnExit];
proc.CommandLine :='rm -rf '+''''+ExcludeTrailingBackslash(p)+'''';
proc.Execute;
proc.Free;

mnprog:=mnprog+2;
SetPosition(mnprog,100);

Log.Add('Application removed.');
Log.Add('-----------');
writeLn('- Finished -');
end else writeLn('Application not found!');

end;

function CheckApps(report: TStringList;const fix: Boolean=false;const forceroot: Boolean=false): Boolean;
var dsApp: TSQLite3Dataset;deps: TStringList;i: Integer;pkit: TPackageKit;
begin
writeLn('Checking dependencies of all registered applications...');
if IsRoot then
writeLn('You are scanning only the ROOT installed applications.')
else
writeLn('You are scanning your local installed applications.');

writeLn('-> Opening database...');
dsApp:= TSQLite3Dataset.Create(nil);
with dsApp do
 begin
   FileName:=RegDir+'applications.db';
   TableName:='AppInfo';
   if not FileExists(FileName) then
   begin
   with FieldDefs do
     begin
       Clear;
       Add('Name',ftString,0,true);
       Add('ID',ftString,0,true);
       Add('Type',ftString,0,true);
       Add('Description',ftString,0,False);
       Add('Version',ftFloat,0,true);
       Add('Publisher',ftString,0,False);
       Add('Icon',ftString,0,False);
       Add('Profile',ftString,0,False);
       Add('AGroup',ftString,0,true);
       Add('InstallDate',ftDateTime,0,False);
       Add('Dependencies',ftMemo,0,False);
     end;
   CreateTable;
 end;
end;
dsApp.Active:=true;

Result:=true;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered:=true;
deps:=TStringList.Create;
pkit:=TPackageKit.Create;
dsApp.First;
while not dsApp.EOF do
begin
 writeLn(' Checking '+dsApp.FieldByName('Name').AsString);
 deps.Text:=dsApp.FieldByName('Dependencies').AsString;
 for i:=0 to deps.Count-1 do
 begin
  if pkit.IsInstalled(deps[i]) then
   report.Add(deps[i]+' found.')
  else
  begin
   report.Add(deps[i]+' is not installed!');
   Result:=false;
   if fix then
   begin
    write('  Repairing dependency '+deps[i]+'  ');
    pkit.InstallPkg(deps[i]);
    writeLn(' [OK]');
    report.Add('Installed dependency '+deps[i]);
   end;
  end;
 end;
 dsApp.Next;
end;
deps.Free;
pkit.Free;
dsApp.Close;
writeLn('Check finished.');
if not Result then writeLn('You have broken dependencies.');

end;

initialization
 if IsRoot then
  RegDir:='/etc/lipa/app-reg/'
  else
  RegDir:=SyblToPath('$INST')+'/app-reg/';
end.

