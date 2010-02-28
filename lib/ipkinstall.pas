{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This library is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Functions to install IPK packages
unit ipkinstall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Process, LiCommon, trStrings, PackageKit,
  sqlite3ds, IPKPackage, ipkdef, HTTPSend, FTPSend, blcksock,
  MD5, liTypes, distri, MTProcs, FileUtil, liBasic, liDBusproc,
  liManageApp, RegExpr;

type

 //** Pointer to TInstallation
 PInstallation = ^TInstallation;
 //** Everything which is needed for an installation

 { TInstallation }

 TInstallation = class
   procedure pkgProgress(pos: Integer; user_data: Pointer);
 private
  //Basic information about the package and the new application
  IAppName,IAppVersion, IAppCMD, IAuthor, ShDesc, IGroup: String;
  IDesktopFiles: String;
  PkgName, PkgPath, PkgID: String;
  //Path to a wizard image
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
  IIconPath: String;
  //Execute external applications that are linked in the IPK-file
  ExecA, ExecB, ExecX: String;
  //Overwrite all files? (needed for patches)
  FOverwrite: Boolean;
  // True if IPK package is a patch
  FPatch: Boolean;
  //Current setup type
  pkType: TPkgType;
  //Disallow-line (every action the package disallows, unformated)
  FDisallow: String;
  //Only set if old package should be removed
  RmApp: Boolean;
  //All supported distribution of this package
  FSupportedDistris: String;
  //HTTP protocol connection
  HTTP: THTTPSend;
  //FTP protocol connection
  FTP: TFTPSend;
  //Container for description
  longdesc: TStringList;
  //Container for license
  license: TStringList;
  //Progress message relais
  FStatusChange: TLiStatusChangeCall;
  FRequest: TRequestCall;
  //Data which contains the status of the current action
  StatusData: TLiStatusData;
  // True if su mode enabled
  SUMode: Boolean;
  // Path to package registration
  RegDir: String;
  //All the things the user forces
  Forces: String;
  //True, if update source should be set
  AddUpdateSource: Boolean;
  //Level of pkg signature
  sigState: TPkgSigState;

  //Set superuser mode correctly
  procedure SetRootMode(b: Boolean);
  //Executed if Linstallation should be done
  function RunNormalInstallation: Boolean;
  //Runs a DLink installation
  function RunDLinkInstallation: Boolean;
  //Does the container installation
  function RunContainerInstallation: Boolean;
  //Socket hook for FTPSend and HTTPSend to get progress
  procedure NetSockHook(Sender: TObject; Reason: THookSocketReason;
     const Value: string);
  //Handler for PackageKit progress
  procedure OnPKitProgress(pos: Integer;dp: Pointer);
  //Add update source of the package
  procedure CheckAddUSource;
  //Catch signals of DBus thread
  procedure DBusThreadStatusChange(ty: LiProcStatus;data: TLiProcData);
 protected
  //UserData
  requestudata: Pointer;
  statechangeudata: Pointer;
  //Check if FTP connection is working
  function CheckFTPConnection(AFTPSend: TFTPSend): Boolean;
  //Set/Get methods for callbacks indication
  procedure SetMainPos(pos: integer);
  procedure SetExtraPos(pos: integer);
  function  MakeUsrRequest(msg: String;qtype: TRqType): TRqResult;
  procedure SendStateMsg(msg: String);
  procedure msg(str: String);
 public
  //** Constructor
  constructor Create;
  //** Destructor
  destructor Destroy; override;
  //** Loads an IPK package @param fname Path to IPK file
  function Initialize(fname: String): Boolean;
  {** Executes the installation
      @returns Sucess of operation}
  function DoInstallation: Boolean;
  //** Gets the maximal steps the installation needs
  function GetMaxInstSteps: Integer;
  //** Function to solve all dependencies on libraries the package has
  function ResolveDependencies: Boolean;
  //** Name of the application
  property AppName: String read IAppName;
  //** Version of the to-be-installed application
  property AppVersion: String read IAppVersion;
  //** ID of the application
  property ID: String read PkgID;
  //** Listaller package type
  property pType: TPkgType read pkType;
  //** Unformatted disallows string
  property Disallows: String read FDisallow;
  //** All distributions the package supports
  property Distris: String read FSupportedDistris;
  //** Path to an wizard image
  property WizImage: String read FWizImage;
  //** List of all profiles the package has
  procedure ReadProfiles(lst: TStringList);
  //** Path to an icon of the applications/setup
  property AppIcon: String read IIconPath;
  //** All registerd .desktop files
  property DesktopFiles: String read IDesktopFiles;
  //** List of all dependencies
  property ADeps: TStringList read Dependencies write Dependencies;
  //** Commend line to execute the new application
  property CMDLn: String read IAppCMD;
  //** Path to the current file information (that fits the profile)
  property IFileInfo: String read FFileInfo;
  //** Level of the package signature
  property SignatureInfo: TPkgSigState read sigState;
  //** Set current profile by ID
  procedure SetCurProfile(i: Integer);
  //** Read the package description
  procedure ReadDescription(sl: TStringList);
  //** Read the package license
  procedure ReadLicense(sl: TStringList);
  //** True on installation with superuser rights
  property SuperuserMode: Boolean read SUMode write SetRootMode;
  //** Set forces identifier string
  property ForceActions: String read Forces write Forces;
  //** True if update source will be registered
  property RegisterUpdateSource: Boolean read AddUpdateSource write AddUpdateSource;
  //** Register event to catch status messages
  procedure RegOnStatusChange(call: TLiStatusChangeCall;data: Pointer);
  //Message events
  procedure RegOnUsrRequest(call: TRequestCall;data: Pointer);
  function  UserRequestRegistered: Boolean;
end;

 //Note: The "Testmode" variable is now in LiCommon

const
  DEPIGNORE_LIST='/etc/lipa/ignore-deps.list';

implementation

procedure TInstallation.RegOnStatusChange(call: TLiStatusChangeCall;data: Pointer);
begin
 FStatusChange:=call;
 statechangeudata:=data;
end;

procedure TInstallation.RegOnUsrRequest(call: TRequestCall;data: Pointer);
begin
  FRequest:=call;
  requestudata:=data;
end;

function TInstallation.UserRequestRegistered: boolean;
begin
 if Assigned(FRequest) then Result:=true else Result:=false;
end;

procedure TInstallation.SetMainPos(pos: integer);
begin
 statusdata.mnprogress:=pos;
 if Assigned(FStatusChange) then FStatusChange(scMnProgress,statusdata,statechangeudata);
end;

procedure TInstallation.SetExtraPos(pos: integer);
begin
 statusdata.exprogress:=pos;
 if Assigned(FStatusChange) then FStatusChange(scExProgress,statusdata,statechangeudata);
end;

function TInstallation.MakeUsrRequest(msg: String;qtype: TRqType): TRqResult;
begin
 if Assigned(FRequest) then
  Result:=FRequest(qtype,PChar(msg),requestudata)
 else p_warning('No user request handler assigned!');
end;

procedure TInstallation.SendStateMsg(msg: String);
begin
 statusdata.msg:=PChar(msg);
 if Assigned(FStatusChange) then FStatusChange(scStepMessage,statusdata,statechangeudata);
end;

procedure TInstallation.msg(str: String);
begin
 statusdata.msg:=PChar(str);
 if Assigned(FStatusChange) then FStatusChange(scMessage,statusdata,statechangeudata)
 else p_info(str);
end;

constructor TInstallation.Create;
begin
 inherited Create;
 msg(rsOpeningDB);
 dsApp:= TSQLite3Dataset.Create(nil);
 if SUMode then
  RegDir:='/etc/lipa/app-reg/'
 else
  RegDir:=SyblToPath('$INST')+'/app-reg/';

  //Create text containers
  license:=TStringList.Create;
  longdesc:=TStringList.Create;

  //Init
  AddUpdateSource:=false;
end;

destructor TInstallation.Destroy;
begin
 DeleteDirectory(tmpdir+PkgName,false);
 inherited Destroy;
 dsApp.Free;
 msg('Database connection closed.');
 if Assigned(Dependencies) then Dependencies.Free;
 license.Free;
 longdesc.Free;
end;

procedure TInstallation.pkgProgress(pos: Integer; user_data: Pointer);
begin
  SetExtraPos(pos);
end;

procedure TInstallation.SetRootMode(b: Boolean);
begin
 SUMode:=b;
 if SUMode then
  RegDir:='/etc/lipa/app-reg/'
 else
  RegDir:=SyblToPath('$INST')+'/app-reg/';
end;

procedure TInstallation.SetCurProfile(i: Integer);
begin
 FFileInfo:='/pkgdata/fileinfo-'+IntToStr(i)+'.id';
 CurProfile:=IntToStr(i);
end;

procedure TInstallation.ReadProfiles(lst: TStringList);
var i: Integer;
begin
 lst.Clear;
 for i:=0 to PkProfiles.Count-1 do
  lst.Add(copy(PkProfiles[i],0,pos(' #',PkProfiles[i])));
end;

function IsAppInstalled(aname: String): Boolean;
var dsApp: TSQLite3Dataset;
begin
dsApp:=TSQLite3Dataset.Create(nil);

LoadAppDB(dsApp);

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered := true;
dsApp.First;

Result:=false;
while not dsApp.EOF do
begin
 if (dsApp.FieldByName('Name').AsString=aName) then
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

procedure TInstallation.ReadDescription(sl: TStringList);
var i: Integer;
begin
 //Asign does not work
 sl.Clear;
 for i:=0 to longdesc.Count-1 do
  sl.Add(longdesc[i]);
end;

procedure TInstallation.ReadLicense(sl: TStringList);
var i: Integer;
begin
//Assign does not work
 sl.Clear;
 for i:=0 to license.Count-1 do
  sl.Add(license[i]);
end;

function TInstallation.ResolveDependencies: Boolean;
var i,j: Integer;
    tmp: TStringList;
    mnpos: Integer;
    DInfo: TDistroInfo;
    one: Double;
    lInd: Integer;
    error: Boolean;
    eIndex: Integer; //Index of the errorneus dep

procedure SearchForPackage(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var pkit: TPackageKit;xtmp: TStringList;h: String;lpos: Integer;
begin
if Index=-1 then
begin
lpos:=0;
lind:=0;
while lInd<Dependencies.Count-1 do
begin
 if lpos<>mnpos then
 begin
  SetExtraPos(Round(mnpos*one));
  lpos:=mnpos;
 end;
 if error then
 begin
  ProcThreadPool.StopThreads;
  break;
 end;
end;

end else
begin

  if error then exit;
 //Open PKit connection and assign tmp stringlist
  pkit:=TPackageKit.Create;
  xtmp:=TStringList.Create;
  pkit.RsList:=xtmp;

  mnpos:=mnpos+1;

  pkit.FindPkgForFile(Dependencies[Index]);

  if (pkit.PkFinishCode=1)or(xtmp.Count<=0) then
  begin
   pkit.Free;
   xtmp.Free;
   eIndex:=Index;
   error:=true;
   exit;
  end;

  h:=xtmp[0];
  tmp.Add(h);
  h:='';
  xtmp.Clear;
  mnpos:=mnpos+1;
  lInd:=Index;

  pkit.Free;
  xtmp.free;
 end;
end;

begin
 Result:=true;
 SetExtraPos(1);
 error:=false;

 p_debug('Resolving dependencies.');
  if (Dependencies.Count>0) and (Dependencies[0]='[detectpkgs]') then
  begin
    Dependencies.Delete(0);

    //Preprepare dependencies
     //Remove all distribution core deps
    if FileExists(DEPIGNORE_LIST) then
    begin
     tmp:=TStringList.Create;
     tmp.LoadFromFile(DEPIGNORE_LIST);
     i:=0;
     while i<=Dependencies.Count-1 do
     begin
      for j:=0 to tmp.Count-1 do
      begin
       try
        if ExecRegExpr(tmp[j],Dependencies[i]) then
        begin
         Dependencies.Delete(i);
         Dec(i);
         break;
        end;
      except
       p_error('Malformed dependency-ignore entry "'+tmp[j]+'"! (Please fix this.)');
       tmp.Delete(j);
       break;
      end;
      end;
       Inc(i);
     end;
     i:=0;
     tmp.Free;
    end;

    DInfo:=GetDistro;
    //With yum backend, PackageKit needs the complete path to find
    //packages. Add the lib dir to libraries
    if DInfo.PackageSystem<>'DEB' then
     for i:=0 to Dependencies.Count-1 do
      if  (LowerCase(ExtractFileExt(Dependencies[i]))='.so')
      and (Dependencies[i][1]<>'/') then
       Dependencies[i]:='/usr/lib/'+Dependencies[i];

    mnpos:=0;
    one:=100/(Dependencies.Count*2);
    SetExtraPos(Round(mnpos*one));
    ShowPKMon();

    tmp:=TStringList.Create;
    // ProcThreadPool.MaxThreadCount:=4;

    p_debug('Start resolve threads.');
     ProcThreadPool.DoParallelLocalProc(@SearchForPackage,-1,Dependencies.Count-1,nil);

    if error then
    begin
     MakeUsrRequest(StringReplace(rsDepNotFound,'%l',Dependencies[eIndex],[rfReplaceAll])+#10+rsInClose,rqError);
     Result:=false;
     tmp.Free;
     exit;
    end;

    RemoveDuplicates(tmp);
    Dependencies.Assign(tmp);

    tmp.Free;
    SetExtraPos(100);
    p_debug('DepResolve finished.');
  end;
end;

function TInstallation.Initialize(fname: String): Boolean;
var pkg: TLiUnpacker;
    n: String;
    i: Integer;
    DInfo: TDistroInfo;
    cont: TIPKControl;

procedure Emergency_FreeAll();
begin
 if pkg is TLiUnpacker then pkg.Free;
 if cont is TIPKControl then cont.Free;
end;

begin
//Create temporary dir
if not DirectoryExists(tmpdir) then
 ForceDirectory(tmpdir);

//The user _really_ is root. (This happens, if the daemon executes this action)
//Set the right paths and execute install
if IsRoot then
begin
SUMode:=true;

if SUMode then
  RegDir:='/etc/lipa/app-reg/'
 else
  RegDir:=SyblToPath('$INST')+'/app-reg/';
end;

if IAppName <> '' then
begin
 Result:=false;
 p_error('This Setup was already initialized. You have to create a new setup object to load a second file!');
 exit;
end;
 Result:=true;

 LoadAppDB(dsApp);

dsApp.Active:=true;
msg('SQLite version: '+dsApp.SqliteVersion);

msg('Loading IPK package...');
//Begin loading package
RmApp:=false;

//Clean up possible mess of an old installation
if DirectoryExists(tmpdir+ExtractFileName(fname)) then
 DeleteDirectory(tmpdir+ExtractFileName(fname),false);

pkg:=TLiUnpacker.Create(fname);
if not SUMode then
begin
 pkg.OnProgress:=@pkgProgress;
 //Uncompress LZMA
 pkg.Decompress;
end else
if not FileExists(pkg.WDir+'ipktar.tar') then
begin
 pkg.OnProgress:=@pkgProgress;
 //Uncompress LZMA
 pkg.Decompress;
end;

sigState:=pkg.CheckSignature;
case sigState of
psNone: msg('Package is unsigned.');
psTrusted: msg('Package has trusted signature.');
psUntrusted: msg('Package signature is UNTRUSTED!');
end;

if not pkg.UnpackFile('arcinfo.pin') then
begin
Emergency_FreeAll();
MakeUsrRequest(rsExtractError+#13+rsPkgDM+#13+rsABLoad,rqError);
Result:=false;
exit;
end;

PkgName:=ExtractFileName(fname);
PkgPath:=fname;

cont:=TIPKControl.Create(pkg.WDir+'/arcinfo.pin'); //Read IPK configuration

cont.LangCode:=GetLangID; //Set language code, so we get localized entries

pkType:=cont.SType;

//!!! Support for patches was dropped in IPK1.0
{
if (xnode.Attributes.GetNamedItem('patch')<>nil)
and(xnode.Attributes.GetNamedItem('patch').NodeValue='true')
then
begin
FPatch:=true;
msg('WARNING: This package patches another application on your machine!');
end else FPatch:=false;
}

FDisallow:=LowerCase(cont.Disallows);

n:=GetSystemArchitecture;

msg('Package-Archs: '+cont.Architectures);
if (pos(n,LowerCase(cont.Architectures))<=0)
and (LowerCase(cont.Architectures)<>'all')
and (pos('architecture',forces)<=0) then
begin
 MakeUsrRequest(rsInvArchitecture,rqError);
 cont.Free;
 pkg.Free;
 Result:=false;
 exit;
end;

if pkType=ptLinstall then
begin

msg('Package type is "linstall"');

if (pos('iofilecheck',FDisallow)>0) then begin
FOverwrite:=true;
msg('WARNING: This package will overwrite existing files without request!');
end else FOverwrite:=false;

pkgID:='';
pkgID:=cont.PkName;
msg('Package idName: '+pkgID);

//Find profiles
i:=1;
PkProfiles:=TStringList.Create;
cont.ReadProfiles(PkProfiles);

for i:=0 to PkProfiles.Count-1 do
begin
msg('Found installation profile '+PkProfiles[PkProfiles.Count-1]);
pkg.UnpackFile('pkgdata/fileinfo-'+IntToStr(i)+'.id');
end;

IAppName:=cont.AppName;

if pkgID='' then
pkgID:=IAppName;

IAppVersion:=cont.AppVersion;

USource:=cont.USource;
if USource='' then
USource:='#';

IAuthor:=cont.Author;
if IAuthor='' then
IAuthor:='#';

IAppCMD:=cont.AppCMD;
if IAppCMD='' then
IAppCMD:='#';

if IAppCMD <> '#' then
msg('Application main exec command is '+IAppCMD);


//Load Description
ShDesc:=cont.SDesc;

case cont.Group of
 gtALL: IGroup:='All';
 gtEDUCATION: IGroup:='Education';
 gtOFFICE: IGroup:='Office';
 gtDEVELOPMENT: IGroup:='Development';
 gtGRAPHIC: IGroup:='Graphic';
 gtNETWORK: IGroup:='Network';
 gtGAMES: IGroup:='Games';
 gtSYSTEM: IGroup:='System';
 gtMULTIMEDIA: IGroup:='Multimedia';
 gtADDITIONAL: IGroup:='Additional';
 gtOTHER: IGroup:='Other';
end;

IIconPath:=cont.Icon;
if IIconPath<>'' then
begin
pkg.UnpackFile(IIconPath);
IIconPath:=pkg.WDir+IIconPath;
end;

//Load info about supported distributions
FSupportedDistris:=LowerCase(cont.DSupport);


//Load Wizard-Image
FWizImage:=GetDataFile('graphics/wizardimage.png');
if (FWizImage<>'') and (FWizImage[1] = '/') then
begin
 pkg.UnpackFile(FWizImage);
 if FileExists(pkg.WDir+FWizImage) then
 FWizImage:=pkg.WDir+FWizImage;
end;

pkg.UnpackFile('preinst');
pkg.UnpackFile('postinst');
pkg.UnpackFile('prerm');
ExecA:='<disabled>';
ExecB:='<disabled>';
ExecX:='<disabled>';
if FileExists(pkg.WDir+'/preinst') then ExecA:=pkg.WDir+'/preinst';
if FileExists(pkg.WDir+'/postinst') then ExecB:=pkg.WDir+'/postinst';
if FileExists(pkg.WDir+'/prerm') then ExecX:=pkg.WDir+'/prerm';

//Load Description
cont.ReadAppDescription(longdesc);

//Load License
cont.ReadAppLicense(license);

//Load Dependencies
Dependencies:=TStringList.Create;

DInfo:=GetDistro;

cont.ReadDependencies('',dependencies);

i:=0;
while i<= Dependencies.Count-1 do
begin
 if StringReplace(Dependencies[i],' ','',[rfReplaceAll])='' then Dependencies.Delete(i);
 Inc(i);
end;

if dependencies.Count>0 then
 dependencies.Insert(0,'[detectpkgs]') //Instruction to detect packages first
else
begin
 cont.ReadDependencies(DInfo.DName,dependencies);
if dependencies.Count<=0 then
begin
 cont.ReadDependencies(DInfo.PackageSystem,dependencies);

 if MakeUsrRequest(rsInvalidDVersion,rqWarning) = rqsNo then
 begin
  MakeUsrRequest(rsNoComp+#13+rsInClose,rqError);
  Dependencies.Free;
  Emergency_FreeAll();
  PkProfiles.Free;
  Result:=false;
  exit;
 end;
 end;

//Resolve names
for i:=0 to dependencies.Count-1 do
if pos(' <',n)>0 then
Dependencies[i]:=copy(n,pos(' <',n)+2,length(n)-pos(' <',n)-2)+' - '+copy(n,1,pos(' <',n)-1);
end;


{ if xnode.FindNode('pkcatalog')<>nil then
 begin
  z.ExtractFiles(ExtractFileName(xnode.FindNode('pkcatalog').NodeValue));
  Dependencies.Add('cat:'+lp+PkgName+xnode.FindNode('pkcatalog').NodeValue)
 end;}

for i:=0 to Dependencies.Count-1 do
if Dependencies[i][1]='.' then pkg.UnpackFile(Dependencies[i]);

pkg.Free;

msg('Profiles count is '+IntToStr(PkProfiles.Count));
if PkProfiles.Count<0 then
begin
Emergency_FreeAll();
MakeUsrRequest(rsPkgInval+#13'Message: No profiles and no file list found!',rqError);
PkProfiles.Free;
Result:=false;
exit;
end;

if PkProfiles.Count=1 then FFileInfo:='/pkgdata/fileinfo-'+copy(PkProfiles[0],pos(' #',PkProfiles[0])+2,length(PkProfiles[0]))+'.id'
else FFileInfo:='*';

SetCurProfile(0);

//Only executed to make sure that "RmApp" property is set

if not Testmode then
if IsAppInstalled(IAppName) then RmApp:=true;

end else //Handle other IPK types
if pkType=ptDLink then begin
msg('Package type is "dlink"');

cont.ReadAppDescription(longdesc);

IAppName:=cont.AppName;
IAppVersion:=cont.AppVersion;

IIconPath:=cont.Icon;
if IIconPath<>'' then
begin
pkg.UnpackFile(IIconPath);
IIconPath:=pkg.WDir+ExtractFileName(IIconPath);
end;

IAuthor:=cont.Author;
if IAuthor='' then
IAuthor:='#';

//Set group as string
case cont.Group of
 gtALL: IGroup:='All';
 gtEDUCATION: IGroup:='Education';
 gtOFFICE: IGroup:='Office';
 gtDEVELOPMENT: IGroup:='Development';
 gtGRAPHIC: IGroup:='Graphic';
 gtNETWORK: IGroup:='Network';
 gtGAMES: IGroup:='Games';
 gtSYSTEM: IGroup:='System';
 gtMULTIMEDIA: IGroup:='Multimedia';
 gtADDITIONAL: IGroup:='Additional';
 gtOTHER: IGroup:='Other';
end;

IDesktopFiles:=cont.Desktopfiles;

pkg.Free;

//Load Description
ShDesc:=cont.SDesc;

Dependencies:=TStringList.Create;

cont.ReadDependencies('',dependencies);
if dependencies.Count>0 then
 dependencies.Insert(0,'[detectpkgs]') //Instruction to detect packages first
else
begin
 cont.ReadDependencies(DInfo.DName,dependencies);
if dependencies.Count<=0 then
begin
 cont.ReadDependencies(DInfo.PackageSystem,dependencies);

 if MakeUsrRequest(rsInvalidDVersion,rqWarning) = rqsNo then
 begin
  MakeUsrRequest(rsNoComp+#13+rsInClose,rqError);
  Dependencies.Free;
  pkg.Free;
  PkProfiles.Free;
  Result:=false;
  exit;
 end;
 end;

//Resolve names
for i:=0 to dependencies.Count-1 do
if pos(' <',n)>0 then
Dependencies[i]:=copy(n,pos(' <',n)+2,length(n)-pos(' <',n)-2)+' - '+copy(n,1,pos(' <',n)-1);
end;

pkg.Free;

end else
if pkType=ptContainer then
begin
msg('Package type is "container"');

//IPK package has been initialized
end;
cont.Free;
end;

function TInstallation.GetMaxInstSteps: Integer;
var fi: TStringList;i,j: Integer;
begin
Result:=0;
j:=0;
fi:=TStringList.Create;
fi.LoadFromFile(tmpdir+ExtractFileName(PkgName)+'/'+FFileInfo);
for i:=0 to fi.Count-1 do
 if fi[i][1]<>'>' then Inc(j);
Result:=((Dependencies.Count+(j div 2))*10)+16;
fi.Free;
end;

procedure TInstallation.NetSockHook(Sender: TObject; Reason: THookSocketReason;
  const Value: string);
begin
//HTTP
if Http.DownloadSize>100 then
begin
 SetExtraPos((HTTP.DownloadSize div 100)*HTTP.Document.Size);
 exit;
end;
//FTP
SetExtraPos(50); //??? Cannot detect file size at time
end;

function TInstallation.CheckFTPConnection(AFtpSend: TFtpSend): Boolean;
begin
  if AFtpSend.Sock.Socket = not(0) then
    Result := AFtpSend.Login
  else
    if AFtpSend.NoOp then
      Result := true
    else
      Result := AFtpSend.Login;
end;

procedure TInstallation.OnPKitProgress(pos: Integer;dp: Pointer);
begin
  //user_data Pointer is always nil
  SetExtraPos(pos); //Set position of extra progress to PackageKit transaction progress
end;

function TInstallation.RunContainerInstallation: Boolean;
var pkg: TLiUnpacker;
    cont: TIPKControl;
    p: TProcess;
    tmp: TStringList;
    i: Integer;
    WDir: String;
begin

Result:=true;
try
pkg:=TLiUnpacker.Create(PkgPath);
cont:=TIPKControl.Create(pkg.WDir+'/arcinfo.pin'); //Read IPK configuration
cont.LangCode:=GetLangID; //Set language code, so we get localized entries

pkg.UnpackFile(cont.Binary);

tmp:=TStringList.Create;
cont.GetFiles(0,tmp);
for i:=0 to tmp.Count-1 do
pkg.UnpackFile(tmp[i]);

tmp.Free;
WDir:=pkg.WDir;
pkg.Free;

 p:=TProcess.Create(nil);

 p.CommandLine:='chmod 755 '''+WDir+cont.Binary+'''';
 p.Options:=[poUsePipes,poWaitonexit];
 p.Execute;
 p.Options:=[poUsePipes,poWaitonexit,poNewConsole];
 if LowerCase(ExtractFileExt(cont.Binary))='.package' then begin
 if FileExists('/usr/bin/package') then
 p.Options:=[poUsePipes,poWaitonexit];

 p.CommandLine:=WDir+cont.Binary;
 p.Execute;
 p.Free;
 end else
 begin
 if cont.InTerminal then
 p.Options:=[poUsePipes, poWaitOnExit, poNewConsole]
 else p.Options:=[poUsePipes, poWaitOnExit];
 p.CommandLine:=WDir+cont.Binary;
 p.Execute;
 p.Free;
 end;

DeleteFile(WDir+cont.Binary);
except
 Result:=false;
end;
end;


procedure TInstallation.DBusThreadStatusChange(ty: LiProcStatus;data: TLiProcData);
begin
  case data.changed of
    pdMainProgress: SetMainPos(data.mnprogress);
    pdInfo: msg(data.msg);
    pdError: MakeUsrRequest(data.msg,rqError);
    pdStatus: p_debug('Thread status changed [finished]');
  end;
end;

function TInstallation.RunNormalInstallation: Boolean;
var
i,j: Integer;
fi,ndirs, s, appfiles: TStringList;
dest,h, FDir: String; // h is an helper variable - used for various actions
dsk, cnf: TIniFile; // Configuration files etc.
pkg: TLiUnpacker; // IPK decompressor
setcm: Boolean;
p, proc: TProcess; // Helper process with pipes
pkit: TPackageKit; //PackageKit object
DInfo: TDistroInfo; //Distribution information
mnpos: Integer; //Current positions of operation
max: Double;

//Necessary if e.g. file copying fails
procedure RollbackInstallation;
var i: Integer;
begin
 for i:=0 to appfiles.Count-1 do
 begin
  msg('Removing '+DeleteModifiers(appfiles[i])+' ...');
  DeleteFile(DeleteModifiers(appfiles[i]));
  mnpos:=mnpos-10;
  SetMainPos(Round(mnpos*max));
 end;
 for i:=0 to ndirs.Count-1 do
 begin
  msg('Removing '+ndirs[i]);
  try
   DeleteDirectory(ndirs[i],false);
  except
   p_error('Caould not remove directory. Please report this bug.');
  end;
 end;
 ndirs.Free;
 appfiles.Free;
end;

procedure Abort_FreeAll();
begin
try
 if Assigned(fi) then fi.Free;
 if Assigned(ndirs) then ndirs.Free;
 if Assigned(s) then s.Free;
 if Assigned(appfiles) then appfiles.Free;
 if Assigned(dsk) then dsk.Free;
 if Assigned(cnf) then cnf.Free;
 if Assigned(pkg) then pkg.Free;
 if Assigned(p) then p.Free;
 if Assigned(proc) then proc.Free;
 if Assigned(pkit) then pkit.Free;
except
 p_error('Error while cleaning up.');
end;
end;

begin
//First resolve all dependencies and prepare installation
ResolveDependencies();
if not FileExists(tmpdir+ExtractFileName(PkgName)+'/'+FFileInfo) then
begin
  MakeUsrRequest(rsInstFailed,rqError);
  p_error('No file information found!');
  p_error('IPK package seems to be broken.');
  Result:=false;
  Abort_FreeAll();
  exit;
end;

fi:=TStringList.Create;
fi.LoadFromFile(tmpdir+ExtractFileName(PkgName)+'/'+FFileInfo);

proc:=TProcess.Create(nil);
proc.Options:=[poUsePipes,poStdErrToOutPut];

DInfo:=GetDistro; //Load DistroInfo

Result:=false;
mnpos:=0;

//!!! @deprecated
{
//Check if fileinfo contains shared files
for i:=0 to (fi.Count div 3)-1 do begin
if IsSharedFile(lp+PkgName+fi[i*3]) then ContSFiles:=true;
end; }

max:=100/GetMaxInstSteps;

SetMainPos(Round(max*mnpos));

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

SetExtraPos(0);
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
pkit.OnProgress:=@OnPKitProgress;

if Dependencies.Count>0 then
begin
//Check if we should install a software catalog
if pos('cat:',Dependencies[0])>0 then
begin
 msg('Installing package catalog...');
 pkit.InstallLocalPkg(copy(Dependencies[0],5,length(Dependencies[0])));

 if pkit.PkFinishCode=1 then
 begin
  MakeUsrRequest(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+IAppName+'.log',[rfReplaceAll]),rqError);
  Result:=false;
  Abort_FreeAll();
  exit;
 end;
 SetExtraPos(0);

 Dependencies[0]:='cat:'+ExtractFileName(copy(Dependencies[0],5,length(Dependencies[0])));

end else
begin
for I:=0 to Dependencies.Count-1 do
begin  //Download & install dependencies
if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
begin
    msg(rsGetDependencyFrom+' '+Dependencies[i]+'.');
    msg(rsPlWait2);
 if pos('http://',LowerCase(Dependencies[i]))>0 then
 begin
  try
    HTTP.HTTPMethod('GET', copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    HTTP.Document.SaveToFile('/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));
  except
   MakeUsrRequest(rsDepDLProblem,rqError);
   Result:=false;
   Abort_FreeAll();
   exit;
  end;
 end else
 begin

with FTP do
begin
    TargetHost := GetServerName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));

  try
    DirectFileName := '/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1));
    DirectFile:=True;
    if not CheckFTPConnection(FTP) then
    begin
      MakeUsrRequest(rsFTPFailed,rqError);
      Result:=false;
      Abort_FreeAll();
      exit;
    end;
    ChangeWorkingDir(GetServerPath(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)));

    RetrieveFile(ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i])-1)), false);
    Logout;
  except
   MakeUsrRequest(rsDepDLProblem,rqError);
   Result:=false;
   Abort_FreeAll();
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

msg('Done.');
end;

SetExtraPos(0);
sleep(18); //Wait...

if (Dependencies[i][1]='/')or(pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
begin

    msg('DepInstall: '+Dependencies[i]+' (using PackageKit +x)');
    msg('');

  if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
  begin
   s:=TStringList.Create;
   pkit.RsList:=s;
   pkit.Resolve(copy(Dependencies[i],pos(' <',Dependencies[i])+2,length(Dependencies[i])-1));
    if pkit.PkFinishCode=1 then
    begin
     pkit.InstallLocalPkg('/tmp/'+ExtractFileName(copy(Dependencies[i],1,pos(' <',Dependencies[i]))));

     SetExtraPos(0);
    end;
   s.Free;
  end else
    pkit.InstallLocalPkg(tmpdir+ExtractFileName(PkgName)+'/'+Dependencies[i]);

    mnpos:=mnpos+5;
    SetMainPos(Round(mnpos*max));

    //Check if the package was really installed
    if pkit.PkFinishCode=1 then
    begin
     MakeUsrRequest(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+IAppName+'.log',[rfReplaceAll]),rqError);
     Result:=false;
     Abort_FreeAll();
     exit;
    end;

    mnpos:=mnpos+5;
    SetMainPos(Round(mnpos*max));

 //If only a file name given install them with distri-tool
end else
begin
     msg('DepInstall: '+Dependencies[i]+' (using PackageKit)');
     msg('');

     //pkit.Resolve(Dependencies[i]);

     if pkit.PkFinishCode=1 then
     begin
      pkit.InstallPkg(Dependencies[i]);

    //Check if the package was really installed
   if pkit.PkFinishCode=1 then
    begin
     msg('Package '+Dependencies[i]+' can not be installed.');
     MakeUsrRequest(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+IAppName+'.log',[rfReplaceAll]),rqError);
     Result:=false;
     Abort_FreeAll();
     exit;
    end;

    end; //E-of > 0

    mnpos:=mnpos+10;
    SetMainPos(Round(mnpos*max));
  end;
   end; //End of PKCatalog end-else
  end; //End of Dependecies.Count term
end; //End of dependency-check

pkit.Free;

SetExtraPos(0);

//Delete old application installation if necessary
if (RmApp)and(not Testmode) then
begin
//Uninstall old application
 UnInstallIPKApp(IAppName,pkgID,FStatusChange,true);
 SetExtraPos(0);
end;

appfiles:=TStringList.Create;
SendStateMsg(rsStep2);
pkg:=TLiUnpacker.Create(PkgPath);

ndirs:=TStringList.Create;
j:=0;
if not DirectoryExists(SyblToPath('$INST')) then SysUtils.CreateDir(SyblToPath('$INST'));


for i:=0 to fi.Count-1 do
begin

if (pos(' <'+LowerCase(DInfo.DName)+'-only>',LowerCase(fi[i]))>0)
or (pos('-only',LowerCase(fi[i]))<=0) then
begin

//Set new target directory on >
if fi[i][1]='>' then
begin
 dest:=copy(fi[i],2,length(fi[i]));
 dest:=SyblToPath(dest);
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

end else
if (fi[i][1]='.')or(fi[i][1]='/') then
begin

//h is now used for the file-path
h:=DeleteModifiers(fi[i]);

FDir:=pkg.WDir+ExtractFileName(PkgPath)+'/';
if not DirectoryExists(FDir) then
CreateDir(FDIR);

if not pkg.UnpackFile(h) then
begin
 MakeUsrRequest(rsExtractError,rqError);
 RollbackInstallation;
 Result:=false;
 Abort_FreeAll();
 exit;
end;

msg('Copy file '+ExtractFileName(h)+' to '+dest+' ...');

if fi[i+1] <> MDPrint((MD5.MD5File(DeleteModifiers(pkg.WDir+h),1024))) then
begin
 MakeUsrRequest(rsHashError,rqError);
 RollbackInstallation;
 Result:=false;
 Abort_FreeAll();
 exit;
end;

Inc(j);

try
if FOverwrite then
FileCopy(DeleteModifiers(pkg.WDir+h),dest+'/'+ExtractFileName(DeleteModifiers(h)))
else
if (not FileExists(dest+'/'+ExtractFileName(DeleteModifiers(h)))) then
 FileCopy(DeleteModifiers(pkg.WDir+h),dest+'/'+ExtractFileName(DeleteModifiers(h)))
else
begin
  MakeUsrRequest(StringReplace(rsCnOverride,'%f',dest+'/'+ExtractFileName(DeleteModifiers(h)),[rfReplaceAll])+#10+rsInClose,rqError);
  RollbackInstallation;
  Result:=false;
  Abort_FreeAll();
  exit;
end;
except
 //Unable to copy the file
 MakeUsrRequest(Format(rsCnCopy,[dest+'/'+ExtractFileName(DeleteModifiers(h))])+#10+rsInClose,rqError);
 RollbackInstallation;
 Result:=false;
 Abort_FreeAll();
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

//Delete temp file
DeleteFile(DeleteModifiers(pkg.WDir+ExtractFileName(h)));

msg('Okay.');

mnpos:=mnpos+10;
SetMainPos(Round(mnpos*max));

  end;
   end;
end;

SendStateMsg(rsStep3);
//Check if every single file needs its own command to get the required rights
//(It's faster if only every folder recieves the rights)
setcm:=false;
for i:=0 to fi.Count-1 do
if pos(' <chmod:',fi[i])>0 then setcm:=true;


if setcm then
begin

//Set rights per file
for i:=0 to fi.Count-1 do
begin
if i mod 2 = 0 then
begin

if fi[i][1]='>' then
 dest:=SyblToPath(fi[i])
else
begin
h:=fi[i];

if pos(' <chmod:',h)>0 then begin
proc.CommandLine := 'chmod '+copy(h,pos(' <chmod:',h)+8,3)+dest+'/'+ExtractFileName(DeleteModifiers(fi[i]));
proc.Execute;
end else
begin
proc.CommandLine := 'chmod 755 '+dest+'/'+ExtractFileName(DeleteModifiers(fi[i]));
proc.Execute;
end;

 //while proc.Running do Application.ProcessMessages;
msg('Rights assigned to '+DeleteModifiers(ExtractFileName(SyblToPath(fi[i]))));
 end;
end;
end;

end else
begin
//Set rights per folder
for i:=0 to ndirs.Count-1 do
begin
proc.CommandLine := 'chmod 755 -R '+SyblToPath(ndirs[i]);
proc.Execute;
msg('Rights assigned to folder '+ExtractFileName(SyblToPath(ndirs[i])));
 end;
end; //End setcm

mnpos:=mnpos+6;
SetMainPos(Round(mnpos*max));

fi.Free;
SendStateMsg(rsStep4);

if Testmode then
 msg('Testmode: Do not register package.')
else
if not FPatch then
begin

if not DirectoryExists(RegDir+LowerCase(IAppName+'-'+pkgID)) then SysUtils.CreateDir(RegDir+LowerCase(IAppName+'-'+pkgID));
FileCopy(pkg.WDir+'/arcinfo.pin',RegDir+LowerCase(IAppName+'-'+pkgID)+'/proginfo.pin');

//Save list of installed files
appfiles.SaveToFile(RegDir+LowerCase(IAppName+'-'+pkgID)+'/appfiles.list');
appfiles.Free;

//Open database connection
dsApp.Open;
dsApp.Edit;

if pkType=ptLinstall then h:='linstall';
if pkType=ptDLink then h:='dlink';
if pkType=ptContainer then h:='containerF';

dsApp.Insert;
dsApp.ExecuteDirect('INSERT INTO "AppInfo" VALUES ('''+IAppName+''', '''+
        pkgID+''', '''+h+''', '''+ShDesc+''','''+IAppVersion+''','''+IAuthor+''','''+'icon'+ExtractFileExt(IIconPath)+''','''+CurProfile+''','''+
        IGroup+''','''+GetDateAsString+''', '''+Dependencies.Text+''');');

//Write changes
dsApp.ApplyUpdates;
dsApp.Close;

if IIconPath[1]='/' then
FileCopy(pkg.WDir+IIconPath,RegDir+LowerCase(IAppName+'-'+pkgID)+'/icon'+ExtractFileExt(IIconPath));

ndirs.SaveToFile(RegDir+LowerCase(IAppName+'-'+pkgID)+'/appdirs.list');

if ExecX<>'<disabled>' then
FileCopy(ExecX,RegDir+LowerCase(IAppName+'-'+pkgID)+'/prerm');

ndirs.Free;

end; //End of Patch check

mnpos:=mnpos+5;
SetMainPos(Round(mnpos*max));
//Execute Program/Script
if ExecB<>'<disabled>' then begin
    proc.CommandLine := 'chmod 777 '+ExecB;
    Proc.Execute;
 //while Proc.Running do Application.ProcessMessages;
    Proc.CommandLine := ExecB;
    Proc.Execute;
 //while Proc.Running do Application.ProcessMessages;
end;

proc.Free;

pkg.Free;

if (USource<>'#')and(AddUpdateSource) then
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
  fi.Add('- '+USource);
  fi.SaveToFile(RegDir+'updates.list');
end;
 fi.Free;
end;

mnpos:=mnpos+5;
SetMainPos(Round(mnpos*max));

Result:=true;
SendStateMsg(rsFinished);
sleep(600);
end;

function TInstallation.RunDLinkInstallation: Boolean;
var i: Integer;cnf,ar: TIniFile;pkit: TPackageKit;mnpos,max: Integer;

procedure Abort_FreeAll();
begin
 if Assigned(cnf) then cnf.Free;
 if Assigned(ar) then ar.Free;
 if Assigned(pkit) then pkit.Free;
end;

begin
max:=Dependencies.Count*6000;
mnpos:=0;
SetMainPos(Round(mnpos*max));
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
pkit.OnProgress:=@OnPKitProgress;

  for i:=1 to Dependencies.Count-1 do
  begin
  if pos('://',Dependencies[i])<=0 then
  begin
   msg('Looking for '+Dependencies[i]);

   pkit.Resolve(Dependencies[i]);

  if pkit.PkFinishCode=1 then
  begin
   msg('Installing '+Dependencies[i]+'...');

   pkit.InstallPkg(Dependencies[i]);
  end;
  end else
  begin
   msg('Looking for '+copy(Dependencies[i],1,pos(' -',Dependencies[i])-1));
   pkit.Resolve(copy(Dependencies[i],1,pos(' -',Dependencies[i])-1));

  if pkit.PkFinishCode=1 then
  begin
   msg('Downloading package...');

 if pos('http://',LowerCase(Dependencies[i]))>0 then
 begin
  try
   HTTP.HTTPMethod('GET', copy(Dependencies[i],pos(' -',Dependencies[i])+2,length(Dependencies[i])-pos(' -',Dependencies[i])+2));
   HTTP.Document.SaveToFile('/tmp/'+ExtractFileName(Dependencies[i]));
  except
   MakeUsrRequest(rsDepDlProblem,rqError);
   Result:=false;
   Abort_FreeAll();
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

    if not CheckFTPConnection(FTP) then
    begin
      MakeUsrRequest(rsFTPFailed,rqError);
      Result:=false;
      Abort_FreeAll();
      exit;
    end;

    ChangeWorkingDir(GetServerPath(Dependencies[i]));

    //???
    //emax:=FileSize(ExtractFileName(Dependencies[i]));

    SetExtraPos(0);

    RetrieveFile(ExtractFileName(Dependencies[i]), false);
    Logout;
  except
   MakeUsrRequest(rsDepDlProblem,rqError);
   Result:=false;
   Abort_FreeAll();
   exit;
  end;
  end;
end;

  msg('Installing '+copy(Dependencies[i],1,pos(' -',Dependencies[i])-1)+'...');
  pkit.InstallLocalPkg('/tmp/'+ExtractFileName(Dependencies[i]));
end;

  mnpos:=mnpos+6000;
  SetMainPos(Round(mnpos*max));
end;

end;

pkit.Free; //Free PackageKit

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

procedure TInstallation.CheckAddUSource;
var fi: TStringList;i: Integer;
begin
if USource<>'#' then
begin
fi:=TStringList.Create;
fi.LoadFromFile(RegDir+'updates.list');
for i:=1 to fi.Count-1 do
if pos(USource,fi[i])>0 then break;
if i=fi.Count then
begin
if MakeUsrRequest(PAnsiChar(rsAddUpdSrc+#13+
   copy(USource,pos(' <',USource)+2,length(USource)-pos(' <',USource)-2)+' ('+copy(uSource,3,pos(' <',USource)-3)+')'+#13+
   PAnsiChar(rsQAddUpdSrc)),rqWarning)=rqsYes then
 begin
  AddUpdateSource:=true;
 end;
 end;
 fi.Free;
end;
end;

function TInstallation.DoInstallation: Boolean;
var cnf: TIniFile;i: Integer;buscmd: ListallerBusCommand;
begin

if pkType=ptLinstall then
begin
//Set Testmode settings
if (IAppCMD='#')and (Testmode) then
begin
 MakeUsrRequest(rsActionNotPossiblePkg, rqError);
 Result:=false;
 exit;
end;
IAppCMD:=SyblToPath(IAppCMD);

//Check if the package downloads native pkgs
for i:=0 to Dependencies.Count-1 do
if (pos('http://',Dependencies[i])>0)or(pos('ftp://',Dependencies[i])>0) then
begin
cnf:=TInifile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('MainConf','AutoDepLoad',true)=false then
 if MakeUsrRequest(StringReplace(rsWDLDep,'%l',Dependencies[i],[rfReplaceAll])+#13+rsWAllow,rqWarning)=rqsNo then
 begin
  Result:=false;
  exit;
 end;
cnf.Free;
end;
end;

if (SUMode)and(not IsRoot) then
begin
 //Create worker thread for this action
 buscmd.cmdtype:=lbaInstallPack;
 buscmd.pkgname:=PkgPath; //Add path to setup file to DBus request
 CheckAddUSource;
 if pkType=ptLinstall then
  buscmd.addsrc:=AddUpdateSource
 else buscmd.addsrc:=false;

 with TLiDBusAction.Create(buscmd) do
 begin
  OnStatus:=@DBusThreadStatusChange;
  ExecuteAction;
  Free;
 end;
 Result:=true; //Transaction submitted
 exit;
end;

//Check if we have update sources to register
CheckAddUSource;

//Load network connections
 HTTP:=THTTPSend.Create;
 FTP:=TFTPSend.Create;

 HTTP.Sock.OnStatus:=@NetSockHook;
 HTTP.UserAgent:='Listaller Downloader';

 FTP.DSock.Onstatus:=@NetSockHook;

 //??? This needs a better solution - take proxy settings from system settings?
 cnf:=TInifile.Create(ConfigDir+'config.cnf');
 if cnf.ReadBool('Proxy','UseProxy',false) then
 begin
  //Set HTTP
  HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
  HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
  HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
  HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted

 //??? Not needed
 {if DInfo.Desktop='GNOME' then begin
 HTTP.ProxyPass:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_user');
 HTTP.ProxyUser:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_password');
  end; }

 //Set FTP
 {FTP.ProxyPort:=cnf.ReadString('Proxy','fPort','');
 FTP.ProxyHost:=cnf.ReadString('Proxy','fServer','');
 FTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
 FTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); }

 end;
  cnf.Free;

 if pkType=ptLinstall then Result:=RunNormalInstallation
 else
  if pkType=ptDLink then Result:=RunDLinkInstallation
  else
   if pkType=ptContainer then Result:=RunContainerInstallation
  else
  begin
   msg('Could not detect package type!');
   msg('TInstallation failure.');
   Result:=false;
  end;

//Free network objects
 HTTP.Sock.OnStatus:=nil;
 FTP.DSock.Onstatus:=nil;
 FreeAndNil(HTTP);
 FreeAndNil(FTP);
end;

end.

