{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Functions to install IPK packages
unit ipkinstall;

{$mode objfpc}{$H+}

interface

uses
  LiHash, Distri, Classes, FTPSend, LiTypes, LiUtils, MTProcs,
  PkTypes, Process, RegExpr, BaseUnix, Blcksock, HTTPSend, IniFiles,
  SysUtils, DepManage, IPKCDef10, StrLocale, LiFileUtil, PackageKit,
  SoftwareDB, Backend_IPK, IPKPackage11, LiStatusObj, GLib2, GExt, LiApp;

type
  TLiInstallation = class(TLiStatusObject)
  private
    //Basic information about the package and the new application
    IAppCMD: String;
    app: TLiAppItem;
    IDesktopFiles: String;
    PkgName, PkgPath: String;
    //Path to a wizard image
    FWizImage: String;
    //Package database connection
    sdb: TSoftwareDB;
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
    //Current setup type
    pkType: LiPkgType;
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
    //List of available mo files
    mofiles: TStringList;
    // True if su mode enabled
    SUMode: Boolean;
    // Path to package registration
    RegDir: String;
    //All the stuff the user forces to do
    Forces: String;
    //True, if update source should be set
    AddUpdateSource: Boolean;
    //Level of pkg signature
    sigState: PkgSignatureState;
    //Store filesize of file ftpsend is downloading
    ftpfilesize: Integer;
    //Daemon-Mode?
    daemonm: Boolean;
    //Testmode
    FTestMode: Boolean;
    // IPK extractor
    unpkg: TLiUnpacker;
    // A GLib mainloop
    loop: PGMainLoop;

    //Set superuser mode correctly
    procedure SetRootMode(b: Boolean);
    //Executed if Linstallation should be done
    function RunNormalInstallation: Boolean;
    //Runs a DLink installation
    function RunDLinkInstallation: Boolean;
    //Does the container installation
    function RunContainerInstallation: Boolean;
    // Socket hook for FTPSend and HTTPSend to get progress
    procedure NetSockHook(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    // Handler for PackageKit progress
    procedure OnPKitProgress(pos: Integer; dp: Pointer);
    // Add update source of the package
    procedure CheckAddUSource(const forceroot: Boolean = false);
  protected
    // Check if FTP connection is working
    function CheckFTPConnection(AFTPSend: TFTPSend): Boolean;
    // Quit the mainloop
    procedure QuitLoop;
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
    //** Function to solve all dependencies the package has
    function ResolveDependencies(const fetchFromDebian: Boolean = true): Boolean;
    //** Get the AppInfo record of this package
    property AppItem: TLiAppItem read app;
    //** Listaller package type
    property pType: LiPkgType read pkType;
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
    property SignatureInfo: PkgSignatureState read sigState;
    //** Set current profile by ID
    procedure SetCurProfile(i: Integer);
    //** Read the package description
    procedure ReadDescription(sl: TStringList);
    //** Read the package license
    procedure ReadLicense(sl: TStringList);
    //** True on installation with superuser rights
    property SuperuserMode: Boolean read SUMode write SetRootMode;
    //** True if setup is executed by listallerd
    property DaemonMode: Boolean read daemonm write daemonm;
    //** Set forces identifier string
    property ForceActions: String read Forces write Forces;
    //** True if update source will be registered
    property RegisterUpdateSource: Boolean read AddUpdateSource write AddUpdateSource;
    //** Setup is in testmode?
    property TestMode: Boolean read FTestMode write FTestMode;
    //** Check if package is okay, if not raise error and return false
    function PkgOkay: Boolean;
  end;

//Note: The "Testmode" variable is now in LiCommon

const
  DEPIGNORE_LIST = '/etc/lipa/ignore-deps.list';

implementation

constructor TLiInstallation.Create;
begin
  inherited Create;
  daemonm := false; //Daemon mode has to be set manually by listallerd
  FTestMode := false;

  loop := nil;

  sdb := TSoftwareDB.Create;
  if SUMode then
    RegDir := LI_CONFIG_DIR + LI_APPDB_PREF
  else
    RegDir := SyblToPath('$INST', FTestMode) + '/' + LI_APPDB_PREF;

  pkProfiles := nil;
  dependencies := nil;
  unpkg := nil;
  app := nil;
  //Create text containers
  license := TStringList.Create;
  longdesc := TStringList.Create;

  mofiles := TStringList.Create;

  //Init
  AddUpdateSource := false;
end;

destructor TLiInstallation.Destroy;
begin
  if PkgName <> '' then
    DeleteDirectory(tmpdir + PkgName, false);
  sdb.Free;
  if Assigned(Dependencies) then
    FreeAndNil(Dependencies);
  if Assigned(unpkg) then
    FreeAndNil(unpkg);
  license.Free;
  longdesc.Free;
  mofiles.Free;
  FreeAndNil(app);
  inherited Destroy;
end;

procedure TLiInstallation.SetRootMode(b: Boolean);
begin
  SUMode := b;
  if SUMode then
    RegDir := LI_CONFIG_DIR + LI_APPDB_PREF
  else
    RegDir := SyblToPath('$INST', FTestMode) + '/' + LI_APPDB_PREF;
end;

procedure TLiInstallation.SetCurProfile(i: Integer);
begin
  FFileInfo := '/pkgdata/fileinfo-' + IntToStr(i) + '.id';
  CurProfile := IntToStr(i);
end;

procedure TLiInstallation.ReadProfiles(lst: TStringList);
var
  i: Integer;
begin
  lst.Clear;
  for i := 0 to PkProfiles.Count - 1 do
    lst.Add(copy(PkProfiles[i], 0, pos(' #', PkProfiles[i])));
end;

procedure TLiInstallation.ReadDescription(sl: TStringList);
var
  i: Integer;
begin
  //Asign does not work
  sl.Clear;
  for i := 0 to longdesc.Count - 1 do
    sl.Add(longdesc[i]);
end;

procedure TLiInstallation.ReadLicense(sl: TStringList);
var
  i: Integer;
begin
  //Assign does not work
  sl.Clear;
  for i := 0 to license.Count - 1 do
    sl.Add(license[i]);
end;

// Quit a GLib mainloop
procedure TLiInstallation.QuitLoop();
begin
  if not Assigned(loop) then
    exit;
  if g_main_loop_is_running(loop) then
    g_main_loop_quit(loop);
end;

function TLiInstallation.ResolveDependencies(
  const fetchFromDebian: Boolean = true): Boolean;
var
  i, j: Integer;
  tmp: TStringList;
  err: LiLibSolveError;
  depMan: TExDepManager;
  solver: TPackageResolver;
begin
  Result := true;
  EmitExProgress(1);

  pdebug('Resolving dependencies.');
  if (Dependencies.Count > 0) then
  begin
    //Preprepare dependencies
    //Remove all distribution core deps
    if FileExists(DEPIGNORE_LIST) then
    begin
      tmp := TStringList.Create;
      tmp.LoadFromFile(DEPIGNORE_LIST);
      i := 0;
      while i <= Dependencies.Count - 1 do
      begin
        for j := 0 to tmp.Count - 1 do
        begin
          try
            if ExecRegExpr(DeleteSybls(tmp[j]), Dependencies[i]) then
            begin
              Dependencies.Delete(i);
              Dec(i);
              break;
            end;
          except
            perror('Malformed dependency-ignore entry "' + tmp[j] +
              '"! (Please fix this.)');
            tmp.Delete(j);
            break;
          end;
        end;
        Inc(i);
      end;
      i := 0;
      tmp.Free;
    end;

    EmitInfoMsg(rsResolvingDynDeps);

    //Resolve all substitution variables in dependency list
    //We use rootmode here cause all distro package install files into /
    for i := 0 to Dependencies.Count - 1 do
      Dependencies[i] := SyblToPath(Dependencies[i], FTestMode, true);

    solver := TPackageResolver.Create;
    solver.DependencyList := Dependencies;
    if not solver.RunResolver then
    begin
      if not solver.Failure then
      begin
        //There was no crash in PackageResolver, we just could not find all libs in the distribution's repos
        Dependencies.Assign(solver.Results);
        //Dependency list now contains all items which couldn't be reolved
        for i := 0 to solver.DependencyList.Count - 1 do
        begin
          //Install dependency with dependency manager
          depMan := TExDepManager.Create;
          err := ERROR;
          if fetchFromDebian then
            err := depman.InstallDependency(solver.DependencyList[i]);
          if err <> NONE then
          begin
            depman.Free;
            EmitError(StrSubst(rsDepPkgsNotFound, '%l',
              solver.DependencyList.Text) + #10 + rsInClose);
            solver.Free;
            Result := false;
            exit;
          end;
        end;
      end
      else
      begin
        //There was a critical error in dependency solver
        EmitError(rsResolveError + #10 + rsEMsg + #10 + solver.ErrorMessage);
        Result := false;
        //No exit here, we can continue
      end;
    end
    else
      //Wow, all files were present!
      Dependencies.Assign(solver.Results);
    solver.Free;

  end;
end;

function TLiInstallation.Initialize(fname: String): Boolean;
var
  n: String;
  i: Integer;
  DInfo: TDistroInfo;
  cont: TIPKControl;
  hres: LI_REQUEST_RES;

  procedure Emergency_FreeAll();
  begin
    if Assigned(cont) then
      FreeAndNil(cont);
    if Assigned(dependencies) then
      FreeAndNil(dependencies);
    if Assigned(pkProfiles) then
      FreeAndNil(pkProfiles);
    FreeAndNil(app);
  end;

begin
  Result := false;
  app := TLiAppItem.Create;
  if Assigned(unpkg) then
    FreeAndNil(unpkg);
  cont := nil;

  if (IsRoot) and (not daemonm) then
  begin
    pwarning('Do not execute this action with superuser rights! (PolicyKit will handle it!)');
    //Listaller should alsways use PolicyKit to install apps as root
    if EmitUserRequestAbortContinue(rsNotExecAsRootPolKit) = LIRQS_No then
      exit;
  end;

  //Create temporary dir
  if not DirectoryExists(tmpdir) then
    ForceDirectory(tmpdir);

  //The user _really_ is root. (This happens, if the daemon executes this action)
  //Set the right paths and execute install
  if IsRoot then
  begin
    SUMode := true;
    SetRootMode(true);
  end;

  if (app.AName <> '') or (app.AId <> '') then
  begin
    Result := false;
    perror('This Setup was already initialized. You have to create a new setup object to load a second file!');
    exit;
  end;
  Result := true;

  // Now load the database
  if not sdb.Load(SUMode) then
  begin
    EmitError('Loading the software databases failed! (One of them might be corrupted)');
    Result := false;
    Emergency_FreeAll();
    exit;
  end;

  EmitInfoMsg('SQLite version: ' + sdb.SQLiteVersion);

  EmitInfoMsg('Loading IPK package...');
  //Begin loading package
  RmApp := false;

  //Clean up possible mess of an old installation
  if not daemonm then //Daemon accepts data from previous install
    if DirectoryExists(tmpdir + ExtractFileName(fname)) then
      DeleteDirectory(tmpdir + ExtractFileName(fname), false);

  //Check if user can write to dir
  if (fpAccess(tmpdir + ExtractFileName(fname), W_OK) > 0) or
    ((DirectoryExists(tmpdir + ExtractFileName(fname))) and (not IsRoot)) then
  begin
    EmitError(rsTmpWriteDenied);
    Emergency_FreeAll();
    Result := false;
    exit;
  end;

  unpkg := TLiUnpacker.Create(fname);
  if not SUMode then
  begin
    try
      unpkg.Prepare;
    except
      on E: Exception do
        EmitError(rsPkgDM + #10 + rsABLoad + #10 + E.Message);
    end;
  end
  else
  if not FileExists(CleanFilePath(unpkg.WDir + '/ipktar.tar')) then
  begin
    try
      unpkg.Prepare;
    except
      on E: Exception do
        EmitError(rsPkgDM + #10 + rsABLoad + #10 + E.Message);
    end;
  end;

  if not daemonm then  //If running as daemon, don't check signature
  begin
    sigState := unpkg.CheckSignature;
    case sigState of
      psNone: EmitInfoMsg(rsPackageIsUnsigned);
      psTrusted: EmitInfoMsg(rsPackageHasTrustedSign);
      psUntrusted: EmitInfoMsg(rsPackageSigIsUntrusted);
    end;
  end;

  if not unpkg.UnpackControlFile('arcinfo.pin') then
  begin
    EmitError(rsExtractError + #10 + rsPkgDM + #10 + rsABLoad);
    Emergency_FreeAll();
    Result := false;
    exit;
  end;

  PkgName := ExtractFileName(fname);
  PkgPath := fname;

  cont := TIPKControl.Create(unpkg.WDir + '/arcinfo.pin'); //Read IPK configuration

  cont.LangCode := GetLangID; //Set language code, so we get localized entries

  cont.GetMoFileList(mofiles); //Grab all available mofiles

  pkType := cont.SType;

  FDisallow := LowerCase(cont.Disallows);

  n := GetSystemArchitecture;
  EmitInfoMsg('Package-Archs: ' + cont.Architecture);
  if (pos(n, LowerCase(cont.Architecture)) <= 0) and
    (LowerCase(cont.Architecture) <> 'all') and (pos('architecture', forces) <= 0) then
  begin
    EmitError(rsInvArchitecture);
    Emergency_FreeAll();
    Result := false;
    exit;
  end;

  IIconPath := cont.Icon;
  if IIconPath <> '' then
  begin
    unpkg.UnpackControlFile(IIconPath);
    IIconPath := CleanFilePath(unpkg.WDir + IIconPath);
  end;

  //Detect distribution details
  DInfo := GetDistro;

  // Invalid package ID => package will be rejected
  app.AId := '';
  app.AId := cont.PkName;
  EmitInfoMsg('Package idName: ' + app.AID);

  // Get categories
  app.Categories := cont.Categories;

  if pkType = ptLinstall then
  begin
    EmitInfoMsg(StrSubst(rsPackageTypeIsX, '%s', 'linstall'));

    if (pos('iofilecheck', FDisallow) > 0) then
    begin
      FOverwrite := true;
      EmitInfoMsg('WARNING: This package will overwrite existing files without request!');
    end
    else
      FOverwrite := false;

    //Find profiles
    i := 1;
    PkProfiles := TStringList.Create;
    cont.ReadProfiles(PkProfiles);

    for i := 0 to PkProfiles.Count - 1 do
    begin
      EmitInfoMsg(StrSubst(rsFoundInstallProfileX, '%s',
        PkProfiles[PkProfiles.Count - 1]));
      unpkg.UnpackControlFile('pkgdata/fileinfo-' + IntToStr(i) + '.id');
    end;

    app.AName := cont.AppName;

    // Copy PChar!
    if app.AId = '' then
      app.AId := app.AName;

    app.Version := cont.AppVersion;

    USource := cont.USource;
    if USource = '' then
      USource := '#';

    app.Author := PChar(cont.Author);

    IAppCMD := cont.AppCMD;
    if IAppCMD = '' then
      IAppCMD := '#';

    if IAppCMD <> '#' then
      EmitInfoMsg('Application main exec command is ' + IAppCMD);

    //Load Description
    app.Summary := cont.Summary;

    //Load info about supported distributions
    FSupportedDistris := LowerCase(cont.DSupport);

    //Load Wizard-Image
    FWizImage := GetDataFile('graphics/wizardimage.png');
    if (FWizImage <> '') and (FWizImage[1] = '/') then
    begin
      unpkg.UnpackControlFile(FWizImage);
      if FileExists(unpkg.WDir + FWizImage) then
        FWizImage := CleanFilePath(unpkg.WDir + FWizImage);
    end;

    unpkg.UnpackControlFile('preinst');
    unpkg.UnpackControlFile('postinst');
    unpkg.UnpackControlFile('prerm');
    ExecA := '<disabled>';
    ExecB := '<disabled>';
    ExecX := '<disabled>';
    if FileExists(unpkg.WDir + '/preinst') then
      ExecA := unpkg.WDir + '/preinst';
    if FileExists(unpkg.WDir + '/postinst') then
      ExecB := unpkg.WDir + '/postinst';
    if FileExists(unpkg.WDir + '/prerm') then
      ExecX := unpkg.WDir + '/prerm';

    //Load Description
    cont.ReadAppDescription(longdesc);

    //Load License
    cont.ReadAppLicense(license);

    //Load Dependencies
    Dependencies := TStringList.Create;


    cont.ReadDependencies('', dependencies);
    i := 0;
    while i <= Dependencies.Count - 1 do
    begin
      if StringReplace(Dependencies[i], ' ', '', [rfReplaceAll]) = '' then
        Dependencies.Delete(i);
      Inc(i);
    end;

    EmitInfoMsg('Profiles count is ' + IntToStr(PkProfiles.Count));
    if PkProfiles.Count < 0 then
    begin
      Emergency_FreeAll();
      EmitError(rsPkgInval + #10'Message: No profiles and no file list found!');
      PkProfiles.Free;
      Result := false;
      exit;
    end;

    if PkProfiles.Count = 1 then
      FFileInfo := '/pkgdata/fileinfo-' + copy(PkProfiles[0],
        pos(' #', PkProfiles[0]) + 2, length(PkProfiles[0])) + '.id'
    else
      FFileInfo := '*';

    SetCurProfile(0);

    //Only executed to make sure that "RmApp" property is set
    if not Testmode then
      if sdb.AppExists(app.AId) then
        RmApp := true;

  end
  else //Handle other IPK types
  if pkType = ptDLink then
  begin
    EmitInfoMsg(StrSubst(rsPackageTypeIsX, '%s', 'dlink'));

    cont.ReadAppDescription(longdesc);

    //DLink packages can only be installed as root!
    SUMode := true;

    IDesktopFiles := cont.Desktopfiles;

    Dependencies := TStringList.Create;

    cont.ReadDependencies('', dependencies);
    if dependencies.Count > 0 then
      dependencies.Insert(0, '[detectpkgs]') //Instruction to detect packages first
    else
    begin
      cont.ReadDependencies(DInfo.DName, dependencies);
      if dependencies.Count <= 0 then
      begin

        if (pos('norequest', forces) <= 0) then
          hres := EmitUserRequestAbortContinue(rsInvalidDVersion + #10 + rsUseCompPQ)
        else
          hres := LIRQS_Yes;

        if hres = LIRQS_No then
        begin
          EmitError(rsInClose);
          Emergency_FreeAll();
          Result := false;
          exit;
        end
        else
        begin
          cont.ReadDependencies(DInfo.PackageSystem, dependencies);
          if dependencies.Count <= 0 then
          begin
            EmitError(rsNoComp + #10 + rsInClose);
            Emergency_FreeAll();
            PkProfiles.Free;
            Result := false;
            exit;
          end;
        end;
      end;

      //Resolve names
      for i := 0 to dependencies.Count - 1 do
        if pos(' (', n) > 0 then
          Dependencies[i] :=
            copy(n, pos(' (', n) + 2, length(n) - pos(' (', n) - 2) +
            ' - ' + copy(n, 1, pos(' (', n) - 1);
    end;
    i := 0;
    while i <= Dependencies.Count - 1 do
    begin
      if StringReplace(Dependencies[i], ' ', '', [rfReplaceAll]) = '' then
        Dependencies.Delete(i);
      Inc(i);
    end;

  end
  else
  if pkType = ptContainer then
  begin
    EmitInfoMsg(StrSubst(rsPackageTypeIsX, '%s', 'container'));
    app.AName := ExtractFileName(cont.Binary);
    //At time we have less intialization of coverIPKs
  end;

  //IPK package has been initialized
  cont.Free;
end;

function TLiInstallation.GetMaxInstSteps: Integer;
var
  fi: TStringList;
  i, j: Integer;
begin
  Result := 0;
  j := 0;
  fi := TStringList.Create;
  fi.LoadFromFile(unpkg.WDir + '/' + FFileInfo);
  for i := 0 to fi.Count - 1 do
    if fi[i][1] <> ')' then
      Inc(j);
  Result := ((Dependencies.Count + (j div 2)) * 10) + 16;
  fi.Free;
end;

procedure TLiInstallation.NetSockHook(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
begin
  //HTTP
  if Http.DownloadSize > 10 then
  begin
    EmitExProgress(Round((100 / HTTP.DownloadSize) * HTTP.Document.Size));
    exit;
  end
  else
  //FTP
  if FTP.DSock.RecvCounter > 10 then
  begin
    EmitExProgress(Round((100 / ftpfilesize) * FTP.DSock.RecvCounter));
  end;
end;

function TLiInstallation.CheckFTPConnection(AFtpSend: TFtpSend): Boolean;
begin
  if AFtpSend.Sock.Socket = not (0) then
    Result := AFtpSend.Login
  else
  if AFtpSend.NoOp then
    Result := true
  else
    Result := AFtpSend.Login;
end;

procedure TLiInstallation.OnPKitProgress(pos: Integer; dp: Pointer);
begin
  //user_data Pointer is always nil
  EmitExProgress(pos); //Set position of extra progress to PackageKit transaction progress
end;

function TLiInstallation.RunContainerInstallation: Boolean;
var
  cont: TIPKControl;
  p: TProcess;
  tmp: TStringList;
  i: Integer;
  WDir: String;
begin
  Result := true;
  EmitStatusChange(LIS_Started);
  try
    cont := TIPKControl.Create(unpkg.WDir + '/arcinfo.pin'); //Read IPK configuration
    cont.LangCode := GetLangID; //Set language code, so we get localized entries

    unpkg.UnpackDataFile(cont.Binary);

    tmp := TStringList.Create;
    cont.GetInternalFilesSection(tmp);
    for i := 0 to tmp.Count - 1 do
      unpkg.UnpackDataFile(tmp[i]);

    tmp.Free;
    WDir := unpkg.WDir;

    p := TProcess.Create(nil);

    p.CommandLine := FindBinary('chmod') + ' 755 ''' + WDir + cont.Binary + '''';
    p.Options := [poUsePipes, poWaitonexit];
    p.Execute;

    if LowerCase(ExtractFileExt(cont.Binary)) = '.package' then
    begin
      p.Options := [poUsePipes, poWaitonexit, poNewConsole];
      if FileExists(FindBinary('package')) then
        p.Options := [poUsePipes, poWaitonexit];

      if not SUMode then
      begin
        p.CommandLine := WDir + cont.Binary;
        p.Execute;
      end
      else
        ExecuteAsRoot(WDir + cont.Binary, rsRunBinAsRoot, '');
    end
    else
    begin
      if not SUMode then
      begin
        if cont.InTerminal then
          p.Options := [poUsePipes, poWaitOnExit, poNewConsole]
        else
          p.Options := [poUsePipes, poWaitOnExit];
        p.CommandLine := WDir + cont.Binary;
        p.Execute;
      end
      else
        ExecuteAsRoot(WDir + cont.Binary, rsRunBinAsRoot, '');
    end;

    p.Free;
    DeleteFile(WDir + cont.Binary);
  except
    Result := false;
  end;
  EmitStatusChange(LIS_Finished);
end;

{procedure TLiInstallation.DBusThreadStatusChange(ty: LI_STATUS; Data: TLiProcData);
begin
  pwarning('DBus status handling needs rethinking. (We''ll use PK for that!');
  case Data.changed of
    pdMainProgress: EmitProgress(Data.mnprogress);
    pdExtraProgress: EmitExProgress(Data.exprogress);
    pdStepMessage: EmitStageMsg(Data.msg);
    pdInfo: EmitInfoMsg(Data.msg);
    pdError: EmitError(Data.msg);
    pdStatus: pdebug('Thread status changed [finished]');
  end;
end;}

function TLiInstallation.RunNormalInstallation: Boolean;
var
  i, j: Integer;
  fi, ndirs, s, appfiles: TStringList;
  dest, h, FDir: String; // h is an helper variable - used for various actions
  dsk: TIniFile; // Desktop files
  setcm: Boolean;
  proc: TProcess; // Helper process with pipes
  pkit: TPackageKit; // PackageKit object
  DInfo: TDistroInfo; // Distribution information
  mnpos: Integer; // Current positions of operation
  max: Double;
  ipkrm: TIPKBackend; // Acces IPK rmbackend directly to remove old IPK pkg
  hash: TLiHash; // Validate file hashes

  //Necessary if e.g. file copying fails
  procedure RollbackInstallation;
  var
    i: Integer;
  begin
    for i := 0 to appfiles.Count - 1 do
    begin
      EmitInfoMsg('Removing ' + DeleteModifiers(appfiles[i]) + ' ...');
      DeleteFile(DeleteModifiers(appfiles[i]));
      mnpos := mnpos - 10;
      EmitProgress(Round(mnpos * max));
    end;
    for i := 0 to ndirs.Count - 1 do
    begin
      EmitInfoMsg('Removing ' + ndirs[i]);
      try
        DeleteDirectory(ndirs[i], false);
      except
        perror('Could not remove directory. Please report this bug.');
      end;
    end;
    appfiles.Clear;
    ndirs.Clear;
  end;

  procedure Abort_FreeAll();
  begin
    try
      if Assigned(fi) then
        FreeAndNil(fi);
      if Assigned(ndirs) then
        FreeAndNil(ndirs);
      if Assigned(s) then
        FreeAndNil(s);
      if Assigned(appfiles) then
        FreeAndNil(appfiles);
      if Assigned(dsk) then
        FreeAndNil(dsk);
      if Assigned(proc) then
        FreeAndNil(proc);
      if Assigned(pkit) then
        FreeAndNil(pkit);
      if Assigned(hash) then
        FreeAndNil(hash);
    except
      perror('Error while cleaning up.');
    end;
  end;

begin
  fi := nil;
  ndirs := nil;
  s := nil;
  appfiles := nil;
  dsk := nil;
  proc := nil;
  pkit := nil;
  hash := nil;
  EmitStatusChange(LIS_Started);
  // Send information about forced stuff:
  if pos('dependencies', forces) > 0 then
    EmitInfoMsg('Dependencies are forced. Skipping dependency check.');
  if pos('architecture', forces) > 0 then
    EmitInfoMsg('Architecture forced. The software might not work on your system architecture.');

  if not FileExists(unpkg.WDir + '/' + FFileInfo) then
  begin
    EmitError(rsInstFailed);
    perror('No file information found!');
    perror('IPK package seems to be broken.');
    Result := false;
    Abort_FreeAll();
    exit;
  end;

  fi := TStringList.Create;
  fi.LoadFromFile(unpkg.WDir + '/' + FFileInfo);

  proc := TProcess.Create(nil);
  proc.Options := [poUsePipes, poStdErrToOutPut];

  DInfo := GetDistro; //Load DistroInfo

  Result := false;
  mnpos := 0;

  //!!! @deprecated
  {//Check if fileinfo contains shared files
   for i:=0 to (fi.Count div 3)-1 do begin
   if IsSharedFile(lp+PkgName+fi[i*3]) then ContSFiles:=true;
   end; }

  max := 100 / GetMaxInstSteps;

  EmitProgress(Round(max * mnpos));

  if Testmode then
  begin
    proc.CommandLine := FindBinary('rm') + ' -rf /tmp/litest';
    proc.Execute;
  end;

  //Check if PackageKit trackmode is enabled:
  ShowPkMon();

  //Now resolve all dependencies and prepare installation
  if pos('dependencies', forces) <= 0 then
    if not ResolveDependencies(true) then
    begin
      Result := false;
      exit;
    end;

  EmitExProgress(0);
  //Execute programs/scripts
  if ExecA <> '<disabled>' then
  begin
    Proc.CommandLine := FindBinary('chmod') + ' 777 ' + ExecA;
    Proc.Execute;
    //while proc.Running do Application.ProcessMessages;
    Proc.CommandLine := ExecA;
    Proc.Execute;
    //while proc.Running do Application.ProcessMessages;
  end;

  pkit := TPackageKit.Create;
  pkit.OnProgress := @OnPKitProgress;

  if pos('dependencies', forces) <= 0 then
    if Dependencies.Count > 0 then
    begin
      for I := 0 to Dependencies.Count - 1 do
      begin  //Download & install dependencies

        EmitExProgress(0);

        EmitInfoMsg('DepInstall: ' + Dependencies[i] + ' (using PackageKit)');
        EmitInfoMsg('');

        //pkit.Resolve(Dependencies[i]);

        if pkit.PkExitStatus = PK_EXIT_ENUM_SUCCESS then
        begin
          pkit.InstallPkg(Dependencies[i]);

          //Check if the package was really installed
          if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
          begin
            EmitInfoMsg('Package ' + Dependencies[i] + ' could not be installed.');
            EmitError(StrSubst(rsInstPkgFailed, '%s', Dependencies[i]) +
              #10 + rsEMsg + #10 + pkit.LastErrorMessage + #10 +
              StrSubst(rsViewLog, '%p', '/tmp/install-' +
              GetAppIDString(app) + '.log'));
            Result := false;
            Abort_FreeAll();
            exit;
          end;

          Dependencies[i] := '*' + Dependencies[i];

        end;

        mnpos := mnpos + 10;
        EmitProgress(Round(mnpos * max));
      end; //End of Dependecies.Count term
    end; //End of dependency-check

  FreeAndNil(pkit);
  EmitExProgress(0);

  //Delete old application installation if necessary
  if (RmApp) and (not Testmode) then
  begin
    //Uninstall old application
    ipkrm := TIPkBackend.Create;
    ipkrm.RegisterOnMessage(FMessage, message_udata);
    ipkrm.RegisterOnStatus(FStatus, status_udata);
    ipkrm.RootMode := SUMode;
    //!!! TODO
    //ipkrm.Initialize(appInfo);
    if ipkrm.CanBeUsed then
      ipkrm.Run;
    FreeAndNil(ipkrm);

    EmitExProgress(0);
  end;

  appfiles := TStringList.Create;
  EmitStageMsg(rsStep2);

  ndirs := TStringList.Create;
  j := 0;
  if not DirectoryExists(SyblToPath('$INST', FTestMode)) then
    SysUtils.CreateDir(SyblToPath('$INST', FTestMode));


  //Unpack files directory
  if not unpkg.UnpackDataFile('files') then
  begin
    EmitError(rsExtractError);
    RollbackInstallation;
    Result := false;
    Abort_FreeAll();
    exit;
  end;

  // We need the checksum validator now...
  hash := TLiHash.Create;

  for i := 0 to fi.Count - 1 do
  begin
    if (pos(' <' + LowerCase(DInfo.DName) + '-only>', LowerCase(fi[i])) > 0) or
      (pos('-only', LowerCase(fi[i])) <= 0) then
    begin

      //Set new target directory on >
      if fi[i][1] = '>' then
      begin
        dest := copy(fi[i], 2, length(fi[i]));
        dest := SyblToPath(dest, FTestMode);
        if not DirectoryExists(dest) then
        begin
          ForceDirectories(dest);
          ndirs.Add(dest);
        end;
        h := dest;
        while not DirectoryExists(dest) do
        begin
          CreateDir(h);
          if DirectoryExists(h) then
            h := Dest
          else
            h := ExtractFilePath(ExcludeTrailingBackslash(h));
        end;

      end
      else
      if (fi[i][1] = '.') or (fi[i][1] = '/') then
      begin
        //h is now used for the file-path
        h := DeleteModifiers(fi[i]);

        FDir := unpkg.WDir + ExtractFileName(PkgPath) + '/';
        if not DirectoryExists(FDir) then
          CreateDir(FDIR);

        EmitInfoMsg('Copy file ' + ExtractFileName(h) + ' to ' + dest + ' ...');

        pdebug('Filename: ' + h);
        if not hash.HashesEqual(fi[i + 1], DeleteModifiers(unpkg.WDir + h)) then
        begin
          RollbackInstallation;
          Result := false;
          Abort_FreeAll();
          exit;
        end;

        Inc(j);

        try
          if FOverwrite then
            FileCopy(DeleteModifiers(unpkg.WDir + h), dest + '/' +
              ExtractFileName(DeleteModifiers(h)))
          else
          if (not FileExists(dest + '/' + ExtractFileName(DeleteModifiers(h)))) then
            FileCopy(DeleteModifiers(unpkg.WDir + h), dest + '/' +
              ExtractFileName(DeleteModifiers(h)))
          else
          begin
            EmitError(StrSubst(rsCnOverride, '%f', dest + '/' +
              ExtractFileName(DeleteModifiers(h))) + #10 + rsInClose);
            RollbackInstallation;
            Result := false;
            Abort_FreeAll();
            exit;
          end;
        except
          //Unable to copy the file
          EmitError(Format(rsCnCopy,
            [dest + '/' + ExtractFileName(DeleteModifiers(h))]) + #10 + rsInClose);
          RollbackInstallation;
          Result := false;
          Abort_FreeAll();
          exit;
        end;

        if (pos('.desktop', LowerCase(ExtractFileName(h))) > 0) then
        begin
          dsk := TIniFile.Create(dest + '/' + ExtractFileName(h));
          dsk.WriteString('Desktop Entry', 'X-AppVersion', app.Version);
          dsk.WriteString('Desktop Entry', 'X-AllowRemove', 'true');
          dsk.WriteString('Desktop Entry', 'X-Author', app.Author);
          dsk.WriteString('Desktop Entry', 'X-Publisher', app.Publisher);
          if dsk.ValueExists('Desktop Entry', 'Icon') then
            dsk.WriteString('Desktop Entry', 'Icon', SyblToPath(
              dsk.ReadString('Desktop Entry', 'Icon', '*'), FTestMode));
          if dsk.ValueExists('Desktop Entry', 'Exec') then
            dsk.WriteString('Desktop Entry', 'Exec', SyblToPath(
              dsk.ReadString('Desktop Entry', 'Exec', '*'), FTestMode));
          FreeAndNil(dsk);
        end;

        if (pos('<setvars>', LowerCase(ExtractFileName(h))) > 0) then
        begin
          s := TStringList.Create;
          s.LoadFromFile(dest + '/' + ExtractFileName(h));
          for j := 0 to s.Count - 1 do
            s[j] := SyblToPath(s[j], FTestMode);
          s.SaveToFile(dest + '/' + ExtractFileName(h));
          FreeAndNil(s);
        end;


        appfiles.Add(dest + '/' + ExtractFileName(fi[i]));

        //Delete temp file
        DeleteFile(DeleteModifiers(unpkg.WDir + ExtractFileName(h)));

        //msg('Okay.');

        mnpos := mnpos + 10;
        EmitProgress(Round(mnpos * max));

      end;
    end;
  end;

  // Remove hash manager from memory
  FreeAndNil(hash);

  EmitStageMsg(rsStep3);
  //Check if every single file needs its own command to get the required rights
  //(It's faster if only every folder recieves the rights)
  setcm := false;
  for i := 0 to fi.Count - 1 do
    if pos(' <chmod:', fi[i]) > 0 then
      setcm := true;


  if setcm then
  begin
    //Set rights per file
    for i := 0 to fi.Count - 1 do
    begin
      if i mod 2 = 0 then
      begin

        if fi[i][1] = '>' then
          dest := SyblToPath(fi[i], FTestMode)
        else
        begin
          h := fi[i];

          if pos(' <chmod:', h) > 0 then
          begin
            proc.CommandLine :=
              FindBinary('chmod') + ' ' + copy(h, pos(' <chmod:', h) + 8, 3) +
              dest + '/' + ExtractFileName(DeleteModifiers(fi[i]));
            proc.Execute;
          end
          else
          begin
            proc.CommandLine :=
              FindBinary('chmod') + ' 755 ' + dest + '/' + ExtractFileName(
              DeleteModifiers(fi[i]));
            proc.Execute;
          end;

          //while proc.Running do Application.ProcessMessages;
          EmitInfoMsg(StrSubst(rsRightsAssignedToX, '%a', DeleteModifiers(
            ExtractFileName(SyblToPath(fi[i], FTestMode)))));
        end;
      end;
    end;

  end
  else
  begin
    //Set rights per folder
    for i := 0 to ndirs.Count - 1 do
    begin
      proc.CommandLine := FindBinary('chmod') + ' 755 -R ' +
        SyblToPath(ndirs[i], FTestMode);
      proc.Execute;
      EmitInfoMsg('Rights assigned to folder ' + ExtractFileName(
        SyblToPath(ndirs[i], FTestMode)));
    end;
  end; //End setcm

  mnpos := mnpos + 6;
  EmitProgress(Round(mnpos * max));

  FreeAndNil(fi);
  EmitStageMsg(rsStep4);

  if Testmode then
    EmitInfoMsg(rsTestmodeDNRegister)
  else
  begin
    FileCopy(unpkg.WDir + '/arcinfo.pin', sdb.AppConfDir + '/application');

    //Save list of installed files
    appfiles.SaveToFile(sdb.AppConfDir + '/files.list');
    FreeAndNil(appfiles);

    if mofiles.Count > 0 then
    begin
      ForceDirectories(sdb.AppConfDir + '/locale/');
      for i := 0 to mofiles.Count - 1 do
      begin
        FileCopy(unpkg.WDir + mofiles[i], sdb.AppConfDir + '/locale/' +
          ExtractFileName(mofiles[i]));
      end;
    end;

    app.Dependencies := Dependencies.Text;

    sdb.AppAddNew(app);

    if length(IIconPath) > 0 then
      if IIconPath[1] = '/' then
        FileCopy(IIconPath, sdb.AppConfDir + '/icon' +
          ExtractFileExt(IIconPath));

    ndirs.SaveToFile(sdb.AppConfDir + '/dirs.list');

    if ExecX <> '<disabled>' then
      FileCopy(ExecX, sdb.AppConfDir + '/prerm');

    mnpos := mnpos + 5;
    EmitProgress(Round(mnpos * max));
    //Execute Program/Script
    if ExecB <> '<disabled>' then
    begin
      proc.CommandLine := FindBinary('chmod') + ' 777 ' + ExecB;
      Proc.Execute;
      //while Proc.Running do Application.ProcessMessages;
      Proc.CommandLine := ExecB;
      Proc.Execute;
      //while Proc.Running do Application.ProcessMessages;
    end;

    if (USource <> '#') and (AddUpdateSource) then
    begin
      CreateUpdateSourceList(RegDir);
      fi := TStringList.Create;
      fi.LoadFromFile(RegDir + 'updates.list');
      for i := 1 to fi.Count - 1 do
        if pos(USource, fi[i]) > 0 then
          break;
      if i = fi.Count then
      begin
        fi.Add('- ' + USource);
        fi.SaveToFile(RegDir + 'updates.list');
      end;
      FreeAndNil(fi);
    end;

  end;
  FreeAndNil(ndirs);
  FreeAndNil(proc);

  mnpos := mnpos + 5;
  EmitProgress(Round(mnpos * max));

  Result := true;
  EmitProgress(100);
  EmitStatusChange(LIS_Finished);
  EmitStageMsg(rsFinished);
  sleep(120);
end;

function TLiInstallation.RunDLinkInstallation: Boolean;
var
  i: Integer;
  ar: TIniFile;
  pkit: TPackageKit;
  mnpos: Integer;
  max: Double;
  pkg: String;
  fpath: String;

  procedure Abort_FreeAll();
  begin
    if Assigned(ar) then
      ar.Free;
    if Assigned(pkit) then
      pkit.Free;
  end;

begin
  EmitStatusChange(LIS_Started);
  max := 100 / (Dependencies.Count * 4);
  mnpos := 0;
  EmitProgress(Round(mnpos * max));
  HTTP.UserAgent := 'Listaller-GET';


{//Set Proxy-Settings
cnf:=TInifile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('Proxy','UseProxy',false) then
begin
//Set HTTP
HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted
//Not needed
if DInfo.Desktop='GNOME' then begin
HTTP.ProxyPass:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_user');
HTTP.ProxyUser:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_password');
 end;
end;}

  ShowPKMon();

  pkit := TPackageKit.Create;
  pkit.OnProgress := @OnPKitProgress;

  for i := 0 to Dependencies.Count - 1 do
  begin
    if pos('://', Dependencies[i]) <= 0 then
    begin
      EmitInfoMsg(StrSubst(rsLookingForX, '%a', Dependencies[i]));

      pkit.ResolveInstalled(Dependencies[i]);

      Inc(mnpos);
      EmitProgress(Round(mnpos * max));

      if pkit.PkExitStatus = PK_EXIT_ENUM_SUCCESS then
      begin
        if pkit.RList.Count <= 0 then
        begin
          EmitInfoMsg(StrSubst(rsInstallingX, '%a', Dependencies[i]));

          Inc(mnpos);
          EmitProgress(Round(mnpos * max));

          pkit.InstallPkg(Dependencies[i]);

          Inc(mnpos);
          Inc(mnpos);
          EmitProgress(Round(mnpos * max));
        end;
      end
      else
      begin
        EmitError(rsPkQueryFailed + #10 + rsEMsg + #10 +
          pkit.LastErrorMessage + #10 + StrSubst(rsViewLog, '%p',
          '/tmp/install-' + GetAppIDString(app) + '.log'));
        Result := false;
        Abort_FreeAll();
        exit;
      end;
    end
    else
    begin
      pkg := copy(Dependencies[i], pos(' (', Dependencies[i]) + 2,
        length(dependencies[i]));
      pkg := copy(pkg, 1, pos(')', pkg) - 1);
      fpath := copy(Dependencies[i], 1, pos(' (', Dependencies[i]) - 1);
      pdebug('Looking for ' + pkg);
      EmitInfoMsg(StrSubst(rsLookingForX, '%a', pkg));
      pkit.ResolveInstalled(pkg);

      Inc(mnpos);
      EmitProgress(Round(mnpos * max));

      if pkit.PkExitStatus = PK_EXIT_ENUM_SUCCESS then
      begin
        if pkit.RList.Count <= 0 then
        begin
          EmitInfoMsg(rsDownloadingPkg);

          if pos('http://', fpath) > 0 then
          begin
            try
              HTTP.HTTPMethod('GET', fpath);
              pdebug('HTTPResCode:=> ' + IntToStr(HTTP.ResultCode));
              HTTP.Document.SaveToFile(tmpdir + ExtractFileName(fpath));
              if HTTP.ResultCode > 210 then
              begin
                EmitError(rsDepDlProblem + #10 + rsECode + ' ' +
                  IntToStr(HTTP.ResultCode));
                Result := false;
                Abort_FreeAll();
                exit;
              end;

            except
              EmitError(rsDepDlProblem);
              Result := false;
              Abort_FreeAll();
              exit;
            end;

          end
          else
          begin
            with FTP do
            begin
              TargetHost := GetServerName(fpath);
              try
                DirectFileName := tmpdir + ExtractFileName(fpath);
                DirectFile := true;

                if not CheckFTPConnection(FTP) then
                begin
                  EmitError(rsFTPFailed);
                  Result := false;
                  Abort_FreeAll();
                  exit;
                end;

                ChangeWorkingDir(GetServerPath(fpath));

                EmitExProgress(0);

                ftpfilesize := FTP.FileSize(ExtractFileName(fpath));
                RetrieveFile(ExtractFileName(fpath), false);
                Logout;
              except
                EmitError(rsDepDlProblem);
                Result := false;
                Abort_FreeAll();
                exit;
              end;
            end;

          end;

          Inc(mnpos);
          EmitProgress(Round(mnpos * max));

          EmitInfoMsg(StrSubst(rsInstallingX, '%a', pkg));
          pkit.InstallLocalPkg('/tmp/' + ExtractFileName(fpath));

          if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
          begin
            EmitError(StrSubst(rsInstPkgFailed, '%s', pkg) +
              #10 + rsEMsg + #10 + pkit.LastErrorMessage);
            Result := false;
            Abort_FreeAll();
            exit;
          end;
        end;

        Inc(mnpos);
        EmitProgress(Round(mnpos * max));
      end;
    end;
  end;

  pkit.Free; //Free PackageKit

  while Length(IDesktopFiles) > 1 do
  begin
    if pos(';', IDesktopFiles) > 0 then
      ar := TInifile.Create('/usr/share/applications/' + copy(
        IDesktopFiles, 0, pos(';', IDesktopFiles) - 1))
    else
      ar := TInifile.Create('/usr/share/applications/' + IDesktopFiles);
    ar.WriteString('Desktop Entry', 'X-AppVersion', app.Version);
    ar.WriteString('Desktop Entry', 'X-Author', app.Author);
    ar.WriteString('Desktop Entry', 'X-Publisher', app.Publisher);
    if pos(';', IDesktopFiles) > 0 then
      IDesktopFiles := copy(IDesktopFiles, pos(';', IDesktopFiles) +
        1, length(IDesktopFiles))
    else
      IDesktopFiles := '';
    ar.Free;
  end;

  EmitStageMsg(rsSuccess);
  EmitStatusChange(LIS_Finished);
end;

// Internal methods to catch PK messages
procedure lisetup_pkprogress_cb(progress: PPkProgress; typ: PkProgressType;
  install: TLiInstallation);
var
  role: PkRoleEnum;
  percentage: gint;
  status: PkStatusEnum;
  text: PGChar;
begin
  if (typ = PK_PROGRESS_TYPE_ROLE) then
  begin
    g_object_get(progress, 'role', @role, nil);
    if (role = PK_ROLE_ENUM_UNKNOWN) then
      exit;
  end;

  if (typ = PK_PROGRESS_TYPE_PERCENTAGE) then
  begin
    g_object_get(progress, 'percentage', @percentage, nil);
    install.EmitProgress(percentage);
  end;

  if (typ = PK_PROGRESS_TYPE_STATUS) then
  begin
    g_object_get(progress, 'status', @status, nil);
    if (status = PK_STATUS_ENUM_FINISHED) then
      exit;
    // Show the status
    //text := pk_status_enum_to_localised_text(status);
    text := '<status_text>';
    install.EmitInfoMsg(text);
  end;


        (* package-id
  if (type == PK_PROGRESS_TYPE_PACKAGE_ID) {
    g_object_get (progress,
            "package-id", &package_id,
            NULL);
    if (package_id == NULL)
      goto out;

    if (!is_console) {
      /* create printable */
      printable = pk_package_id_to_printable (package_id);

      /* TRANSLATORS: the package that is being processed */
      g_print ("%s:\t%s\n", _("Package"), printable);
      goto out;
    }
  }
        *)
end;

procedure lisetup_pkfinished_cb(obj: PGObject; res: PGAsyncResult;
  install: TLiInstallation);
var
  error: PGError;
  results: PPkResults;
  role: PPkRoleEnum;
  error_code: PPkError;
begin
  error := nil;
  error_code := nil;
{  GPtrArray *array;
  PkPackageSack *sack;
  PkRestartEnum restart;
  PkRoleEnum role;}

  results := pk_client_generic_finish(obj, res, @error);

  if error <> nil then
  begin
    perror('failed fetching results: ' + error^.message);
    g_error_free(error);
    install.EmitError(error^.message);
    install.QuitLoop();
    exit;
  end;

  // get the role
  g_object_get(G_OBJECT(results), 'role', @role, nil);

  // check error code
  error_code := pk_results_get_error_code(results);

  if (error_code <> nil) then
  begin
    install.EmitError('Error:'#10 + pk_error_get_details(error_code));
  end;

  if Assigned(error_code) then
    g_object_unref(error_code);

  install.QuitLoop;
end;

// Do software installation
procedure TLiInstallation.CheckAddUSource(const forceroot: Boolean = false);
var
  fi: TStringList;
  i: Integer;
  found: Boolean;
  s: String;
begin
  if not forceroot then
    s := RegDir
  else
    s := LI_CONFIG_DIR + LI_APPDB_PREF;

  if (USource <> '#') and (USource <> '') then
  begin

    if not FileExists(s + 'updates.list') then
      if (IsRoot) or (not forceroot) then
      begin
        CreateUpdateSourceList(s);
      end
      else
        found := false; //Unable to create updatelist on root-filesystem without su rights

    if FileExists(s + 'updates.list') then
    begin
      fi := TStringList.Create;
      fi.LoadFromFile(s + 'updates.list');
      found := false;
      for i := 1 to fi.Count - 1 do
        if pos(USource, fi[i]) > 0 then
        begin
          break;
          found := true;
        end;
      fi.Free;
    end;

    AddUpdateSource := false;
    if not found then
    begin
      if EmitUserRequestAbortContinue(PAnsiChar(rsAddUpdSrc + #10 +
        USource + #10 + PAnsiChar(rsQAddUpdSrc))) = LIRQS_Yes then
      begin
        AddUpdateSource := true;
      end;
    end
    else
      AddUpdateSource := true;
  end;
end;

function TLiInstallation.PkgOkay: Boolean;
begin
  Result := true;
  if (app.AId = '') or (app.AName = '') then
  begin
    pwarning('Package check failed!');
    EmitError(rsUnknownErrorOC + #10 + rsPkgDM + #10 + rsABLoad);
    Result := false;
    exit;
  end;
end;

function TLiInstallation.DoInstallation: Boolean;
var
  cnf: TIniFile;
  i: Integer;
  pkclient: PPkClient;
  tmp: TStringList;
begin
  Result := false;
  if not PkgOkay then
    exit;

  tmp := TStringList.Create;
  FindDirs(tmp, tmpdir);
  if tmp.Count > 1 then
  begin
    tmp.Free;
    EmitError(rsOneInstAtTime);
    exit;
  end;
  tmp.Free;

  //Force installing DLink packages as root
  if pkType = ptDLink then
    SUMode := true;

  if not IsRoot then
    if pkType = ptLinstall then
    begin
      //Set Testmode settings
      if (IAppCMD = '#') and (Testmode) then
      begin
        EmitError(rsActionNotPossiblePkg);
        Result := false;
        exit;
      end;

      IAppCMD := SyblToPath(IAppCMD, FTestMode);
      //Check if the package downloads native pkgs
      for i := 0 to Dependencies.Count - 1 do
        if (pos('http://', Dependencies[i]) > 0) or
          (pos('ftp://', Dependencies[i]) > 0) then
        begin
          cnf := TInifile.Create(ConfigDir + 'config.cnf');
          if cnf.ReadBool('MainConf', 'AutoDepLoad', true) = false then
            if EmitUserRequestAbortContinue(StrSubst(rsWDLDep, '%l', Dependencies[i]) +
              #10 + rsWAllow) = LIRQS_No then
            begin
              Result := false;
              exit;
            end;
          cnf.Free;
        end;
    end;

  if (pkType <> ptContainer) then
    if (SUMode) and (not IsRoot) then
    begin
      // Query PackageKit daemon to perform the installation
      pkclient := pk_client_new();

      pk_client_install_files_async(pkclient, false, StringToPPchar(PkgPath, 0),
        nil, PkProgressCallback(@lisetup_pkprogress_cb),
        self, GAsyncReadyCallback(@lisetup_pkfinished_cb), self);

      // Run mainloop
      loop := g_main_loop_new(nil, false);
      g_main_loop_run(loop);

      g_object_unref(pkclient);
      g_main_loop_unref(loop);
      loop := nil;

      //Force check of root update source
     { CheckAddUSource(true);
      if pkType = ptLinstall then
        buscmd.addsrc := AddUpdateSource
      else
        buscmd.addsrc := false; }
      Result := true;
      exit;
    end;

  if (not IsRoot) and (pkType = ptLinstall) then
    //Check if we have update source to register
    CheckAddUSource;

  //Load network connections
  HTTP := THTTPSend.Create;
  FTP := TFTPSend.Create;

  HTTP.Sock.OnStatus := @NetSockHook;
  HTTP.UserAgent := 'Listaller Downloader';

  FTP.DSock.Onstatus := @NetSockHook;

{ //??? This needs a better solution - take proxy settings from system settings?
 cnf:=TInifile.Create(ConfigDir+'config.cnf');
 if cnf.ReadBool('Proxy','UseProxy',false) then
 begin
  //Set HTTP
  HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
  HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
  HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
  HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted

 //??? Not needed
  //if DInfo.Desktop='GNOME' then
  //  begin
  //HTTP.ProxyPass:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_user');
  //HTTP.ProxyUser:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_password');
   //end;

 //Set FTP
 FTP.ProxyPort:=cnf.ReadString('Proxy','fPort','');
 FTP.ProxyHost:=cnf.ReadString('Proxy','fServer','');
 FTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
 FTP.ProxyPass:=cnf.ReadString('Proxy','Password','');

 end;
  cnf.Free;}

  if pkType = ptLinstall then
    Result := RunNormalInstallation
  else
  if pkType = ptDLink then
    Result := RunDLinkInstallation
  else
  if pkType = ptContainer then
    Result := RunContainerInstallation
  else
  begin
    EmitInfoMsg(rsCouldNotDetectPkgType);
    EmitInfoMsg('TLiInstallation failure.');
    Result := false;
  end;

  //Free network objects
  HTTP.Sock.OnStatus := nil;
  FTP.DSock.Onstatus := nil;
  FreeAndNil(HTTP);
  FreeAndNil(FTP);
end;

end.

