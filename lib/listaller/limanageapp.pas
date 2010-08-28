{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This library is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Functions to manage applications (install/uninstall, dependency-check)
unit limanageapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, GetText, liTypes, liUtils, MTProcs,
  PkTypes, Process, IniFiles, SysUtils, IPKCDef10, strLocale,
  liDBusProc, LiFileUtil, PackageKit, SoftwareDB;

type
  PAppManager = ^TAppManager;

  TAppManager = class
  private
    SUMode: Boolean;
    FReq: UserRequestCall;
    FApp: NewAppEvent;
    FStatus: StatusChangeEvent;

    //State data
    sdata: TLiStatusData; //Contains the current progress

    procedure msg(s: String);
    function request(s: String; ty: TRqType): TRqResult;
    procedure newapp(s: String; oj: AppInfo);
    procedure setstate(state: LiProcStatus);
    procedure setpos(i: Integer);

    function IsInList(nm: String; list: TStringList): Boolean;
    //** Method that removes MOJO/LOKI installed applications @param dsk Path to the .desktop file of the application
    function UninstallMojo(dsk: String): Boolean;
    //** Catch the PackageKit progress
    procedure PkitProgress(pos: Integer; xd: Pointer);
    //** Catch status messages from DBus action
    procedure DBusStatusChange(ty: LiProcStatus; data: TLiProcData);
    procedure InternalRemoveApp(obj: AppInfo);
  protected
    //Some user data for callbacks
    statechange_udata: Pointer;
    request_udata: Pointer;
    newapp_udata: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    //** Load software list entries
    procedure LoadEntries;
    //** Removes an application
    procedure UninstallApp(obj: AppInfo);
 {** Checks dependencies of all installed apps
    @param report Report of the executed actions
    @param fix True if all found issues should be fixed right now
    @returns True if everything is okay, False if dependencies are missing}
    function CheckApps(report: TStringList; const fix: Boolean = false;
      const forceroot: Boolean = false): Boolean;
    procedure RegOnStatusChange(call: StatusChangeEvent; data: Pointer);
    procedure RegOnRequest(call: UserRequestCall; data: Pointer);
    procedure RegOnNewApp(call: NewAppEvent; data: Pointer);
    property SuperuserMode: Boolean read SUMode write SUMode;
    function UserRequestRegistered: Boolean;
  end;

{** Removes an IPK application
     @param AppName Name of the application, that should be uninstalled
     @param AppID ID of the application
     @param FStatus Callback to receive the status of the procedure (set to nil if not needed)
     @param fast Does a quick uninstallation if is true (Set to "False" by default)
     @param RmDeps Remove dependencies if true (Set to "True" by default)}
procedure UninstallIPKApp(AppName, AppID: String; FStatus: StatusChangeEvent;
  fast: Boolean = false; RmDeps: Boolean = true);

//** Checks if package is installed
function IsPackageInstalled(aName: String = ''; aID: String = '';
  sumode: Boolean = false): Boolean;

//** Helper procedure to create USource file if missing
procedure CreateUpdateSourceList(path: String);

implementation

{ TAppManager }

constructor TAppManager.Create;
begin
  inherited Create;
  FApp := nil;
  FStatus := nil;
  FReq := nil;
end;

destructor TAppManager.Destroy;
begin
  inherited;
end;

function TAppManager.UserRequestRegistered: Boolean;
begin
  if Assigned(FReq) then
    Result := true
  else
    Result := false;
end;

procedure TAppManager.RegOnStatusChange(call: StatusChangeEvent; data: Pointer);
begin
  if Assigned(call) then
  begin
    FStatus := call;
    statechange_udata := data;
  end
  else
    perror('Received invalid ´StatusChangeEvent´ pointer!');
end;

procedure TAppManager.RegOnRequest(call: UserRequestCall; data: Pointer);
begin
  if Assigned(call) then
  begin
    FReq := call;
    request_udata := data;
  end
  else
    perror('Received invalid ´UserRequestCall´ pointer!');
end;

procedure TAppManager.RegOnNewApp(call: NewAppEvent; data: Pointer);
begin
  if Assigned(call) then
  begin
    FApp := call;
    newapp_udata := data;
  end
  else
    perror('Received invalid ´StatusChangeEvent´ pointer!');
end;

procedure TAppManager.Msg(s: String);
begin
  sdata.msg := PChar(s);
  if Assigned(FStatus) then
    FStatus(scMessage, sdata, statechange_udata);
end;

function TAppManager.Request(s: String; ty: TRqType): TRqResult;
begin
  if Assigned(FReq) then
    Result := FReq(ty, PChar(s), request_udata);
end;

procedure TAppManager.NewApp(s: String; oj: AppInfo);
begin
  if Assigned(FApp) then
    FApp(PChar(s), @oj, newapp_udata);
end;

procedure TAppManager.SetPos(i: Integer);
begin
  sdata.mnprogress := i;
  if Assigned(FStatus) then
    FStatus(scMnProgress, sdata, statechange_udata);
end;

procedure TAppManager.SetState(state: LiProcStatus);
begin
  sdata.lastresult := state;
  if Assigned(FStatus) then
    FStatus(scStatus, sdata, statechange_udata);
end;

function TAppManager.IsInList(nm: String; list: TStringList): Boolean;
begin
  Result := list.IndexOf(nm) > -1;
end;

procedure TAppManager.PkitProgress(pos: Integer; xd: Pointer);
begin
  //User defindes pointer xd is always nil here
  setpos(pos);
end;

procedure TAppManager.LoadEntries;
var
  ini: TIniFile;
  tmp, xtmp: TStringList;
  i, j: Integer;
  db: TSoftwareDB;
  blst: TStringList;

  //Internal function to process desktop files
  procedure ProcessDesktopFile(fname: String);
  var
    d: TIniFile;
    entry: AppInfo;
    dt: TMOFile;
    lp: String;
    translate: Boolean; //Used, because Assigned(dt) throws an AV
    gr: AppCategory;
    //Translate string if possible
    function ldt(s: String): String;
    var
      h: String;
    begin
      h := s;
      try
        if translate then
        begin
          h := dt.Translate(s);
          if h = '' then
            h := s;
        end;
      except
        Result := h;
      end;
      Result := s;
    end;

  begin
    d := TIniFile.Create(fname);
    translate := false;

    if (not SUMode) and (d.ReadString('Desktop Entry', 'Exec', '')[1] <> '/') then
    else
      if (LowerCase(d.ReadString('Desktop Entry', 'NoDisplay', 'false')) <>
        'true') and (pos('yast', LowerCase(fname)) <= 0) and
        (LowerCase(d.ReadString('Desktop Entry', 'Hidden', 'false')) <> 'true') and
        (not IsInList(d.ReadString('Desktop Entry', 'Name', ''), blst))
        // and(pos('system',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
        and (pos('core', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <=
        0) and (pos('.hidden', LowerCase(d.ReadString('Desktop Entry',
        'Categories', ''))) <= 0)
        // and(pos('base',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
        and (pos('wine', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <=
        0) and (pos('wine', LowerCase(d.ReadString('Desktop Entry',
        'Categories', ''))) <= 0) and
        (d.ReadString('Desktop Entry', 'X-KDE-ParentApp', '#') = '#') and
        (pos('screensaver', LowerCase(d.ReadString('Desktop Entry',
        'Categories', ''))) <= 0) and
        (pos('setting', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <= 0)
        // and(pos('utility',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
        and (d.ReadString('Desktop Entry', 'OnlyShowIn', '') = '') and
        (d.ReadString('Desktop Entry', 'X-AllowRemove', 'true') = 'true') then
      begin
        msg(rsLoading + '  ' + ExtractFileName(fname));

        //Check for Autopackage.org installation
        if pos('apkg-remove', LowerCase(d.ReadString('Desktop Entry',
          'Actions', ''))) > 0 then
          entry.UId := PChar('!' + d.ReadString('Desktop Action Apkg-Remove',
            'Exec', ''))
        else
          entry.UId := PChar(fname);

        if d.ReadString('Desktop Entry', 'X-Ubuntu-Gettext-Domain', '') <> '' then
        begin
          try
            lp := '/usr/share/locale-langpack/' + GetLangID +
              '/LC_MESSAGES/' + d.ReadString('Desktop Entry',
              'X-Ubuntu-Gettext-Domain', 'app-install-data') + '.mo';
            if not FileExists(lp) then
              lp := '/usr/share/locale/de/' + GetLangID +
                '/LC_MESSAGES/' + d.ReadString('Desktop Entry',
                'X-Ubuntu-Gettext-Domain', 'app-install-data') + '.mo';
            if FileExists(lp) then
            begin
              dt := TMOFile.Create(lp);
              translate := true;
            end;
          finally
          end;

        end;

        if (pos('education', LowerCase(
          d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
          gr := gtEDUCATION
        else if (pos('office', LowerCase(
            d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
            gr := gtOFFICE
          else if (pos('development', LowerCase(
              d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
              gr := gtDEVELOPMENT
            else if (pos('graphic', LowerCase(
                d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                gr := gtGRAPHIC
              else if (pos('network', LowerCase(
                  d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                  gr := gtNETWORK
                else if (pos('game', LowerCase(
                    d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                    gr := gtGAMES
                  else if (pos('system', LowerCase(
                      d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                      gr := gtSYSTEM
                    else if (pos('audio', LowerCase(
                        d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                        gr := gtMULTIMEDIA
                      else if (pos('video', LowerCase(
                          d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                          gr := gtMULTIMEDIA
                        else if (pos('utils', LowerCase(
                            d.ReadString('Desktop Entry', 'Categories', ''))) > 0) then
                            gr := gtADDITIONAL
                          else
                            gr := gtOTHER;

        with entry do
        begin
          if d.ValueExists('Desktop Entry', 'Name[' + GetLangID + ']') then
            Name := PChar(d.ReadString('Desktop Entry', 'Name[' +
              GetLangID + ']', '<error>'))
          else
            Name := PChar(ldt(d.ReadString('Desktop Entry', 'Name', '<error>')));

          Name := PChar(StringReplace(Name, '&', '&&', [rfReplaceAll]));

          Category := gr;

          // instLst.Add(Lowercase(d.ReadString('Desktop Entry','Name','<error>')));

          if d.ValueExists('Desktop Entry', 'Comment[' + GetLangID + ']') then
            ShortDesc := PChar(d.ReadString('Desktop Entry', 'Comment[' +
              GetLangID + ']', ''))
          else
            ShortDesc := PChar(ldt(d.ReadString('Desktop Entry', 'Comment', '')));

          Author := PChar(rsAuthor + ': ' + d.ReadString(
            'Desktop Entry', 'X-Publisher', '<error>'));
          if Author = rsAuthor + ': ' + '<error>' then
            Author := '';
          Version := '';
          if d.ReadString('Desktop Entry', 'X-AppVersion', '') <> '' then
            Version := PChar(rsVersion + ': ' +
              d.ReadString('Desktop Entry', 'X-AppVersion', ''));

          entry.IconName := PChar(
            GetAppIconPath(d.ReadString('Desktop Entry', 'Icon', '')));

          if not FileExists(entry.IconName) then
          begin
            entry.IconName := '';
            msg(StrSubst(rsCannotLoadIcon, '%a', Name));
          end;
        end;
        newapp(fname, entry);
        //  if Assigned(dt) then dt.Free;
        if translate then
          dt.Free;

      end
      else
        msg(StrSubst(rsSkippedX, '%a', ExtractFileName(fname)));
    d.Free;
  end;

begin
  j := 0;

  msg(rsLoading);
  blst := TStringList.Create; //Create Blacklist

  if sumode then
    pdebug('SUMode: Enabled')
  else
    pdebug('SUMode: Disabled');

  db := TSoftwareDB.Create;
  db.Load(sumode);
  db.OnNewApp := FApp;

  if blst.Count < 4 then
  begin
    blst.Clear;
    blst.LoadFromFile(LI_CONFIG_DIR + 'blacklist');
    blst.Delete(0);
  end;

  db.GetApplicationList(blst);

{if (CBox.ItemIndex=0) or (CBox.ItemIndex=10) then
begin
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


  ini := TIniFile.Create(ConfigDir + 'config.cnf');

  //Search for other applications that are installed on this system...
  if SUMode then //Only if user wants to see shared apps
  begin
    tmp := FindAllFiles('/usr/share/applications/', '*.desktop', true);
    xtmp := FindAllFiles('/usr/local/share/applications/', '*.desktop', true);
    for i := 0 to xtmp.Count - 1 do
      tmp.Add(xtmp[i]);
    xtmp.Free;
  end
  else
    tmp := FindAllFiles(GetEnvironmentVariable('HOME') +
      '/.local/share/applications', '*.desktop', false);

  for i := 0 to tmp.Count - 1 do
  begin
    ProcessDesktopFile(tmp[i]);
  end;

  tmp.Free;
  ini.Free;

  //Check LOKI-success:
  if j > 100 then
    msg(rsLOKIError);

  msg(rsReady); //Loading list finished!

  db.Free;
  blst.Free; //Free blacklist
end;

//Uninstall Mojo and LOKI Setups
function TAppManager.UninstallMojo(dsk: String): Boolean;
var
  inf: TIniFile;
  tmp: TStringList;
  t: TProcess;
  mandir: String;
begin
  Result := true;
  pdebug('MoJo remover: dsk: ' + dsk);
  msg(rsPkgCouldBeInstalledWithLoki);
  inf := TIniFile.Create(dsk);
  if not DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry',
    'Exec', '?'))) then
  begin
    pwarning('Listaller cannot handle this installation!');
    request(rsCannotHandleRM, rqError);
    inf.Free;
  end
  else
    if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
      '.mojosetup') then
    begin
      //MOJO
      mandir := ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
        '.mojosetup';
      inf.Free;
      msg('Mojo manifest found.');
      setpos(40);
      tmp := TStringList.Create;
      tmp.Assign(FindAllFiles(mandir + '/manifest', '*.xml', false));
      if tmp.Count <= 0 then
        exit;
      setpos(50);
      msg(rsRemovingApp);
      t := TProcess.Create(nil);
      t.CommandLine := mandir + '/mojosetup uninstall ' + copy(
        ExtractFileName(tmp[0]), 1, pos('.', ExtractFileName(tmp[0])) - 1);
      t.Options := [poUsePipes, poWaitonexit];
      tmp.Free;
      setpos(60);
      t.Execute;
      t.Free;
      setpos(100);
    end
    else
    //LOKI
      if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
        '.manifest') then
      begin
        setpos(50);
        msg(rsLOKISetupFound);
        msg(rsRemovingApp);

        t := TProcess.Create(nil);
        t.CommandLine := ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
          '/uninstall';
        t.Options := [poUsePipes, poWaitonexit];

        setpos(60);
        t.Execute;
        t.Free;
        setpos(100);
      end
      else
      begin
        Result := false;
        perror('Listaller cannot handle this installation type!');
        request(rsCannotHandleRM, rqError);
        inf.Free;
      end;
end;

procedure TAppManager.DBusStatusChange(ty: LiProcStatus; Data: TLiProcData);
begin
  case Data.changed of
    pdMainProgress: setpos(Data.mnprogress);
    pdInfo: msg(Data.msg);
    pdError: request(Data.msg, rqError);
    pdStatus:
    begin
      sdata.lastresult := ty;
      if Assigned(FStatus) then
        FStatus(scStatus, sdata, statechange_udata);
    end;
  end;
end;

//Can remove Autopackage.org or native package.
// Only for interal use, called by UninstallApp.
// This function exists to speed up the removal process.
procedure TAppManager.InternalRemoveApp(obj: AppInfo);
var
  t: TProcess;
  pkit: TPackageKit;
  Name, id: String;
begin
  setpos(0);

  //Needed
  Name := obj.Name;
  id := obj.UId;

  if copy(id, 1, 4) <> 'pkg:' then
  begin
    msg(rsReadingAppInfo);

    if not FileExists(obj.UId) then
    begin
      if DirectoryExistsUTF8(RegDir + LowerCase(id)) then
      begin
        //Remove IPK app
        UninstallIPKApp(Name, id, FStatus, false);

        msg('Finished!');
        exit;
      end
      else
      begin
        request(rsAppRegistBroken, rqError);
        exit;
      end;

    end
    else
    begin //Autopackage
      if id[1] = '!' then
      begin
        t := TProcess.Create(nil);
        t.CommandLine := copy(obj.UId, 2, length(obj.Uid));
        t.Options := [poUsePipes, poWaitonexit];
        t.Execute;
        t.Free;
        exit;
      end
      else
      begin
        request(rsUnableToRemoveApp, rqError);
        exit;
      end;
    end;

  end
  else
  begin
    setpos(50);
    pkit := TPackageKit.Create;
    pkit.OnProgress := @PkitProgress;
    Name := copy(id, 5, length(id));
    msg(StrSubst(rsRMAppC, '%a', Name) + ' ...');
    pkit.RemovePkg(Name);

    if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
    begin
      request(rsRmError + #10 + rsEMsg + #10 + pkit.LastErrorMessage, rqError);
      pkit.Free;
      exit;
    end;

    setpos(100);
    msg(rsDone);
    pkit.Free;
    exit;
  end;

end;


//Initialize appremove: Detect rdepends if package is native, if package is native, add "pkg:" to
// identification string - if not, pkg has to be Loki/Mojo, so intitiate Mojo-Removal. After rdepends and pkg resolve is done,
// run uninstall as root if necessary. At the end, RemoveAppInternal() is called (if LOKI-Remove was not run) to uninstall
// native or autopackage setup.
procedure TAppManager.UninstallApp(obj: AppInfo);
var
  id: String;
  i: Integer;
  pkit: TPackageKit;
  tmp: TStringList;
  f, g: String;
  buscmd: ListallerBusCommand;
begin
  id := obj.UId;
  if id = '' then
  begin
    perror('Invalid application info passed: No ID found.');
    exit;
  end;
  SetState(prStarted);
  if (FileExists(id)) and (id[1] = '/') and (copy(id, 1, 4) <> 'pkg:') then
  begin
    ShowPKMon();

    msg(rsCallingPackageKitPKMonExecActions);
    msg(rsDetectingPackage);

    pkit := TPackageKit.Create;
    pkit.OnProgress := @PkitProgress;

    pkit.PkgNameFromFile(id, false); //!!! ,false for debugging
    setpos(20);

    while not pkit.Finished do ;

    if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
    begin
      request(PAnsiChar(rsPKitProbPkMon + #10 + rsEMsg + #10 +
        pkit.LastErrorMessage),
        rqError);
      pkit.Free;
      exit;
    end;

    tmp := TStringList.Create;
    for i := 0 to pkit.RList.Count - 1 do
      tmp.Add(pkit.RList[i].PackageId);

    if (tmp.Count > 0) then
    begin
      f := tmp[0];

      msg(StrSubst(rsPackageDetected, '%s', f));
      msg(rsLookingForRevDeps);

      tmp.Clear;

      setpos(18);
      pdebug('GetRequires()');
      pkit.GetRequires(f);

      setpos(25);
      g := '';

      for i := 0 to tmp.Count - 1 do
      begin
        pdebug(tmp[i]);
        g := g + #10 + tmp[i];
      end;

      pdebug('Asking dependency question...');
      pkit.Free;
      if (StringReplace(g, ' ', '', [rfReplaceAll]) = '') or
        (request(StringReplace(StringReplace(
        StringReplace(rsRMPkg, '%p', f, [rfReplaceAll]), '%a', obj.Name, [rfReplaceAll]),
        '%pl', PChar(g), [rfReplaceAll]), rqWarning) = rqsYes) then
        obj.UId := PChar('pkg:' + f)
      else
        exit;
    end;
    tmp.Free;
    pdebug('Done. ID is set.');

    //Important: ID needs to be the same as AppInfo.UId
    id := obj.UId;
  end;


  pdebug('Application UId is: ' + obj.UId);
  if (SUMode) and (not IsRoot) then
  begin
    //Create worker thread for this action
    buscmd.cmdtype := lbaUninstallApp;
    buscmd.appinfo := obj;
    with TLiDBusAction.Create(buscmd) do
    begin
      pdebug('DbusAction::run!');
      OnStatus := @DBusStatusChange;
      ExecuteAction;
      Free;
      SetState(prFinished);
    end;
    exit;
  end;

  if (id[1] = '/') then
    UninstallMojo(id)
  else
    InternalRemoveApp(obj);
  SetState(prFinished);
end;

function TAppManager.CheckApps(report: TStringList; const fix: Boolean = false;
  const forceroot: Boolean = false): Boolean;
var
  db: TSoftwareDB;
  app: AppInfo;
  deps: TStringList;
  i: Integer;
  pkit: TPackageKit;
begin
  Result := true;
  msg(rsCheckDepsRegisteredApps);
  if forceroot then
    msg(rsYouScanOnlyRootInstalledApps)
  else
    msg(rsYouScanOnlyLocalInstalledApps);

  db := TSoftwareDB.Create;
  if db.Load(forceroot) then
  begin
    deps := TStringList.Create;
    pkit := TPackageKit.Create;

    while not db.EndReached do
    begin
      app := db.DataField.App;
      writeLn(' Checking ' + app.Name);
      deps.Text := app.Dependencies;
      for i := 0 to deps.Count - 1 do
      begin
        pkit.ResolveInstalled(deps[i]);
        if pkit.PkExitStatus = PK_EXIT_ENUM_SUCCESS then
        begin
          if pkit.RList.Count > 0 then
            report.Add(deps[i] + ' found.')
          else
          begin
            report.Add(StrSubst(rsDepXIsNotInstall, '%s', deps[i]));
            Result := false;
            if fix then
            begin
              Write('  Repairing dependency ' + deps[i] + '  ');
              pkit.InstallPkg(deps[i]);
              writeLn(' [OK]');
              report.Add(StrSubst(rsInstalledDepX, '%s', deps[i]));
            end;
          end;
        end
        else
        begin
          request(rsPkQueryFailed + #10 + rsEMsg + #10 +
            pkit.LastErrorMessage, rqError);
          Result := false;
          exit;
        end;
      end;
      db.NextField;
    end;
    deps.Free;
    pkit.Free;
    db.Close;
  end
  else
    pdebug('No database found!');
  db.Free;
  writeLn('Check finished.');
  if not Result then
    writeLn('You have broken dependencies.');
end;

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

function IsPackageInstalled(aname: String; aid: String; sumode: Boolean): Boolean;
var
  db: TSoftwareDB;
begin
  if (aname = '') and (aid = '') then
  begin
    pwarning('Empty strings received for IsPackageInstalled() query.');
    Result := false;
    exit;
  end;
  db := TSoftwareDB.Create;
  if db.Load(sumode) then
    Result := db.AppExisting(aname, aId)
  else
    Result := false; //No database => no application installed
  db.Free;
end;

/////////////////////////////////////////////////////

procedure UninstallIPKApp(AppName, AppID: String; FStatus: StatusChangeEvent;
  fast: Boolean = false; RmDeps: Boolean = true);
var
  tmp, tmp2, slist: TStringList;
  p, f: String;
  i, j: Integer;
  k: Boolean;
  upd: String;
  proc: TProcess;
  dlink: Boolean;
  t: TProcess;
  pkit: TPackageKit;
  db: TSoftwareDB;
  mnprog: Integer;
  bs: Double;
  ipkc: TIPKControl;

  sdata: TLiStatusData;

  procedure SetPosition(prog: Double);
  begin
    sdata.mnprogress := Round(prog);
    if Assigned(FStatus) then
      FStatus(scMnProgress, sdata, nil);
  end;

  procedure msg(s: String);
  begin
    sdata.msg := PChar(s);
    if Assigned(FStatus) then
      FStatus(scMessage, sdata, nil)
    else
      pinfo(s);
  end;

begin
  p := RegDir + LowerCase(AppID) + '/';
  p := CleanFilePath(p);

  mnprog := 0;

  SetPosition(0);

  //Check if an update source was set
  ipkc := TIPKControl.Create(p + 'application');
  upd := ipkc.USource;
  ipkc.Free;

  msg(rsStartingUninstall);

  db := TSoftwareDB.Create;
  db.Load;
  msg(rsDBOpened);

  db.OpenFilterAppList;
  while not db.EndReached do
  begin
    if (db.DataField.App.Name = AppName) and (db.DataField.App.PkName = AppID) then
    begin

      if db.DataField.App.PkType = ptDLink then
        dlink := true
      else
        dlink := false;

      bs := 6;
      SetPosition(4);
      mnprog := 4;

      if not dlink then
      begin
        tmp := TStringList.Create;
        tmp.LoadFromfile(p + 'files.list');
        bs := (bs + tmp.Count) / 100;
      end;

      if not fast then
      begin
        if FileExists(p + 'prerm') then
        begin
          msg('PreRM-Script found.');
          t := TProcess.Create(nil);
          t.Options := [poUsePipes, poWaitonexit];
          t.CommandLine := FindBinary('chmod') + ' 775 ''' + p + 'prerm''';
          t.Execute;
          msg('Executing prerm...');
          t.CommandLine := '''' + p + 'prerm''';
          t.Execute;
          t.Free;
          msg('Done.');
        end;

        ///////////////////////////////////////
        if RmDeps then
        begin
          msg(rsRMUnsdDeps);
          tmp2 := TStringList.Create;
          tmp2.Text := db.DataField.App.Dependencies;

          if tmp2.Count > -1 then
          begin
            bs := (bs + tmp2.Count) / 100;
            for i := 0 to tmp2.Count - 1 do
            begin
              f := tmp2[i];
              //Skip catalog based packages - impossible to detect unneeded dependencies
              if pos('cat:', f) > 0 then
                break;

              //Asterisk (*) indicates that package was newly installed by installer
              if (LowerCase(f) <> 'libc6') and (f[1] <> '*') then
              begin
                //Check if another package requires this package
                t := TProcess.Create(nil);
                if pos(')', f) > 0 then
                  msg(f + ' # ' + copy(f, pos(' (', f) + 2, length(f) -
                    pos(' (', f) - 2))
                else
                  msg(f);

                pkit := TPackageKit.Create;

                if pos(')', f) > 0 then
                  pkit.GetRequires(copy(f, pos(' (', f) + 2, length(f) -
                    pos(' (', f) - 2))
                else
                  pkit.GetRequires(f);

                if pkit.RList.Count <= 1 then
                begin
                  if pos(')', f) > 0 then
                    pkit.RemovePkg(copy(f, pos(' (', f) + 2, length(f) -
                      pos(' (', f) - 2))
                  else
                    pkit.RemovePkg(f);
                  //GetOutPutTimer.Enabled:=true;

                  msg('Removing ' + f + '...');
                end;

                pkit.Free;
              end;
              Inc(mnprog);
              SetPosition(bs * mnprog);
            end; //End of tmp2-find loop

          end
          else
            msg('No installed deps found!');

          tmp2.Free;
        end; //End of remove-deps request
      end; //End of "fast"-request
      //////////////////////////////////////////////////////

      if not dlink then
      begin
        slist := TStringList.Create;


        //Undo Mime-registration (if necessary)
        for i := 0 to tmp.Count - 1 do
        begin
          if pos('<mime>', tmp[i]) > 0 then
          begin
            msg('Uninstalling MIME-Type "' + ExtractFileName(tmp[i]) + '" ...');
            t := TProcess.Create(nil);
            if (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i]))) = '.png') or
              (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i]))) = '.xpm') then
            begin
              t.CommandLine :=
                FindBinary('xdg-icon-resource') + ' uninstall ' +
                SysUtils.ChangeFileExt(ExtractFileName(DeleteModifiers(tmp[i])), '');
              t.Execute;
            end
            else
            begin
              t.CommandLine :=
                FindBinary('xdg-mime') + ' uninstall ' + DeleteModifiers(
                f + '/' + ExtractFileName(tmp[i]));
              t.Execute;
            end;
            t.Free;
          end;
        end;

        msg('Removing files...');
        //Uninstall application
        for i := 0 to tmp.Count - 1 do
        begin

          f := SyblToPath(tmp[i]);

          f := DeleteModifiers(f);

          k := false;
          for j := 0 to slist.Count - 1 do
            if f = slist[j] then
              k := true;

          if not k then
            DeleteFile(f);

          Inc(mnprog);
          SetPosition(bs * mnprog);
        end;

        Inc(mnprog);
        SetPosition(bs * mnprog);

        msg('Removing empty dirs...');
        tmp.LoadFromFile(p + 'dirs.list');
        proc := TProcess.Create(nil);
        proc.Options := [poWaitOnExit, poStdErrToOutPut, poUsePipes];
        for i := 0 to tmp.Count - 1 do
        begin
          proc.CommandLine := FindBinary('rm') + ' -rf ' + tmp[i];
          proc.Execute;
        end;
        proc.Free;

        if upd <> '#' then
        begin
          CreateUpdateSourceList(RegDir);
          if FileExists(RegDir + 'updates.list') then
          begin
            tmp.LoadFromFile(RegDir + 'updates.list');
            msg('Removing update-source...');
            for i := 1 to tmp.Count - 1 do
              if pos(upd, tmp[i]) > 0 then
              begin
                tmp.Delete(i);
                break;
              end;
            tmp.SaveToFile(RegDir + 'updates.list');
            tmp.Free;
          end;
        end;

      end;

    end;
    db.NextField;
  end;

  if mnprog > 0 then
  begin
    msg('Unregistering...');

    db.AppDeleteCurrent;
    db.Close;

    proc := TProcess.Create(nil);
    proc.Options := [poWaitOnExit];
    proc.CommandLine := FindBinary('rm') + ' -rf ' + '''' +
      ExcludeTrailingBackslash(p) + '''';
    proc.Execute;
    proc.Free;

    Inc(mnprog);
    SetPosition(bs * mnprog);

    msg('Application removed.');
    msg('- Finished -');
  end
  else
    msg('Application not found!');
  db.Free;
end;

procedure CreateUpdateSourceList(path: String);
var
  fi: TStringList;
begin
  if not FileExists(path + 'updates.list') then
  begin
    ForceDirectories(path);
    fi := TStringList.Create;
    fi.Add('List of update repositories v.1.0');
    fi.SaveToFile(path + 'updates.list');
    fi.Free;
  end;
end;

end.

