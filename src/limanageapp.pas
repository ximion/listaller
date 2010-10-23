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
  Classes, GetText, LiTypes, LiUtils, MTProcs,
  PkTypes, Process, IniFiles, SysUtils, IPKCDef10, strLocale,
  liDBusProc, LiFileUtil, PackageKit, SoftwareDB, AppInstallDB;

type
  TDesktopData = record
    Name: String;
    Categories: String;
    IconName: String;
    SDesc: String;
    Author: String;
    Version: String;
    FName: String;
  end;

  TLiAppManager = class
  private
    SUMode: Boolean;
    FReq: UserRequestCall;
    FApp: NewAppEvent;
    FStatus: StatusChangeEvent;

    //State data
    sdata: LiStatusData; //Contains the current progress

    procedure msg(s: String);
    function  EmitRequest(s: String; ty: LiRqType): LiRqResult;
    procedure EmitNewApp(s: String; oj: LiAppInfo);
    procedure EmitStateChange(state: LiProcStatus);
    procedure EmitPosChange(i: Integer);

    function IsInList(nm: String; list: TStringList): Boolean;
    //** Method that removes MOJO/LOKI installed applications @param dsk Path to the .desktop file of the application
    function UninstallMojo(dsk: String): Boolean;
    //** Catch the PackageKit progress
    procedure PkitProgress(pos: Integer; xd: Pointer);
    //** Catch status messages from DBus action
    procedure DBusStatusChange(ty: LiProcStatus; data: TLiProcData);
    procedure InternalRemoveApp(obj: LiAppInfo);
  protected
    //Some user data for callbacks
    statechange_udata: Pointer;
    request_udata: Pointer;
    newapp_udata: Pointer;
    //** ReadIn .desktop files
    function ReadDesktopFile(fname: String): TDesktopData;
  public
    constructor Create;
    destructor Destroy; override;
    //** Rescann all apps installed on the system
    procedure RescanEntries;
    //** Update AppInstall database
    procedure UpdateAppDB;
    (** Removes an IPK application (can _only_ remove IPK apps)
     @param AppName Name of the application, that should be uninstalled
     @param AppID ID of the application
     @param fast Does a quick uninstallation if is true (Set to "False" by default)
     @param RmDeps Remove dependencies if true (Set to "True" by default)
     *)
    procedure UninstallIPKApp(AppName, AppID: String;fast: Boolean = false; RmDeps: Boolean = true);
    //** Removes an application
    procedure UninstallApp(obj: LiAppInfo);
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

//** Checks if package is installed
function IsPackageInstalled(aName: String = ''; aID: String = '';
  sumode: Boolean = false): Boolean;

//** Helper procedure to create USource file if missing
procedure CreateUpdateSourceList(path: String);

implementation

{ TLiAppManager }

constructor TLiAppManager.Create;
begin
  inherited Create;
  FApp := nil;
  FStatus := nil;
  FReq := nil;
end;

destructor TLiAppManager.Destroy;
begin
  inherited;
end;

function TLiAppManager.UserRequestRegistered: Boolean;
begin
  if Assigned(FReq) then
    Result := true
  else
    Result := false;
end;

procedure TLiAppManager.RegOnStatusChange(call: StatusChangeEvent; data: Pointer);
begin
  if CheckPtr(call, 'StatusChangeEvent') then
  begin
    FStatus := call;
    statechange_udata := data;
  end;
end;

procedure TLiAppManager.RegOnRequest(call: UserRequestCall; data: Pointer);
begin
  if CheckPtr(call, 'UserRequestCall') then
  begin
    FReq := call;
    request_udata := data;
  end;
end;

procedure TLiAppManager.RegOnNewApp(call: NewAppEvent; data: Pointer);
begin
  if CheckPtr(call, 'StatusChangeEvent') then
  begin
    FApp := call;
    newapp_udata := data;
  end;
end;

procedure TLiAppManager.Msg(s: String);
begin
  sdata.msg := PChar(s);
  if Assigned(FStatus) then
    FStatus(scMessage, sdata, statechange_udata);
end;

function TLiAppManager.EmitRequest(s: String; ty: LiRqType): LiRqResult;
begin
  if Assigned(FReq) then
    Result := FReq(ty, PChar(s), request_udata);
end;

procedure TLiAppManager.EmitNewApp(s: String; oj: LiAppInfo);
begin
  if Assigned(FApp) then
    FApp(PChar(s), @oj, newapp_udata);
end;

procedure TLiAppManager.EmitPosChange(i: Integer);
begin
  sdata.mnprogress := i;
  if Assigned(FStatus) then
    FStatus(scMnProgress, sdata, statechange_udata);
end;

procedure TLiAppManager.EmitStateChange(state: LiProcStatus);
begin
  sdata.lastresult := state;
  if Assigned(FStatus) then
    FStatus(scStatus, sdata, statechange_udata);
end;

function TLiAppManager.IsInList(nm: String; list: TStringList): Boolean;
begin
  Result := list.IndexOf(nm) > -1;
end;

procedure TLiAppManager.PkitProgress(pos: Integer; xd: Pointer);
begin
  //User defindes pointer xd is always nil here
  EmitPosChange(pos);
end;

procedure TLiAppManager.RescanEntries;
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
    entry: LiAppInfo;
    dt: TMOFile;
    lp: String;
    translate: Boolean; //Used, because Assigned(dt) throws an AV
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
          entry.RemoveId := PChar('!' + d.ReadString('Desktop Action Apkg-Remove',
            'Exec', ''))
        else
          entry.RemoveId := PChar(fname);

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

        with entry do
        begin
          if d.ValueExists('Desktop Entry', 'Name[' + GetLangID + ']') then
            Name := PChar(d.ReadString('Desktop Entry', 'Name[' +
              GetLangID + ']', '<error>'))
          else
            Name := PChar(ldt(d.ReadString('Desktop Entry', 'Name', '<error>')));

          Name := PChar(StringReplace(Name, '&', '&&', [rfReplaceAll]));

          Categories := PChar(d.ReadString('Desktop Entry', 'Categories', ''));

          // instLst.Add(Lowercase(d.ReadString('Desktop Entry','Name','<error>')));

          if d.ValueExists('Desktop Entry', 'Comment[' + GetLangID + ']') then
            Summary := PChar(d.ReadString('Desktop Entry', 'Comment[' +
              GetLangID + ']', ''))
          else
            Summary := PChar(ldt(d.ReadString('Desktop Entry', 'Comment', '')));

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
        EmitNewApp(fname, entry);
        //  if Assigned(dt) then dt.Free;
        if translate then
          dt.Free;

      end
      else
        msg(StrSubst(rsSkippedX, '%a', ExtractFileName(fname)));
    d.Free;
  end;

begin
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

  db.GetApplicationList(fAllApps, blst);


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

  msg(rsReady); //Loading list finished!

  db.Free;
  blst.Free; //Free blacklist
end;

//Read information about an app from .desktop file
function TLiAppManager.ReadDesktopFile(fname: String): TDesktopData;
var
  d: TIniFile;
  data: TDesktopData;
begin
  d := TIniFile.Create(fname);
  Result.Name := '';
  data.Name := '';

  //Check for apps which should not be displayed
  if (LowerCase(d.ReadString('Desktop Entry', 'NoDisplay', '')) <> 'true') and
    (pos('yast', LowerCase(fname)) <= 0) and
    (LowerCase(d.ReadString('Desktop Entry', 'Hidden', 'false')) <> 'true')
    // and(pos('system',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
    and (pos('core', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <=
    0) and (pos('.hidden', LowerCase(d.ReadString('Desktop Entry',
    'Categories', ''))) <= 0)
    // and(pos('base',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
    and (pos('wine', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <=
    0) and (pos('wine', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <=
    0) and (d.ReadString('Desktop Entry', 'X-KDE-ParentApp', '#') = '#') and
    (pos('screensaver', LowerCase(d.ReadString('Desktop Entry', 'Categories', ''))) <=
    0) and (pos('setting', LowerCase(d.ReadString('Desktop Entry',
    'Categories', ''))) <= 0)
    // and(pos('utility',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
    and (d.ReadString('Desktop Entry', 'OnlyShowIn', '') = '') and
    (d.ReadString('Desktop Entry', 'X-AllowRemove', 'true') = 'true') then
  begin

    //NOTE: We skip Ubuntu-specific GetText stuff at time
        { if d.ReadString('Desktop Entry', 'X-Ubuntu-Gettext-Domain', '') <> '' then
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
        end; }

    data.Categories := d.ReadString('Desktop Entry', 'Categories', '');

    data.Name := d.ReadString('Desktop Entry', 'Name', '<error>');
    data.SDesc := d.ReadString('Desktop Entry', 'Comment', '');
    //Listaller-specific extra data
    data.Author := d.ReadString('Desktop Entry', 'X-Publisher', '');
    data.Version := d.ReadString('Desktop Entry', 'X-AppVersion', '');

    data.IconName := d.ReadString('Desktop Entry', 'Icon', '');
  end;

  d.Free;
  Result := data;
end;

//Update AppInstall database
procedure TLiAppManager.UpdateAppDB;
var
  tmp, xtmp: TStringList;
  i: Integer;
  ddata: TDesktopData;
  data: LiAppInfo;
  appID: String;
  appRmID: String;
  sdb: TSoftwareDB;
begin
  if (sumode) and (not IsRoot) then
  begin
    pwarning('Cannot update AppDB without beeing root!');
    exit;
  end;
  // Search for .desktop files
  tmp := FindAllFiles('/usr/share/applications/', '*.desktop', true);
  xtmp := FindAllFiles('/usr/local/share/applications/', '*.desktop', true);
  for i := 0 to xtmp.Count - 1 do
    tmp.Add(xtmp[i]);
  xtmp.Free;

  sdb := TSoftwareDB.Create;
  //Open the appinstall databases
  sdb.Load(SUMode);
  // Update the database
  for i := 0 to tmp.Count - 1 do
  begin
    ddata := ReadDesktopFile(tmp[i]);
    appID := StrSubst(ExtractFileName(tmp[i]), '.desktop', '');
    if ddata.Name = '' then
      Continue;
    //If not already in list, add it
    if sdb.AppExists(appID) then
      Continue
    else
    begin
      pdebug('AppID: '+appID);
      appRmId := GenerateAppID(tmp[i]);
      //Build a new AppInfo record
      data.Name:=PChar(ddata.Name);
      data.RemoveId:=PChar(appRmId);
      data.PkName:=PChar(appID);
      data.PkType:=ptExtern;
      data.Categories:=PChar(ddata.Categories);
      data.IconName:=PChar(ddata.IconName);
      data.Summary:=PChar(ddata.SDesc);
      sdb.AppAddNew(data);
    end;
  end;
  tmp.Free;
  sdb.Finalize; //Write to disk
  sdb.Free;
end;

//Uninstall Mojo and LOKI Setups
function TLiAppManager.UninstallMojo(dsk: String): Boolean;
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
    EmitRequest(rsCannotHandleRM, rqError);
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
      EmitPosChange(40);
      tmp := TStringList.Create;
      tmp.Assign(FindAllFiles(mandir + '/manifest', '*.xml', false));
      if tmp.Count <= 0 then
        exit;
      EmitPosChange(50);
      msg(rsRemovingApp);
      t := TProcess.Create(nil);
      t.CommandLine := mandir + '/mojosetup uninstall ' + copy(
        ExtractFileName(tmp[0]), 1, pos('.', ExtractFileName(tmp[0])) - 1);
      t.Options := [poUsePipes, poWaitonexit];
      tmp.Free;
      EmitPosChange(60);
      t.Execute;
      t.Free;
      EmitPosChange(100);
    end
    else
    //LOKI
      if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
        '.manifest') then
      begin
        EmitPosChange(50);
        msg(rsLOKISetupFound);
        msg(rsRemovingApp);

        t := TProcess.Create(nil);
        t.CommandLine := ExtractFilePath(inf.ReadString('Desktop Entry', 'Exec', '?')) +
          '/uninstall';
        t.Options := [poUsePipes, poWaitonexit];

        EmitPosChange(60);
        t.Execute;
        t.Free;
        EmitPosChange(100);
      end
      else
      begin
        Result := false;
        perror('Listaller cannot handle this installation type!');
        EmitRequest(rsCannotHandleRM, rqError);
        inf.Free;
      end;
end;

procedure TLiAppManager.DBusStatusChange(ty: LiProcStatus; Data: TLiProcData);
begin
  case Data.changed of
    pdMainProgress: EmitPosChange(Data.mnprogress);
    pdInfo: msg(Data.msg);
    pdError: EmitRequest(Data.msg, rqError);
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
procedure TLiAppManager.InternalRemoveApp(obj: LiAppInfo);
var
  t: TProcess;
  pkit: TPackageKit;
  Name, id: String;
begin
  EmitPosChange(0);

  //Needed
  Name := obj.Name;
  id := obj.RemoveId;

  if copy(id, 1, 4) <> 'pkg:' then
  begin
    msg(rsReadingAppInfo);

    if not FileExists(obj.RemoveId) then
    begin
      if DirectoryExistsUTF8(PkgRegDir + LowerCase(id)) then
      begin
        //Remove IPK app
        UninstallIPKApp(Name, id, false);

        msg('Finished!');
        exit;
      end
      else
      begin
        EmitRequest(rsAppRegistBroken, rqError);
        exit;
      end;

    end
    else
    begin //Autopackage
      if id[1] = '!' then
      begin
        t := TProcess.Create(nil);
        t.CommandLine := copy(obj.RemoveId, 2, length(obj.RemoveId));
        t.Options := [poUsePipes, poWaitonexit];
        t.Execute;
        t.Free;
        exit;
      end
      else
      begin
        EmitRequest(rsUnableToRemoveApp, rqError);
        exit;
      end;
    end;

  end
  else
  begin
    EmitPosChange(50);
    pkit := TPackageKit.Create;
    pkit.OnProgress := @PkitProgress;
    Name := copy(id, 5, length(id));
    msg(StrSubst(rsRMAppC, '%a', Name) + ' ...');
    pkit.RemovePkg(Name);

    if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
    begin
      EmitRequest(rsRmError + #10 + rsEMsg + #10 + pkit.LastErrorMessage, rqError);
      pkit.Free;
      exit;
    end;

    EmitPosChange(100);
    msg(rsDone);
    pkit.Free;
    exit;
  end;

end;

procedure TLiAppManager.UninstallIPKApp(AppName, AppID: String;fast: Boolean = false; RmDeps: Boolean = true);
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
begin
  p := PkgRegDir + LowerCase(AppID) + '/';
  p := CleanFilePath(p);

  mnprog := 0;

  EmitPosChange(0);

  //Check if an update source was set
  ipkc := TIPKControl.Create(p + 'application');
  upd := ipkc.USource;
  ipkc.Free;

  msg(rsStartingUninstall);

  db := TSoftwareDB.Create;
  db.Load;
  msg(rsDBOpened);

  db.OpenFilter(fAllApps);
  while not db.EndReached do
  begin
    if (db.CurrentDataField.App.Name = AppName) and (db.CurrentDataField.App.PkName = AppID) then
    begin

      if db.CurrentDataField.App.PkType = ptDLink then
        dlink := true
      else
        dlink := false;

      bs := 6;
      EmitPosChange(4);
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
          tmp2.Text := db.CurrentDataField.App.Dependencies;

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
              EmitPosChange(round(bs * mnprog));
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
          EmitPosChange(round(bs * mnprog));
        end;

        Inc(mnprog);
        EmitPosChange(round(bs * mnprog));

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
          CreateUpdateSourceList(PkgRegDir);
          if FileExists(PkgRegDir + 'updates.list') then
          begin
            tmp.LoadFromFile(PkgRegDir + 'updates.list');
            msg('Removing update-source...');
            for i := 1 to tmp.Count - 1 do
              if pos(upd, tmp[i]) > 0 then
              begin
                tmp.Delete(i);
                break;
              end;
            tmp.SaveToFile(PkgRegDir + 'updates.list');
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

    db.DeleteCurrentField;
    db.CloseFilter;

    proc := TProcess.Create(nil);
    proc.Options := [poWaitOnExit];
    proc.CommandLine := FindBinary('rm') + ' -rf ' + '''' +
      ExcludeTrailingBackslash(p) + '''';
    proc.Execute;
    proc.Free;

    Inc(mnprog);
    EmitPosChange(round(bs * mnprog));

    msg('Application removed.');
    msg('- Finished -');
  end
  else
    msg('Application not found!');
  db.Free;
end;

//Initialize appremove: Detect rdepends if package is native, if package is native, add "pkg:" to
// identification string - if not, pkg has to be Loki/Mojo, so intitiate Mojo-Removal. After rdepends and pkg resolve is done,
// run uninstall as root if necessary. At the end, RemoveAppInternal() is called (if LOKI-Remove was not run) to uninstall
// native or autopackage setup.
procedure TLiAppManager.UninstallApp(obj: LiAppInfo);
var
  id: String;
  i: Integer;
  pkit: TPackageKit;
  tmp: TStringList;
  f, g: String;
  buscmd: ListallerBusCommand;
begin
  id := obj.RemoveId;
  if id = '' then
  begin
    perror('Invalid application info passed: No ID found.');
    exit;
  end;
  EmitStateChange(prStarted);
  if (FileExists(id)) and (id[1] = '/') and (copy(id, 1, 4) <> 'pkg:') then
  begin
    ShowPKMon();

    msg(rsCallingPackageKitPKMonExecActions);
    msg(rsDetectingPackage);

    pkit := TPackageKit.Create;
    pkit.OnProgress := @PkitProgress;

    pkit.PkgNameFromFile(id, false); //!!! ,false for debugging
    EmitPosChange(20);

    while not pkit.Finished do ;

    if pkit.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
    begin
      EmitRequest(PAnsiChar(rsPKitProbPkMon + #10 + rsEMsg + #10 +
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

      EmitPosChange(18);
      pdebug('GetRequires()');
      pkit.GetRequires(f);

      EmitPosChange(25);
      g := '';

      for i := 0 to tmp.Count - 1 do
      begin
        pdebug(tmp[i]);
        g := g + #10 + tmp[i];
      end;

      pdebug('Asking dependency question...');
      pkit.Free;
      if (StringReplace(g, ' ', '', [rfReplaceAll]) = '') or
        (EmitRequest(StringReplace(StringReplace(
        StringReplace(rsRMPkg, '%p', f, [rfReplaceAll]), '%a', obj.Name, [rfReplaceAll]),
        '%pl', PChar(g), [rfReplaceAll]), rqWarning) = rqsYes) then
        obj.RemoveId := PChar('pkg:' + f)
      else
        exit;
    end;
    tmp.Free;
    pdebug('Done. ID is set.');

    //Important: ID needs to be the same as AppInfo.RemoveId
    id := obj.RemoveId;
  end;


  pdebug('Application UId is: ' + obj.RemoveId);
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
      EmitStateChange(prFinished);
    end;
    exit;
  end;

  if (id[1] = '/') then
    UninstallMojo(id)
  else
    InternalRemoveApp(obj);
  EmitStateChange(prFinished);
end;

function TLiAppManager.CheckApps(report: TStringList; const fix: Boolean = false;
  const forceroot: Boolean = false): Boolean;
var
  db: TSoftwareDB;
  app: LiAppInfo;
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
      app := db.CurrentDataField.App;
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
          EmitRequest(rsPkQueryFailed + #10 + rsEMsg + #10 +
            pkit.LastErrorMessage, rqError);
          Result := false;
          exit;
        end;
      end;
      db.NextField;
    end;
    deps.Free;
    pkit.Free;
    db.CloseFilter;
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
    Result := db.AppExists(aId)
  else
    Result := false; //No database => no application installed
  db.Free;
end;

/////////////////////////////////////////////////////

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
