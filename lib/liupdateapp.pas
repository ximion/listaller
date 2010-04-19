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
//** Functions to update applications
unit liupdateapp;

{$mode objfpc}{$H+}

interface

uses
  Blcksock, Classes, Contnrs, FileUtil, FTPSend, HTTPSend, IniFiles,
  ipkdef, ipkPackage, liUtils, liDBusProc, liManageApp, liTypes, MD5,
  Process, SqLite3DS, SysUtils, strLocale;

type

  TUpdateInfo = class
  public
    AppName: String;
    NVersion: String;
    Oversion: String;
    ID: String;
    updid: Integer;
    desc: TStringList;
    files: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  PAppUpdater = ^TAppUpdater;

  TAppUpdater = class
  private
    SUMode: Boolean;
    HTTP: THTTPSend;
    FTP: TFTPSend;
    AppReg: String;
    FReq: TRequestCall;
    FStatus: TLiStatusChangeCall;
    FNewUpd: TNewUpdateEvent;

    ulist: TObjectList;
    sdata: TLiStatusData;

    //User data
    statechangeudata, requestudata, newupdudata: Pointer;

    procedure SetMnPos(i: Integer);
    procedure SetExPos(i: Integer);
    procedure Msg(s: String);
    procedure StepMsg(s: String);
    function Request(s: String; ty: TRqType): TRqResult;
    procedure NewUpdate(nm: String; id: Integer);
    function ValidUpdateId(uid: Integer): Boolean;
    //Hook on HTTP socket
    procedure HookSock(Sender: TObject; Reason: THookSocketReason; const Value: String);
    //Catch DBus messages
    procedure DBusStatusChange(ty: LiProcStatus; Data: TLiProcData);
  public
    constructor Create;
    destructor Destroy; override;

    function CheckUpdates: Boolean;
    procedure SetSumode(su: Boolean);
    procedure RegOnStatusChange(call: TLiStatusChangeCall; Data: Pointer);
    procedure RegOnRequest(call: TRequestCall; Data: Pointer);
    procedure RegOnNewUpdate(call: TNewUpdateEvent; Data: Pointer);

    function UpdateIDGetNewVersion(uid: Integer): String;
    function UpdateIDGetOldVersion(uid: Integer): String;

    function ExecuteUpdate(uid: Integer): Boolean;
  end;

implementation

{ TUpdateInfo }

constructor TUpdateInfo.Create;
begin
  desc := TStringList.Create;
  files := TStringList.Create;
end;

destructor TUpdateInfo.Destroy;
begin
  desc.Free;
  files.Free;
end;

{ TAppUpdater }

constructor TAppUpdater.Create;
begin
  HTTP := THTTPSend.Create;
  FTP := TFTPSend.Create;
  HTTP.Sock.OnStatus := @HookSock;
  ulist := TObjectList.Create(true);
  HTTP.UserAgent := 'Listaller-Update';
  SetSuMode(false);
end;

destructor TAppUpdater.Destroy;
begin
  ulist.Free;
  HTTP.Free;
  FTP.Free;
  inherited;
end;

procedure TAppUpdater.SetSumode(su: Boolean);
begin
  SUMode := su;
  if SUMode then
    AppReg := LI_CONFIG_DIR + LI_APPDB_PREF
  else
    AppReg := SyblToPath('$INST') + '/' + LI_APPDB_PREF;
  ulist.Clear;
end;

procedure TAppUpdater.RegOnStatusChange(call: TLiStatusChangeCall; Data: Pointer);
begin
  FStatus := call;
  statechangeudata := Data;
end;

procedure TAppUpdater.RegOnRequest(call: TRequestCall; Data: Pointer);
begin
  FReq := call;
  requestudata := Data;
end;

procedure TAppUpdater.RegOnNewUpdate(call: TNewUpdateEvent; Data: Pointer);
begin
  FNewUpd := call;
  newupdudata := Data;
end;

procedure TAppUpdater.Msg(s: String);
begin
  sdata.msg := PChar(s);
  if Assigned(FStatus) then
    FStatus(scMessage, sdata, statechangeudata);
end;

procedure TAppUpdater.StepMsg(s: String);
begin
  sdata.msg := PChar(s);
  if Assigned(FStatus) then
    FStatus(scStepMessage, sdata, statechangeudata);
end;

function TAppUpdater.Request(s: String; ty: TRqType): TRqResult;
begin
  if Assigned(FReq) then
    Result := FReq(ty, PChar(s), requestudata);
end;

procedure TAppUpdater.NewUpdate(nm: String; id: Integer);
begin
  if Assigned(FNewUpd) then
    FNewUpd(PChar(nm), id, newupdudata);
end;

procedure TAppUpdater.SetMnPos(i: Integer);
begin
  sdata.mnprogress := i;
  if Assigned(FStatus) then
    FStatus(scMnprogress, sdata, statechangeudata);
end;

procedure TAppUpdater.SetExPos(i: Integer);
begin
  sdata.exprogress := i;
  if Assigned(FStatus) then
    FStatus(scExProgress, sdata, statechangeudata);
end;

procedure TAppUpdater.HookSock(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
begin
  if HTTP.DownloadSize > 20 then
  begin
    SetExPos(Round(100 / HTTP.DownloadSize * HTTP.Document.Size));
  end;
end;

function TAppUpdater.CheckUpdates: Boolean;
var
  tmp, h, sinfo, sources: TStringList;
  j, k: Integer;
  ok: Boolean;
  p: String;
  progpos: Integer;
  ui: TUpdateInfo;
  control: TIPKControl;
  dsApp: TSQLite3Dataset; //AppDB connection
  max: Integer;
begin
  if ulist.Count > 0 then
  begin
    p_error('Already searched for new updates. Object has to be cleaned before performing new search!');
    Result := false;
    exit;
  end;
  tmp := TStringList.Create;
  h := TStringList.Create;
  sinfo := TStringList.Create;
  sources := TStringList.Create;

  progpos := 0;

  if not FileExists(AppReg + 'updates.list') then
  begin
    Result := false;
    exit;
  end;

  h.LoadFromFile(AppReg + 'updates.list');
  if h.Count = 1 then
  begin
    request(rsNoUpdates, rqInfo);
    Result := false;
    exit;
  end;

  for k := 1 to h.Count - 1 do
  begin
    if pos(' (', h[k]) > 0 then
    begin
      if h[k][1] = '-' then
        sources.Add(copy(h[k], 2, pos(' (', h[k]) - 2));
    end
    else
    begin
      if h[k][1] = '-' then
        sources.Add(copy(h[k], 2, length(h[k])));
    end;
  end;
  max := sources.Count * 2;
  h.Free;

  dsApp := TSQLite3Dataset.Create(nil);
  LoadAppDB(dsApp, sumode);
  msg('Software database opened.');

  dsApp.Active := true;

  dsApp.SQL := 'SELECT * FROM AppInfo';
  dsApp.Open;
  dsApp.Filtered := true;
  dsApp.Edit;

  for k := 0 to sources.Count - 1 do
  begin
    HTTP.Clear;
    HTTP.HTTPMethod('GET', sources[k] + '/' + 'source.pin');
    tmp.LoadFromStream(HTTP.Document);
    Inc(progpos);
    SetMnPos(Round(100 / max * progpos));

    if not DirectoryExists(tmpdir) then
      ForceDirectories(tmpdir);

    if (tmp.Count <= 0) or (pos('ipk-standard', LowerCase(tmp[0])) <= 0) then
      break;
    tmp.SaveToFile(TMPDIR + 'source0.pin');

    control := TIPKControl.Create(TMPDIR + 'source0.pin');

    ok := false;

    dsApp.First;
    while not dsApp.EOF do
    begin
      if (control.PkName = dsApp.FieldByName('PkName').AsString) then
      begin
        ok := true;
        break;
      end;
      dsApp.Next;
    end;

    if ok then
    begin
      HTTP.Clear;
      HTTP.HTTPMethod('GET', sources[k] + '/' + 'content.id');
      sleep(4);
      sinfo.LoadFromStream(HTTP.Document);

      ui := TUpdateInfo.Create;
      p := '??';
      for j := 0 to sinfo.Count - 1 do
        if length(sinfo[j]) > 0 then
        begin
          if sinfo[j][1] = '>' then
            p := copy(sinfo[j], 2, length(sinfo[j]))
          else
            if (sinfo[j][1] = '.') or (sinfo[j][1] = '/') then
            begin
              if sinfo[j + 1] <> MDPrint(
                (MD5.MD5File(CleanFilePath(DeleteModifiers(SyblToPath(p) + '/' + ExtractFileName(sinfo[j]))),
                1024))) then
              begin
                ui.files.Add(copy(sources[k], 2, length(sources[k])) + CleanFilePath(
                  '/' + SyblToX(p) + '/' + ExtractFileName(sinfo[j]) + '.xz'));
                ui.files.Add(SyblToPath(
                  CleanFilePath(SyblToPath(p) + '/' + ExtractFileName(sinfo[j]))));
              end;
            end;
        end;

      ulist.Add(ui);
      ui.AppName := control.AppName;
      ui.NVersion := control.AppVersion;
      ui.ID := control.PkName;
      Inc(progpos);
      SetMnPos(Round(100 / max * progpos));

      if ui.files.Count > 0 then
      begin
        ui.updid := ulist.Count - 1;
        NewUpdate(ui.AppName, ui.updid);
      end
      else
        ui.Free;
    end;
    control.Free;
  end;

  tmp.Free;
  sinfo.Free;

  dsApp.Free;

  if ulist.Count <= 0 then
    request(rsNoUpdates, rqInfo);
end;

function TAppUpdater.ValidUpdateId(uid: Integer): Boolean;
begin
  Result := true;
  if (uid > ulist.Count - 1) or (uID < 0) then
  begin
    p_error('Invalid update ID received. (This may be a bug in the application using libInstaller)');
    Result := false;
  end;
end;

function TAppUpdater.UpdateIDGetNewVersion(uid: Integer): String;
begin
  Result := '?';
  if not ValidUpdateId(uid) then
    exit;
  Result := TUpdateInfo(ulist[uid]).NVersion;
end;

function TAppUpdater.UpdateIDGetOldVersion(uid: Integer): String;
begin
  Result := '?';
  if not ValidUpdateId(uid) then
    exit;
  Result := TUpdateInfo(ulist[uid]).OVersion;
end;

procedure TAppUpdater.DBusStatusChange(ty: LiProcStatus; Data: TLiProcData);
begin
  case Data.changed of
    pdMainProgress: SetMnPos(Data.mnprogress);
    pdExtraProgress: SetExPos(Data.exprogress);
    pdInfo: msg(Data.msg);
    pdError: request(Data.msg, rqError);
    pdStatus:
    begin
      sdata.lastresult := ty;
      if Assigned(FStatus) then
        FStatus(scStatus, sdata, statechangeudata);
    end;
  end;
end;

function TAppUpdater.ExecuteUpdate(uid: Integer): Boolean;
var
  i, k: Integer;
  xz: TLiUpdateBit;
  c: TProcess;
  dsk: TIniFile;
  s: TStringList;
  buscmd: ListallerBusCommand;
  prog, max: Integer; //To set progress bar position
  xh, tmp: String;
  files: TStringList;
  dsApp: TSQLite3Dataset; //AppDB connection
begin
  Result := true;
  if not ValidUpdateId(uid) then
    exit;

  if (SUMode) and (not IsRoot) then
  begin
    //Create worker thread for this action
    buscmd.cmdtype := lbaUninstallApp;
    buscmd.updid := uid;
    with TLiDBusAction.Create(buscmd) do
    begin
      OnStatus := @DBusStatusChange;
      ExecuteAction;
      Free;
    end;
    exit;
  end;

  tmp := CleanFilePath(tmpdir + '/liupd/');
  ForceDirectories(tmp);
  c := TProcess.Create(nil);
  c.Options := [poUsePipes, poWaitonexit];

  dsApp := TSQLite3Dataset.Create(nil);
  LoadAppDB(dsApp);
  dsApp.Active := true;

  msg('Begin update of ' + TUpdateInfo(ulist[uid]).AppName);
  xz := TLiUpdateBit.Create;
  files := TUpdateInfo(ulist[uid]).files;
  max := (files.Count div 2);
  prog := 0;
  for i := 0 to files.Count - 1 do
    if i mod 2 = 0 then
    begin
      msg('GET: ' + files[i]);

      try
        HTTP.Clear;
        HTTP.HTTPMethod('GET', files[i]);

        xh := tmp + ExtractFileName(files[i]);
        HTTP.Document.SaveToFile(xh);
        msg('Install...');
        xz.Decompress(xh, DeleteModifiers(files[i + 1]));

        DeleteFile(xh);
        //DeleteFile(DeleteModifiers(ulist[j][i+1])+'/'+ExtractFileName(ulist[j][i])); //Delete old File (not always necessary, but sometimes needed)

      except
        request(rsExtractError, rqError);
        msg(rsUpdConfError);
        xz.Free;
        exit;
      end;

      if (pos('.desktop', LowerCase(ExtractFileName(files[i + 1]))) > 0) then
      begin
        msg('Writing configuration for ' + ExtractFileName(files[i + 1]));
        dsk := TIniFile.Create(files[i + 1]);
        if dsk.ValueExists('Desktop Entry', 'Icon') then
          dsk.WriteString('Desktop Entry', 'Icon', SyblToPath(
            dsk.ReadString('Desktop Entry', 'Icon', '*')));
        if dsk.ValueExists('Desktop Entry', 'Exec') then
          dsk.WriteString('Desktop Entry', 'Exec', SyblToPath(
            dsk.ReadString('Desktop Entry', 'Exec', '*')));
        dsk.Free;
      end;

      if (pos(' <setvars>', LowerCase(ExtractFileName(files[i + 1]))) > 0) then
      begin
        msg('Writing configuration for ' + ExtractFileName(files[i + 1]));
        s := TStringList.Create;
        s.LoadFromFile(files[i + 1]);
        for k := 0 to s.Count - 1 do
          s[k] := SyblToPath(s[k]);
        s.SaveToFile(files[i + 1]);
        s.Free;
      end;

      msg('chmod...');
      msg('Assign rights..');
      if pos(' <chmod:', files[i + 1]) > 0 then
      begin
        c.CommandLine := FindBinary('chmod') + ' ' + copy(
          files[i + 1], pos(' <chmod:', files[i + 1]) + 8, 3) + SyblToPath(files[i + 1]) +
          '/' + ExtractFileName(DeleteModifiers(files[i + 1]));
        c.Execute;
      end
      else
      begin
        c.CommandLine := FindBinary('chmod 755') + ' ' + DeleteModifiers(
          SyblToPath(files[i + 1])) + '/' + ExtractFileName(files[i]);
        c.Execute;
      end;
      msg('Finishing...');
      Inc(prog);
      SetMnPos(Round((100 / max) * prog));
      msg('Okay');
    end;

  xz.Free;

  if TUpdateInfo(ulist[uid]).NVersion <> '' then
  begin
    dsApp.SQL := 'SELECT * FROM AppInfo';
    dsApp.Open;
    dsApp.Filtered := true;
    dsApp.Edit;
    dsApp.First;
    dsApp.ExecuteDirect('UPDATE AppInfo SET Version = ''' + TUpdateInfo(
      ulist[uid]).NVersion + ''' WHERE PkName = ''' + TUpdateInfo(ulist[uid]).ID + '''');

   { while not dsApp.EOF do
    begin
     if (dsApp.FieldByName('Name').AsString=TUpdateInfo(ulist[uid]).AppName) and (dsApp.FieldByName('PkName').AsString=) then
     begin

      dsApp.FieldByName('Version').Value:=TUpdateInfo(ulist[uid]).NVersion;
      break;
     end;
    dsApp.Next;
    end;}
    dsApp.ApplyUpdates;
    dsApp.Close;
  end;

  ulist.Delete(uid); //Remove update information

  msg('Cleaning up...');
  c.Free;
  dsApp.Free;

  FileUtil.DeleteDirectory(tmp, false);
  StepMsg('Update finished!');
end;

end.

