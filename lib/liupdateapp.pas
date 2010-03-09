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
  Classes, SysUtils, HTTPSend, FTPSend, liManageApp, liBasic, liCommon,
  liTypes, ipkdef, Contnrs, SqLite3DS, trStrings, MD5;

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
  destructor Destroy;override;
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
  function  Request(s: String;ty: TRqType): TRqResult;
  procedure NewUpdate(nm: String;id: Integer);
 public
  constructor Create;
  destructor  Destroy;override;

  function CheckUpdates: Boolean;
  procedure SetSumode(su: Boolean);
  procedure RegOnStatusChange(call: TLiStatusChangeCall;data: Pointer);
  procedure RegOnRequest(call: TRequestCall;data: Pointer);
  procedure RegOnNewUpdate(call: TNewUpdateEvent;data: Pointer);
 end;

implementation

{ TUpdateInfo }

constructor TUpdateInfo.Create;
begin
 desc:=TStringList.Create;
 files:=TStringList.Create;
end;

destructor TUpdateInfo.Destroy;
begin
 desc.Free;
 files.Free;
end;

{ TAppUpdater }

constructor TAppUpdater.Create;
begin
 HTTP:=THTTPSend.Create;
 FTP:=TFTPSend.Create;
 ulist:=TObjectList.Create(true);
 HTTP.UserAgent:='Listaller-Update';
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
 SUMode:=su;
 if SUMode then
  AppReg:=LI_CONFIG_DIR+LI_APPDB_PREF
 else
  AppReg:=SyblToPath('$INST')+'/'+LI_APPDB_PREF;
end;

procedure TAppUpdater.RegOnStatusChange(call: TLiStatusChangeCall;data: Pointer);
begin
 FStatus:=call;
 statechangeudata:=data;
end;

procedure TAppUpdater.RegOnRequest(call: TRequestCall;data: Pointer);
begin
 FReq:=call;
 requestudata:=data;
end;

procedure TAppUpdater.RegOnNewUpdate(call: TNewUpdateEvent;data: Pointer);
begin
 FNewUpd:=call;
 newupdudata:=data;
end;

procedure TAppUpdater.Msg(s: String);
begin
 sdata.msg:=PChar(s);
 if Assigned(FStatus) then FStatus(scMessage,sdata,statechangeudata);
end;

function TAppUpdater.Request(s: String;ty: TRqType): TRqResult;
begin
 if Assigned(FReq) then Result:=FReq(ty,PChar(s),requestudata);
end;

procedure TAppUpdater.NewUpdate(nm: String;id: Integer);
begin
 if Assigned(FNewUpd) then FNewUpd(PChar(nm),id,newupdudata);
end;

procedure TAppUpdater.SetMnPos(i: Integer);
begin
 sdata.mnprogress:=i;
 if Assigned(FStatus) then FStatus(scMnprogress,sdata,statechangeudata);
end;

procedure TAppUpdater.SetExPos(i: Integer);
begin
 sdata.exprogress:=i;
 if Assigned(FStatus) then FStatus(scExProgress,sdata,statechangeudata);
end;

function TAppUpdater.CheckUpdates: Boolean;
var
  tmp,h,sinfo,sources: TStringList;
  j,k: Integer;
  ok: Boolean;
  p: String;
  progpos: Integer;
  ui: TUpdateInfo;
  control: TIPKControl;
  dsApp: TSQLite3Dataset; //AppDB connection
  max: Integer;
  lastID: Integer;
begin
  if ulist.Count>0 then
  begin
   p_error('Already searched for new updates. Object has to be cleaned before performing new search!');
   Result:=false;
   exit;
  end;
  tmp:=TStringList.Create;
  h:=TStringList.Create;
  sinfo:=TStringList.Create;
  sources:=TStringList.Create;

  progpos:=0;
  lastID:=0;

  if not FileExists(AppReg+'updates.list') then
  begin Result:=false;exit;end;

  h.LoadFromFile(AppReg+'updates.list');
  if h.Count=1 then
  begin
   request(rsNoUpdates,rqInfo);
   Result:=false;
   exit;
  end;

  for k:=1 to h.Count-1 do
  begin
  p_debug(h[k]);
  if pos(' <',h[k])>0 then
  begin
   if h[k][1]='-' then sources.Add(copy(h[k],2,pos(' <',h[k])-2))
  end else
  begin
   if h[k][1]='-' then sources.Add(copy(h[k],2,length(h[k])));
  end;
  end;
  max:=sources.Count;
  h.Free;

  dsApp:= TSQLite3Dataset.Create(nil);
  LoadAppDB(dsApp);
  msg('Software database opened.');

  dsApp.Active:=true;

  dsApp.SQL:='SELECT * FROM AppInfo';
  dsApp.Open;
  dsApp.Filtered:=true;
  dsApp.Edit;

  for k:=0 to sources.Count-1 do
  begin
    HTTP.Clear;
    HTTP.HTTPMethod('GET', sources[k]+'/'+'source.pin');
    tmp.LoadFromStream(HTTP.Document);

    if not DirectoryExists(tmpdir) then ForceDirectories(tmpdir);

    if (tmp.Count<=0)or(pos('type:',LowerCase(tmp[0]))<=0) then break;
    tmp.SaveToFile(TMPDIR+'source0.pin');

    control:=TIPKControl.Create(TMPDIR+'source0.pin');

    ok:=false;

    dsApp.First;
    while not dsApp.EOF do
    begin
     p_debug(control.PkName+' # '+dsApp.FieldByName('PkName').AsString);
     if (control.PkName=dsApp.FieldByName('PkName').AsString) then
     begin
      ok:=true;
      break;
     end;
     dsApp.Next;
    end;

    if ok then
    begin
     HTTP.Clear;
     HTTP.HTTPMethod('GET', sources[k]+'/'+'content.id');
     sleep(4);
     sinfo.LoadFromStream(HTTP.Document);

     ui:=TUpdateInfo.Create;
    for j:=0 to sinfo.Count-1 do
     if length(sinfo[j])>0 then
     begin
     if sinfo[j][1]='>' then p:=SyblToPath(copy(sinfo[j],2,length(sinfo[j])))
     else
      if sinfo[j+1]<>MDPrint((MD5.MD5File(DeleteModifiers(SyblToPath(sinfo[j])),1024))) then
      begin
       ui.files.Add(sources[k]+'/'+DeleteModifiers(SyblToX(sinfo[j])));
       ui.files.Add(SyblToPath(CleanFilePath(p+'/'+sinfo[j])));
      end;
     end;

    ulist.Add(ui);
    ui.AppName:=control.AppName;
    ui.NVersion:=control.AppVersion;
    ui.ID:=control.PkName;
    SetMnPos(Round(((progpos+1)/max)*100));

    if ui.files.Count>0 then
    begin
     Inc(lastID);
     ui.updid:=lastID;
     NewUpdate(ui.AppName,ui.updid);
    end else
     ui.Free;
  end;
  control.Free;
  end;

  tmp.Free;
  sinfo.Free;

  dsApp.Free;
  writeLn('Database connection closed.');

request(rsNoUpdates,rqInfo);
end;

end.

