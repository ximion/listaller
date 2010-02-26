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
//** Command-line application for IPK-package handling
program lipa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, //We use NoGUI widgetset
  Classes, SysUtils, CustApp,
  Process, liBasic, installer,
  TRStrings, IniFiles, Distri,
  LiTranslator, ipkdef,
  appman, liTypes, Forms, liCommon;

type

  { TLipa }

  TLipa = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    xs: Integer;
    //** Exception handler
    procedure OnExeception(Sender : TObject;E : Exception);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  private
  end;

{ TLipa }

var
  Application: TLipa;

////////////////////////////////////////////////////////////////////////////////
///////////////////This will be done if "lipa" is started

procedure OnSetupStatusChange(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;
begin
 case change of
  scMessage    : if not verbose then writeLn(' '+data.msg);
  scStepMessage: if not Application.HasOption('verbose') then
                  writeLn(' '+rsState+': '+data.msg);
  scMnProgress : with Application do
                 begin
                      if not HasOption('verbose') then
                      begin
                      //Simple, stupid progress animation
                       if xs=0 then begin xs:=1; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=1 then begin xs:=2; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=2 then begin xs:=3; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=3 then begin xs:=4; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=4 then begin xs:=5; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=5 then begin xs:=6; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=6 then begin xs:=7; write(#13' '+IntToStr(data.mnprogress)+'%    ');end else
                       if xs=7 then begin xs:=0; write(#13' '+IntToStr(data.mnprogress)+'%    ');end;
                       end;
                 end;
  end;
end;

function OnSetupUserRequest(mtype: TRqType;msg: PChar;data: Pointer): TRqResult;cdecl;
var s: String;

function AskQuestion: TRqResult;
begin
 writeLn(' '+msg);
  writeLn('');
  write(rsYesNo1);
  readln(s);
  s:=LowerCase(s);
  if (s=LowerCase(rsYes))or(s=LowerCase(rsY)) then
   Result:=ryYes
  else
  begin
  Result:=rsNo;
  writeLn(rsInstAborted);
  halt(6);
  end;
end;

begin
case mtype of
rqQuestion: begin
  writeLn(rsQuestion);
  Result:=AskQuestion;
  end;
rqError: begin
  writeLn(' '+msg);
  readln;
  halt(5);
  end;
rqWarning: begin
  writeLn(rsWarning);
  Result:=AskQuestion;
end;
rqInfo: begin
  writeLn(msg);
  readln;
end;
end;
end;

////////////////////////////////////////////////////////////////////////////////
//
procedure TLipa.DoRun;
var
  ErrorMsg,a,c: String;
  t: TProcess;
  i: Integer;
  x: Boolean;
  setup: TInstallPack;
  lst: TStringList;
  proc: TProcess;
  cnf: TIniFile;
begin
  // quick check parameters

  ErrorMsg:=CheckOptions('h','help');
  ErrorMsg:=CheckOptions('?','help');
  //
  ErrorMsg:=CheckOptions('v','version');
 { if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Halt;
  end;  }

  if paramstr(1)='' then
  begin
   writeln('Usage: ',ExeName,' <command> [file} (options)');
   Terminate;
  end;

  if HasOption('h','help') then
  begin
    WriteHelp;
    Halt(0);
  end;
  
  if HasOption('?','help') then
  begin
    WriteHelp;
    Halt(0);
  end;

  if HasOption('v','version') then
  begin
    writeLn('Version: '+LiVersion);
    Halt(0);
  end;

  if (HasOption('b','build'))or(HasOption('u','gen-update')) then
  begin
   if (FileExists('/usr/bin/libuild'))
   or (FileExists('/usr/lib/listaller/libuild')) then
   begin
    proc:=TProcess.Create(nil);
    proc.Options:=[];
    c:='';
    for i:=1 to paramcount-1 do
     c:=c+' '+paramstr(i);
    if FileExists('/usr/bin/libuild') then
     proc.CommandLine:='libuild'+c
    else proc.CommandLine:='/usr/lib/listaller/libuild'+c;

    proc.Execute;
    proc.Free;
    Terminate;
   end else writeLn(rsInstallLiBuild);
  end;

  if HasOption('s','solve') then
  begin
    writeLn(SyblToPath('$'+paramstr(2)));
    halt(0);
  end;

  for i:=1 to paramcount do
   if FileExists(paramstr(i)) then a:=paramstr(i);

  if HasOption('i','install') then
  begin
    if not FileExists(a) then
    begin
     writeLn(StringReplace(rsFileNotExists,'%f',a,[rfReplaceAll]));
     halt(1);
     exit;
    end;

    setup:=TInstallPack.Create;
    //Assign callbacks
    setup.SetStatusChangeCall(@OnSetupStatusChange);
    setup.SetUserRequestCall(@OnSetupUserRequest);

    //Check Testmode
    if HasOption('testmode') then Testmode:=true
    else Testmode:=false;
    setup.SetTestmode(Testmode);

    setup.Initialize(a);
    writeLn('== '+StringReplace(rsInstOf,'%a',setup.GetAppName+' '+setup.GetAppVersion,[rfReplaceAll])+' ==');

    lst:=TStringList.Create;
    setup.ReadLongDescription(lst);
    if lst.Count>0 then
    begin
     writeLn(rsProgDesc);
     for i:=0 to lst.Count-1 do
      writeLn(lst[i]);
    end;

    setup.ReadLicense(lst);
    if lst.Count>0 then
    begin
      writeLn(rsLicense);
     for i:=0 to lst.Count-1 do
     begin
      writeLn(lst[i]);
      readLn;
     end;
     c:='';
     repeat
      writeLn('');
      write(rsDoYouAcceptLicenseCMD+' ');
      readLn(c);
      c:=LowerCase(c);
      if (c=LowerCase(rsN)) or (c=LowerCase(rsNo)) then
      begin
       setup.Free;
       writeLn(rsInstAborted);
       halt(6);
       exit;
      end;
     until (c=LowerCase(rsY))or(c=LowerCase(rsYes));
    end;

    //Clear temporary list
    lst.Clear;

    // Check for active profiles
    setup.ReadProfiles(lst);

    if lst.Count=1 then writeLn('Using profile: '+lst[0])
    else
    begin
     writeLn(rsSelectIModeA);
     for i:=0 to lst.Count-1 do
      writeLn(' '+IntToStr(i+1)+') '+lst[i]);
     repeat
      write(rsModeNumber+' ');
      readLn(c);
      try
       if (StrToInt(c)-1>=lst.Count)or(StrToInt(c)-1<0) then
        writeLn(rsSelectListNumber);
      except
       writeLn(rsEnterNumber);
       c:=IntToStr(lst.Count+2);
      end;
     until (StrToInt(c)-1<=lst.Count)and(StrToInt(c)-1>-1);
      i:=StrToInt(c)-1;
      setup.SetProfileID(i);
    end;
    //Free tmp list
    lst.Free;

    writeLn('- '+rsOkay);
    writeLn('-> '+rsPreparingInstall);

 { cnf:=TInifile.Create(ConfigDir+'config.cnf');
  //Create HTTP object
   HTTP := THTTPSend.Create;
   //HTTP.Sock.OnStatus:=@HookSock;
   HTTP.UserAgent:='Listaller-GET';
  //Create FTP object
   FTP := TFTPSend.Create;
   //FTP.DSock.Onstatus:=@HookSock;
  if cnf.ReadBool('Proxy','UseProxy',false) then
  begin
   //Set HTTP
    HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
    HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
    HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
    HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted
  end;
   cnf.Free;
 //Assign HTTP/FTP objects to Installation service object
  setup.HTTPSend:=HTTP;
  setup.FTPSend:=FTP;
  }

 writeLn('-> '+rsRunning);
 if not HasOption('verbose') then
 writeLn(' '+rsState+': '+rsStep1);

 //Do the installation
 setup.StartInstallation;

 if HasOption('verbose') then
  for i:=0 to lst.Count-1 do writeLn(lst[i]);

 if not Testmode then
 begin
  writeLn(StringReplace(rsWasInstalled,'%a',setup.GetAppName,[rfReplaceAll]));
  writeLn('Finished.');
 end else
 begin
  writeLn(rsExecAppTesting);
  proc.CommandLine:=setup.GetAppCMD;
  Proc.Options:=[poWaitOnExit];
  Proc.Execute;
  writeLn(rsTestFinished);
  writeLn(rsCleaningUp);
   Proc.CommandLine := 'rm -rf /tmp/litest';
   Proc.Execute;
 end;
 proc.Free;

  halt(0);
 end;

 //???
{
 if HasOption('checkapps') then
 begin
  lst:=TStringList.Create;
  if not CheckApps(lst) then
  begin
   write(rsShowDetailedInfoCMD+' ');
   readLn(c);
   if (c=LowerCase(rsY))or(c=LowerCase(rsYes)) then
    for i:=0 to lst.Count-1 do
     writeLn(lst[i]);
   write(rsLipaAutoFixQ+' ');
   readln(c);
   if (c=LowerCase(rsY))or(c=LowerCase(rsYes)) then
    CheckApps(lst,true)
   else
    writeLn(rsAborted);
  end else
  begin
   write(rsShowDetailedInfoCMD+' ');
   readLn(c);
   if (c=LowerCase(rsY))or(c=LowerCase(rsYes)) then
    for i:=0 to lst.Count-1 do
     writeLn(lst[i]);
  end;
 end; }

  // stop program loop
  Terminate;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Help & Main functions////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TLipa.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  //Needed for e.g. the checkapps-parameter
  if IsRoot then
  RegDir:='/etc/lipa/app-reg/'
  else
  RegDir:=SyblToPath('$INST')+'/app-reg/';
end;

destructor TLipa.Destroy;
begin
  inherited Destroy;
end;

procedure TLipa.WriteHelp;
begin
writeln('Usage: ',ExeName,' <command> [file] (options)');

writeLn(rsLipaInfo1);
writeLn(rsCommands);
writeLn('-s, --solve [variable]                     '+rsLipaInfo2);
writeLn('-i, --install [IPK-Package]                '+rsLipaInfo3);
writeLn('  '+rsOptions);
writeLn('    --testmode                             '+rsLipaInfo4);
writeLn('    --verbose                              '+rsLipaInfo5);
writeLn('--checkapps                                '+rsLipaInfo6);
if FileExists('/usr/bin/libuild')or(FileExists('/usr/lib/listaller/libuild')) then
begin
writeLn(rsCMDInfoPkgBuild);
writeLn('-b, --build [IPS-File] [Output-IPK]        '+rsLiBuildInfoA);
writeLn('-u, --gen-update [IPS-File] [Repo-Path]    '+rsLiBuildInfoB);
writeLn('-b, --build [IPS-File]                     '+rsLiBuildInfoC);
end;
end;

procedure TLipa.OnExeception(Sender : TObject;E : Exception);
begin
writeLn(rsInternalError);
writeLn('[Message]: '+E.Message);
writeLn('('+rsAborted+')');
halt(8);
end;

{$IFDEF WINDOWS}{$R lipa.rc}{$ENDIF}

begin
  Application:=TLipa.Create(nil);
  Application.OnException:=@Application.OnExeception;
  Application.Run;
  Application.Free;
end.
