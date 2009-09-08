{ lipa.lpr
  Copyright (C) Listaller Project 2008-2009

  lipa.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  lipa.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Command-line application for IPK-package handling
program lipa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, //We use NoGUI widgetset
  Classes, SysUtils, CustApp,
  Process, LiCommon, installer,
  TRStrings, IniFiles, HTTPSend,
  FTPSend, Distri, LiTranslator, ipkdef,
  ipkbuild, ipkhandle;

type

  { TLipa }

  TLipa = class(TCustomApplication)
    procedure setupError(Sender: TObject; msg: String);
    procedure setupMainPosChange(Sender: TObject; pos: Integer);
    procedure setupMainVisibleChange(Sender: TObject; vis: Boolean);
    procedure setupStateMessage(Sender: TObject; msg: String);
    procedure setupTermQuestion(Sender: TObject; msg: String);
  protected
    procedure DoRun; override;
  public
    //** Exception handler
    procedure OnExeception(Sender : TObject;E : Exception);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  private
   xs: Integer;
  end;

{ TLipa }


////////////////////////////////////////////////////////////////////////////////
///////////////////This will be done if "lipa" is started

procedure TLipa.setupError(Sender: TObject; msg: String);
begin
  writeLn(rsInstPerformError);
  writeLn(' '+msg);
  halt(2);
end;

procedure TLipa.setupMainPosChange(Sender: TObject; pos: Integer);
begin
if HasOption('verbose') then
begin
 //Simple, stupid progress animation
  if xs=0 then begin xs:=1; write(#13' #         '+IntToStr(pos));end else
  if xs=1 then begin xs:=2; write(#13' ##        '+IntToStr(pos));end else
  if xs=2 then begin xs:=3; write(#13' ###       '+IntToStr(pos));end else
  if xs=3 then begin xs:=4; write(#13' #####     '+IntToStr(pos));end else
  if xs=4 then begin xs:=5; write(#13' ######    '+IntToStr(pos));end else
  if xs=5 then begin xs:=6; write(#13' #######   '+IntToStr(pos));end else
  if xs=6 then begin xs:=7; write(#13' ########  '+IntToStr(pos));end else
  if xs=7 then begin xs:=0; write(#13'                             ');end;
end;
end;

procedure TLipa.setupMainVisibleChange(Sender: TObject; vis: Boolean);
begin
  if vis then
  begin
  writeLn('');
  write('[ ')
  end else
  begin
   write(' ]');
   writeLn('');
  end;
end;

procedure TLipa.setupStateMessage(Sender: TObject; msg: String);
begin
if not HasOption('verbose') then
  writeLn(' '+rsState+': '+msg);
end;

procedure TLipa.setupTermQuestion(Sender: TObject; msg: String);
var s: String;
begin
  writeLn(rsQuestion);
  writeLn(' '+msg);
  writeLn('');
  write(rsYesNo1);
  readln(s);
  s:=LowerCase(s);
  if (s=LowerCase(rsYes))or(s=LowerCase(rsY)) then
  else
  begin
  writeLn(rsInstAborted);
  halt(6);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
var HTTP: THttpSend; FTP: TFTPSend;
procedure TLipa.DoRun;
var
  ErrorMsg,a,c: String;
  t: TProcess;
  i: Integer;
  x: Boolean;
  setup: TInstallation;
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
   if FileExists('/bin/libuild') then
   begin
    proc:=TProcess.Create(nil);
    proc.Options:=[];
    c:='';
    for i:=1 to paramcount-1 do
     c:=c+' '+paramstr(i);
    proc.CommandLine:='libuild'+c;
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
    //Check Testmode
    if HasOption('testmode') then Testmode:=true
    else Testmode:=false;

    setup:=TInstallation.Create;
    setup.OnError:=@setupError;
    setup.OnMainPosChange:=@setupMainPosChange;
    setup.OnMainVisibleChange:=@setupMainVisibleChange;
    setup.OnTermQuestion:=@setupTermQuestion;
    setup.Initialize(a);
    writeLn('== '+StringReplace(rsInstOf,'%a',setup.AppName+' '+setup.AppVersion,[rfReplaceAll])+' ==');
    writeLn('-> '+rsResolvingDep);
    setup.ResolveDependencies;

    writeLn('- '+rsDone);
    if setup.DescFile <> '' then
    begin
     writeLn(rsProgDesc);
     lst:=TStringList.Create;
     lst.LoadFromFile(setup.DescFile);
     for i:=0 to lst.Count-1 do
      writeLn(lst[i]);
     lst.Free;
    end;
    if setup.LicenseFile <> '' then
    begin
      writeLn(rsLicense);
     lst:=TStringList.Create;
     lst.LoadFromFile(setup.LicenseFile);
     for i:=0 to lst.Count-1 do
     begin
      writeLn(lst[i]);
      readLn;
     end;
     lst.Free;
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
    // Check for active profiles
    lst:=TStringList.Create;
    for i:=0 to setup.Profiles.Count-1 do
      lst.Add(copy(setup.Profiles[i],0,pos(' #',setup.Profiles[i])));
    if lst.Count=0 then writeLn('Using profile: '+lst[0])
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
      setup.CurrentProfile:=lst[i];
      setup.IFileInfo:='/stuff/fileinfo-'+copy(setup.Profiles[i],pos(' #',setup.Profiles[i])+2,length(setup.Profiles[i]))+'.id';
    end;
    lst.Free;
    writeLn('- '+rsOkay);
    writeLn('-> '+rsPreparingInstall);

     if setup.IFileInfo='' then
     begin
      writeLn(rsPKGError+#13'Message: No file information that is assigned to this profile was not found!'+#13+rsAppClose);
      halt(1);
      exit;
     end;

  cnf:=TInifile.Create(ConfigDir+'config.cnf');
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

 //Assign event handler
 setup.OnStateMessage:=@setupStateMessage;

 writeLn('-> '+rsRunning);
 if not HasOption('verbose') then
 writeLn(' '+rsState+': '+rsStep1);

 proc:=TProcess.Create(nil);
 proc.Options:=[poUsePipes];
 lst:=TStringList.Create;
 //GetOutPutTimer.Enabled:=true;
 setup.DoInstallation(Proc,lst);
 //GetOutPutTimer.Enabled:=false;
 setup.HTTPSend:=nil;
 setup.FTPSend:=nil;
 lst.Free;
 HTTP.Free;
 FTP.Free;

 if HasOption('verbose') then
  for i:=0 to lst.Count-1 do writeLn(lst[i]);

 if not Testmode then
 begin
  writeLn(StringReplace(rsWasInstalled,'%a',setup.AppName,[rfReplaceAll]));
  writeLn('Finished.');
 end else
 begin
  writeLn(rsExecAppTesting);
  proc.CommandLine:=setup.CMDLn;
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
 end;

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
writeln('Usage: ',ExeName,' <command> [file} (options)');

writeLn(rsLipaInfo1);
writeLn(rsCommands);
writeLn('-s, --solve [variable]                     '+rsLipaInfo2);
writeLn('-i, --install [IPK-Package]                '+rsLipaInfo3);
writeLn('  '+rsOptions);
writeLn('    --testmode                             '+rsLipaInfo4);
writeLn('    --verbose                              '+rsLipaInfo5);
writeLn('--checkapps                                '+rsLipaInfo6);
if FileExists('/bin/libuild') then
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

var
  Application: TLipa;

{$IFDEF WINDOWS}{$R lipa.rc}{$ENDIF}

begin
  Application:=TLipa.Create(nil);
  Application.OnException:=@Application.OnExeception;
  Application.Run;
  Application.Free;
end.
