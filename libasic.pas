{ Copyright (C) 2009 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this unit. If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains basic functions and types which are used nearly everywhere
unit libasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOS, Process, IniFiles, BaseUnix, pwd;

const
  //** Version of the Listaller applicationset
  LiVersion='0.3.10a~dev';
  //** Working directory of Listaller
  lp='/tmp/';

var
  //** The Listaller package lib directory
  RegDir: String='';
  //** True if Listaller is in testmode
  Testmode: Boolean=false;

//** Creates Listaller's config dir @returns Current config dir
function  ConfigDir: String;
//** Get current data file (check /usr/share and current dir)
function  GetDataFile(s: String): String;
//** Executes a command-line application @returns The application's last output string
function  CmdResult(cmd:String):String;
//** Executes a command-line application @returns The application's exit code
function  CmdResultCode(cmd:String):Integer;
{** Executes a command-line application
    @param cmd Command that should be executed
    @param Result TStringList to recive the output}
procedure CmdResultList(cmd:String;Result: TStringList);
//** Executes a command-line application @returns The final application output string (without breaks in line and invisible codes)
function  CmdFinResult(cmd: String): String;
//** Get server name from an url @returns The server name
function  GetServerName(url:string):string;
//** Path on an server (from an url) @returns The path on the server
function  GetServerPath(url:string):string;
{** Fast check if entry is in a list
    @param nm Name of the entry that has to be checked
    @param list The string list that has to be searched}
function  IsInList(nm: String;list: TStringList): Boolean;
//** Shows pkMon if option is set in preferences
procedure ShowPKMon();
//** Get the current, usable language variable
function GetLangID: String;
//** Reads the current system architecture @returns Detected architecture as string
function GetSystemArchitecture: String;
{** Executes an application as root using an graphical input prompt
    @param cmd Command line string to execute
    @param comment Description of the action the user is doing (why are root-rights needed?
    @param icon Path to an icon for the operation
    @param optn Set of TProcessOption to apply on the process object}
function  ExecuteAsRoot(cmd: String;comment: String; icon: String;optn: TProcessOptions=[]): Boolean;
{** Check if user is root
 @returns If user is root (Bool)}
function IsRoot: Boolean;
//** Get current date as string
function GetDateAsString: String;
//** Show an error message
procedure p_error(msg: String);
//** Displays a simple warning
procedure p_warning(msg: String);
//** Displays info
procedure p_info(msg: String);
//** Show a debug bessage
procedure p_debug(msg: String);

implementation

uses distri; //Access to distribution data

function GetServerName(url:string):string;
var p1,p2 : integer;
begin
url := StringReplace(url,'ftp://','',[rfReplaceAll]);
url := StringReplace(url,'http://','',[rfReplaceAll]);
p1 := 1;
p2 := Pos('/',url);
//
Result := Copy(url,P1,(P2-1));
end;

function GetServerPath(url:string):string;
var p1,p2 : integer;
begin
url := StringReplace(url,'http://','',[rfReplaceAll]);
url := StringReplace(url,'ftp://','',[rfReplaceAll]);
url := StringReplace(url,'www.','',[rfReplaceAll]);
p1 := Pos('/',url);
p2 := Length(url);
Result := ExtractFilePath(Copy(url,P1,P2));
end;

function GetDataFile(s: String): String;
var D: TDistroInfo;
begin
if (FileExists(ExtractFilePath(paramstr(0))+s))
or (DirectoryExists(ExtractFilePath(paramstr(0))+s)) then
Result:=ExtractFilePath(paramstr(0))+s
else
if pos('graphics/',s)>0 then
begin
D:=GetDistro;
//Apply graphics branding
if FileExists('/usr/share/listaller/branding/'+LowerCase(D.DName)+'/'+s) then
 Result:='/usr/share/listaller/branding/'+LowerCase(D.DName)+'/'+s
else
 Result:='/usr/share/listaller/'+s;
end else
 Result:='/usr/share/listaller/'+s;
end;

function GetSystemArchitecture: String;
var p: TProcess;x: TStringList;s: String;
begin
 p:=TProcess.Create(nil);
 p.Options:=[poUsePipes, poWaitOnExit];
 p.CommandLine:='uname -m';
 p.Execute;
 x:=TStringList.Create;
 x.LoadFromStream(p.OutPut);
 s:=x[0];
 x.Free;
 p.Free;

if (s='i686')
or (s='i586')
or (s='i486')
then s:='i386';
Result:=s;
end;

function GetDateAsString: String;
var
  Year,Month,Day,WDay : word;
begin
  GetDate(Year,Month,Day,WDay);
  Result:=IntToStr(Day)+'.'+IntToStr(Month)+'.'+IntToStr(Year);
end;

function GetLangID: String;
var LANG: String;i: Integer;
begin
 LANG:=GetEnvironmentVariable('LANG');
 if LANG='' then
 begin
   for i:=1 to Paramcount-1 do
    if (ParamStr(i)='--LANG') or
     (ParamStr(i)='-l') or
     (ParamStr(i)='--lang') then LANG:=ParamStr(i+1);
 end;
 if not DirectoryExists('/usr/share/locale-langpack/'+LANG) then
  Result:=copy(LANG,1,2)
 else
  Result:=LANG;
end;

function ConfigDir: String;
var h: String;
begin
h:=getenvironmentvariable('HOME');
h:=h+'/.config';
if not DirectoryExists(h) then CreateDir(h);
h:=h+'/Listaller';
if not DirectoryExists(h) then CreateDir(h);
Result:=h+'/';
end;

function CmdResult(cmd:String):String;
var t:TProcess;
s:TStringList;
begin
 Result:='';
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:=cmd;
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   if s.Count<= 0 then Result:='err' else
   Result:=s[0];
  finally
  s.free;
  end;
 finally
 t.Free;
 end;
end;

function IsRoot: Boolean;
var p : PPasswd;
begin
p:=fpgetpwuid(fpgetuid);
Result:=false;
 if assigned(p) then
    begin
       if p^.pw_name<>'root' then
         Result:=false
        else  Result:=true;
     end else
   p_error('Internal error');
end;

procedure ShowPKMon();
var cnf: TIniFile;t: TProcess;
begin
//Check if PackageKit checkmode is enabled:
cnf:=TIniFile.Create(ConfigDir+'config.cnf');
  if cnf.ReadBool('MainConf','ShowPkMon',false) then
  begin
   t:=TProcess.Create(nil);
   t.CommandLine:='pkmon -v';
   t.Options:=[poNewConsole];
   t.Execute;
  end;
cnf.Free;
end;

procedure p_error(msg: String);
begin
 writeLn('[ERROR] '+msg);
end;

procedure p_warning(msg: String);
begin
 writeLn('[WARNING] '+msg);
end;

procedure p_info(msg: String);
begin
 writeLn(msg);
end;

procedure p_debug(msg: String);
begin
 writeLn('[DEBUG] '+msg);
end;

function CmdFinResult(cmd:String):String;
var t:TProcess;
Buffer: string;
BytesAvailable: DWord;
BytesRead:LongInt;
begin
 Result:='';
 buffer:='';
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes];
 t.CommandLine:=cmd;
 try
  t.Execute;
  while t.Running do
  begin
      BytesAvailable := t.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
       begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := t.OutPut.Read(Buffer[1], BytesAvailable);
        if (pos(#13,Buffer)>0)or(pos(#26,Buffer)>0)or(Pos(#10,Buffer)>0)then Result:='';
        Result := Result + copy(Buffer,1, BytesRead);
        BytesAvailable := t.OutPut.NumBytesAvailable;
      end;
  end;
 finally
 t.Free;
 end;
end;

function CmdResultCode(cmd:String):Integer;
var t:TProcess;
begin
 Result:=0;
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes,poWaitonexit];
 t.CommandLine:=cmd;
 try
  t.Execute;
   Result:=t.ExitStatus;
 finally
 t.Free;
 end;
end;

function ExecuteAsRoot(cmd: String;comment: String; icon: String;optn: TProcessOptions=[]): Boolean;
var p: TProcess; DInfo: TDistroInfo;
begin
DInfo:=GetDistro;
p:=TProcess.Create(nil);
if DInfo.DBase='KDE' then
begin
 if FileExists('/usr/bin/kdesu') then
  p.CommandLine := 'kdesu -d --comment "'+comment+'" -i '+icon+' '+cmd
 else
 if FileExists('/usr/bin/kdesudo') then
  p.CommandLine := 'kdesudo -d --comment "'+comment+'" -i '+icon+' '+cmd
end else
begin
 if DInfo.DName='Fedora' then
  //Fedora uses Consolehelper to run apps as root. So we use "beesu" to make CH work for Listaller
  p.CommandLine := 'beesu -l '+cmd
 else
   if FileExists('/usr/bin/gksudo') then
    p.CommandLine := 'gksudo --message "'+comment+'" '+cmd
    else
   if FileExists('/usr/bin/gksu') then
    p.CommandLine := 'gksu --message "'+comment+'" '+cmd
    else
   if FileExists('/usr/bin/gnomesu') then
    p.CommandLine := 'gnomesu '+cmd
   else
   begin
    writeLn('Unable to execute the application as root.'#13'Please do this manually!');
    p.Free;
    Result:=false;
    exit;
   end;
 end;
    p.Options:=optn;
    p.Execute;
    Result:=true;
    if p.ExitStatus>0 then Result:=false;
    p.Free;
end;

procedure CmdResultList(cmd:String;Result: TStringList);
var t:TProcess;
s:TStringList;
begin
 t:=tprocess.create(nil);
 t.CommandLine:=cmd;
 t.Options:=[poUsePipes,poWaitonexit];
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   if t.ExitStatus<=0 then
   Result.Assign(s)
   else Result.Clear;

  finally
  s.free;
  end;
 finally
 t.Free;
 end;
end;

function IsInList(nm: String;list: TStringList): Boolean;
begin
Result:=list.IndexOf(nm)>-1;
end;

end.

