{ Copyright (C) 2009-2010 Matthias Klumpp

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
  Classes, SysUtils, Process, IniFiles, BaseUnix, pwd, Dos;

const
  //** Version of the Listaller applicationset
  LiVersion='0.3.79a+dev~git';
  //** Working directory of Listaller
  TMPDIR='/tmp/listaller/'; //Never forget trailing backslash!

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
//** Search for files in dir
procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string;const recursive: Boolean=true);
//** Find all dirs in a directory (NOT recursive!)
procedure FindDirs(DirList: TStringList;StartDir: String);
//** Reads the current system architecture @returns Detected architecture as string
function GetSystemArchitecture: String;
{** Executes an application as root using an graphical input prompt
    @param cmd Command line string to execute
    @param comment Description of the action the user is doing (why are root-rights needed?
    @param icon Path to an icon for the operation
    @param optn Set of TProcessOption to apply on the process object}
function  ExecuteAsRoot(cmd: String;comment: String; icon: String;optn: TProcessOptions=[]): Boolean;
//** Cleans up messy file path
function CleanFilePath(path: String): String;
{** Check if user is root
 @returns If user is root (Bool)}
function IsRoot: Boolean;
//** Remove duplicate entries from stringlist
procedure RemoveDuplicates(sl: TStringList);
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
//** Replace string a with b
function StrSubst(s,a,b: String): String;
//** Find absolute path for system binary
function FindBinary(name: String): String;
//** Check if program is running @param cmd Command name
function IsCommandRunning(cmd: String):Boolean;

const
 LI_CONFIG_DIR = '/etc/lipa/';
 LI_APPDB_PREF = 'app-reg/';

implementation

uses distri; //Access to distribution data

function GetServerName(url: String): String;
var p1,p2 : integer;
begin
url := StringReplace(url,'ftp://','',[rfReplaceAll]);
url := StringReplace(url,'http://','',[rfReplaceAll]);
p1 := 1;
p2 := Pos('/',url);
//
Result := Copy(url,P1,(P2-1));
end;

function IsCommandRunning(cmd: String):Boolean;
var t:TProcess;
s:TStringList;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.CommandLine:=FindBinary('ps')+' -A'+cmd;
 t.Options:=[poUsePipes,poWaitonexit];
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   Result:=Pos(cmd,s.Text)>0;
  finally
  s.free;
  end;
 finally
 t.Free;
 end;
end;

function GetServerPath(url: String): String;
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

procedure FindDirs(DirList: TStringList;StartDir: String);
var
  SR: TSearchRec;
  IsFound: Boolean;
begin
  IsFound := SysUtils.FindFirst(StartDir+'*', faAnyFile, SR) = 0;
  while IsFound do
  begin
    if ((SR.Attr and faDirectory) <> 0) and
         (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := SysUtils.FindNext(SR) = 0;
  end;
  SysUtils.FindClose(SR);
end;

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: String;const recursive: Boolean=true);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
begin
  if FileMask = '' then FileMask:='*';

  if StartDir[length(StartDir)] <> '/' then
    StartDir := StartDir + '/';

  IsFound := SysUtils.FindFirst(StartDir+FileMask, faAnyFile-faDirectory, SR) = 0;
  while IsFound do
  begin
    FilesList.Add(StartDir + SR.Name);
    IsFound := SysUtils.FindNext(SR) = 0;
  end;
  SysUtils.FindClose(SR);

  if recursive then
  begin
  // Build a list of subdirectories
  DirList := TStringList.Create;
  IsFound := SysUtils.FindFirst(StartDir+'*', faAnyFile, SR) = 0;
  while IsFound do
  begin
    if ((SR.Attr and faDirectory) <> 0) and
         (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := SysUtils.FindNext(SR) = 0;
  end;
  SysUtils.FindClose(SR);

  // Scan the list of subdirectories
  for i := 0 to DirList.Count - 1 do
  begin
    FindFiles(FilesList, DirList[i], FileMask);
  end;
  DirList.Free;
  end;
end;

function FindBinary(name: String): String;
var res: String;

function FExists(p: String): Boolean;
begin
 res:=p+'/'+name;
 Result:=FileExists(res);
end;
begin
 if FExists('/usr/local/sbin') then Result:=res
 else if FExists('/usr/local/bin') then Result:=res
 else if FExists('/usr/sbin') then Result:=res
 else if FExists('/usr/bin') then Result:=res
 else if FExists('/sbin') then Result:=res
 else if FExists('/bin') then Result:=res
 else if FExists('/usr/games') then Result:=res
 else
  Result:=name;
end;

function GetSystemArchitecture: String;
var p: TProcess;x: TStringList;s: String;
begin
 p:=TProcess.Create(nil);
 p.Options:=[poUsePipes, poWaitOnExit];
 p.CommandLine:=FindBinary('uname')+' -m';
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
h:=GetEnvironmentVariable('HOME');
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
   t.CommandLine:=FindBinary('pkmon')+' -v';
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

function CleanFilePath(path: String): String;
begin
 Result:=StringReplace(path,'//','/',[rfReplaceAll]);
end;

//This should no longer be necessary since Listaller uses PolicyKit
function ExecuteAsRoot(cmd: String;comment: String; icon: String;optn: TProcessOptions=[]): Boolean;
var p: TProcess; DInfo: TDistroInfo;
begin
DInfo:=GetDistro;
p:=TProcess.Create(nil);
if DInfo.DBase='KDE' then
begin
 if FileExists(FindBinary('kdesu')) then
  p.CommandLine := FindBinary('kdesu')+' -d --comment "'+comment+'" -i '+icon+' '+cmd
 else
 if FileExists(FindBinary('kdesudo')) then
  p.CommandLine := FindBinary('kdesudo')+' -d --comment "'+comment+'" -i '+icon+' '+cmd
end else
begin
 if DInfo.DName='Fedora' then
  //Fedora uses Consolehelper to run apps as root. So we use "beesu" to make CH work for Listaller
  p.CommandLine := FindBinary('beesu')+' -l '+cmd
 else
   if FileExists(FindBinary('gksudo')) then
    p.CommandLine := FindBinary('gksudo')+' --message "'+comment+'" '+cmd
    else
   if FileExists(FindBinary('gksu')) then
    p.CommandLine := FindBinary('gksu')+' --message "'+comment+'" '+cmd
    else
   if FileExists(FindBinary('gnomesu')) then
    p.CommandLine := FindBinary('gnomesu')+' '+cmd
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

function StrSubst(s,a,b: String): String;
begin
 Result:=StringReplace(s,a,b,[rfReplaceAll]);
end;

end.

