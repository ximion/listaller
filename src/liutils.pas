{ Copyright (C) 2008-2010 Matthias Klumpp

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
//** This unit contains basic functions and consts which are used nearly everywhere
unit liutils;

{$mode objfpc}{$H+}

interface

uses
  Dos, pwd, Classes, Process, RegExpr, BaseUnix, IniFiles, SysUtils, StrLocale;

const
  //** Version of the Listaller applicationset
  LiVersion = {$I liversion.inc};
  //** Working directory of Listaller
  TMPDIR = '/tmp/listaller/'; //Never forget trailing backslash!
  //** Root configuration directory
  LI_CONFIG_DIR = '/etc/lipa/';
  //** Prefix for application database
  LI_APPDB_PREF = 'info/';
  //** Path to global Listaller package lib dir
  RootPkgRegDir = LI_CONFIG_DIR + LI_APPDB_PREF;

var
  //** The Listaller package lib directory
  PkgRegDir: String = '';

type
  //** Urgency levels
  TUrgencyLevel = (ulLow, ulNormal, ulCritical);

//** Creates Listaller's config dir @returns Current config dir
function ConfigDir: String;
//** Get current data file (check /usr/share and current dir)
function GetDataFile(s: String): String;
//** Executes a command-line application @returns The application's last output string
function CmdResult(cmd: String): String;
//** Executes a command-line application @returns The application's exit code
function CmdResultCode(cmd: String): Integer;
{** Executes a command-line application
    @param cmd Command that should be executed
    @param Result TStringList to recive the output}
procedure CmdResultList(cmd: String; Result: TStringList);
//** Executes a command-line application @returns The final application output string (without breaks in line and invisible codes)
function CmdFinResult(cmd: String): String;
//** Get server name from an url @returns The server name
function GetServerName(url: String): String;
//** Path on an server (from an url) @returns The path on the server
function GetServerPath(url: String): String;
{** Fast check if entry is in a list
    @param nm Name of the entry that has to be checked
    @param list The string list that has to be searched}
function IsInList(nm: String; list: TStringList): Boolean;
//** Shows pkMon if option is set in preferences
procedure ShowPKMon();
//** Get the current, usable language variable
function GetLangID: String;
//** Check if mo filename matches locale
function MoFileMatchesLang(id: String; mo: String): Boolean;
//** Search for files in dir
procedure FindFiles(FilesList: TStringList; StartDir, FileMask: String;
  const recursive: Boolean = true);
//** Find all dirs in a directory (NOT recursive!)
procedure FindDirs(DirList: TStringList; StartDir: String);
//** Reads the current system architecture @returns Detected architecture as string
function GetSystemArchitecture: String;
{** Executes an application as root using an graphical input prompt
    @param cmd Command line string to execute
    @param comment Description of the action the user is doing (why are root-rights needed?
    @param icon Path to an icon for the operation
    @param optn Set of TProcessOption to apply on the process object}
function ExecuteAsRoot(cmd: String; comment: String; icon: String;
  optn: TProcessOptions = []): Boolean;
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
procedure perror(msg: String);
//** Displays a simple warning
procedure pwarning(msg: String);
//** Displays info
procedure pinfo(msg: String);
//** Show a debug bessage
procedure pdebug(msg: String);
//** Replace string a with b
function StrSubst(s, a, b: String): String;
//** Find absolute path for system binary
function FindBinary(Name: String): String;
//** Check if program is running @param cmd Command name
function IsCommandRunning(cmd: String): Boolean;
//** Remove modifiers from a string @returns Cleaned string
function DeleteModifiers(s: String): String;
//** Replaces placeholders (like $INSt or $APP) with their current paths @returns Final path as string
function SyblToPath(s: String; const tm: Boolean = false; root: Boolean = false): String;
//** Removes symbol or replace it an simple dummy path (Used in IPKPackager) @returns Cleaned string
function SyblToX(s: String): String;
//** Deletes all variables in string
function DeleteSybls(s: String): String;
//** Check if file is a shared one @returns States as bool
function HasSharedMod(fname: String): Boolean;
{** Get dependencies of an executable
    @param f Name of the binary file
    @param lst StringList to recieve the output
    @returns Success of operation}
function GetELFDepends(f: String; lst: TStringList): Boolean;
//** Advanced file copy method @returns Success of the command
function FileCopy(Source, dest: String): Boolean;
//** Check if pointer is valid
function CheckPtr(ptr: Pointer;objname: String = ''): Boolean;
//** Check if file is configuration file
function HasConfigMod(fname: String): Boolean;
//** Search for full application icon path on system
function GetAppIconPath(icon: String): String;
//** Create an appID from .desktop-file path
function GenerateAppID(desktopFile: String): String;
//** Restore desktop file path from appID
function GetDesktopFile(appID: String): String;

//** Shows system notification box
procedure ShowPopupNotify(msg: String; urgency: TUrgencyLevel; time: Integer);

implementation

uses distri; //Access to distribution data

function GetServerName(url: String): String;
var
  p1, p2: Integer;
begin
  url := StringReplace(url, 'ftp://', '', [rfReplaceAll]);
  url := StringReplace(url, 'http://', '', [rfReplaceAll]);
  p1 := 1;
  p2 := Pos('/', url);

  Result := Copy(url, P1, (P2 - 1));
end;

function IsCommandRunning(cmd: String): Boolean;
var
  t: TProcess;
  s: TStringList;
begin
  Result := false;
  t := tprocess.Create(nil);
  t.CommandLine := FindBinary('ps') + ' -A' + cmd;
  t.Options := [poUsePipes, poWaitonexit];
  try
    t.Execute;
    s := TStringList.Create;
    try
      s.LoadFromStream(t.Output);
      Result := Pos(cmd, s.Text) > 0;
    finally
      s.Free;
    end;
  finally
    t.Free;
  end;
end;

function GetServerPath(url: String): String;
var
  p1, p2: Integer;
begin
  url := StringReplace(url, 'http://', '', [rfReplaceAll]);
  url := StringReplace(url, 'ftp://', '', [rfReplaceAll]);
  url := StringReplace(url, 'www.', '', [rfReplaceAll]);
  p1 := Pos('/', url);
  p2 := Length(url);
  Result := ExtractFilePath(Copy(url, P1, P2));
end;

function CheckPtr(ptr: Pointer;objname: String = ''): Boolean;
begin
  if not Assigned(ptr) then
  begin
    if objname = '' then
     perror('Received invalid object pointer!')
    else
     perror('Received invalid ´'+objname+'´ pointer!');
    Result := false;
  end else
   Result := true;
end;

function GetDataFile(s: String): String;
var
  D: TDistroInfo;
begin
  if (FileExists(ExtractFilePath(ParamStr(0)) + s)) or
    (DirectoryExists(ExtractFilePath(ParamStr(0)) + s)) then
    Result := ExtractFilePath(ParamStr(0)) + s
  else if (FileExists(ExtractFilePath(ParamStr(0)) + '../' + s)) or
      (DirectoryExists(ExtractFilePath(ParamStr(0)) + '../' + s)) then
      Result := ExtractFilePath(ParamStr(0)) + '../' + s
    else
      if pos('graphics/', s) > 0 then
      begin
        D := GetDistro;
        //Apply graphics branding
        if FileExists('/usr/share/listaller/branding/' + LowerCase(D.DName) +
          '/' + s) then
          Result := '/usr/share/listaller/branding/' + LowerCase(D.DName) + '/' + s
        else
          Result := '/usr/share/listaller/' + s;
      end
      else
        Result := '/usr/share/listaller/' + s;
end;

procedure FindDirs(DirList: TStringList; StartDir: String);
var
  SR: TSearchRec;
  IsFound: Boolean;
begin
  IsFound := SysUtils.FindFirst(StartDir + '*', faAnyFile, SR) = 0;
  while IsFound do
  begin
    if ((SR.Attr and faDirectory) <> 0) and (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := SysUtils.FindNext(SR) = 0;
  end;
  SysUtils.FindClose(SR);
end;

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: String;
  const recursive: Boolean = true);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: Integer;
begin
  if FileMask = '' then
    FileMask := '*';

  if StartDir[length(StartDir)] <> '/' then
    StartDir := StartDir + '/';

  IsFound := SysUtils.FindFirst(StartDir + FileMask, faAnyFile - faDirectory, SR) = 0;
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
    IsFound := SysUtils.FindFirst(StartDir + '*', faAnyFile, SR) = 0;
    while IsFound do
    begin
      if ((SR.Attr and faDirectory) <> 0) and (SR.Name[1] <> '.') then
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

function FindBinary(Name: String): String;
var
  res: String;

  function FExists(p: String): Boolean;
  begin
    res := p + '/' + Name;
    Result := FileExists(res);
  end;

begin
  if FExists('/usr/local/sbin') then
    Result := res
  else if FExists('/usr/local/bin') then
      Result := res
    else if FExists('/usr/sbin') then
        Result := res
      else if FExists('/usr/bin') then
          Result := res
        else if FExists('/sbin') then
            Result := res
          else if FExists('/bin') then
              Result := res
            else if FExists('/usr/games') then
                Result := res
              else
                Result := Name;
end;

function GetSystemArchitecture: String;
var
  p: TProcess;
  x: TStringList;
  s: String;
begin
  p := TProcess.Create(nil);
  p.Options := [poUsePipes, poWaitOnExit];
  p.CommandLine := FindBinary('uname') + ' -m';
  p.Execute;
  x := TStringList.Create;
  x.LoadFromStream(p.OutPut);
  s := x[0];
  x.Free;
  p.Free;

  if (s = 'i686') or (s = 'i586') or (s = 'i486') then
    s := 'i386';
  if (s = 'x86_64') then
    s := 'amd64';
  Result := s;
end;

function GetDateAsString: String;
var
  Year, Month, Day, WDay: word;
begin
  Year := 0;
  Month := 0;
  Day := 0;
  WDay := 0;
  GetDate(Year, Month, Day, WDay);
  Result := IntToStr(Day) + '.' + IntToStr(Month) + '.' + IntToStr(Year);
end;

procedure RemoveDuplicates(sl: TStringList);
var
  i, j: Integer;
begin
  i := 0;
  while i <= sl.Count - 1 do
  begin
    for j := i + 1 to sl.Count - 1 do
    begin
      if sl.Strings[i] = sl.Strings[j] then
      begin
        Dec(i);
        sl.Delete(j);
        break;
      end;
    end;
    Inc(i);
  end;
end;

function GenerateAppID(desktopFile: String): String;
begin
  Result := '';
  if trim(desktopFile) = '' then exit;
  Result := ExtractFilePath(desktopFile);
  Result := StrSubst(StrSubst(Result, '/usr/', ''), 'share/applications/','');
  Result := ExcludeTrailingPathDelimiter(Result) + StrSubst(ExtractFileName(desktopFile), '.desktop', '');
end;

function GetDesktopFile(appID: String): String;
begin
  Result := '';
  if trim(appID) = '' then exit;
  Result := StrSubst(appID, 'local/', 'local/share/applications/');
  if pos('local/share/applications/', Result) <= 0 then
   Result := 'share/applications/' + appID;
  Result := '/usr/' + Result;
  Result := Result + '.desktop';
end;

function GetLangID: String;
var
  LANG: String;
  i: Integer;
begin
  LANG := GetEnvironmentVariable('LANG');
  if LANG = '' then
  begin
    for i := 1 to Paramcount - 1 do
      if (ParamStr(i) = '--LANG') or (ParamStr(i) = '-l') or
        (ParamStr(i) = '--lang') then
        LANG := ParamStr(i + 1);
  end;
  if not DirectoryExists('/usr/share/locale-langpack/' + LANG) then
    Result := copy(LANG, 1, 2)
  else
    Result := LANG;
end;

function ConfigDir: String;
var
  h: String;
begin
  h := GetEnvironmentVariable('HOME');
  h := h + '/.config';
  if not DirectoryExists(h) then
    CreateDir(h);
  h := h + '/Listaller';
  if not DirectoryExists(h) then
    CreateDir(h);
  Result := h + '/';
end;

function CmdResult(cmd: String): String;
var
  t: TProcess;
  s: TStringList;
begin
  Result := '';
  t := tprocess.Create(nil);
  t.Options := [poUsePipes, poWaitOnExit];
  t.CommandLine := cmd;
  try
    t.Execute;
    s := TStringList.Create;
    try
      s.LoadFromStream(t.Output);
      if s.Count <= 0 then
        Result := 'err'
      else
        Result := s[0];
    finally
      s.Free;
    end;
  finally
    t.Free;
  end;
end;

function IsRoot: Boolean;
var
  p: PPasswd;
begin
  p := fpgetpwuid(fpgetuid);
  Result := false;
  if assigned(p) then
  begin
    if p^.pw_name <> 'root' then
      Result := false
    else
      Result := true;
  end
  else
    perror('Internal error');
end;

procedure ShowPKMon();
var
  cnf: TIniFile;
  t: TProcess;
begin
  if not FileExists(FindBinary('pkmon')) then
  begin
    perror('Could not find "pkmon". Monitor not started.');
    exit;
  end;

  //Check if PackageKit checkmode is enabled:
  cnf := TIniFile.Create(ConfigDir + 'config.cnf');
  if cnf.ReadBool('MainConf', 'ShowPkMon', false) then
  begin
    t := TProcess.Create(nil);
    t.CommandLine := FindBinary('pkmon') + ' -v';
    t.Options := [poNewConsole];
    t.Execute;
  end;
  cnf.Free;
end;

procedure perror(msg: String);
begin
  writeLn('error: ' + msg);
end;

procedure pwarning(msg: String);
begin
  writeLn('warning: ' + msg);
end;

procedure pinfo(msg: String);
begin
  writeLn(msg);
end;

procedure pdebug(msg: String);
begin
  {$IFNDEF NoDebugMsg}
  writeLn('debug: ' + msg);
  {$ENDIF}
end;

function CmdFinResult(cmd: String): String;
var
  t: TProcess;
  Buffer: String;
  BytesAvailable: DWord;
  BytesRead: longint;
begin
  Result := '';
  buffer := '';
  t := tprocess.Create(nil);
  t.Options := [poUsePipes];
  t.CommandLine := cmd;
  try
    t.Execute;
    while t.Running do
    begin
      BytesAvailable := t.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable > 0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := t.OutPut.Read(Buffer[1], BytesAvailable);
        if (pos(#13, Buffer) > 0) or (pos(#26, Buffer) > 0) or
          (Pos(#10, Buffer) > 0) then
          Result := '';
        Result := Result + copy(Buffer, 1, BytesRead);
        BytesAvailable := t.OutPut.NumBytesAvailable;
      end;
    end;
  finally
    t.Free;
  end;
end;

function CmdResultCode(cmd: String): Integer;
var
  t: TProcess;
begin
  Result := 0;
  t := tprocess.Create(nil);
  t.Options := [poUsePipes, poWaitonexit];
  t.CommandLine := cmd;
  try
    t.Execute;
    Result := t.ExitStatus;
  finally
    t.Free;
  end;
end;

function CleanFilePath(path: String): String;
begin
  Result := StringReplace(path, '//', '/', [rfReplaceAll]);
end;

//This should no longer be necessary since Listaller uses PolicyKit
function ExecuteAsRoot(cmd: String; comment: String; icon: String;
  optn: TProcessOptions = []): Boolean;
var
  p: TProcess;
  DInfo: TDistroInfo;
begin
  DInfo := GetDistro;
  p := TProcess.Create(nil);
  if DInfo.DBase = 'KDE' then
  begin
    if FileExists(FindBinary('kdesu')) then
      p.CommandLine := FindBinary('kdesu') + ' -d --comment "' +
        comment + '" -i ' + icon + ' ' + cmd
    else
      if FileExists(FindBinary('kdesudo')) then
        p.CommandLine := FindBinary('kdesudo') + ' -d --comment "' +
          comment + '" -i ' + icon + ' ' + cmd;
  end
  else
  begin
    if DInfo.DName = 'Fedora' then
      //Fedora uses Consolehelper to run apps as root. So we use "beesu" to make CH work for Listaller
      p.CommandLine := FindBinary('beesu') + ' -l ' + cmd
    else
      if FileExists(FindBinary('gksudo')) then
        p.CommandLine := FindBinary('gksudo') + ' --message "' + comment + '" ' + cmd
      else
        if FileExists(FindBinary('gksu')) then
          p.CommandLine := FindBinary('gksu') + ' --message "' + comment + '" ' + cmd
        else
          if FileExists(FindBinary('gnomesu')) then
            p.CommandLine := FindBinary('gnomesu') + ' ' + cmd
          else
          begin
            writeLn('Unable to execute the application as root.'#13'Please do this manually!');
            p.Free;
            Result := false;
            exit;
          end;
  end;
  p.Options := optn;
  p.Execute;
  Result := true;
  if p.ExitStatus > 0 then
    Result := false;
  p.Free;
end;

procedure CmdResultList(cmd: String; Result: TStringList);
var
  t: TProcess;
  s: TStringList;
begin
  t := tprocess.Create(nil);
  t.CommandLine := cmd;
  t.Options := [poUsePipes, poWaitonexit];
  try
    t.Execute;
    s := TStringList.Create;
    try
      s.LoadFromStream(t.Output);
      if t.ExitStatus <= 0 then
        Result.Assign(s)
      else
        Result.Clear;

    finally
      s.Free;
    end;
  finally
    t.Free;
  end;
end;

function MoFileMatchesLang(id: String; mo: String): Boolean;
begin
  Result := false;
  mo := ExtractFileName(mo);
  if copy(mo, pos(mo, '-') + 1, length(mo)) = id + '.mo' then
    Result := true;
  if id + '.mo' = mo then
    Result := true;
end;

function IsInList(nm: String; list: TStringList): Boolean;
begin
  Result := list.IndexOf(nm) > -1;
end;

function StrSubst(s, a, b: String): String;
begin
  Result := StringReplace(s, a, b, [rfReplaceAll]);
end;

function GetELFDepends(f: String; lst: TStringList): Boolean;
var
  p: TProcess;
  s: TStringList;
  i: Integer;
begin
  p := TProcess.Create(nil);
  p.Options := [poUsePipes, poWaitOnExit];
  p.CommandLine := FindBinary('ldd') + ' ' + f;
  p.Execute;
  Result := true;
  if p.ExitStatus > 0 then
  begin
    Result := false;
    p.Free;
    exit;
  end;
  s := TStringList.Create;
  s.LoadFromStream(p.Output);
  p.Free;
  for i := 0 to s.Count - 1 do
    if pos('=>', s[i]) > 0 then
    begin
      lst.Add(copy(s[i], 2, pos('=', s[i]) - 2));
    end
    else
      lst.Add(copy(s[i], 2, pos('(', s[i]) - 2));
  s.Free;
end;

function HasSharedMod(fname: String): Boolean;
begin
  Result := pos(' <s>', fname) > 0;
end;

function HasConfigMod(fname: String): Boolean;
begin
  Result := pos(' <config>', fname) > 0;
end;

function GetXHome(const TestMode: Boolean = false): String;
begin
  if TestMode then
  begin
    Result := '/tmp/litest';
    CreateDir(Result);
  end
  else
    Result := GetEnvironmentVariable('HOME');
end;

function SyblToPath(s: String; const tm: Boolean = false; root: Boolean = false): String;
var
  n: String;
  DInfo: TDistroInfo;
begin
  s := StringReplace(s, '$HOME', GetEnvironmentVariable('HOME'), [rfReplaceAll]);
  s := StrSubst(s, '$', '');

  n := GetSystemArchitecture;

  DInfo := GetDistro;

  if root then
  begin
    s := StringReplace(s, 'INST', '/opt/appfiles', [rfReplaceAll]);
    s := StringReplace(s, 'DEP', '/opt/shared', [rfReplaceAll]);
    s := StringReplace(s, 'SHARE', '/usr/share', [rfReplaceAll]);
    s := StringReplace(s, 'OPT', '/opt', [rfReplaceAll]);
    s := StringReplace(s, 'BIN', '/usr/bin', [rfReplaceAll]);
    s := StringReplace(s, 'SBIN', '/usr/sbin', [rfReplaceAll]);

    //Fedora uses lib64 dir.
    if (LowerCase(n) = 'x86_64') and (DInfo.DName = 'Fedora') then
      s := StringReplace(s, 'LIB', '/usr/lib64', [rfReplaceAll])
    else
      s := StringReplace(s, 'LIB', '/usr/lib', [rfReplaceAll]);

    s := StringReplace(s, 'LIB32', '/usr/lib32', [rfReplaceAll]);

    s := StringReplace(s, 'APP', '/usr/share/applications', [rfReplaceAll]);
    s := StringReplace(s, 'ICON-16', '/usr/share/icons/hicolor/16x16/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-24', '/usr/share/icons/hicolor/24x24/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-32', '/usr/share/icons/hicolor/32x32/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-48', '/usr/share/icons/hicolor/48x48/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-64', '/usr/share/icons/hicolor/64x64/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-128', '/usr/share/icons/hicolor/128x128/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-256', '/usr/share/icons/hicolor/256x256/apps',
      [rfReplaceAll]);
    s := StringReplace(s, 'PIX', '/usr/share/pixmaps', [rfReplaceAll]);
  end
  else
  begin
    if not DirectoryExists(GetXHome(tm) + '/.applications/') then
    begin
      CreateDir(GetXHome(tm) + '/.applications/');
      if fpSymLink(PChar(GetXHome(tm) + '/.applications/'), PChar(
        GetXHome(tm) + '/' + rsApplications)) <> 0 then
        perror('Unable to creat symlink!');
    end;
    if not DirectoryExists(GetXHome(tm) + '/.appfiles') then
      CreateDir(GetXHome(tm) + '/.appfiles/');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons');
    //Iconpaths
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/16x16') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/16x16');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/24x24') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/24x24');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/32x32') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/32x32');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/48x48') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/48x48');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/64x64') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/64x64');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/128x128') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/128x128');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/256x256') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/256x256');
    if not DirectoryExists(GetXHome(tm) + '/.appfiles/icons/common') then
      CreateDir(GetXHome(tm) + '/.appfiles/icons/common');
    if LowerCase(n) = 'x86_64' then
    begin
      if not DirectoryExists(GetXHome(tm) + '/.appfiles/lib64') then
        CreateDir(GetXHome(tm) + '/.appfiles/lib64');
    end
    else
      if not DirectoryExists(GetXHome(tm) + '/.appfiles/lib') then
        CreateDir(GetXHome(tm) + '/.appfiles/lib');

    //SBin and Bin go to the same directory on HOME install
    s := StringReplace(s, 'BIN', GetXHome(tm) + '/.appfiles/binary', [rfReplaceAll]);
    s := StringReplace(s, 'SBIN', GetXHome(tm) + '/.appfiles/binary', [rfReplaceAll]);

    if LowerCase(n) = 'i386' then
      s := StringReplace(s, 'LIB', GetXHome(tm) + '/.appfiles/lib32', [rfReplaceAll])
    else
      s := StringReplace(s, 'LIB', GetXHome(tm) + '/.appfiles/lib', [rfReplaceAll]);

    s := StringReplace(s, 'LIB32', GetXHome(tm) + '/.appfiles/lib32', [rfReplaceAll]);

    s := StringReplace(s, 'DEP', GetXHome(tm) + '/.appfiles', [rfReplaceAll]);
    s := StringReplace(s, 'INST', GetXHome(tm) + '/.appfiles', [rfReplaceAll]);
    s := StringReplace(s, 'SHARE', GetXHome(tm) + '/.appfiles', [rfReplaceAll]);
    s := StringReplace(s, 'OPT', GetXHome(tm) + '/.appfiles', [rfReplaceAll]);
    s := StringReplace(s, 'APP', GetXHome(tm) + '/.applications', [rfReplaceAll]);
    s := StringReplace(s, 'ICON-16', GetXHome(tm) + '/.appfiles/icons/16x16',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-24', GetXHome(tm) + '/.appfiles/icons/24x24',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-32', GetXHome(tm) + '/.appfiles/icons/32x32',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-48', GetXHome(tm) + '/.appfiles/icons/48x48',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-64', GetXHome(tm) + '/.appfiles/icons/64x64',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-128', GetXHome(tm) + '/.appfiles/icons/128x128',
      [rfReplaceAll]);
    s := StringReplace(s, 'ICON-256', GetXHome(tm) + '/.appfiles/icons/256x256',
      [rfReplaceAll]);
    s := StringReplace(s, 'PIX', GetXHome(tm) + '/.appfiles/icons/common', [rfReplaceAll]);
  end;
  Result := s;
end;

function SyblToX(s: String): String;
begin
  s := StringReplace(s, '$INST', '', [rfReplaceAll]);
  s := StringReplace(s, '$SHARE', '', [rfReplaceAll]);
  s := StringReplace(s, '$BIN', '', [rfReplaceAll]);
  s := StringReplace(s, '$OPT', '/opt', [rfReplaceAll]);
  s := StringReplace(s, '$APP', '/app', [rfReplaceAll]);
  s := StringReplace(s, '$HOME', '/hdir', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-16', '/icon16', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-24', '/icon24', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-32', '/icon32', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-48', '/icon48', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-64', '/icon64', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-128', '/icon128', [rfReplaceAll]);
  s := StringReplace(s, '$ICON-256', '/icon265', [rfReplaceAll]);
  s := StringReplace(s, '$PIX', '/icon', [rfReplaceAll]);
  s := StringReplace(s, '$LIB', '/lib', [rfReplaceAll]);
  s := StringReplace(s, '$LIB32', '/lib32', [rfReplaceAll]);
  Result := s;
end;

function DeleteSybls(s: String): String;
begin
  s := StrSubst(s, '$INST', '');
  s := StrSubst(s, '$SHARE', '');
  s := StrSubst(s, '$DEP', '');
  s := StrSubst(s, '$BIN', '');
  s := StrSubst(s, '$OPT', '');
  s := StrSubst(s, '$APP', '');
  s := StrSubst(s, '$HOME', '');
  s := StrSubst(s, '$ICON-16', '');
  s := StrSubst(s, '$ICON-24', '');
  s := StrSubst(s, '$ICON-32', '');
  s := StrSubst(s, '$ICON-48', '');
  s := StrSubst(s, '$ICON-64', '');
  s := StrSubst(s, '$ICON-128', '');
  s := StrSubst(s, '$ICON-256', '');
  s := StrSubst(s, '$PIX', '');
  s := StrSubst(s, '$LIB', '');
  s := StrSubst(s, '$LIB32', '');
  if s[1] = '/' then
    s := copy(s, 2, length(s));
  Result := s;
end;

function DeleteModifiers(s: String): String;
var
  h: String;
begin
  h := SysUtils.StringReplace(s, ' <s>', '', [rfReplaceAll]);
  h := SysUtils.StringReplace(s, ' <config>', '', [rfReplaceAll]);
  h := ReplaceRegExpr(' <chmod:([0-7]{3})>', h, '', false);
  h := ReplaceRegExpr(' <([a-zA-Z_]{4,})-only>', h, '', false);
  h := SysUtils.StringReplace(h, ' <mime>', '', [rfReplaceAll]);
  h := SysUtils.StringReplace(h, ' <setvars>', '', [rfReplaceAll]);
  h := SysUtils.StringReplace(h, '>', '', [rfReplaceAll]);
  Result := h;
end;

function FileCopy(Source, dest: String): Boolean;
var
  fSrc, fDst, len: Integer;
  ct, units, size: longint;
  buffer: packed array [0..2047] of byte;
begin
  ct := 0;
  Result := false; { Assume that it WONT work }
  if Source <> dest then
  begin
    fSrc := FileOpen(Source, fmOpenRead);
    if fSrc >= 0 then
    begin
      size := FileSeek(fSrc, 0, 2);
      units := size div 2048;
      FileSeek(fSrc, 0, 0);
      fDst := FileCreate(dest);
      if fDst >= 0 then
      begin
        while size > 0 do
        begin
          len := FileRead(fSrc, buffer, sizeof(buffer));
          FileWrite(fDst, buffer, len);
          size := size - len;
          if units > 0 then
            ct := ct + 1;
        end;
        FileSetDate(fDst, FileGetDate(fSrc));
        FileClose(fDst);
        FileSetAttr(dest, FileGetAttr(Source));
        Result := true;
      end;
      FileClose(fSrc);
    end;
  end;
end;

function SolveBuildTimeSybl(str: String): String;
begin
  Result := str;
end;

function GetAppIconPath(icon: String): String;
var
  res: String;
  hasExt: Boolean;

  function GetExt(str: String): String;
  begin
    Result := LowerCase(ExtractFileExt(str));
  end;

  function CheckRes: Boolean;
  begin
    Result := true;
    if not FileExists(res) then
      res := '';
    if res = '' then
      Result := false;
  end;

  function Checkdir(dir: String): Boolean;
  begin
    dir := dir + '/';
    if not hasExt then
    begin
      res := dir + icon + '.png';
      if not Checkres then
      begin
        res := dir + icon + '.xpm';
        if not Checkres then
        begin
          res := dir + icon + '.jpg';
          if not Checkres then
          begin
            res := dir + icon + '.bmp';
            Checkres;
          end;
        end;
      end;
    end
    else
    begin
      res := dir + icon;
      Checkres;
    end;
    Result := true;
    if res = '' then
      Result := false;
  end;

begin
  Result := '';
  if icon = '' then
    exit;
  //Cannot work with those files
  if GetExt(icon) = '.tiff' then
    exit;
  if GetExt(icon) = '.svg' then
    exit;

  if (FileExists(icon)) then
    Result := icon;
  hasExt := true;
  if GetExt(icon) = '' then
    hasExt := false;

  if Checkdir('/usr/share/icons/hicolor/64x64/apps') then
    Result := res;
  if res = '' then
    if Checkdir('/usr/share/pixmaps') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/share/icons') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/share/icons/hicolor/48x48/apps') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/share/icons/hicolor/128x128/apps') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/share/icons/locolor/64x64/apps') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/share/icons/default.kde4/64x64/apps') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/lib/kde4/share/icons/hicolor/64x64/apps') then
      Result := res;
  if res = '' then
    if Checkdir('/usr/share/icons/default.kde/64x64/apps') then
      Result := res;
end;

procedure ShowPopupNotify(msg: String; urgency: TUrgencyLevel; time: Integer);
var
  p: TProcess;
  DInfo: TDistroInfo;
  s: String;
begin
  DInfo := GetDistro;
  p := TProcess.Create(nil);
  p.Options := [poUsePipes];
  if DInfo.DBase = 'KDE' then
  begin
    p.CommandLine := FindBinary('kdialog') + ' --passivepopup "' +
      msg + '" --title "Listaller Message" ' + IntToStr(time);
    p.Execute;
    p.Free;
    exit;
  end
  else
    if FileExists('/usr/bin/notify-send') then
    begin
      case urgency of
        ulNormal: s := 'normal';
        ulLow: s := 'low';
        ulCritical: s := 'critical';
      end;
      p.CommandLine := FindBinary('notify-send') + ' --urgency=' +
        s + ' --expire-time=' + IntToStr(time * 1000) + ' "' + msg + '"';
      p.Execute;
      p.Free;
      exit;
    end;
end;

initialization
  if IsRoot then
    PkgRegDir := LI_CONFIG_DIR + LI_APPDB_PREF
  else
    PkgRegDir := SyblToPath('$INST') + '/' + LI_APPDB_PREF;

end.

