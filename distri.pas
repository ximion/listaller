{ Copyright (C) 2008-2009 Matthias Klumpp

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
//** Unit to research information about the current distribution (LSB-conform)
unit distri;

{$mode delphi}{$H+}

interface

uses SysUtils, Classes, Process, BaseUnix, pwd;

type
//** Contains information about the current Linux distribution
TDistroInfo = record
//** Name of the distro
DName: String;
//** Release number
Release: String;
//** Package management system
PackageSystem: String;
//** The detected desktop environment
Desktop: String;
//** The desktop-base (KDE/GNOME), used to choose the right applications for various actions
DBase: String;
end;

//** Get the distro-infos
function GetDistro: TDistroInfo;
{** Check if user is root
 @returns If user is root (Bool)}
function IsRoot: Boolean;
//** Check if program is running @param cmd Command name
function IsCommandRunning(cmd:String):Boolean;

implementation

function IsCommandRunning(cmd:String):Boolean;
var t:TProcess;
s:TStringList;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.CommandLine:='ps -A'+cmd;
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

function IsRoot: Boolean;
var p : PPasswd;
begin
p:=fpgetpwuid(fpgetuid);
Result:=false;
 if assigned(p) then
    begin
       if p.pw_name<>'root' then
         Result:=false
        else  Result:=true;
     end else
   writeLn('Internal error');
end;

function GetDistro: TDistroInfo;
var uv: TStringList;i: Integer;
begin
//Check which Distribution is used
Result.DName:='';
uv:=TStringList.Create;
if FileExists('/etc/lipa/distribution') then
begin
uv.LoadFromFile('/etc/lipa/distribution');
Result.DName:=copy(uv[0],pos(' ',uv[0])+1,length(uv[0]));
Result.Release:=copy(uv[1],pos(' ',uv[1])+1,length(uv[1]));
Result.PackageSystem:=copy(uv[2],pos(' ',uv[2])+1,length(uv[2]));
uv.Clear;
end else
begin
if FileExists('/etc/lsb-release') then
uv.LoadFromFile('/etc/lsb-release');

for i:=0 to uv.Count-1 do begin
if pos('UBUNTU',UpperCase(uv[i]))>0 then
Result.DName:='Ubuntu';

if pos('SUSE',UpperCase(uv[i]))>0 then
Result.DName:='SuSE';;

if pos('DEBIAN',UpperCase(uv[i]))>0 then
Result.DName:='Debian';

if pos('MANDRIVA',UpperCase(uv[i]))>0 then
Result.DName:='Mandriva';

if pos('PCLINUXOS',UpperCase(uv[i]))>0 then
Result.DName:='PCLinuxOS';

if pos('XANDROS',UpperCase(uv[i]))>0 then
Result.DName:='Xandros';

if pos('FEDORA',UpperCase(uv[i]))>0 then
Result.DName:='Fedora';

if pos('MOBLIN',UpperCase(uv[i]))>0 then
Result.DName:='Moblin';

end;
if Result.DName='' then begin
uv.LoadFromFile('/proc/version');
for i:=0 to uv.Count-1 do begin
if pos('UBUNTU',UpperCase(uv[i]))>0 then
Result.DName:='Ubuntu';

if pos('SUSE',UpperCase(uv[i]))>0 then
Result.DName:='openSUSE';

if pos('DEBIAN',UpperCase(uv[i]))>0 then
Result.DName:='Debian';

if pos('MANDRIVA',UpperCase(uv[i]))>0 then
Result.DName:='Mandriva';

if pos('PCLINUXOS',UpperCase(uv[i]))>0 then
Result.DName:='PCLinuxOS';

if pos('XANDROS',UpperCase(uv[i]))>0 then
Result.DName:='Xandros';

if pos('FEDORA',UpperCase(uv[i]))>0 then
Result.DName:='Fedora';

 end;
end;
if (Result.DName='')and(FileExists('/etc/issue')) then
begin
uv.LoadFromFile('/etc/issure');
for i:=0 to uv.Count-1 do
begin
if pos('UBUNTU',UpperCase(uv[i]))>0 then
Result.DName:='Ubuntu';

if pos('SUSE',UpperCase(uv[i]))>0 then
Result.DName:='openSUSE';

if pos('DEBIAN',UpperCase(uv[i]))>0 then
Result.DName:='Debian';

if pos('MANDRIVA',UpperCase(uv[i]))>0 then
Result.DName:='Mandriva';

if pos('PCLINUXOS',UpperCase(uv[i]))>0 then
Result.DName:='PCLinuxOS';

if pos('XANDROS',UpperCase(uv[i]))>0 then
Result.DName:='Xandros';

if pos('FEDORA',UpperCase(uv[i]))>0 then
Result.DName:='Fedora';

if pos('MOBLIN',UpperCase(uv[i]))>0 then
Result.DName:='Moblin';

end;
 
end;
if Result.DName='Ubuntu' then
Result.PackageSystem:='DEB';

if Result.DName='openSUSE' then
Result.PackageSystem:='RPM';

if Result.DName='Debian' then
Result.PackageSystem:='DEB';

if Result.DName='Mandriva' then
Result.PackageSystem:='RPM';

if Result.DName='PCLinuxOS' then
Result.PackageSystem:='RPM';

if Result.DName='Xandros' then
Result.PackageSystem:='DEB';

if Result.DName='Fedora' then
Result.PackageSystem:='RPM';

if Result.DName='Moblin' then
Result.PackageSystem:='RPM';

if FileExists('/etc/lsb-release') then begin
uv.LoadFromFile('/etc/lsb-release');
for i:=0 to uv.Count-1 do
if pos('DISTRIB_RELEASE=',uv[i])>0 then break;
Result.Release:=copy(uv[i],pos('=',uv[i])+1,length(uv[i]));
end;
uv.Clear;
uv.Add('Name: '+Result.DName);
uv.Add('Release: '+Result.Release);
uv.Add('PackageFormat: '+Result.PackageSystem);
try
uv.SaveToFile('/etc/lipa/distribution');
except end;
end;
if (pos('kde',LowerCase(GetEnvironmentVariable('GDMSESSION')))>0)or (pos('kde',LowerCase(GetEnvironmentVariable('DESKTOP_SESSION')))>0)
or (GetEnvironmentVariable('KDE_FULL_SESSION')='true') then
Result.DBase:='KDE' else
Result.DBase:='GNOME'; //Gnome/Xfce/E17 ...

//Read the "real" DE
Result.Desktop:=GetEnvironmentVariable('DESKTOP_SESSION');
if Result.Desktop='' then
Result.Desktop:=GetEnvironmentVariable('GDMSESSION');
Result.Desktop:=UpperCase(Result.Desktop);

if Result.DName='Moblin' then
begin
 Result.DBase:='GNOME';
 Result.Desktop:='Linux Mobile (Clutter)';
end;

uv.Free;
end;

end.
