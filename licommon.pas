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
//** This unit contains some common functions to work with IPK files
unit licommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, process, RegExpr,
  distri, Graphics, liBasic;

type
//** Urgency levels
TUrgencyLevel = (ulLow, ulNormal, ulCritical);

//** Remove modifiers from a string @returns Cleaned string
function  DeleteModifiers(s: String): String;
//** Replaces placeholders (like $INSt or $APP) with their current paths @returns Final path as string
function  SyblToPath(s: String): String;
//** Removes every symbol or replace it an simpla dummy path @returns Cleaned string
function  SyblToX(s: String): String;
//** Check if file is a shared one @returns States as bool
function  IsSharedFile(s: String): Boolean;
{** Fast check if entry is in a list
    @param nm Name of the entry that has to be checked
    @param list The string list that has to be searched}
function  IsInList(nm: String;list: TStringList): Boolean;
{** Get dependencies of an executable
    @param f Name of the binary file
    @param lst StringList to recieve the output
    @returns Success of operation}
function GetLibDepends(f: String; lst: TStringList): Boolean;
//** Advanced file copy method @returns Success of the command
function  FileCopy(source,dest: String): Boolean;

//** Shows system notification box
procedure ShowPopupNotify(msg: String;urgency: TUrgencyLevel;time:Integer);

implementation

function GetLibDepends(f: String; lst: TStringList): Boolean;
var p: TProcess;s: TStringList;i: Integer;
begin
  p:=TProcess.Create(nil);
  p.Options:=[poUsePipes,poWaitOnExit];
  p.CommandLine:='ldd '+f;
  p.Execute;
  Result:=true;
  if p.ExitStatus>0 then
  begin
   Result:=false;
   p.Free;
   exit;
  end;
  s:=TStringList.Create;
  s.LoadFromStream(p.Output);
  p.Free;
  for i:=0 to s.Count-1 do
   if pos('=>',s[i])>0 then
   begin
    lst.Add(copy(s[i],2,pos('=',s[i])-2));
   end else
    lst.Add(copy(s[i],2,pos('(',s[i])-2));
  s.Free;
end;

function IsSharedFile(s: String): Boolean;
begin
Result:=pos(' <s>',s)>0;
end;

function IsInList(nm: String;list: TStringList): Boolean;
begin
Result:=list.IndexOf(nm)>-1;
end;

function GetXHome: String;
begin
 if TestMode then
 begin
  Result:='/tmp/litest';
  CreateDir(Result);
  end else Result:=GetEnvironmentVariable('HOME');
end;

function SyblToPath(s: String): String;
var t: TProcess;n: String;x: TStringList;
begin
s:=StringReplace(s,'$HOME',GetEnvironmentVariable('HOME'),[rfReplaceAll]);

 t:=TProcess.Create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:='uname -m';
 t.Execute;
 x:=TStringList.Create;
 x.LoadFromStream(t.OutPut);
 n:=x[0];
 x.Free;
 t.Free;

if IsRoot then
begin
s:=StringReplace(s,'$INST','/opt/appfiles',[rfReplaceAll]);
s:=StringReplace(s,'$INST-X','/usr/share',[rfReplaceAll]);
s:=StringReplace(s,'$OPT','/opt',[rfReplaceAll]);
s:=StringReplace(s,'$BIN','/usr/bin',[rfReplaceAll]);

 if LowerCase(n)='x86_64' then
  s:=StringReplace(s,'$LIB','/usr/lib64',[rfReplaceAll])
 else
  s:=StringReplace(s,'$LIB','/usr/lib',[rfReplaceAll]);
s:=StringReplace(s,'$APP','/usr/share/applications',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-16','/usr/share/icons/hicolor/16x16/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-24','/usr/share/icons/hicolor/24x24/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-32','/usr/share/icons/hicolor/32x32/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-48','/usr/share/icons/hicolor/48x48/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-64','/usr/share/icons/hicolor/64x64/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-128','/usr/share/icons/hicolor/128x128/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-256','/usr/share/icons/hicolor/256x256/apps',[rfReplaceAll]);
s:=StringReplace(s,'$PIX','/usr/share/pixmaps',[rfReplaceAll]);
end else
begin
if not DirectoryExists(GetXHome+'/applications/') then CreateDir(GetXHome+'/applications/');
if not DirectoryExists(GetXHome+'/applications/files') then CreateDir(GetXHome+'/applications/files');
if not DirectoryExists(GetXHome+'/applications/files/icons') then CreateDir(GetXHome+'/applications/files/icons');
//Iconpaths
if not DirectoryExists(GetXHome+'/applications/files/icons/16x16')  then CreateDir(GetXHome+'/applications/files/icons/16x16');
if not DirectoryExists(GetXHome+'/applications/files/icons/24x24')  then CreateDir(GetXHome+'/applications/files/icons/24x24');
if not DirectoryExists(GetXHome+'/applications/files/icons/32x32')  then CreateDir(GetXHome+'/applications/files/icons/32x32');
if not DirectoryExists(GetXHome+'/applications/files/icons/48x48')  then CreateDir(GetXHome+'/applications/files/icons/48x48');
if not DirectoryExists(GetXHome+'/applications/files/icons/64x64')  then CreateDir(GetXHome+'/applications/files/icons/64x64');
if not DirectoryExists(GetXHome+'/applications/files/icons/128x128')then CreateDir(GetXHome+'/applications/files/icons/128x128');
if not DirectoryExists(GetXHome+'/applications/files/icons/256x256')then CreateDir(GetXHome+'/applications/files/icons/256x256');
if not DirectoryExists(GetXHome+'/applications/files/icons/common')then CreateDir(GetXHome+'/applications/files/icons/common');
if LowerCase(n)='x86_64' then
begin
 if not DirectoryExists(GetXHome+'/applications/files/lib64')then CreateDir(GetXHome+'/applications/files/lib64');
end else
 if not DirectoryExists(GetXHome+'/applications/files/lib')then CreateDir(GetXHome+'/applications/files/lib');
//
s:=StringReplace(s,'$BIN',GetXHome+'/applications/files/binary',[rfReplaceAll]);
if LowerCase(n)='x86_64' then
  s:=StringReplace(s,'$LIB',GetXHome+'/applications/files/lib64',[rfReplaceAll])
 else
  s:=StringReplace(s,'$LIB',GetXHome+'/applications/files/lib',[rfReplaceAll]);

s:=StringReplace(s,'$INST',GetXHome+'/applications/files',[rfReplaceAll]);
s:=StringReplace(s,'$INST-X',GetXHome+'/applications/files',[rfReplaceAll]);
s:=StringReplace(s,'$OPT',GetXHome+'/applications/files',[rfReplaceAll]);
s:=StringReplace(s,'$APP',GetXHome+'/applications',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-16',GetXHome+'/applications/files/icons/16x16',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-24',GetXHome+'/applications/files/icons/24x24',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-32',GetXHome+'/applications/files/icons/32x32',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-48',GetXHome+'/applications/files/icons/48x48',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-64',GetXHome+'/applications/files/icons/64x64',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-128',GetXHome+'/applications/files/icons/128x128',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-256',GetXHome+'/applications/files/icons/256x256',[rfReplaceAll]);
s:=StringReplace(s,'$PIX',GetXHome+'/applications/files/icons/common',[rfReplaceAll]);
end;
Result:=s;
end;

function SyblToX(s: String): String;
begin
s:=StringReplace(s,'$INST','',[rfReplaceAll]);
s:=StringReplace(s,'$INST-X','',[rfReplaceAll]);
s:=StringReplace(s,'$OPT','/opt',[rfReplaceAll]);
s:=StringReplace(s,'$APP','/app',[rfReplaceAll]);
s:=StringReplace(s,'$HOME','/hdir',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-16','/icon16',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-24','/icon24',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-32','/icon32',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-48','/icon48',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-64','/icon64',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-128','/icon128',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-256','/icon265',[rfReplaceAll]);
s:=StringReplace(s,'$PIX','/icon',[rfReplaceAll]);
s:=StringReplace(s,'$LIB','/lib',[rfReplaceAll]);
s:=StringReplace(s,'$LIB','/binary',[rfReplaceAll]);
Result:=s;
end;

function DeleteModifiers(s: String): String;
var h: String;
begin
h:=SysUtils.StringReplace(s,' <s>','',[rfReplaceAll]);
h:=ReplaceRegExpr(' <chmod:([0-7]{3})>', h, '', false);
h:=ReplaceRegExpr(' <([a-zA-Z_]{4,})-only>', h, '', false);
h:=SysUtils.StringReplace(h,' <mime>','',[rfReplaceAll]);
h:=SysUtils.StringReplace(h,' <setvars>','',[rfReplaceAll]);
h:=SysUtils.StringReplace(h,'>','',[rfReplaceAll]);
Result:=h;
end;

function FileCopy(source,dest: String): Boolean;
var
 fSrc,fDst,len: Integer;
 ct,units,size: Longint;
 buffer: packed array [0..2047] of Byte;
begin
 ct:=0;
 Result := False; { Assume that it WONT work }
 if source <> dest then
 begin
   fSrc := FileOpen(source,fmOpenRead);
   if fSrc >= 0 then
   begin
     size := FileSeek(fSrc,0,2);
     units:=size div 2048;
     FileSeek(fSrc,0,0);
     fDst := FileCreate(dest);
     if fDst >= 0 then
     begin
       while size > 0 do
       begin
         len := FileRead(fSrc,buffer,sizeof(buffer));
         FileWrite(fDst,buffer,len);
         size := size - len;
         if units > 0 then
         ct:=ct+1;
       end;
       FileSetDate(fDst,FileGetDate(fSrc));
       FileClose(fDst);
       FileSetAttr(dest,FileGetAttr(source));
       Result := True;
     end;
     FileClose(fSrc);
   end;
 end;
end;

procedure ShowPopupNotify(msg: String;urgency: TUrgencyLevel;time:Integer);
var p: TProcess;DInfo: TDistroInfo;s: String;
begin
 DInfo:=GetDistro;
 p:=TProcess.Create(nil);
 p.Options:=[poUsePipes];
 if DInfo.DBase='KDE' then
 begin
  p.CommandLine:='kdialog --passivepopup "'+msg+'" --title "Listaller Message" '+IntToStr(time);
  p.Execute;
  p.Free;
  exit;
 end else
 if FileExists('/usr/bin/notify-send') then
 begin
  case urgency of
  ulNormal: s:='normal';
  ulLow: s:='low';
  ulCritical: s:='critical';
  end;
  p.CommandLine:='notify-send '+'--urgency='+s+' --expire-time='+IntToStr(time*1000)+' "'+msg+'"';
  p.Execute;
  p.Free;
  exit;
 end;
end;

initialization
 if IsRoot then
  RegDir:='/etc/lipa/app-reg/'
  else
  RegDir:=SyblToPath('$INST')+'/app-reg/';

end.

