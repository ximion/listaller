{ ipkhandle.pas
  Copyright (C) Listaller Project 2008-2009

  ipkhandle.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  ipkhandle.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
//** Functions to handle IPK packages
unit ipkhandle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, utilities, Forms, Process, trstrings;

 {** Removes an IPK application
     @param AppName Name of the application, that should be uninstalled
     @param AppID ID of the application
     @param Log TStrings to get the log output
     @param fast Does a quick uninstallation if is true
     @returns Current progress of the operation}
 function UninstallIPKApp(AppName, AppID: String; var Log: TStrings; fast: Boolean=false):Integer;

 var
   //** Path to the Python PackageKit wrapper
   pkit: String;
   //** Path to package registration
   RegDir: String='/etc/lipa/app-reg/';

implementation

function UninstallIPKApp(AppName,AppID: String; var Log: TStrings; fast:Boolean=false): Integer;
var tmp,tmp2,s,slist: TStringList;p,f: String;i,j: Integer;reg: TIniFile;k: Boolean;upd: String;
    proc: TProcess;dlink: Boolean;t: TProcess;
begin
Result:=0;
pkit:=ExtractFilePath(Application.ExeName)+'pkitbind/pkitbind.py ';
p:=RegDir+AppName+'~'+AppID+'/';
//InfoMemo.Lines.Add('Begin uninstallation...');

//ExProgress.Max:=((tmp.Count div 2)*10)+4;
reg:=TIniFile.Create(p+'proginfo.pin');
upd:=reg.ReadString('Application','UpdSource','#');
reg.Free;

reg:=TIniFile.Create(RegDir+'appreg.lst');

if LowerCase(reg.ReadString(AppName+'~'+AppID,'pType','')) = 'dlink' then dlink:=true;

if not dlink then begin
tmp:=TStringList.Create;
tmp.LoadFromfile(p+'appfiles.list');
//UProgress.Max:=((tmp.Count)*10)+4;
end;

if not fast then begin
if FileExists(p+'prerm') then begin
Log.Add('PreRM-Script found.');
t:=TProcess.Create(nil);
t.Options:=[poUsePipes,poWaitonexit];
t.CommandLine:='chmod 775 '''+p+'prerm''';
t.Execute;
Log.Add('Executing prerm...');
t.CommandLine:=''''+p+'prerm''';
t.Execute;
t.Free;
Log.Add('Done.');
end;

///////////////////////////////////////
Log.Add(strRMUnsdDeps);
if reg.SectionExists('DepOS') then begin
i:=1;
while reg.ValueExists('DepOS','ID'+IntToStr(i)) do Inc(i);
Application.ProcessMessages;
for j:=i-1 downto 1 do begin
f:=reg.ReadString('DepOS','ID'+IntToStr(j),'');
if (LowerCase(f)<>'libc6') then begin
Application.ProcessMessages;
//Check if another package requires this package
t:=TProcess.Create(nil);
if pos('>',f)>0 then
Log.Add(f+' # '+copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2))
else Log.Add(f);
if pos('>',f)>0 then
 t.CommandLine:=pKit+'--get-requires '+copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2)
else t.CommandLine:=pKit+'--get-requires '+f;
t.Options:=[poUsePipes,poWaitonexit];
 try
  t.Execute;
  s:=tstringlist.Create;
  try
  while t.Running do Application.ProcessMessages;
   s.LoadFromStream(t.Output);
   if s.Count <=1 then begin
   if pos('>',f)>0 then
   t.CommandLine:=pKit+'--remove '+copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2)
   else t.CommandLine:=pKit+'--remove '+f;
   Application.ProcessMessages;
   //GetOutPutTimer.Enabled:=true;
   t.Execute;
   Log.Add('Removing '+f+'...');
  end;
  finally
  s.free;
  end;
 finally
 t.Free;
end;
 Application.ProcessMessages;
  end;
 end; //End of downto-loop

end else Log.Add('No installed deps found!');

end; //End of "fast"-request
//////////////////////////////////////////////////////

if not dlink then begin
slist:=TStringList.Create;
if reg.ReadBool(AppName,'ContSFiles',false) then begin
tmp2:=TStringList.Create;
reg.ReadSections(tmp2);

for i:=0 to tmp2.Count-1 do begin
if reg.ReadBool(tmp2[i],'ContSFiles',false) then begin
s:=TStringList.Create;

s.LoadFromFile(p+'/appfiles.list');
for j:=0 to s.Count-1 do
if pos(' <s>',s[j])>0 then slist.Add(DeleteModifiers(s[j]));
s.Free;
  end;
tmp2.Free;
 end;
end; //End of shared-test

//Undo Mime-registration (if necessary)
if pos( '<mime>',tmp[i])>0 then begin
Log.Add('Uninstalling MIME-Type...');
t:=TProcess.Create(nil);
if (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i])))='.png')
or (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i])))='.xpm') then begin
t.CommandLine:='xdg-icon-resource uninstall '+SysUtils.ChangeFileExt(ExtractFileName(DeleteModifiers(tmp[i])),'');
t.Execute
end else begin
t.CommandLine:='xdg-mime uninstall '+DeleteModifiers(f+'/'+ExtractFileName(tmp[i]));
t.Execute;
end;
t.Free;
end;
end;

Log.Add('Removing files...');
//Uninstall application
for i:=0 to tmp.Count-1 do begin

f:=SyblToPath(tmp[i]);

f:=DeleteModifiers(f);

k:=false;
for j:=0 to slist.Count-1 do
if f = slist[j] then k:=true;

if not k then
DeleteFile(f);

Result:=Result+10;
Application.ProcessMessages;
end;

//InfoMemo.Lines.Add('Direcory remove...');
tmp.LoadFromFile(p+'AppDirs.list');
proc:=TProcess.Create(nil);
for i:=0 to tmp.Count-1 do begin
  proc.CommandLine :='rm -rf '+tmp[i];
  proc.Execute;
while proc.Running do Application.ProcessMessages;
end;
proc.Free;

if upd<>'#' then begin
tmp.LoadFromFile(RegDir+'updates.list');
//InfoMemo.Lines.Add('Removing update-source...');
for i:=1 to tmp.Count-1 do
if pos(upd,tmp[i])>0 then begin tmp.Delete(i);break;end;
tmp.SaveToFile(RegDir+'updates.list');
tmp.Free;
end;

Result:=Result+2;
tmp:=TStringList.Create;
reg.ReadSections(tmp);
for i:=0 to tmp.Count-1 do
if  (copy(tmp[i],1,pos('~',tmp[i])-1)=AppName)
and (copy(tmp[i],pos('~',tmp[i])+1,length(tmp[i]))=AppID) then begin
reg.EraseSection(tmp[i]);
break;
end;
tmp.Free;
reg.Free;

proc:=TProcess.Create(nil);
proc.Options:=[poWaitOnExit];
proc.CommandLine :='rm -rf '+''''+ExcludeTrailingBackslash(p)+'''';
proc.Execute;
proc.Free;

Result:=Result+2;
{InfoMemo.Lines.Add('Application removed.');
InfoMemo.Lines.Add('-----------');
ExProgress.Visible:=false; }
end;

end.

