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
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Functions to handle IPK packages
unit ipkhandle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, common, Forms, Process, trstrings, packagekit,
  sqlite3ds, db;

 {** Removes an IPK application
     @param AppName Name of the application, that should be uninstalled
     @param AppID ID of the application
     @param Log TStrings to get the log output
     @param fast Does a quick uninstallation if is true
     @returns Current progress of the operation}
 function UninstallIPKApp(AppName, AppID: String; var Log: TStrings; fast: Boolean=false):Integer;

 var
   //** Path to package registration
   RegDir: String='/etc/lipa/app-reg/';

implementation

function UninstallIPKApp(AppName,AppID: String; var Log: TStrings; fast:Boolean=false): Integer;
var tmp,tmp2,s,slist: TStringList;p,f: String;i,j: Integer;k: Boolean;upd: String;
    proc: TProcess;dlink: Boolean;t: TProcess;
    pkit: TPackageKit;
    dsApp: TSQLite3Dataset;
begin
Result:=0;
p:=RegDir+LowerCase(AppName+'-'+AppID)+'/';
//InfoMemo.Lines.Add('Begin uninstallation...');

{
//ExProgress.Max:=((tmp.Count div 2)*10)+4;
reg:=TIniFile.Create(p+'proginfo.pin');
upd:=reg.ReadString('Application','UpdSource','#');
reg.Free;    }

writeLn('- IPK uninstallation -');

writeLn('Opening database...');
dsApp:= TSQLite3Dataset.Create(nil);
with dsApp do
 begin
   FileName:=RegDir+'applications.db';
   TableName:='AppInfo';
   if not FileExists(FileName) then
   begin
   with FieldDefs do
     begin
       Clear;
       Add('Name',ftString,0,true);
       Add('ID',ftString,0,true);
       Add('Type',ftString,0,true);
       Add('Description',ftString,0,False);
       Add('Version',ftFloat,0,true);
       Add('Publisher',ftString,0,False);
       Add('Icon',ftString,0,False);
       Add('Profile',ftString,0,False);
       Add('AGroup',ftString,0,true);
       Add('InstallDate',ftDateTime,0,False);
       Add('Dependencies',ftMemo,0,False);
     end;
   CreateTable;
 end;
end;
dsApp.Active:=true;;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered := true;
dsApp.First;
while not dsApp.EOF do
begin
 if (dsApp.FieldByName('Name').AsString=AppName) and (dsApp.FieldByName('ID').AsString=AppID) then
 begin

 if LowerCase(dsApp.FieldByName('Type').AsString) = 'dlink'
 then dlink:=true
 else dlink:=false;

if not dlink then
begin
tmp:=TStringList.Create;
tmp.LoadFromfile(p+'appfiles.list');
//UProgress.Max:=((tmp.Count)*10)+4;
end;

if not fast then
begin
if FileExists(p+'prerm') then
begin
Log.Add('PreRM-Script found.');
writeLn('PreRM-Script found.');
t:=TProcess.Create(nil);
t.Options:=[poUsePipes,poWaitonexit];
t.CommandLine:='chmod 775 '''+p+'prerm''';
t.Execute;
Log.Add('Executing prerm...');
writeLn('Executing prerm...');
t.CommandLine:=''''+p+'prerm''';
t.Execute;
t.Free;
Log.Add('Done.');
writeLn('Done.');
end;

///////////////////////////////////////
Log.Add(strRMUnsdDeps);
writeLn(strRMUnsdDeps);
tmp2:=TStringList.Create;
tmp2.Text:=dsApp.FieldByName('Dependencies').AsString;

if tmp2.Count>-1 then
begin
Application.ProcessMessages;
for i:=0 to tmp2.Count-1 do
begin
f:=tmp2[i];
if (LowerCase(f)<>'libc6') then
begin
Application.ProcessMessages;
//Check if another package requires this package
t:=TProcess.Create(nil);
if pos('>',f)>0 then
Log.Add(f+' # '+copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2))
else Log.Add(f);
writeLn(Log[Log.Count-1]);

pkit:=TPackageKit.Create;
s:=tstringlist.Create;
if pos('>',f)>0 then
 pkit.GetRequires(copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2),s)
else pkit.GetRequires(f,s);

   if s.Count <=1 then begin
   if pos('>',f)>0 then
   pkit.RemovePkg(copy(f,pos(' <',f)+2,length(f)-pos(' <',f)-2))
   else pkit.RemovePkg(f);
   Application.ProcessMessages;
   //GetOutPutTimer.Enabled:=true;

   Log.Add('Removing '+f+'...');
   writeLn('Removing '+f+'...');
  end;

 s.free;
 pkit.Free;

 Application.ProcessMessages;
  end;
 end; //End of tmp2-find loop

end else begin Log.Add('No installed deps found!');writeLn('No installed deps found!');end;

tmp2.Free;

end; //End of "fast"-request
//////////////////////////////////////////////////////

if not dlink then
begin
slist:=TStringList.Create;

{if reg.ReadBool(AppName,'ContSFiles',false) then
begin
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
end; //End of shared-test  }

//Undo Mime-registration (if necessary)
for i:=0 to tmp.Count-1 do
begin
if pos( '<mime>',tmp[i])>0 then
begin
Log.Add('Uninstalling MIME-Type...');
writeLn('Uninstalling MIME-Type...');
t:=TProcess.Create(nil);
if (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i])))='.png')
or (LowerCase(ExtractFileExt(DeleteModifiers(tmp[i])))='.xpm') then begin
t.CommandLine:='xdg-icon-resource uninstall '+SysUtils.ChangeFileExt(ExtractFileName(DeleteModifiers(tmp[i])),'');
t.Execute
end else
begin
t.CommandLine:='xdg-mime uninstall '+DeleteModifiers(f+'/'+ExtractFileName(tmp[i]));
t.Execute;
end;
t.Free;
end;
end;

Log.Add('Removing files...');
writeLn('Removing files...');
//Uninstall application
for i:=0 to tmp.Count-1 do
begin

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
writeLn('Removing empty dirs...');
tmp.LoadFromFile(p+'appdirs.list');
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

end;

end;
dsApp.Next;
end;

if Result>0 then
begin
Result:=Result+2;
writeLn('Unregistering...');

dsApp.ExecuteDirect('DELETE FROM AppInfo WHERE rowid='+IntToStr(dsApp.RecNo));
dsApp.ApplyUpdates;
dsApp.Close;
dsApp.Free;
writeLN('Database connection closed.');

proc:=TProcess.Create(nil);
proc.Options:=[poWaitOnExit];
proc.CommandLine :='rm -rf '+''''+ExcludeTrailingBackslash(p)+'''';
proc.Execute;
proc.Free;

Result:=Result+2;
Log.Add('Application removed.');
Log.Add('-----------');
writeLn('- Finished -');
end;

end;

end.

