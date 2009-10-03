{ Copyright (C) 2008-2009 Matthias Klumpp

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
//** Functions to manage applications (install/uninstall, dependency-check)
unit management;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Ds, IniFiles, GetText, TRStrings, LiCommon,
  DB, FileUtil, packagekit, Process, ipkhandle, liTypes;

{** Process .desktop-file and add info to list @param fname Name of the .desktop file
      @param tp Category name}
//function ProcessDesktopFile(fname: String; tp: String): Boolean;

//** Load software list entries
procedure LoadEntries;
//** Method that removes MOJO/LOKI installed applications @param dsk Path to the .desktop file of the application
function UninstallMojo(dsk: String): Boolean;
//** Removes an application
procedure UninstallApp(obj: TAppInfo);

{** Checks dependencies of all installed apps
    @param report Report of the executed actions
    @param fix True if all found issues should be fixed right now
    @returns True if everything is okay, False if dependencies are missing}
function CheckApps(report: TStringList;const fix: Boolean=false;const forceroot: Boolean=false): Boolean;

var Root: Boolean=false;
    FMsg: TMessageCall;
    FReq: TRequestCall;
    FApp: TAppEvent;
    FProg: TProgressCall;

implementation


procedure msg(s: String;t: TMType);
begin
 if Assigned(FMsg) then FMsg(PChar(s),t);
end;

function request(s: String;ty: TRqType): TRqResult;
begin
 if Assigned(FReq) then Result:=FReq(ty,PChar(s));
end;

procedure newapp(s: String;oj: TAppInfo);
begin
 if Assigned(FApp) then FApp(PChar(s),oj);
end;

procedure setpos(i: Integer);
begin
 if Assigned(FProg) then FProg(i);
end;

function IsInList(nm: String;list: TStringList): Boolean;
begin
Result:=list.IndexOf(nm)>-1;
end;

procedure LoadEntries;
var ini: TIniFile;tmp,xtmp: TStringList;i,j: Integer;p,n: String;
    entry: TAppInfo;
    dsApp: TSQLite3Dataset;
    blst: TStringList;

//Internal function to process desktop files
procedure ProcessDesktopFile(fname: String);
var d: TIniFile;entry: TAppInfo;dt: TMOFile;lp: String;
    translate: Boolean; //Used, because Assigned(dt) throws an AV
    gr: TGroupType;
//Translate string if possible/necessary
function ldt(s: String): String;
var h: String;
begin
 h:=s;
 try
 if translate then
 begin
  h:=dt.Translate(s);
  if h='' then h:=s;
 end;
 except
  Result:=h;
 end;
 Result:=s;
end;
begin
       d:=TIniFile.Create(fname);
       msg(rsLoading+'  '+ExtractFileName(fname),mtInfo);
       translate:=false;

       if (not Root)and(d.ReadString('Desktop Entry','Exec','')[1]<>'/')
       then
       else
       if (LowerCase(d.ReadString('Desktop Entry','NoDisplay','false'))<>'true')
       and (pos('yast',LowerCase(fname))<=0)
       and(LowerCase(d.ReadString('Desktop Entry','Hidden','false'))<>'true')
       and(not IsInList(d.ReadString('Desktop Entry','Name',''),blst))
      // and(pos('system',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('core',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('.hidden',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
      // and(pos('base',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('wine',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('wine',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(d.ReadString('Desktop Entry','X-KDE-ParentApp','#')='#')
       and(pos('screensaver',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(pos('setting',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
      // and(pos('utility',LowerCase(d.ReadString('Desktop Entry','Categories','')))<=0)
       and(d.ReadString('Desktop Entry','OnlyShowIn','')='')
       and(d.ReadString('Desktop Entry','X-AllowRemove','true')='true')then
       begin

       //Check for Autopackage.org installation
       if pos('apkg-remove',LowerCase(d.ReadString('Desktop Entry','Actions','')))>0 then
       entry.UId:=PChar('!'+d.ReadString('Desktop Action Apkg-Remove','Exec',''))
       else
       entry.UId:=PChar(fname);

       if d.ReadString('Desktop Entry','X-Ubuntu-Gettext-Domain','')<>'' then
       begin
       try
       lp:='/usr/share/locale-langpack/'+GetLangID+'/LC_MESSAGES/'+
                         d.ReadString('Desktop Entry','X-Ubuntu-Gettext-Domain','app-install-data')+'.mo';
       if not FileExists(lp) then
        lp:='/usr/share/locale/de/'+GetLangID+'/LC_MESSAGES/'
            +d.ReadString('Desktop Entry','X-Ubuntu-Gettext-Domain','app-install-data')+'.mo';
       if FileExists(lp) then
       begin
        dt:=TMOFile.Create(lp);
        translate:=true;
       end;
       finally
       end;

       end;

       if (pos('education',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtEDUCATION
       else if (pos('office',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtOFFICE
       else if (pos('development',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtDEVELOPMENT
       else if (pos('graphic',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtGRAPHIC
       else if (pos('network',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtNETWORK
       else if (pos('game',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtGAMES
       else if (pos('system',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtSYSTEM
       else if (pos('audio',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtMULTIMEDIA
       else if (pos('video',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtMULTIMEDIA
       else if (pos('utils',LowerCase(d.ReadString('Desktop Entry','Categories','')))>0) then
        gr:=gtADDITIONAL
       else
        gr:=gtOTHER;

       with entry do
       begin
       if d.ValueExists('Desktop Entry','Name['+GetLangID+']') then
        Name:=PChar(d.ReadString('Desktop Entry','Name['+GetLangID+']','<error>'))
       else
        Name:=PChar(ldt(d.ReadString('Desktop Entry','Name','<error>')));

        Name:=PChar(StringReplace(Name,'&','&&',[rfReplaceAll]));

        Group:=gr;

        // instLst.Add(Lowercase(d.ReadString('Desktop Entry','Name','<error>')));

        if d.ValueExists('Desktop Entry','Comment['+GetLangID+']') then
         ShortDesc:=PChar(d.ReadString('Desktop Entry','Comment['+GetLangID+']',''))
        else
         ShortDesc:=PChar(ldt(d.ReadString('Desktop Entry','Comment','')));

        Author:=PChar(rsAuthor+': '+d.ReadString('Desktop Entry','X-Publisher','<error>'));
        if Author=rsAuthor+': '+'<error>' then
        Author:='';
        Version:='';
        if d.ReadString('Desktop Entry','X-AppVersion','')<>'' then
        Version:=PChar(rsVersion+': '+d.ReadString('Desktop Entry','X-AppVersion',''));

        //Load the icons
        if (LowerCase(ExtractFileExt(d.ReadString('Desktop Entry','Icon','')))<>'.tiff') then
        begin
        try
        if (d.ReadString('Desktop Entry','Icon','')<>'')
        and(d.ReadString('Desktop Entry','Icon','')[1]<>'/') then
        begin
        if FileExists('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
            Icon:=PChar('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') else
        if FileExists('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')) then
            Icon:=PChar('/usr/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')) else
        //
        if FileExists('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.xpm')
        and (ExtractFileExt(d.ReadString('Desktop Entry','Icon',''))='.xpm')then
            Icon:=PChar('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.xpm')
        else if FileExists('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.xpm') then
             Icon:=PChar('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.xpm');
        if (FileExists('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.png'))
        and (ExtractFileExt(d.ReadString('Desktop Entry','Icon',''))='.png')then
            Icon:=PChar('/usr/share/pixmaps/'+ChangeFileExt(d.ReadString('Desktop Entry','Icon',''),'')+'.png')
        else if FileExists('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/share/pixmaps/'+d.ReadString('Desktop Entry','Icon','')+'.png');
        if FileExists('/usr/share/icons/hicolor/128x128/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
            Icon:=PChar('/usr/share/icons/hicolor/128x128/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') else
        if FileExists('/usr/share/icons/hicolor/128x128/apps/'+d.ReadString('Desktop Entry','Icon','')) then
            Icon:=PChar('/usr/share/icons/hicolor/128x128/apps/'+d.ReadString('Desktop Entry','Icon',''));

        { This code is EXPERIMENTAL!}
        //Load KDE4 Icons
          //GetEnvironmentVariable('KDEDIRS')

        if FileExists('/usr/share/icons/default.kde4/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/share/icons/default.kde4/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png')
        else
        if FileExists('/usr/lib/kde4/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/lib/kde4/share/icons/hicolor/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png')
        else
        if FileExists('/usr/share/icons/default.kde/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png') then
                Icon:=PChar('/usr/share/icons/default.kde/64x64/apps/'+d.ReadString('Desktop Entry','Icon','')+'.png');

        end else
        begin
         if (FileExists(d.ReadString('Desktop Entry','Icon','')))
         and(LowerCase(ExtractFileExt(d.ReadString('Desktop Entry','Icon','')))<>'.svg') then
            Icon:=PChar(d.ReadString('Desktop Entry','Icon',''));
        end;
        //If icon loading failed
        except writeLn('ERROR: Unable to load icon!');msg(StringReplace(rsCannotLoadIcon,'%a',Name,[rfReplaceAll]),mtWarning);
        end;
       end;
      end;

         newapp(fname,entry);
      //  if Assigned(dt) then dt.Free;
         if translate then dt.Free;

        end;
       d.Free;
end;

begin
j:=0;

msg(rsLoading,mtInfo);
blst:=TStringList.Create; //Create Blacklist

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
dsApp.Active:=true;

if blst.Count<4 then
begin
blst.Clear;
blst.LoadFromFile('/etc/lipa/blacklist');
blst.Delete(0);
end;


if not DirectoryExists(RegDir) then
begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered:=true;
dsApp.First;
while not dsApp.EOF do
begin

 entry.Name:=PChar(dsApp.FieldByName('Name').AsString);

 blst.Add(entry.Name);
 entry.UId:=PChar(dsApp.FieldByName('ID').AsString);

 entry.Version:=PChar(rsVersion+': '+dsApp.FieldByName('Version').AsString);
 entry.Author:=PChar(rsAuthor+': '+dsApp.FieldByName('Publisher').AsString);
 if dsApp.FieldByName('Publisher').AsString='' then entry.Author:='';
 p:=RegDir+LowerCase(entry.Name+'-'+entry.UId)+'/';

 n:=LowerCase(dsApp.FieldByName('AGroup').AsString);

 if n='all' then entry.Group:=gtALL;
 if n='education' then entry.Group:=gtEDUCATION;
 if n='office' then entry.Group:=gtOFFICE;
 if n='development' then entry.Group:=gtDEVELOPMENT;
 if n='graphic' then entry.Group:=gtGRAPHIC;
 if n='network' then entry.Group:=gtNETWORK;
 if n='games' then entry.Group:=gtGAMES;
 if n='system' then entry.Group:=gtSYSTEM;
 if n='multimedia' then entry.Group:=gtMULTIMEDIA;
 if n='additional' then entry.Group:=gtADDITIONAL;
 if n='other' then entry.Group:=gtOTHER;

// InstLst.Add(LowerCase(dsApp.FieldByName('ID').AsString));

 entry.ShortDesc:=PChar(dsApp.FieldByName('Description').AsString);
 if entry.ShortDesc='#' then entry.ShortDesc:='No description given';

 if FileExists(p+'icon.png') then
 entry.Icon:=PChar(p+'icon.png');
 newapp('ipk',entry);

 dsApp.Next;
end;
dsApp.Close;

{if (CBox.ItemIndex=0) or (CBox.ItemIndex=10) then
begin
tmp:=TStringList.Create;
xtmp:=TStringList.Create;

j:=0;
for i:=0 to xtmp.Count-1 do begin
try
ReadXMLFile(Doc, xtmp[i]);
xnode:=Doc.FindNode('product');
 SetLength(AList,ListLength+1);
 Inc(ListLength);
 AList[ListLength-1]:=TListEntry.Create(MnFrm);
 AList[ListLength-1].Parent:=SWBox;
 AList[ListLength-1].AppLabel.Caption:=xnode.Attributes.GetNamedItem('desc').NodeValue;
 instLst.Add(LowerCase(xnode.Attributes.GetNamedItem('desc').NodeValue));
 blst.Add(AList[ListLength-1].AppLabel.Caption);
xnode:=Doc.DocumentElement.FindNode('component');
 AList[ListLength-1].Vlabel.Caption:=strVersion+': '+xnode.Attributes.GetNamedItem('version').NodeValue;
IdList.Add(xtmp[i]);
//Unsupported
AList[ListLength-1].MnLabel.Visible:=false;
AList[ListLength-1].DescLabel.Visible:=false;
AList[Listlength-1].id:=IDList.Count-1;
AList[ListLength-1].SetPositions;
Application.ProcessMessages;
except
j:=101;
end;
end;

tmp.free;
xtmp.Free;

end; //End Autopackage  }

n:=ConfigDir;
ini:=TIniFile.Create(n+'config.cnf');

//Search for other applications that are installed on this system...
tmp:=TStringList.Create;
xtmp:=TStringList.Create;

if Root then //Only if user is root
begin
tmp.Assign(FindAllFiles('/usr/share/applications/','*.desktop',true));
xtmp.Assign(FindAllFiles('/usr/local/share/applications/','*.desktop',true));
end else
tmp.Assign(FindAllFiles(GetEnvironmentVariable('HOME')+'/.local/share/applications','*.desktop',false));

for i:=0 to xtmp.Count-1 do tmp.Add(xtmp[i]);

xtmp.Free;

for i:=0 to tmp.Count-1 do
       begin
       ProcessDesktopFile(tmp[i]);
       end;
       tmp.Free;
ini.Free;

//Check LOKI-success:
if j>100 then
msg(rsLOKIError,mtWarning);

msg(rsReady,mtInfo); //Loading list finished!

dsApp.Free;
blst.Free; //Free blacklist
end;



//Uninstall Mojo and LOKI Setups
function UninstallMojo(dsk: String): Boolean;
var inf: TIniFile;tmp: TStringList;t: TProcess;mandir: String;
begin
Result:=true;
msg('Package could be installed with MoJo/LOKI...',mtInfo);
inf:=TIniFile.Create(dsk);
if not DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))) then
begin
writeLn('Listaller cannot handle this installation!');
request(rsCannotHandleRM,rqError);inf.Free;end else
if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'.mojosetup') then
begin
//MOJO
mandir:=ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'.mojosetup';
inf.Free;
msg('Mojo manifest found.',mtInfo);
setpos(40);
tmp:=TStringList.Create;
tmp.Assign(FindAllFiles(mandir+'/manifest','*.xml',false));
if tmp.Count<=0 then exit;
setpos(50);
msg('Uninstalling application...',mtInfo);
 t:=TProcess.Create(nil);
 t.CommandLine:=mandir+'/mojosetup uninstall '+copy(ExtractFileName(tmp[0]),1,pos('.',ExtractFileName(tmp[0]))-1);
 t.Options:=[poUsePipes,poWaitonexit];
 tmp.Free;
 setpos(60);
 t.Execute;
 t.Free;
 setpos(100);
end else
//LOKI
if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'.manifest') then
begin
 setpos(50);
 msg('LOKI setup detected.',mtInfo);
 msg('Uninstalling application...',mtInfo);

 t:=TProcess.Create(nil);
 t.CommandLine:=ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'/uninstall';
 t.Options:=[poUsePipes,poWaitonexit];

 setpos(60);
 t.Execute;
 t.Free;
 setpos(100);
end else
begin
 Result:=false;
 writeLn('Listaller cannot handle this installation type!');
 request(rsCannotHandleRM,rqError);inf.Free;
end;
end;

procedure UninstallApp(obj: TAppInfo);
var f,g: String; t:TProcess;tmp: TStringList;pkit: TPackageKit;i: Integer;
    name,id: String;
begin

msg('Connecting to PackageKit... (run "pkmon" to see the actions)',mtInfo);

setpos(0);

//Needed
name:=obj.Name;
id:=obj.UId;

msg('Reading application information...',mtInfo);

if not FileExists(obj.UId) then
begin
if DirectoryExists(RegDir+LowerCase(name+'-'+id)) then
begin
 //Remove IPK app
 UninstallIPKApp(name,id,FMsg,FProg,false);

msg('Finished!',mtInfo);
exit;
end else
begin
 request('The registration of this package is broken!',rqError);exit;
end;

end else
begin //Autopackage
 if id[1]='!' then
 begin
 t:=TProcess.Create(nil);
 t.CommandLine:=copy(obj.UId,2,length(obj.Uid));
 t.Options:=[poUsePipes,poWaitonexit];
 t.Execute;
 t.Free;
 exit;
 end;
end;

if (id[1]='/')
then
begin

// /!\
///////////////////////////////////////////////////////

ShowPKMon();

msg('Detecting package...',mtInfo);

pkit:=TPackageKit.Create;
tmp:=TStringList.Create;
pkit.RsList:=tmp;
pkit.PkgNameFromFile(id);
while not pkit.PkFinished do setpos(0);

if tmp.Count<=0 then
begin UninstallMojo(id);tmp.Free;pkit.Free;exit;end;
if pkit.PkFinishCode>0 then
begin request(rsPKitProbPkMon,rqError);pkit.Free;tmp.Free;exit;end;
f:=tmp[0];

msg('Looking for reverse-dependencies...',mtInfo);

tmp.Clear;

pkit.GetRequires(f);
while not pkit.PkFinished do setpos(0);
g:='';
for i:=0 to tmp.Count-1 do
g:=g+#10+tmp[i];
tmp.Free;

// Ehm... Dont't know what the function of this code was - needs testing!
//g:=copy(g,pos(f,g)+length(f),length(g));

msg('Package detected: '+f,mtInfo);
if (StringReplace(g,' ','',[rfReplaceAll])='')or
(request(StringReplace(StringReplace(StringReplace(rsRMPkg,'%p',f,[rfReplaceAll]),'%a',obj.Name,[rfReplaceAll]),'%pl',PAnsiChar(g),[rfReplaceAll]),
        rqWarning)=rqsYes)
then
begin

msg('Uninstalling '+f+' ...',mtInfo);
pkit.RemovePkg(f);

//??? Needs rethinking
//while not pkit.PkFinished do setpos(pkit.Progress);

if pkit.PkFinishCode>0 then begin
request(rsRmError,rqError);exit;pkit.Free;end;

setpos(100);
msg('Done.',mtInfo);
exit;
end else exit;

////////////////////////////////////////////////////////////////////////////
//
  end;
end;

function CheckApps(report: TStringList;const fix: Boolean=false;const forceroot: Boolean=false): Boolean;
var dsApp: TSQLite3Dataset;deps: TStringList;i: Integer;pkit: TPackageKit;s: String;
begin
writeLn('Checking dependencies of all registered applications...');
if forceroot then
writeLn('You are scanning only the ROOT installed applications.')
else
writeLn('You are scanning your local installed applications.');

if not forceroot then
  s:=SyblToPath('$INST')+'/app-reg/'
else
  s:='/etc/lipa/app-reg/';

writeLn('-> Opening database...');
dsApp:= TSQLite3Dataset.Create(nil);
with dsApp do
 begin
   FileName:=s+'applications.db';
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
dsApp.Active:=true;

Result:=true;

dsApp.SQL:='SELECT * FROM AppInfo';
dsApp.Open;
dsApp.Filtered:=true;
deps:=TStringList.Create;
pkit:=TPackageKit.Create;
dsApp.First;
while not dsApp.EOF do
begin
 writeLn(' Checking '+dsApp.FieldByName('Name').AsString);
 deps.Text:=dsApp.FieldByName('Dependencies').AsString;
 for i:=0 to deps.Count-1 do
 begin
  pkit.Resolve(deps[i]);
  if pkit.PkFinishCode=0 then
   report.Add(deps[i]+' found.')
  else
  begin
   report.Add(deps[i]+' is not installed!');
   Result:=false;
   if fix then
   begin
    write('  Repairing dependency '+deps[i]+'  ');
    pkit.InstallPkg(deps[i]);
    writeLn(' [OK]');
    report.Add('Installed dependency '+deps[i]);
   end;
  end;
 end;
 dsApp.Next;
end;
deps.Free;
pkit.Free;
dsApp.Close;
writeLn('Check finished.');
if not Result then writeLn('You have broken dependencies.');
end;

end.

