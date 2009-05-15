{ ipkbuild.pas
  Copyright (C) Listaller Project 2008-2009

  ipkbuild.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  ipkbuild.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Functions to build IPK packages from source files
unit ipkbuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, XMLRead, XMLWrite, DOM, AbZipper, AbArcTyp, MD5, common, Process,
  OPBitmapFormats;

type

 { TPackInfo }
  TPackInfo = record
   desc: TStringList;
   pkName, Maintainer, Author: String;
   Version: String;
   build, depDEB,depRPM: TStringList;
   path: String;
   out: String;
  end;

{** Builds a ZIP structure
    @param azipfilename Name of the ZIP file
    @param afiles List of files the archive should contain}
procedure BuildZIP(azipfilename : string; afiles : TStringList);
//** Helper method: Finds a child-node in a XML tree structure @returns First child of the found node as TDOMNode
function FindChildNode(dn: TDOMNode; n: String): TDOMNode;
//** Helper method: Finds a child-node in a XML tree structure @returns Found node as TDOMNode
function FindChildNodeX(dn: TDOMNode; n: String): TDOMNode;
//** Set a node's value (Deprecated)
procedure SetNode(dn: TDOMNode; val: String);
{** Creates an IPK-Update-Source (IPKUS)
    @param FName Name of the IPS source file
    @param path Path to an folder where the source should be created}
procedure CreateUpdateSource(FName, path: String);
{** Builds the application from source using the <build> elements
    @param Connection to an TPackageInfo}
procedure BuildApplication(pk: TPackInfo);
{** Builds an IPK package from an IPS source
    @param fi IPS source
    @param o Output filename
    @param genbutton Generate distro info button}
procedure BuildPackage(fi: String;o:String;genbutton:Boolean=false);
{** Creates the "Linux distribution compatible" button.
    @param dlist Names of the distributions that are compatible
    @param of Name of the output PNG file}
procedure CreateLiCompButton(dlist: TStringList;op: String);

implementation

const
 //** Working directory of Listaller's build tool
 WDir='/tmp/listaller/pkgbuild/';
 //** Size of Linux output pipe
 READ_BYTES = 2048;

////////////////////////////////////////////////////////////////////////////////
///////////////////Functions that are used by the others////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure BuildZIP(aZipFileName: string; afiles : TStringList);
  var
    zip : TAbZipper; j:integer;
  begin
    zip := TAbZipper.Create(nil);
    try
      if afiles.count > 0 then
      begin
        zip.BaseDirectory :=WDir;
        zip.FileName := azipfilename;
        zip.StoreOptions := [soStripDrive];
        for j:=0 to afiles.Count-1 do
        begin
          zip.addfiles(StringReplace(afiles[j],WDir,'',[rfReplaceAll]), 0);
        end;
        zip.Save;
        zip.CloseArchive;
      end
    finally
      zip.Free;
    end;
end;

function FindChildNode(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i].FirstChild;break;exit;end;
end;
end;

function FindChildNodeX(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i];break;exit;end;
end;
end;

procedure SetNode(dn: TDOMNode; val: String);
begin
dn.NodeValue:=val;
end;

procedure BuildApplication(pk: TPackInfo);
var i: Integer;
   M: TMemoryStream;
   n: LongInt;
   BytesRead: LongInt;
   p: TProcess;
   s: String;
begin
CreateDir(pk.out+'/pkbuild');

p:=TProcess.Create(nil);
p.Options:=[poUsePipes];

for i:=0 to pk.build.count-1 do begin
 if pk.build[i][1]='.' then begin
 p.CurrentDirectory:=ExtractFilePath(copy(pk.path+pk.build[i],0,pos(' ',pk.path+pk.build[i])));
 p.CommandLine:=StringReplace(pk.path+pk.build[i],'%IDIR',pk.out+'/pkbuild',[rfReplaceAll]);
 end else begin
 p.CurrentDirectory:=ExtractFilePath(copy(pk.build[i],0,pos(' ',pk.build[i])));
 p.CommandLine:=StringReplace(pk.build[i],'%IDIR',pk.out+'/pkbuild',[rfReplaceAll]);
 end;

 writeLn('[Exec]: '+pk.build[i]);
 p.Execute;
 n:=0;

 M := TMemoryStream.Create;
 m.Clear;
   BytesRead := 0;
   while P.Running do
   begin
     M.SetSize(BytesRead + READ_BYTES);
     n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0
     then begin
     SetString(s, PChar(M.Memory + BytesRead), n);
       write(s);
       Inc(BytesRead, n);
     end
     else begin
       // no data, wait 100 ms
       Sleep(100);
     end;
   end;
   // read last part
   repeat
     M.SetSize(BytesRead + READ_BYTES);
     // try reading it
     n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0
     then begin
     SetString(s, PChar(M.Memory + BytesRead), n);
       write(s);
       Inc(BytesRead, n);
     end;
   until n <= 0;
   M.Free;

   if p.ExitStatus > 0 then begin
   writeLn('Build failed.');
   writeLn('Please fix all errors to continue');
   writeLn('[Aborted]');
   halt(p.ExitStatus);
   end;
end;

p.Free;
pk.build.Free;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Create Update-Source from IPS-File///////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure CreateUpdateSource(FName, path: String);
var ips,script,fls: TStringList;i,j: Integer;zip : TAbZipper;h,dir:String;
begin
ips:=TStringList.Create;
ips.LoadFromFile(FName);
script:=TStringList.Create;
fls:=TStringList.Create;

  for i:=1 to ips.Count-1 do begin
  if pos('!-Files #',ips[i])>0 then break
  else script.Add(ips[i]);
  end;

  for j:=i+1 to ips.Count-1 do
  fls.Add(ips[j]);

  ips.Free;
writeLn('Building update source...');
writeLn('Please wait!');
i:=0;
while i <= fls.Count-1 do begin

if fls[i][1]='>' then dir:=copy(fls[i],2,length(fls[i]))
else begin
if (fls[i][1]='/')or(fls[i][1]='.') then begin

 if (not FileExists(DeleteModifiers(fls[i])))
 and (not FileExists(ExtractFilePath(FName)+fls[i])) then begin
 writeLn('error: ');
 writeln('The file '+DeleteModifiers(fls[i])+' does not exists!');
 writeLn('(Aborted)');
 halt(101);
 end;

 if not DirectoryExists(path+'/'+StringReplace(fls[i],'$','',[]))
 then CreateDir(path+'/'+StringReplace(dir,'$','',[]));
 h:=path+'/'+StringReplace(dir,'$','',[]);
 while not DirectoryExists(path+'/'+StringReplace(dir,'$','',[])) do begin
 CreateDir(h);
 if DirectoryExists(h) then h:=path+'/'+StringReplace(dir,'$','',[])
 else h:=ExtractFilePath(ExcludeTrailingBackslash(h));
 end;

 if (i=fls.Count-1)or(MD5.MDPrint(MD5.MD5File(DeleteModifiers(fls[i]),1024))<>fls[i+1]) then begin
 zip := TAbZipper.Create(nil);
 zip.BaseDirectory :=h;
        writeln('Writing '+ExtractFileName(DeleteModifiers(fls[i]))+' ...');
        zip.FileName := h+'/'+ExtractFileName(DeleteModifiers(fls[i])+'.zip');
        zip.StoreOptions := [soStripDrive];
        zip.addfiles(DeleteModifiers(fls[i]),0);
        zip.Save;
        zip.CloseArchive;
 zip.Free;
 SysUtils.RenameFile(h+'/'+ExtractFileName(DeleteModifiers(fls[i]))+'.zip',h+'/'+ExtractFileName(DeleteModifiers(fls[i])));
 if (fls[i][1]='/')or(fls[i][1]='.')then
  fls.Insert(i+1,'');

 if fls[i][1]='/' then
 fls[i+1]:=MDPrint(MD5.MD5File(DeleteModifiers(fls[i])))
 else
 fls[i+1]:=MDPrint(MD5.MD5File(ExtractFilePath(FName)+DeleteModifiers(fls[i])));

 //FileCopy(DeleteModifiers(Files[i]),path+StringReplace(Files[i+2],'$INST','',[rfReplaceAll])+'/'+ExtractFileName(DeleteModifiers(Files[i])));
 end;
 writeLn('File '+ExtractFileName(DeleteModifiers(fls[i]))+' checked out.');

 end;
  end;

 Inc(i);
end;

 writeLn('Save configuration...');
 script.SaveToFile(path+'source.pin');
 ips:=TStringList.Create;
 ips.Add('IPK-Source.Version: 0.8');
 for i:=0 to script.Count-1 do
 ips.Add(script[i]);
 fls.SaveToFile(path+'sinfo.id');
 for i:=0 to fls.Count-1 do
 ips.Add(fls[i]);
 ips.SaveToFile(FName);
 ips.Free;
 script.Free;

writeLn('Done.');
writeLn('');
writeLn('Copy the folder '+path+' to an location on a webserver,');
writeLn('to create an update source in this directory.');
writeLn('To update the repository, execute the "lipa -u" command with the same parameters again.');
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Function to build IPK-Packages///////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure BuildPackage(fi: String;o:String;genbutton:Boolean=false);
var i,j,id: Integer;fc: TStringList;pc: TXMLDocument;xn,nn: TDOMNode;lh,h,s,pkgtype: String;tmp,ips,files,fsec,script: TStringList;
  dlist: TStringList;aname: String;
begin
if FileExists(o) then begin
writeLn('error: ');
writeLn('This file already exists.');
writeLn('Please choose another one!');
exit;
end;
if FileExists(WDir) then begin
writeLn('Cleaning up...');
  FileUtil.DeleteDirectory(WDir,false);
  end;

  writeLn('Reading file...');
ips:=TStringList.Create;
ips.LoadFromFile(fi);
script:=TStringList.Create;
files:=TStringList.Create;
fsec:=TStringList.Create;

if LowerCase(ips[0])<>'ipk-source.version: 0.8' then begin
 writeLn('This is no valid IPS source file');
 writeLn('[Aborted]');
 halt(1);
end;

  for i:=1 to ips.Count-1 do begin
  if pos('!-Files #',ips[i])>0 then break
  else script.Add(ips[i]);
  end;

  for j:=i to ips.Count-1 do
  fsec.Add(ips[j]);

ips.Free;
writeLn('Building package...');
writeLn('Please wait!');
  if not DirectoryExists('/tmp/listaller/') then SysUtils.CreateDir('/tmp/listaller/');
  if not DirectoryExists(WDir) then SysUtils.CreateDir(WDir) else begin
  DeleteDirectory(WDir,true);SysUtils.CreateDir(WDir);
  end;

  //Creating dirs
  CreateDir(WDir+'data/');
  SysUtils.CreateDir(WDir+'stuff/');

  fc:=TStringList.Create;
  tmp:=TStringList.Create;

  writeLn('Preparing files.');
  write('[..');

  for j:=0 to fsec.Count-1 do begin
   files.Clear;
   fc.Clear;
   id:=-1;
   if pos('!-Files #',fsec[j])>0 then
   begin
    id:=StrToInt(copy(fsec[j],pos('#',fsec[j])+1,length(fsec[j])));
    for i:=j+1 to fsec.count-1 do
     if pos('!-Files #',fsec[i])>0 then break
       else files.Add(fsec[i]);
    end;

  i:=0;
 while i < files.Count-1 do begin

 if files[i][1]='>' then s:=copy(files[i],2,length(files[i]))
 else begin
 if (files[i][1]='/')or(files[i][1]='.') then begin
 if (not FileExists(DeleteModifiers(Files[i])))
 and (not FileExists(ExtractFilePath(fi)+DeleteModifiers(Files[i]))) then begin
 writeLn('error: ');
 writeln('The file '+DeleteModifiers(Files[i])+' doesn''t exists!');
 writeLn('[Building abortet!]');
 halt(6);
 end;

 if not DirectoryExists(WDir+'data'+SyblToX(s))
 then CreateDir(WDir+'data'+SyblToX(s));
 h:=WDir+'data'+SyblToX(s);
 while not DirectoryExists(WDir+'data'+SyblToX(s)) do begin
 CreateDir(h);
 if DirectoryExists(h) then h:=WDir+'data'+SyblToX(s)
 else h:=ExtractFilePath(ExcludeTrailingBackslash(h));
 end;

 if files[i][1]='.' then
 FileCopy(ExtractFilePath(fi)+DeleteModifiers(Files[i]),WDir+'data'+SyblToX(s)+'/'+ExtractFileName(DeleteModifiers(Files[i])))
 else
 FileCopy(DeleteModifiers(Files[i]),WDir+'data'+SyblToX(s)+'/'+ExtractFileName(DeleteModifiers(Files[i])));

 fc.Add('/data'+SyblToX(s)+'/'+ExtractFileName(DeleteModifiers(Files[i])));
 //'/data'+StringReplace(Files[i+2],'$INST','',[rfReplaceAll])+'/'+ExtractFileName(DeleteModifiers(Files[i]))+copy(Files[i],pos(ExtractFileName(DeleteModifiers(Files[i])),Files[i])+length(ExtractFileName(DeleteModifiers(Files[i]))),length(Files[i])));
//writeLn('Add: '+fc[fc.Count-1]);
 tmp.Add(WDir+'data'+SyblToX(s)+'/'+ExtractFileName(DeleteModifiers(Files[i])));
 fc.Add(MD5.MDPrint(MD5.MD5File(DeleteModifiers(Files[i]),1024)));//ExcludeTrailingBackslash(Files[i+1]));
 fc.Add(ExcludeTrailingBackslash(s));
 write('.');

  end;
 end;
 Inc(i);
 end;

//
if id >-1 then begin
 fc.SaveToFile(WDir+'stuff/'+'fileinfo-'+IntToStr(id)+'.id');
 tmp.Add(WDir+'stuff/'+'fileinfo-'+IntToStr(id)+'.id');
end;
//

 end; //End of file including
 write('..]');
 writeLn('');
 fsec.Free;

script.SaveTofile(WDir+'arcinfo.pin');
ReadXMLFile(pc,WDir+'arcinfo.pin');

xn:=pc.FindNode('package');
pkgtype:=xn.Attributes.GetNamedItem('type').NodeValue;
tmp.Add(WDir+'arcinfo.pin');
pkgtype:=LowerCase(pkgtype);
if pkgtype='linstall' then begin
writeLn('Creating normal ipk-package.');

xn:=pc.DocumentElement.FindNode('application');

if genbutton then begin
 dlist:=TStringList.Create;
 aname:=xn.Attributes.GetNamedItem('name').NodeValue;
  if FindChildNode(xn,'dsupport')<>nil then
    lh:=FindChildNode(xn,'dsupport').NodeValue;
  i:=1;
    while length(lh)>0 do begin
       i:=pos(',',lh);
      if i = 0 then begin
       dlist.Add(lh);
       lh:='';
      end else begin
       dlist.Add(copy(lh,1,i-1));
       delete(lh,1,i);
      end;
    end;
end;

writeLn('Creating install-script...');
SysUtils.CreateDir(WDir+'stuff/');

if FindChildNode(xn,'icon')<>nil then
if not FileExists(FindChildNode(xn,'icon').NodeValue) then begin
writeLn('error: ');
writeLn('Icon-path is invalid!');
writeLn('Building canceled');
halt(104);
end else begin
FileCopy(FindChildNode(xn,'icon').NodeValue,WDir+'stuff/'+'packicon.png'); //ACHTUNG! Muss noch geaendert werden, um weitere Bildtypen zu unterstützen
SetNode(FindChildNode(xn,'icon'),'/stuff/'+'packicon.png');
tmp.Add(WDir+'stuff/'+'packicon.png');
end;

if FindChildNode(xn,'license')<>nil then begin
lh:=FindChildNode(xn,'license').NodeValue;
if (not FileExists(lh)) and (lh<>'<none>') then
begin
writeLn('error: ');
writeLn('License file is invalid!');
writeLn('Building canceled!');
exit;
end else begin
FileCopy(lh,WDir+'stuff/'+'license'+ExtractFileExt(lh));
SetNode(FindChildNode(xn,'license'),'/stuff/'+'license'+ExtractFileExt(lh));
tmp.Add(WDir+'stuff/'+'license'+ExtractFileExt(lh));
end;
end else writeLn(' - No license file found!');

if FindChildNode(xn,'description')<>nil then begin
lh:=FindChildNode(xn,'description').NodeValue;
if (not FileExists(lh))and(lh<>'<none>') then
begin
writeLn('error: ');
writeLn('Description file is invalid!');
writeLn('Building canceled!');
halt(6);
end else begin
FileCopy(lh,WDir+'stuff/'+'description'+ExtractFileExt(lh));
tmp.Add(WDir+'stuff/'+'description'+ExtractFileExt(lh));

SetNode(FindChildNode(xn,'description'),'/stuff/'+'description'+ExtractFileExt(lh));
end;
end else writeLn(' - No long description found!');

//
xn:=pc.FindNode('package');
if FileExists(ExtractFilePath(fi)+'/preinst') then begin
FileCopy(ExtractFilePath(fi)+'/preinst',WDir+'preinst');
tmp.Add(WDir+'preinst');
end;
if FileExists(ExtractFilePath(fi)+'/postinst') then begin
FileCopy(ExtractFilePath(fi)+'/postinst',WDir+'postinst');
tmp.Add(WDir+'postinst');
end;
if FileExists(ExtractFilePath(fi)+'/prerm') then begin
FileCopy(ExtractFilePath(fi)+'/prerm',WDir+'prerm');
tmp.Add(WDir+'prerm');
end;

end else begin //If pkgtype is another one
if pkgtype='dlink' then begin
writeLn('Building dlink ipk-package.');

xn:=pc.DocumentElement.FindNode('application');

lh:=FindChildNode(xn,'description').NodeValue;
if (not FileExists(lh))and(lh<>'<none>') then
begin
writeLn('error: ');
writeLn('Description file is invalid!');
writeLn('(Aborted)');
halt(9);
end;

FileCopy(lh,WDir+'stuff/'+'description'+ExtractFileExt(lh));
tmp.Add(WDir+'stuff/'+'description'+ExtractFileExt(lh));

SetNode(FindChildNode(xn,'description'),'/stuff/'+'description'+ExtractFileExt(lh));

if (FindChildNode(xn,'icon')<>nil) then begin
if (not FileExists(FindChildNode(xn,'icon').NodeValue))and(FindChildNode(xn,'icon').NodeValue<>'') then begin
writeLn('error: ');
writeLn('Icon-path is invalid!');
writeLn('Building canceled');
halt(104);
end else begin
FileCopy(FindChildNode(xn,'icon').NodeValue,WDir+'stuff/'+'packicon.png'); //Could be changed to support more picture-filetypes
SetNode(FindChildNode(xn,'icon'),'/stuff/'+'packicon.png');
tmp.Add(WDir+'stuff/'+'packicon.png');
end;
end; //END <>nil


end;
if pkgtype='container' then begin
writeLn('Building container package...');

FileCopy(FindChildNode(xn,'package').NodeValue,WDir+'data/'+ExtractFileName(FindChildNode(xn,'package').NodeValue));
tmp.Add(WDir+'data/'+ExtractFileName(FindChildNode(xn,'package').NodeValue));
SetNode(FindChildNode(xn,'package'),'/data/'+ExtractFileName(FindChildNode(xn,'package').NodeValue));
end;
end;

randomize;
nn:=pc.CreateElement('id');
nn.AppendChild(pc.CreateTextNode('{'+IntToStr(random(9))+IntToStr(random(9))+IntToStr(random(9))+'-'+IntToStr(random(9))+IntToStr(random(9))+IntToStr(random(9))
+'-'+IntToStr(random(9))+IntToStr(random(9))+IntToStr(random(9))+'-'+IntToStr(random(9))+IntToStr(random(9))+IntToStr(random(9))+'}'));
xn.AppendChild(nn);

WriteXMLFile(pc,WDir+'arcinfo.pin');

i:=random(9999)+1000;
writeLn('Generate package...');

writeLn('Creating package...');
writeLn(o+' ('+ChangeFileExt(o,'')+IntToStr(i)+'.zip'+')');
BuildZIP(ChangeFileExt(o,'')+IntToStr(i)+'.zip',tmp);
tmp.Free;
sleep(20);
RenameFile(ChangeFileExt(o,'')+IntToStr(i)+'.zip',o);
Files.Free;
Script.Free;
writeLn('Done!');
if (genbutton)and(Assigned(dlist)) then
begin
if aname <> '' then
CreateLiCompButton(dlist,ExtractFilePath(o)+'/'+aname+'-licomp.png')
else
CreateLiCompButton(dlist,ExtractFilePath(o)+'/'+ChangeFileExt(ExtractFileName(o),'')+'-licomp.png');

dlist.Free;
end;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Creates the Linux-compatibility button///////////////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure CreateLiCompButton(dlist: TStringList; op: String);
var bt,buf,res:TPNGImage;i,j: Integer;
begin
writeLn('Creating "Linux compatible" button '+ExtractFileName(op)+' ...');
bt:=TPNGImage.Create;
buf:=TPNGImage.Create;
res:=TPNGImage.Create;
buf.LoadFromFile('/usr/share/listaller/graphics/libutton/left.png');
j:=dlist.Count div 2;
if (dlist.Count mod 2)>0 then Inc(j);
Dec(j);

for i:=0 to j-1 do begin
if i= 0 then
bt.LoadFromFile('/usr/share/listaller/graphics/libutton/firstblock.png')
else
bt.LoadFromFile('/usr/share/listaller/graphics/libutton/block.png');

res.Width:=buf.Width+bt.Width;
res.Height:=bt.Height;
res.Canvas.Draw(0,0,buf);
res.Canvas.Draw(buf.Width,0,bt);
buf.Width:=res.Width;
buf.Height:=res.Height;
buf.Canvas.Draw(0,0,res);
end;

bt.LoadFromFile('/usr/share/listaller/graphics/libutton/lastblock.png');
res.Width:=buf.Width+bt.Width;
res.Height:=bt.Height;
res.Canvas.Draw(0,0,buf);
res.Canvas.Draw(buf.Width,0,bt);

j:=0;
for i:=0 to dlist.Count-1 do
begin
 if FileExists('/usr/share/listaller/graphics/libutton/distro/'+LowerCase(dlist[i])+'.png') then begin
bt.LoadFromFile('/usr/share/listaller/graphics/libutton/distro/'+LowerCase(dlist[i])+'.png');
if i mod 2 = 0 then begin
j:=i;
res.Canvas.Draw(100+30*i,14,bt)
end else
res.Canvas.Draw(100+30*j,74,bt);
 end else begin
  writeLn('No icon found for distribution "'+dlist[i]+'"');
  writeLn('You can add the icon yourself.');
 end;
end;

res.SaveToFile(op);
bt.free;
buf.free;
res.free;
end;

end.

