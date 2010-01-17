{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** Functions to build IPK packages from source files
unit ipkbuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IPKPackage, MD5, LiCommon, Process,
  OPBitmapFormats, ipkdef, litypes;

type

 //** Information about the new package
  TPackInfo = record
   desc: TStringList;
   pkName, Maintainer, Author: String;
   Version: String;
   build, depDEB,depRPM: TStringList;
   path: String;
   out: String;
  end;

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
var ips,script,fls: TStringList;i,j: Integer;ubit: TLiUpdateBit;h,dir:String;
begin
ips:=TStringList.Create;
ips.LoadFromFile(FName);
script:=TStringList.Create;
fls:=TStringList.Create;

  for i:=1 to ips.Count-1 do begin
  if pos('!-Files ~',ips[i])>0 then break
  else script.Add(ips[i]);
  end;

  for j:=i+1 to ips.Count-1 do
  fls.Add(ips[j]);

  ips.Free;
writeLn('Building update source...');
writeLn('Please wait!');
ubit:=TLiUpdateBit.Create;

i:=0;
while i <= fls.Count-1 do
begin

if fls[i][1]='>' then dir:=copy(fls[i],2,length(fls[i]))
else
begin
if (fls[i][1]='/')or(fls[i][1]='.') then
begin

 if (not FileExists(DeleteModifiers(fls[i])))
 and (not FileExists(ExtractFilePath(FName)+fls[i])) then
 begin
 writeLn('error: ');
 writeln('The file '+DeleteModifiers(fls[i])+' does not exists!');
 writeLn('[Action canceled]');
 halt(101);
 end;

 if not DirectoryExists(path+'/'+StringReplace(fls[i],'$','',[]))
 then CreateDir(path+'/'+StringReplace(dir,'$','',[]));
 h:=path+'/'+StringReplace(dir,'$','',[]);
 while not DirectoryExists(path+'/'+StringReplace(dir,'$','',[])) do
 begin
 CreateDir(h);
 if DirectoryExists(h) then h:=path+'/'+StringReplace(dir,'$','',[])
 else h:=ExtractFilePath(ExcludeTrailingBackslash(h));
 end;

 if (i=fls.Count-1)or(MD5.MDPrint(MD5.MD5File(DeleteModifiers(fls[i]),1024))<>fls[i+1]) then
 begin
  writeln('Writing '+ExtractFileName(DeleteModifiers(fls[i]))+' ...');
 ubit.Compress(DeleteModifiers(fls[i]),h+'/'+ExtractFileName(DeleteModifiers(fls[i])));
 ubit.Free;

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

 ubit.Free;

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
var i,j,id: Integer;fc: TStringList;lh,h,s: String;pkgtype: TPkgType;sl,files,fsec,script: TStringList;
  dlist: TStringList;aname: String;
  control: TIPKScript;
  ipkpkg: TLiPackager;
begin
if FileExists(o) then begin
writeLn('error: ');
writeLn(' This file already exists.');
writeLn(' Please choose another target!');
halt(6);
exit;
end;
if FileExists(WDir) then begin
writeLn('Cleaning up...');
  FileUtil.DeleteDirectory(WDir,false);
  end;

  writeLn('Reading file...');

sl:=TStringList.Create; //ReadIn file
sl.LoadFromFile(fi);
script:=TStringList.Create;
files:=TStringList.Create;
fsec:=TStringList.Create;

if LowerCase(sl[0])<>'ipk-standard-version: 1.0' then begin
 writeLn(' This is no valid IPS source file');
 halt(1);
end;

  for i:=0 to sl.Count-1 do begin
  if pos('!-Files ~',sl[i])>0 then break
  else script.Add(sl[i]);
  end;

  for j:=i to sl.Count-1 do
  fsec.Add(sl[j]);

sl.Free;
writeLn('Building package...');
writeLn('Please wait!');
writeLn('');
  if not DirectoryExists('/tmp/listaller/') then SysUtils.CreateDir('/tmp/listaller/');
  if not DirectoryExists(WDir) then SysUtils.CreateDir(WDir) else begin
  DeleteDirectory(WDir,true);SysUtils.CreateDir(WDir);
  end;

  //Creating dirs
  CreateDir(WDir+'data/');
  SysUtils.CreateDir(WDir+'pkginfo/');

  fc:=TStringList.Create;
  ipkpkg:=TLiPackager.Create(o);

  writeLn('Preparing files.');
  write('[..');

  for j:=0 to fsec.Count-1 do
  begin
   files.Clear;
   fc.Clear;
   id:=-1;
   if pos('!-Files ~',fsec[j])>0 then
   begin
    id:=StrToInt(copy(fsec[j],pos('~',fsec[j])+1,length(fsec[j])));
    for i:=j+1 to fsec.count-1 do
     if pos('!-Files ~',fsec[i])>0 then break
       else files.Add(fsec[i]);
    end;

  i:=0;
 while i < files.Count-1 do begin

 if files[i][1]='>' then
 begin
  //We have a new target folder. Set path
  s:=copy(files[i],2,length(files[i]));
  //Copy targed folder section
  fc.Add(files[i]);
 end else
 begin
 if (files[i][1]='/')or(files[i][1]='.') then
 begin
 if (not FileExists(DeleteModifiers(Files[i])))
 and (not FileExists(ExtractFilePath(fi)+DeleteModifiers(Files[i]))) then
 begin
 writeLn('error: ');
 writeln(' The file '+DeleteModifiers(Files[i])+' doesn''t exists!');
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

 ipkpkg.AddFile('./data'+SyblToX(s),WDir+'/data'+SyblToX(s)+'/'+ExtractFileName(DeleteModifiers(Files[i])));

 fc.Add(MD5.MDPrint(MD5.MD5File(DeleteModifiers(Files[i]),1024)));//ExcludeTrailingBackslash(Files[i+1]));
 write('.');

  end;
 end;
 Inc(i);
 end;

//
if id >-1 then begin
 fc.SaveToFile(WDir+'pkginfo/'+'fileinfo-'+IntToStr(id)+'.id');

 ipkpkg.AddFile('./pkginfo/',WDir+'pkginfo/fileinfo-'+IntToStr(id)+'.id');
end;
//

 end; //End of file including
 write('..]');
 writeLn('');
 fsec.Free;


control:=TIPKScript.Create;
control.LoadFromList(script);

script.Free; //Was only temporary

pkgtype:=control.SType;

if pkgtype=ptLinstall then
begin
writeLn('Mode: Build standard IPK');

if genbutton then
begin
 dlist:=TStringList.Create;
 aname:=control.AppName;
 lh:=control.DSupport;
  i:=1;
    while length(lh)>0 do
    begin
       i:=pos(',',lh);
      if i = 0 then
      begin
       dlist.Add(lh);
       lh:='';
      end else
      begin
       dlist.Add(copy(lh,1,i-1));
       delete(lh,1,i);
      end;
    end;
 writeLn(' I: A compatibility button will be generated.');
end;

writeLn('Making control-script...');
SysUtils.CreateDir(WDir+'pkginfo/');

if control.Icon<>'' then
if not FileExists(control.Icon) then
begin
 writeLn('error: ');
 writeLn(' Icon-path is invalid!');
 halt(9);
end else
begin
 FileCopy(control.Icon,WDir+'pkginfo/'+'packicon.png'); //!!! Changes needed to support more image types
 control.Icon:='/pkginfo/'+'packicon.png';
 ipkpkg.AddFile('./pkginfo',WDir+'pkginfo/'+'packicon.png');
end;

sl:=TStringList.Create;

control.ReadAppLicense(sl);
if sl.Count>0 then
begin
control.WriteAppLicense(sl);
{
FileCopy(lh,WDir+'pkginfo/'+'license'+ExtractFileExt(lh));
SetNode(FindChildNode(xn,'license'),'/pkginfo/'+'license'+ExtractFileExt(lh));
tmp.Add(WDir+'pkginfo/'+'license'+ExtractFileExt(lh));
}
end else writeLn(' W: No license file found!');

sl.Clear;
control.ReadAppDescription(sl);
if sl.Count>0 then
begin
control.WriteAppDescription(sl);
{
FileCopy(lh,WDir+'pkginfo/'+'description'+ExtractFileExt(lh));
tmp.Add(WDir+'pkginfo/'+'description'+ExtractFileExt(lh));

SetNode(FindChildNode(xn,'description'),'/pkginfo/'+'description'+ExtractFileExt(lh));}
end else writeLn(' W: No long description found!');

sl.Free;

//
if FileExists(ExtractFilePath(fi)+'/preinst') then
begin
FileCopy(ExtractFilePath(fi)+'/preinst',WDir+'preinst');
ipkpkg.AddFile('.',WDir+'preinst');

writeLn(' I: Preinst script found.');
end;
if FileExists(ExtractFilePath(fi)+'/postinst') then
begin
FileCopy(ExtractFilePath(fi)+'/postinst',WDir+'postinst');
ipkpkg.AddFile('.',WDir+'postinst');
writeLn(' I: Postinst script found.');
end;
if FileExists(ExtractFilePath(fi)+'/prerm') then
begin
FileCopy(ExtractFilePath(fi)+'/prerm',WDir+'prerm');
ipkpkg.AddFile('.',WDir+'prerm');
writeLn(' I: Prerm script found.');
end;

end else
begin //If pkgtype is another one
if pkgtype=ptDLink then
begin
writeLn('Mode: Build DLink IPK package');

sl:=TStringList.Create;
control.ReadAppDescription(sl);

if sl.Count<=0 then
begin
 writeLn('error: ');
 writeLn(' Description file is invalid!');
 halt(10);
end;

control.WriteAppDescription(sl);

if control.Icon<>'' then
begin
if (not FileExists(control.Icon))and(control.Icon<>'') then
begin
writeLn('error: ');
writeLn(' Icon-path is invalid!');
halt(9);
end else
begin
FileCopy(control.Icon,WDir+'pkginfo/'+'packicon.png'); //!!! Should be changed to support more picture-filetypes
control.Icon:='/pkginfo/'+'packicon.png';
ipkpkg.AddFile('./pkginfo',WDir+'pkginfo/'+'packicon.png');
writeLn(' Info: Icon added.');
end;
end; //END <>nil


end;
//??? Container builder needs more work!
//    (Add additional file processor)
if pkgtype=ptContainer then
begin
writeLn('Mode: Build container IPK package');

FileCopy(control.Binary,WDir+'data/'+ExtractFileName(control.Binary));
ipkpkg.AddFile('./data',WDir+'data/'+ExtractFileName(control.Binary));
control.Binary:='/data/'+ExtractFileName(control.Binary);
end;
end;

randomize;

control.SaveToFile(WDir+'arcinfo.pin');

ipkpkg.AddFile('.',WDir+'arcinfo.pin');

ipkpkg.Finalize; //Freeze the IPK package state

files.Free;

i:=random(9999)+1000;

writeLn('Creating package...');
writeLn(' #-> '+o);
if not ipkpkg.ProduceIPKPackage then
begin
 writeln('error:');
 writeLn(' Build of IPK package failed!');
 helt(1);
 exit;
end;
sleep(20);

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

