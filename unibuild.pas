{ Copyright (C) 2008-2009 Matthias Klumpp

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
//** Functions to build DEB/RPM files from one IPS source file
unit unibuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, liBasic, ipkdef, ipkbuild, Process;

function ReadInformation(fips: String): TPackInfo;
procedure CreateDEB(pk: TPackInfo);
procedure CreateRPM(pk: TPackInfo);

implementation

const
   //** Size of the Linux output pipe
   READ_BYTES = 2048;

procedure CheckFileA(fname: String);
begin
if not FileExists(fname) then begin writeLn('File "'+fname+'" does not exists!');writeLn('Action aborted.');halt(6);end;
end;

function ReadInformation(fips: String): TPackinfo;
var tmp,script: TIPKScript;h: String;i,at: Integer;
begin
  CheckFileA(fips);
  writeLn('Reading ips script file...');

  script:=TIPKScript.Create;
  script.LoadFromFile(fips);

    result.build:=TStringList.Create;
    result.desc:=TStringList.Create;
    result.depDEB:=TStringList.Create;
    result.depRPM:=TStringList.Create;


   Result.PkName:=script.PkName;
   writeLn('Package name: '+script.PkName);
   result.Maintainer:=script.Maintainer;
   writeLn('Package maintainer: '+result.Maintainer);


    //??? Needs update of IPS1.0 standard!

    {h:=FindChildNode(xn,'build').NodeValue;
    at:=1;
    while length(h)>0 do begin
       at:=pos(';',h);
      if at = 0 then begin
       result.build.Add(h);
       h:='';
      end else begin
       result.build.Add(copy(h,1,at-1));
       delete(h,1,at);
      end;
    end;

    xn:=dc.FindNode('package');
    xn:=FindChildNodeX(xn,'DepDEB');
     for i:=0 to xn.ChildNodes.Count-1 do
        result.depDEB.Add(xn.ChildNodes.Item[i].FirstChild.NodeValue);
    xn:=dc.FindNode('package');
    xn:=FindChildNodeX(xn,'DepRPM');
    for i:=0 to xn.ChildNodes.Count-1 do
        result.depRPM.Add(xn.ChildNodes.Item[i].FirstChild.NodeValue);

   xn:=dc.DocumentElement.FindNode('application');

   CheckFileA(FindChildNode(xn,'description').NodeValue);
   result.Desc.LoadFromFile(FindChildNode(xn,'description').NodeValue);
   result.version:=FindChildNode(xn,'version').NodeValue;

   DeleteFile('/tmp/build-pk.xml');
   }
end;

procedure CreateDEB(pk: TPackInfo);
var control: TStringList;n: String;i: Integer;
   M: TMemoryStream;
   nm: LongInt;
   BytesRead: LongInt;
   p: TProcess;
   s: String;
begin
  CreateDir(pk.out+'/pkbuild');
  CreateDir(pk.out+'/pkbuild/DEBIAN');
  control:=TStringList.Create;
  with control do begin
    Add('Package: '+pk.pkName);
    n:=CmdResult('uname -m');
    if (n='i686')
    or (n='i586')
    or (n='i486')
    then n:='i386';
    Add('Architecture: '+n);
    Add('Version: '+pk.Version);
    Add('Maintainer: '+pk.Maintainer);
    Add('Priority: extra');
    if pk.depDEB.Count>0 then begin
    n:=pk.depDEB[0];
     for i:=1 to pk.depDEB.Count-1 do
       n:=n+', '+pk.depDEB[i]
    end;
    Add('Depends: '+n);
    Add('Description: '+pk.desc[0]);
    for i:=1 to pk.desc.Count-1 do
    Add('  '+pk.desc[i]);

    SaveToFile(pk.out+'/pkbuild/DEBIAN/control');
    Free;
  end;

p:=TProcess.Create(nil);
p.Options:=[poUsePipes];
p.CurrentDirectory:=pk.out;

 p.CommandLine:='dpkg -b '+pk.out+'pkbuild';

 writeLn('[Exec]: dpkg');
 p.Execute;
 nm:=0;

 M := TMemoryStream.Create;
 m.Clear;
   BytesRead := 0;
   while P.Running do
   begin
     M.SetSize(BytesRead + READ_BYTES);
     nm := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if nm > 0
     then begin
     SetString(s, PChar(M.Memory + BytesRead), nm);
       write(s);
       Inc(BytesRead, nm);
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
     nm := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if nm > 0
     then begin
     SetString(s, PChar(M.Memory + BytesRead), nm);
       write(s);
       Inc(BytesRead, nm);
     end;
   until nm <= 0;
   M.Free;

   if p.ExitStatus > 0 then begin
   writeLn('Build failed.');
   writeLn('Please fix all errors to continue');
   writeLn('[Aborted]');
   halt(p.ExitStatus);
 end;
 SysUtils.RenameFile(pk.out+'pkbuild.deb',pk.out+pk.pkName+'.deb');
end;

procedure CreateRPM(pk: TPackInfo);
var spec: TStringList;n: String;i: Integer;
   M: TMemoryStream;
   nm: LongInt;
   BytesRead: LongInt;
   p: TProcess;
   s: String;
begin
  spec:=TStringList.Create;
  with spec do begin
    Add('Name: '+pk.pkName);
    Add('Version: '+pk.Version);
   // Add('Maintainer: '+pk.Maintainer);
    Add('Group: Applications/Other');
    Add('Summary: '+pk.desc[0]);
    if pk.Author <> '' then
    Add('Vendor: '+pk.Author);
    Add('Release: 1');
    Add('License: XPL');
    if pk.depRPM.Count>0 then begin
    n:=pk.depRPM[0];
     for i:=1 to pk.depRPM.Count-1 do
       n:=n+', '+pk.depRPM[i]
    end;
    Add('Requires: '+n);
    Add('%description');
    for i:=1 to pk.desc.Count-1 do
    Add('  '+pk.desc[i]);

    Add('%files');
    Add('%defattr(-,root,root)');
    Add('/');
    SaveToFile(pk.out+'/'+pk.pkName+'.spec');
    Free;
  end;

p:=TProcess.Create(nil);
p.Options:=[poUsePipes];
p.CurrentDirectory:=pk.out;
writeLn('rpmbuild -bb --rmspec --buildroot="'+pk.out+'pkbuild/"'+' --dbpath="'+pk.out+'" '+pk.out+pk.pkName+'.spec');

p.CommandLine:='sudo rpmbuild -ba --rmspec --buildroot="'+pk.out+'pkbuild/"'+' --dbpath="'+pk.out+'" '+pk.out+pk.pkName+'.spec';

 writeLn('[Exec]: rpmbuild');
 p.Execute;


 M := TMemoryStream.Create;
 m.Clear;
   BytesRead := 0;
   while P.Running do
   begin
     M.SetSize(BytesRead + READ_BYTES);
     nm := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if nm > 0
     then begin
     SetString(s, PChar(M.Memory + BytesRead), nm);
       write(s);
       Inc(BytesRead, nm);
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
     nm := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if nm > 0
     then begin
     SetString(s, PChar(M.Memory + BytesRead), nm);
       write(s);
       Inc(BytesRead, nm);
     end;
   until nm <= 0;
   M.Free;

   if p.ExitStatus > 0 then begin
   writeLn('Build failed.');
   writeLn('Please fix all errors to continue');
   writeLn('[Aborted]');
   halt(p.ExitStatus);
 end;
end;

end.

