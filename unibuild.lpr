{
 * unibuild.lpr
 * Copyright (C) Listaller Project 2008-2009
 *
 * unibuild.lpr is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * unibuild.lpr is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
}
//** Helper application that builds DEB/RPM files from one IPS source file
program unibuild;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Process, XMLRead, DOM, ExtCtrls,
  StrUtils, Forms, LCLType, ipkbuild,
  utilities;

type

  { TUniBuilder }

  TUniBuilder = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    function ReadInformation(fips: String): TPackInfo;
    procedure CreateDEB(pk: TPackInfo);
    procedure CreateRPM(pk: TPackInfo);
  end;

{ TUniBuilder }

const
   //** Size of the Linux output pipe
   READ_BYTES = 2048;

procedure TUniBuilder.DoRun;
var
  ErrorMsg: String;
  pki: TPackInfo;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  ErrorMsg:=CheckOptions('b','build');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Halt;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;

  if HasOption('b','build') then begin
    pki:=ReadInformation(paramstr(2));
    pki.out:=ExtractFilePath(paramstr(2));
    pki.path:=ExtractFilePath(paramstr(2));
    writeLn('== Creating application ==');
    BuildApplication(pki);
    writeLn('== Creating DEB package ==');
    CreateDEB(pki);
    writeLn('== Creating RPM package ==');
    CreateRPM(pki);
    Halt;
  end;

  {
  Mini Brainstorm:
   - Unibuild gets all information from the IPK-package
   - Builds DEB-structure using dh_make or build its own structure
   - Creates a .spec-File using the data
   - Runs debuild and rpmbuild
   - Deletes the debian-dir and the spec-file
   - Finished
  }

  // stop program loop
  Terminate;
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

procedure CheckFileA(fname: String);
begin
if not FileExists(fname) then begin writeLn('File "'+fname+'" does not exists!');writeLn('Action aborted.');halt(6);end;
end;

function TUniBuilder.ReadInformation(fips: String): TPackinfo;
var tmp,script: TStringList;dc: TXMLDocument;xn: TDOMNode;h: String;i,at: Integer;
begin
  CheckFileA(fips);
  writeLn('Reading ips script file...');
  tmp:=TStringList.Create;
  script:=TStringList.Create;
  tmp.LoadFromFile(fips);
  writeLn(tmp[0]);
  for i:=1 to tmp.Count-1 do begin
    if pos('!-Files #',tmp[i])>0 then break
    else script.Add(tmp[i]);
    end;

    tmp.Free;
   script.SaveToFile('/tmp/build-pk.xml');
   script.Free;

    result.build:=TStringList.Create;
    result.desc:=TStringList.Create;
    result.depDEB:=TStringList.Create;
    result.depRPM:=TStringList.Create;

   ReadXMLFile(dc,'/tmp/build-pk.xml');

   xn:=dc.FindNode('package');
   Result.PkName:=FindChildNode(xn,'pkname').NodeValue;
   writeLn('Package name: '+result.pkName);
   result.Maintainer:=FindChildNode(xn,'maintainer').NodeValue;
   writeLn('Package maintainer: '+result.Maintainer);
    h:=FindChildNode(xn,'build').NodeValue;
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
end;

procedure TUniBuilder.CreateDEB(pk: TPackInfo);
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
p.CurrentDirectory:=pk.path;

for i:=0 to pk.build.count-1 do begin
 p.CommandLine:='dpkg -b '+pk.out+'/pkbuild/';

 writeLn('[Exec]: dpkg');
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
end;

procedure TUniBuilder.CreateRPM(pk: TPackInfo);
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
    Add('Maintainer: '+pk.Maintainer);
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
p.CurrentDirectory:=pk.path;

for i:=0 to pk.build.count-1 do begin
 p.CommandLine:='rpmbuild -bb --rmspec --buildroot="'+pk.out+'/pkbuild/"'+' '+pk.out+'/'+pk.pkName+'.spec';

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
end;

constructor TUniBuilder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TUniBuilder.Destroy;
begin
  inherited Destroy;
end;

procedure TUniBuilder.WriteHelp;
begin
  writeLn('Unibuild builds RPM and DEB files from one IPK source file');
  writeln('Usage:');
  writeLn('-h                = Show this help');
  writeLn('-b, --build       = Builds file');
end;

var
  Application: TUniBuilder;

begin
  Application:=TUniBuilder.Create(nil);
  Application.Run;
  Application.Free;
end.

