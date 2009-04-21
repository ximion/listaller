unit packagekit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, common, Process, Forms;

//** PackageKit wrapper
type TPackageKit = class
private
 pkit: String;
 OPSuccess: Boolean;
 procedure CmdResult(cmd: String;s: TStringList);
 function GetPkVersion: String;
public
 constructor Create;
 destructor Destroy; override;
 //** Check if package is installed @param pkg Name of the package @returns TRUE if package is installed, FALSE if not
 function IsInstalled(pkg: String): Boolean;
 {Returns the reverse dependencies of a package
 @param pkg Name of the package
 @param lst TStringList to recieve the output}
 procedure GetRequires(pkg: String;lst: TStringList);
 //** Removes a package @param pkg Name of the package @returns Success of operation
 function RemovePkg(pkg: String): Boolean;
 //** Installs a package from repo @param pkg Name of the package @returns Success of operation
 function InstallPkg(pkg: String): Boolean;
 //** Get the name of the package, the file belongs to (!for installed pkgs only!) @param fname Name of the file @returns Name of the package or error, if package was not found
 function PkgNameFromFile(fname: String): String;
 //** Installs a package from file @param fname Name of the package file @returns Success of operation
 function InstallLocalPkg(fname: String): Boolean;
 //** Get the name of the package, the file belongs to (!for not installed pkgs too!) @param fname Name of the file @returns Name of the package or error, if no package was found
 function PkgNameFromNIFile(fname: String): String;
 //** Reads if the last performed operation was successfull
 property OperationSucessfull: Boolean read OPSuccess;
 //** Reads the current Packagekit version as string
 property Version: String read GetPkVersion;
end;

implementation

{ TPackageKit }
 constructor TPackageKit.Create;
 begin
  inherited Create;
  pkit := GetDataFile('pkitbind/pkitbind.py')+' ';
 end;

 destructor TPackageKit.Destroy;
 begin
  inherited Destroy;
 end;

procedure TPackageKit.CmdResult(cmd:String;s: TStringList);
var t:TProcess;
begin
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:=cmd;
 try
  t.Execute;
  s.LoadFromStream(t.Output);
 finally
 t.Free;
 end;
end;

function TPackageKit.GetPkVersion: String;
var s: TStringList;
begin
s:=TStringList.Create;
CmdResult('pkcon --version',s);
if s.Count>=0 then
Result:=s[0]
else Result:='?';
end;

function TPackageKit.IsInstalled(pkg: String): Boolean;
var t:TProcess;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:=pkit+'--is-installed '+pkg;
 try
  t.Execute;
  if t.ExitStatus=1 then Result:=true;
 finally
 t.Free;
 end;
 opsuccess:=Result;
end;

procedure TPackageKit.GetRequires(pkg: String;lst: TStringList);
var s: TStringList;i: Integer;
begin
 if not Assigned(lst) then exit;
 s:=TStringList.Create;
 CmdResult(pkit+'--get-requires '+pkg,s);
 for i:=1 to s.Count-1 do
 lst.Add(s[i]);
end;

function TPackageKit.RemovePkg(pkg: String): Boolean;
var t:TProcess;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes];
 t.CommandLine:=pkit+'--remove '+pkg;
 try
  t.Execute;
  while t.Running do Application.ProcessMessages;
  if t.ExitStatus=0 then Result:=true;
 finally
 t.Free;
 end;
 opsuccess:=Result;
end;

function TPackageKit.InstallPkg(pkg: String): Boolean;
var t:TProcess;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes];
 t.CommandLine:=pkit+'--install '+pkg;
 try
  t.Execute;
  while t.Running do Application.ProcessMessages;
  if t.ExitStatus=0 then Result:=true;
 finally
 t.Free;
 end;
 opsuccess:=Result;
end;

function TPackageKit.PkgNameFromFile(fname: String): String;
var s: TStringList;
begin
 s:=TStringList.Create;
 CmdResult(pkit+'--s-file '+fname,s);
 if s.Count>=0 then
 Result:=s[0]
 else
 Result:='Failed!';
 s.Free;
 if Result='Failed!' then
 opsuccess:=false
 else opsuccess:=true;
end;

function TPackageKit.InstallLocalPkg(fname: String): Boolean;
var t:TProcess;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes];
 t.CommandLine:=pkit+'--install-local '+fname;
 try
  t.Execute;
  while t.Running do Application.ProcessMessages;
  if t.ExitStatus=0 then Result:=true;
 finally
 t.Free;
 end;
 opsuccess:=Result;
end;

function TPackageKit.PkgNameFromNIFile(fname: String): String;
var s: TStringList;
begin
 s:=TStringList.Create;
 CmdResult(pkit+'--s-dfile '+fname,s);
 if s.Count>=0 then
 Result:=s[0]
 else
 Result:='Failed!';
 s.Free;
 if Result='Failed!' then
 opsuccess:=false
 else opsuccess:=true;
end;

end.

