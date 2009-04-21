unit packagekit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, common, Process;

//** PackageKit wrapper
type TPackageKit = class
private
 pkit: String;
 procedure CmdResult(cmd: String;s: TStringList);
public
 constructor Create;
 destructor Destroy;
 function IsInstalled(pkg: String): Boolean;
 procedure GetInstalled(pkg: String;lst: TStringList);
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
 Result:='';
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

function TPackageKit.IsInstalled(pkg: String): Boolean;
var t:TProcess;
begin
 Result:='';
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:=pkit+'--is-installed '+pkg;
 try
  t.Execute;
  Result:=false;
  if t.ExitStatus=1 then Result:=true;
 finally
 t.Free;
 end;
end;

procedure TPackageKit.GetInstalled(pkg: String;lst: TStringList);
var s: TStringList;i: Integer;
begin
if not Assigned(lst) then exit;
s:=TStringList;
CmdResult(pkg'--get-requires '+pkg,s);
for i:=1 to s.Count-1 do
lst.Add(s[i]);
end;

end.
