{ Copyright (C) 2010 Matthias Klumpp

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
//** Contains wrapper for GNUTar
unit tarfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, FileUtil, liUtils;

type
 TTarArchive = class
 private
  proc: TProcess;
  cmdln: String;
  tarfile: String;
  bdir: String;
  procedure SetCMD(param: String);
  function ValuesGood: Boolean;
 public
  constructor Create;
  destructor  Destroy;override;

  function NewFromDir(dir: String): Integer;
  function AddFile(fname: String): Integer;
  function ExtractFile(fname: String): Integer;
  function FileInArchive(fname: String): Boolean;
  property TarArchive: String read tarfile write tarfile;
  property BaseDir: String read bdir write bdir;
 end;

implementation

{ TTarArchive }

constructor TTarArchive.Create;
begin
 proc:=TProcess.Create(nil);
 cmdln:='tar';
 proc.CommandLine:=cmdln;
 proc.Options:=[poUsePipes,poStdErrToOutPut,poWaitOnExit];
 tarfile:='';
end;

destructor TTarArchive.Destroy;
begin
 proc.Free;
 inherited;
end;

procedure TTarArchive.SetCMD(param: String);
begin
 cmdln:=FindBinary('tar')+' '+param;
 proc.CommandLine:=cmdln;
end;

function TTarArchive.NewFromDir(dir: String): Integer;
begin
 if (not DirectoryExists(dir))
 or (not ValuesGood) then
 begin
  Result:=16;
  exit;
 end;

 SetCMD('cf '+tarfile+' '+dir);
 proc.Execute;
 Result:=proc.ExitStatus;
end;

function TTarArchive.ValuesGood: Boolean;
begin
 bdir:=StringReplace(bdir,'//','/',[rfReplaceAll]);
 tarfile:=StringReplace(tarfile,'//','/',[rfReplaceAll]);
 bdir:=StringReplace(bdir,'///','/',[rfReplaceAll]);
 tarfile:=StringReplace(tarfile,'///','/',[rfReplaceAll]);
 Result:=true;

 if (not SysUtils.DirectoryExists(bdir))
 or (tarfile='') then
 begin
  Result:=false;
  exit;
 end;
end;

function TTarArchive.AddFile(fname: String): Integer;
var newfname: String;
begin
 if (not FileExistsUTF8(fname))
 or (not ValuesGood) then
 begin
  Result:=16;
  p_debug('"'+fname+'"');
  exit;
 end;

 fname:=StringReplace(fname,'//','/',[rfReplaceAll]);
 newFName:=CreateRelativePath(fname,bdir);
 SetCMD('--append --file='+tarfile+' -C '+bdir+' '+newFName);
 proc.Execute;
 Result:=proc.ExitStatus;
end;

function TTarArchive.ExtractFile(fname: String): Integer;
begin
 if (not ValuesGood) then
 begin
  Result:=16;
  exit;
 end;
 if fname[1]='/' then fname:=copy(fname,2,length(fname)-1);

 SetCMD('--extract --file='+tarfile+' -C '+bdir+' '+FName);
 proc.Execute;
 Result:=proc.ExitStatus;
end;

function TTarArchive.FileInArchive(fname: String): Boolean;
begin
 if (not ValuesGood) then
 begin
  Result:=false;
  exit;
 end;
 SetCMD('--list --file='+tarfile+' '+FName);
 proc.Execute;
 if proc.ExitStatus<>0 then
  Result:=false
 else
  Result:=true;
end;

end.

