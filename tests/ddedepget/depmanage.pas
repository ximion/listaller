{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Management of 3rd-party shared libraries and dependencies
unit depmanage;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiTypes,
  LiUtils, Process, FileUtil, SysUtils, DDEResolve, SoftwareDB, TarArchive;

type
  //** Error codes of Listallers library solver
  LiLibSolveError = (NONE,
    DOWNLOAD_FAILED,
    RESOLVE_FAILED,
    UNPACK_FAILED,
    FATAL_COPY_ERROR);

  ArchEntry = record
    Name: String;
    comp: CompressionMethod;
  end;

  TDEBConverter = class
  private
    FName: String;
    WDir: String;
    function CheckCompression(nm: String; sl: TStringList): ArchEntry;
  public
    constructor Create(debfile: String);
    destructor Destroy; override;

    function GetDataArchive: TTarArchive;
    function UnpackDataTo(ar: TTarArchive; path: String): Integer;
    function UnpackDataTo(path: String): Integer;
    function UnpackDataToTmp: String;
    function GetControlScript(control: TStringList): Integer;
    property FileName: String read FName;
    property WorkDir: String read WDir;
  end;

  TDepManager = class
  private
    function GetVersionFromControl(cont: TStringList): String;
  public
    constructor Create;
    destructor Destroy; override;

    function InstallDependency(libname: String): LiLibSolveError;
  end;


implementation

{ TDEBConverter }

constructor TDEBConverter.Create(debfile: String);
begin
  FName := debfile;
  WDir := TMPDIR + 'libwork/' + ExtractFileName(FName) + '/';
  ForceDirectory(WDir);
end;

destructor TDEBConverter.Destroy;
begin
  //Clean up
  DeleteDirectory(WDir, false);
  inherited;
end;

function TDEBConverter.CheckCompression(nm: String; sl: TStringList): ArchEntry;
var
  i: Integer;
begin
  i := sl.IndexOf(nm + '.tar');
  if i >= 0 then
    Result.comp := cmNone
  else
  begin
    i := sl.IndexOf(nm + '.tar.gz');
    if i >= 0 then
      Result.comp := cmGZip
    else
    begin
      i := sl.IndexOf(nm + '.tar.bz2');
      if i >= 0 then
        Result.comp := cmBZip2
      else
      begin
        i := sl.IndexOf(nm + '.tar.lzma');
        if i >= 0 then
          Result.comp := cmLZMA
        else
        begin
          Result.comp := cmNONE;
          exit;
        end;
      end;
    end;
  end;
  Result.Name := sl[i];
end;

function TDEBConverter.GetDataArchive: TTarArchive;
var
  pr: TProcess;
  Data: TTarArchive;
  sl: TStringList;
  entry: ArchEntry;
begin
  pr := TProcess.Create(nil);
  pr.Options := [poUsePipes, poWaitOnExit];
  Result := nil;
  if not FileExists(FName) then
    exit;
  pr.CurrentDirectory := WDir;
  pr.CommandLine := FindBinary('ar') + ' t ''' + FName + '''';
  pr.Execute;

  sl := TStringList.Create;
  sl.LoadFromStream(pr.Output);

  entry := CheckCompression('data', sl);

  sl.Free;
  pr.CommandLine := FindBinary('ar') + ' x ''' + FName + '''';
  pr.Execute;

  Data := TTarArchive.Create;
  Data.TarArchive := WDir + entry.Name;
  Data.Compression := entry.comp;
  Result := Data;
end;

function TDEBConverter.UnpackDataTo(ar: TTarArchive; path: String): Integer;
begin
  ar.BaseDir := path;
  Result := ar.ExtractFile('*');
end;

function TDEBConverter.UnpackDataTo(path: String): Integer;
var
  ar: TTarArchive;
begin
  ar := GetDataArchive;
  if not (ar is TTarArchive) then
    Result := 8
  else
    Result := UnpackDataTo(ar, path);
end;

function TDEBConverter.UnpackDataToTmp: String;
begin
  ForceDirectories(WDir + '/tmp');
  if UnpackDataTo(WDir + '/tmp') = 0 then
    Result := WDir + '/tmp'
  else
    Result := '';
end;

function TDEBConverter.GetControlScript(control: TStringList): Integer;
var
  pr: TProcess;
  arch: TTarArchive;
  sl: TStringList;
  entry: ArchEntry;
begin
  pr := TProcess.Create(nil);
  pr.Options := [poUsePipes, poWaitOnExit];
  Result := 0;
  if not FileExists(FName) then
    exit;
  pr.CurrentDirectory := WDir;
  pr.CommandLine := FindBinary('ar') + ' t ''' + FName + '''';
  pr.Execute;

  sl := TStringList.Create;
  sl.LoadFromStream(pr.Output);

  entry := CheckCompression('control', sl);

  sl.Free;
  if not FileExists(WDir + entry.Name) then
  begin
    pr.CommandLine := FindBinary('ar') + ' x ''' + FName + '''';
    pr.Execute;
  end;

  arch := TTarArchive.Create;
  arch.BaseDir := WDir;
  arch.TarArchive := WDir + entry.Name;
  arch.Compression := entry.comp;
  Result := arch.ExtractFile('./control');
  if not FileExists(WDir + 'control') then
    Result := 4;
  if Result = 0 then
  begin
    control.LoadFromFile(WDir + 'control');
  end;
end;

{ TDepManager }

constructor TDepManager.Create;
begin

end;

destructor TDepManager.Destroy;
begin
  inherited;
end;

function TDepManager.GetVersionFromControl(cont: TStringList): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to cont.Count - 1 do
    if pos('Version: ', cont[i]) > 0 then
    begin
      Result := copy(cont[i], pos(': ', cont[i]) + 2, length(cont[i]));
      break;
    end;
end;

function TDepManager.InstallDependency(libname: String): LiLibSolveError;
var
  solver: TDDEResolver;
  dc: TDEBConverter;
  cont: TStringList;
  pkg: DDEPackage;
  sdb: TSoftwareDB;
  i: Integer;
  dir: String;
  files: TStringList;
begin
  Result := NONE;
  solver := TDDEResolver.Create;
  if solver.ResolveString(libname) then
  begin
    if not solver.DownloadPackage then
    begin
      Result := DOWNLOAD_FAILED;
    end;
  end
  else
    Result := RESOLVE_FAILED;
  if Result <> NONE then
    exit;

  pkg := solver.Pack;
  solver.Free;

  sdb := TSoftwareDB.Create;
  sdb.Load;

  dc := TDEBConverter.Create(pkg.Path);

  cont := TStringList.Create;
  if dc.GetControlScript(cont) > 0 then
  begin
    Result := UNPACK_FAILED;
    exit;
  end;
  pkg.Version := GetVersionFromControl(cont);

  if sdb.DepExisting(pkg.PkName, pkg.Version) then
  begin
    sdb.Free;
    dc.Free;
    exit;
  end;

  ForceDirectories(SyblToPath('$DEP'));

  dir := dc.UnpackDataToTmp;
  if dir = '' then
  begin
    Result := UNPACK_FAILED;
    dc.Free;
    sdb.Free;
    exit;
  end;
  files := FindAllFiles(dir, '*', true);

  for i := 0 to files.Count - 1 do
  begin
    files[i] := CleanFilePath(files[i]);
    ForceDirectories(SyblToPath('$DEP') + StrSubst(ExtractFilePath(files[i]), CleanFilePath(dir + '/usr'), ''));
    if not FileCopy(files[i], SyblToPath('$DEP') + StrSubst(files[i], CleanFilePath(dir + '/usr'), '')) then
    begin
      p_debug(files[i] + ' >> ' + SyblToPath('$DEP') + StrSubst(files[i], CleanFilePath(dir + '/usr'), ''));
      Result := FATAL_COPY_ERROR;
      exit;
    end;
  end;
  dc.Free;

  for i := 0 to files.Count - 1 do
    files[i] := CleanFilePath(SyblToPath('$DEP') + StrSubst(files[i], CleanFilePath(dir + '/usr'), ''));

  sdb.DepAddNew(pkg.PkName, pkg.version, pkg.Distro, ExtractFileName(libname));

  ForceDirectories(sdb.DepConfDir + pkg.PkName + '/');
  files.SaveToFile(sdb.DepConfDir + pkg.PkName + '/files.info');
  files.Free;
  cont.SaveToFile(sdb.DepConfDir + pkg.PkName + '/control');
  cont.Free;
  sdb.Free;
end;

end.

