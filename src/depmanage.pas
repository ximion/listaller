(* Copyright (C) 2010 Matthias Klumpp
 *
 * Licensed under the GNU General Public License Version 3
 *
 * This unit is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This unit is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this unit. If not, see <http://www.gnu.org/licenses/>.
 *)
//** Management of 3rd-party shared libraries and dependencies
unit depmanage;

{$mode objfpc}{$H+}

interface

uses
  GLib2, Classes, LiTypes, LiUtils, PkTypes, Process, SysUtils, DDEResolve,
  PackageKit, SoftwareDB, TarArchive, LiFileUtil;

type
  //** Error codes of Listallers library solver
  LiLibSolveError = (NONE,
    DOWNLOAD_FAILED,
    RESOLVE_FAILED,
    UNPACK_FAILED,
    FATAL_COPY_ERROR,
    ERROR);

  ArchEntry = record
    Name: String;
    comp: CompressionMethod;
  end;

  TDepResolveProgress = procedure(depIndex: Integer; prog: Integer) of object;

  //** Work with Debian packages
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

  //** Resolve dependency list to package list
  TPackageResolver = class
  private
    DepList: TStringList;
    running: Boolean;
    pkit: array[1..4] of TPackageKit;
    lastIndex: Integer;
    loop: PGMainLoop;
    resList: TStringList;
    tmpDeps: TStringList;
    FProgress: TDepResolveProgress;
    failed: Boolean;
    errMsg: String;
    procedure MakeProgress(index: Integer; prog: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function RunResolver: Boolean;

    property DependencyList: TStringList read DepList write DepList;
    property Results: TStringList read resList write resList;
    property Working: Boolean read running;
    property Failure: Boolean read failed;
    property ErrorMessage: String read errMsg;
    property OnProgress: TDepResolveProgress read FProgress write FProgress;
  end;

  //** Manage external dependencies
  TExDepManager = class
  private
    function GetVersionFromControl(cont: TStringList): String;
  public
    constructor Create;
    destructor Destroy; override;

    function InstallDependency(libname: String): LiLibSolveError;
  end;

implementation

{ TPackageResolver }

constructor TPackageResolver.Create;
var
  i: Integer;
begin
  running := false;
  for i := 1 to length(pkit) do
  begin
    pkit[i] := TPackageKit.Create;
    pkit[i].AsyncMode := true;
    pkit[i].Tag := -1;
  end;
  resList := TStringList.Create;
  tmpDeps := TStringList.Create;
end;

destructor TPackageResolver.Destroy;
var
  i: Integer;
begin
  resList.Free;
  tmpDeps.Free;
  for i := 1 to length(pkit) do
    pkit[i].Free;
  inherited;
end;

procedure TPackageResolver.MakeProgress(index: Integer; prog: Integer);
begin
  if Assigned(FProgress) then
    FProgress(index, prog);
end;

//GLib callback for async package resolving
function PkResolverOnIdle(solver: TPackageResolver): GBoolean; cdecl;
var
  i: Integer;
  pk: TPackageKit;
  err: LiLibSolveError;
begin
  Result := true;
  //Sanity check
  if not (solver is TPackageResolver) then
    exit;

  //Quit if everything is resolved
  if solver.lastIndex > solver.DependencyList.Count - 1 then
  begin
    g_main_loop_quit(solver.loop);
    Result := false;
    exit;
  end;

  for i := 1 to length(solver.pkit) do
  begin
    pk := solver.pkit[i] as TPackageKit;
    if pk.Tag = -1 then
    begin
      pk.Tag := solver.lastIndex;
      pdebug('New Job: ' + IntToStr(pk.Tag));
      //If resolve call fails, set dep to "not resolved"
      if not pk.FindPkgForFile(solver.tmpDeps[pk.Tag]) then
        pk.Tag := -1;

      solver.lastIndex := solver.lastIndex + 1;
    end
    else
      if pk.Finished then
      begin
        pdebug('Finished! [ ' + IntToStr(pk.Tag) + ' ]');
        solver.MakeProgress(pk.Tag, 100);

        //Check if package was found
        if pk.RList.Count <= 0 then
        begin
          //Package was not found!
          if pk.PkExitStatus <> PK_EXIT_ENUM_SUCCESS then
          begin
            solver.failed := true;
            solver.errMsg := pk.LastErrorMessage;
            g_main_loop_quit(solver.loop);
            Result := false;
            exit;
          end;

        end
        else
        begin
          solver.resList.Add(pk.RList[0].PackageId);
          solver.DepList.Delete(solver.DepList.IndexOf(solver.tmpDeps[pk.Tag]));
        end;
        //Make TpackageKit ready to accept new jobs
        pk.Tag := -1;
      end;
  end;
end;

function TPackageResolver.RunResolver: Boolean;
var
  idle: PGSource;
begin
  Result := false;
  failed := false;
  errMsg := 'No messages found.';
  MakeProgress(0, 0);
  ShowPKMon();

  pdebug('Start resolving deps.');

  loop := g_main_loop_new(nil, false);
  idle := g_idle_source_new();

  resList.Clear;
  tmpDeps.Assign(depList);
  lastIndex := 0;

  lastIndex := 0;
  g_source_set_callback(idle, TGSourceFunc(@PkResolverOnIdle), self, nil);
  g_source_attach(idle, g_main_loop_get_context(loop));

  //Start resolving!
  running := true;
  g_main_loop_run(loop);
  //Cleanup...
  running := false;
  g_source_destroy(idle);
  g_main_loop_unref(loop);

  tmpDeps.Clear;

  //If there are some entries left, not all stuff could be resolved
  //Set to false if not everything is resolved or a PackageKit failure appeared
  if (depList.Count > 0) or (failed) then
    Result := false;

  RemoveDuplicates(resList);
  pdebug('DepResolve finished.');
end;

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

constructor TExDepManager.Create;
begin

end;

destructor TExDepManager.Destroy;
begin
  inherited;
end;

function TExDepManager.GetVersionFromControl(cont: TStringList): String;
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

function TExDepManager.InstallDependency(libname: String): LiLibSolveError;
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

  if sdb.DepExists(pkg.PkName, pkg.Version) then
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
    ForceDirectories(SyblToPath('$DEP') + StrSubst(ExtractFilePath(files[i]),
      CleanFilePath(dir + '/usr'), ''));
    if not FileCopy(files[i], SyblToPath('$DEP') +
      StrSubst(files[i], CleanFilePath(dir + '/usr'), '')) then
    begin
      pdebug(files[i] + ' >> ' + SyblToPath('$DEP') +
        StrSubst(files[i], CleanFilePath(dir + '/usr'), ''));
      Result := FATAL_COPY_ERROR;
      exit;
    end;
  end;
  dc.Free;

  for i := 0 to files.Count - 1 do
    files[i] := CleanFilePath(SyblToPath('$DEP') +
      StrSubst(files[i], CleanFilePath(dir + '/usr'), ''));

  sdb.DepAddNew(pkg.PkName, pkg.version, pkg.Distro, ExtractFileName(libname));

  ForceDirectories(sdb.DepConfDir + pkg.PkName + '/');
  files.SaveToFile(sdb.DepConfDir + pkg.PkName + '/files.info');
  files.Free;
  cont.SaveToFile(sdb.DepConfDir + pkg.PkName + '/control');
  cont.Free;
  sdb.Free;
end;

end.

