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
//** Management of 3rd-party shared libraries
unit slibmanage;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, Process, FileUtil, SysUtils, TarArchive, DDEResolve, LiTypes,
  SoftwareDB;

type
  //** Error codes of Listallers library solver
  LiLibSolveError = (NONE,
          DOWNLOAD_FAILED,
          RESOLVE_FAILED,
          UNPACK_FAILED);

  ArchEntry = record
    name: String;
    comp: CompressionMethod;
  end;

  TDEBConverter = class
  private
    FName: String;
    WDir: String;
    function CheckCompression(nm: String;sl: TStringList): ArchEntry;
  public
    constructor Create(debfile: String);
    destructor Destroy; override;

    function GetDataArchive: TTarArchive;
    function UnpackDataTo(ar: TTarArchive; path: String): Integer;
    function UnpackDataTo(path: String): Integer;
    function GetControlScript(control: TStringList): Integer;
    property FileName: String read FName;
    property WorkDir: String read WDir;
  end;

  TLibManager = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    function InstallLibrary(libname: String): LiLibSolveError;
  end;


implementation

{ TDEBConverter }

constructor TDEBConverter.Create(debfile: String);
begin
  FName := debfile;
  WDir := TMPDIR + 'libwork/'+ExtractFileName(FName)+'/';
  ForceDirectory(WDir);
end;

destructor TDEBConverter.Destroy;
begin
  //Clean up
  DeleteDirectory(WDir, false);
  inherited;
end;

function TDEBConverter.CheckCompression(nm: String; sl: TStringList): ArchEntry;
var i: Integer;
begin
  i := sl.IndexOf(nm+'.tar');
  if i >= 0 then
    Result.Comp := cmNone
  else
  begin
    i := sl.IndexOf(nm+'.tar.gz');
    if i >= 0 then
      Result.Comp := cmGZip
    else
    begin
      i := sl.IndexOf(nm+'.tar.bz2');
      if i >=0 then
        Result.Comp := cmBZip2
      else
      begin
        i := sl.IndexOf(nm+'.tar.lzma');
        if i >= 0 then
         Result.Comp := cmLZMA
        else
        begin
          Result.Comp := cmNONE;
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
  data: TTarArchive;
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

  entry := CheckCompression('data',sl);

  sl.Free;
  pr.CommandLine := FindBinary('ar') + ' x ''' + FName + '''';
  pr.Execute;

  data := TTarArchive.Create;
  data.TarArchive := WDir + entry.Name;
  data.Compression := entry.Comp;
  Result := data;
end;

function TDEBConverter.UnpackDataTo(ar: TTarArchive; path: String): Integer;
begin
  ar.BaseDir := path;
  Result := ar.ExtractFile('*');
end;

function TDEBConverter.UnpackDataTo(path: String): Integer;
var ar: TTarArchive;
begin
  ar := GetDataArchive;
  if not (ar is TTarArchive) then
   Result := 8
  else
   Result := UnpackDataTo(ar, path);
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

  entry := CheckCompression('control',sl);

  sl.Free;
  pr.CommandLine := FindBinary('ar') + ' x ''' + FName + '''';
  pr.Execute;

  arch := TTarArchive.Create;
  arch.TarArchive := WDir + entry.Name;
  arch.Compression := entry.Comp;
  Result := arch.ExtractFile('control');
  if Result = 0 then
  begin
   control.LoadFromFile(WDir + entry.Name + '/control');
  end;
end;

{ TLibManager }

constructor TLibManager.Create;
begin

end;

destructor TLibManager.Destroy;
begin
  inherited;
end;

function TLibManager.InstallLibrary(libname: String): LiLibSolveError;
var solver: TDDEResolver;
 dc: TDEBConverter;
 cont: TStringList;
 pkg: DDEPackage;
 sdb: TSoftwareDB;
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

  ForceDirectories('junk/test');
  dc.UnpackDataTo('junk/test');
  dc.Free;
end;

end.

