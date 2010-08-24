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
//** Contains class to package signed and unsigned IPK package source files (IPK1.1 layout)
unit ipkpackage11;

{$mode objfpc}{$H+}

interface

uses
  Unix, Classes, GPGSign, LiTypes, LiUtils, LiFileUtil, SysUtils, TarArchive;

type

  { TLiPackager }

  //** Creates IPK packages from preprocessed source files
  TLiPackager = class
  private
    OutFileName: String;
    pkrandom: String;
    wdir: String;
    ctar, dtar: TTarArchive;
    finalized: Boolean;
    bdir: String;
    arcdata, arccontrol: String;

    function RandomID: String;
  public
    constructor Create(aIPKFile: String);
    destructor Destroy; override;

    //** Add new data to the IPK structure @return False if already finalized or other error
    function AddDataFile(fname: String): Boolean;
    //** Add a control file to the IPK structure @return False if already finalized or other error
    function AddControlFile(fname: String): Boolean;
    //** Finalize the package
    function Finalize: Integer;
    //** Sign the package
    function SignPackage: Boolean;
    //** Compress package and copy it to output @returns Success of operation
    function ProduceIPKPackage: Boolean;
    //** Base directory (root of package)
    property BaseDir: String read bdir write bdir;
    //** Set IPK file name
    property IPKFile: String read OutFileName write OutFileName;
  end;

  { TLiUnpacker }

  //** Unpacks IPK package structure
  TLiUnpacker = class
  private
    ipkfile: String;
    workdir: String;
    signChecked: Boolean;
  public
    constructor Create(aIPKFile: String);
    destructor Destroy; override;

    //** Prepare IPK tar file for installation & signature verifying
    function Prepare: Boolean;
    //** Verify signature (if there is any)
    function CheckSignature: TPkgSigState;
    //** Unpack data file @returns Success of operation
    function UnpackDataFile(fname: String): Boolean;
    //** Unpack data file @returns Success of operation
    function UnpackControlFile(fname: String): Boolean;
    //** Unpacker's working dir
    property WDir: String read workdir;
  end;

  //** Create small LZMA compressed files for update sources
  TLiUpdateBit = class
  private
    xz: TTarArchive;
  public
    constructor Create;
    destructor Destroy; override;

    //** Compress files to XZ
    procedure Compress(infile: String; outfile: String);
    //** Decompress a file
    procedure Decompress(infile: String; outfile: String);
  end;

implementation

{ TLiPackager }

constructor TLiPackager.Create(aIPKFile: String);
begin
  inherited Create;
  randomize;
  finalized := false;
  OutFileName := aIPKFile;
  wdir := tmpdir + ChangeFileExt(ExtractFileName(OutFileName), '') + '-build/';
  ForceDirectories(wdir);
  arcdata := wdir + 'data.tar.xz';
  arccontrol := wdir + 'control.tar.xz';

  ctar := TTarArchive.Create;
  ctar.Compression := cmXZ; //Control Tar is always XZ compressed
  ctar.TarArchive := arccontrol;
  dtar := TTarArchive.Create;
  dtar.Compression := cmXZ;
  //Data Tar can have various compression methods (XZ by default)
  dtar.TarArchive := arcdata;
end;

destructor TLiPackager.Destroy;
begin
  ctar.Free;
  dtar.Free;
  inherited;
end;

function TLiPackager.RandomID: String;
begin
  Result := IntToStr(random(99));
end;

function TLiPackager.AddDataFile(fname: String): Boolean;
begin
  if finalized then
    Result := false
  else
  begin
    dtar.BaseDir := bdir;
    if dtar.AddFile(fname) = 0 then
      Result := true
    else
      Result := false;
  end;
end;

function TLiPackager.AddControlFile(fname: String): Boolean;
begin
  if finalized then
    Result := false
  else
  begin
    ctar.BaseDir := bdir;
    if ctar.AddFile(fname) = 0 then
      Result := true
    else
      Result := false;
  end;
end;

function TLiPackager.Finalize: Integer;
begin
  Result := 0;
  if finalized then
    exit;

  p_info('Finalizing package.');
  if ctar.Finalize > 0 then
    raise Exception.Create('Error while creating control container!');
  if dtar.Finalize > 0 then
    raise Exception.Create('Error while creating data container!');

  finalized := true;
end;

function TLiPackager.SignPackage: Boolean;
var
  sign: TGPGSignWrapper;
  rs: Integer;
begin
  Result := false;
  if (not Finalized) then
    raise Exception.Create('IPK file was not finalized before signing.');

  //Concat data to sign it
  fpsystem(FindBinary('cat') + ' ' + ctar.TarArchive + ' ' + dtar.TarArchive +
    ' > ' + wdir + 'combined.tmp');

  sign := TGPGSignWrapper.Create;
  sign.FileName := wdir + 'combined.tmp';
  Result := true;
  if FileExistsUTF8(wdir + '_signature') then
    DeleteFile(wdir + '_signature');

  if not sign.Signfile(wdir + '_signature') then
  begin
    Result := false;
    sign.Free;
    exit;
  end;
  sign.Free;

  DeleteFile(wdir + 'combined.tmp');
end;

function TLiPackager.ProduceIPKPackage: Boolean;
var
  mntar: TTarArchive;
  basename: String;
begin
  Result := true;
  basename := basedir + ExtractFileName(OutFileName) +
    RandomID + RandomID + RandomID + '.tar';
  mntar := TTarArchive.Create;
  mntar.Compression := cmNone; //Container Tar is not compressed
  mntar.TarArchive := basename;
  mntar.BaseDir := wdir;

  if (not Finalized) then
    raise Exception.Create('IPK file was not finalized.');

  if mntar.AddFile(arccontrol) > 0 then
  begin
    Result := false;
    exit;
  end;
  if mntar.AddFile(arcdata) > 0 then
  begin
    Result := false;
    exit;
  end;
  if FileExists(wdir + '_signature') then
    if mntar.AddFile(wdir + '_signature') > 0 then
    begin
      Result := false;
      exit;
    end;

  if mntar.Finalize > 0 then
    raise Exception.Create('Error while building package.');

  if FileExists(OutFileName) then
    Exception.Create('Output file already exists!');

  mntar.Free;

  FileCopy(basename, outfilename);
  DeleteFile(basename);
end;

{ TLiUnpacker }

constructor TLiUnpacker.Create(aIPKFile: String);
begin
  inherited Create;
  ipkfile := aIPKFile;
  workdir := tmpdir + ChangeFileExt(ExtractFileName(ipkfile),'') + '-install/';
  ForceDirectories(workdir);
  signChecked := false;
end;

destructor TLiUnpacker.Destroy;
begin
  inherited;
end;

function TLiUnpacker.Prepare: Boolean;
var
  mnarc: TTarArchive;
begin
  Result := true;
  if not FileExists(ipkfile) then
    Exception.Create('IPK file does not exists!');

  FileCopy(ipkfile, workdir + 'ipktar.tar');
  mnarc := TTarArchive.Create;
  mnarc.TarArchive := workdir + 'ipktar.tar';
  mnarc.Compression := cmNone; //Covering tar is not compressed
  mnarc.BaseDir := workdir;
  if mnarc.ExtractFile('*') > 0 then //Extract everything
   Result := false;
  mnarc.Free;
  DeleteFile(workdir + 'ipktar.tar');
end;

function TLiUnpacker.CheckSignature: TPkgSigState;
var
  hasSignature: Boolean;
  sign: TGPGSignWrapper;
  res: Integer;
begin
  hasSignature := false;

  Result := psNone;
  //Check if package has signature
  hasSignature := FileExists(workdir + '_signature');

  if hasSignature then
  begin
    fpsystem(FindBinary('cat') + ' ' + workdir + 'control.tar.xz ' + workdir + 'data.tar.xz > ' + workdir + 'combined.tmp');
    Result := psUntrusted;
    //Now check signature
    sign := TGPGSignWrapper.Create;
    sign.FileName := workdir + 'combined.tmp';
    if sign.Verify(workdir + '_signature') then
      Result := psTrusted;
    sign.Free;
    DeleteFile(workdir + 'combined.tmp');
  end;
  signChecked := true;
end;

function TLiUnpacker.UnpackDataFile(fname: String): Boolean;
var
  arc: TTarArchive;
begin
  if not signChecked then
    CheckSignature;
  Result := false;
  if length(fname) < 2 then
    exit;

  fname := CleanFilePath(fname);

  arc := TTarArchive.Create;
  arc.TarArchive := workdir + 'data.tar.xz';
  arc.BaseDir := workdir;
  arc.Compression := cmXZ;
  //Create dir struct
  //ForceDirectories(ExtractFilePath(fdest));
  if arc.ExtractFile(fname) = 0 then
    Result := true;

  arc.Free;
end;

function TLiUnpacker.UnpackControlFile(fname: String): Boolean;
var
  arc: TTarArchive;
begin
  if not signChecked then
    CheckSignature;
  Result := false;
  if length(fname) < 2 then
    exit;

  fname := CleanFilePath(fname);
  arc := TTarArchive.Create;
  arc.TarArchive := workdir + 'control.tar.xz';
  arc.BaseDir := workdir;
  arc.Compression := cmXZ;
  //Create dir struct
  //ForceDirectories(ExtractFilePath(fdest));
  if arc.ExtractFile(fname) = 0 then
    Result := true;

  arc.Free;
end;

{ TLiUpdateBit }

constructor TLiUpdateBit.Create;
begin
  inherited;
  xz := TTarArchive.Create;
  xz.Compression := cmLZMA;
end;

destructor TLiUpdateBit.Destroy;
begin
  xz.Free;
  inherited;
end;

procedure TLiUpdateBit.Compress(infile: String; outfile: String);
begin
  xz.BaseDir := ExtractFilePath(infile);
  xz.TarArchive := outfile;
  xz.AddFile(infile);
  xz.Finalize;
end;

procedure TLiUpdateBit.Decompress(infile: String; outfile: String);
begin
  //NEEDS WORK!
  xz.TarArchive := infile;
  xz.BaseDir := ExtractFilePath(outfile);
  xz.ExtractFile('*');
end;

end.

