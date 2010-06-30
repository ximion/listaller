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
//** Contains class to package signed and unsigned IPK package source files
unit ipkpackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, gpgsign, liUtils, liTypes, SysUtils, TarFile;

type
  //** Creates IPK packages from preprocessed source files

  { TLiPackager }

  TLiPackager = class
  private
    OutFileName: String;
    pkrandom: String;
    basename: String;
    mntar: TTarArchive;
    finalized: Boolean;
    bdir: String;
    maxbytes: Int64;

    function RandomID: String;
  public
    constructor Create(aIPKFile: String);
    destructor Destroy; override;

    //** Add a new file to the IPK structure @return False if already finalized or other error
    function AddFile(fname: String): Boolean;
    //** Finalize the base file for signing
    procedure Finalize;
    //** Sign the package
    function SignPackage: Boolean;
    //** Compress package and copy it to output @returns Success of operation
    function ProduceIPKPackage: Boolean;
    //** Base directory (root of package)
    property BaseDir: String read bdir write bdir;
    //** Set IPK file name
    property IPKFile: String read OutFileName write OutFileName;
  end;

  //** Unpacks IPK package structure

  { TLiUnpacker }

  TLiUnpacker = class
  private
    ipkfile: String;
    workdir: String;
    signChecked: Boolean;
  public
    constructor Create(aIPKFile: String);
    destructor Destroy; override;

    //** Prepare IPK tar file for extracting
    procedure Prepare;
    //** Verify signature (if there is any)
    function CheckSignature: TPkgSigState;
    //** Unpack file @returns Success of operation
    function UnpackFile(fname: String): Boolean;
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
  pkrandom := '-' + RandomID + RandomID + RandomID;
  finalized := false;
  OutFileName := aIPKFile;
  basename := tmpdir + ExtractFileName(OutFileName) + pkrandom + '.tar';
  mntar := TTarArchive.Create;
  mntar.Compression:=cmXZ; //IPK packages are XZ compressed
  mntar.TarArchive := basename;
end;

destructor TLiPackager.Destroy;
begin
  if not finalized then
    mntar.Free;
  inherited;
end;

function TLiPackager.RandomID: String;
begin
  Result := IntToStr(random(99));
end;

function TLiPackager.AddFile(fname: String): Boolean;
begin
  if finalized then
    Result := false
  else
  begin
    mntar.BaseDir := bdir;
    if mntar.AddFile(fname) = 0 then
      Result := true
    else
      Result := false;
  end;
end;

procedure TLiPackager.Finalize;
begin
  p_info('Finalizing package.');
  if mntar.Finalize > 0 then
      raise Exception.Create('Error while building package.');
  mntar.Free;
  finalized := true;
end;

function TLiPackager.SignPackage: Boolean;
var
  sign: TGPGSignWrapper;
  oldbase: String;
  rs: Integer;
begin
  Result := false;
  if (not Finalized) then
    raise Exception.Create('IPK file was not finalized before signing.');

  oldbase := basename;
  sign := TGPGSignWrapper.Create;
  sign.FileName := oldbase;
  Result := true;
  if FileExistsUTF8(ExtractFilePath(oldbase) + '/signature.asc') then
    DeleteFile(ExtractFilePath(oldbase) + '/signature.asc');

  if not sign.Signfile(ExtractFilePath(oldbase) + '/signature.asc') then
  begin
    Result := false;
    sign.Free;
    exit;
  end;
  sign.Free;

  pkrandom := '-' + RandomID + RandomID + RandomID;
  basename := tmpdir + ExtractFileName(OutFileName) + pkrandom + '.tar';
  mntar := TTarArchive.Create;
  mntar.Compression:=cmNone; //No compression here
  mntar.TarArchive := basename;

  mntar.BaseDir := ExtractFilePath(oldbase);

  RenameFile(oldbase, ExtractFilePath(oldbase) + '/content.tar');
  oldbase := ExtractFilePath(oldbase) + '/content.tar';

  rs := mntar.AddFile(oldbase);
  if rs = 0 then
  begin
    rs := mntar.AddFile(ExtractFilePath(oldbase) + 'signature.asc');
    if rs <> 0 then
      raise Exception.Create('Error while combining signed package.');
  end
  else
    raise Exception.Create('Error while combining signed package.');
  mntar.Finalize;
  mntar.Free;

  DeleteFile(ExtractFilePath(oldbase) + 'signature.asc');
  DeleteFile(oldbase);
end;

function TLiPackager.ProduceIPKPackage: Boolean;
begin
  Result := true;
  if FileExists(OutFileName) then
    Exception.Create('Output file already exists!');
  if (not Finalized) then
    raise Exception.Create('IPK file was not finalized.');

  if not finalized then mntar.Finalize;

  FileCopy(basename,outfilename);
  DeleteFile(basename);
end;

{ TLiUnpacker }

constructor TLiUnpacker.Create(aIPKFile: String);
begin
  inherited Create;
  ipkfile := aIPKFile;
  workdir := tmpdir + ExtractFileName(ipkfile) + '/';
  SysUtils.ForceDirectories(workdir);
  signChecked := false;
end;

destructor TLiUnpacker.Destroy;
begin
  inherited;
end;

procedure TLiUnpacker.Prepare;
begin
  if not FileExists(ipkfile) then
    Exception.Create('IPK file does not exists!');

  FileCopy(ipkfile, workdir+'ipktar.tar');
  //Some more praparation later...
end;

function TLiUnpacker.CheckSignature: TPkgSigState;
var
  mnarc: TTarArchive;
  hasSignature: Boolean;
  sign: TGPGSignWrapper;
  res: Integer;
begin
  hasSignature := false;
  mnarc := TTarArchive.Create;
  mnarc.TarArchive := workdir + 'ipktar.tar';
  mnarc.Compression:=cmNone; //If we have a signature, covering tar is not compressed
  mnarc.BaseDir := workdir;

  Result := psNone;
  //Check if package has signature
  hasSignature := mnarc.FileInArchive('signature.asc');

  if hasSignature then
  begin
    res := mnarc.ExtractFile('signature.asc');
    res += mnarc.ExtractFile('content.tar');

    if res <> 0 then
    begin
      //!!! This should be done better!
      raise Exception.Create('Could not verify signature!');
    end;

    DeleteFile(workdir + 'ipktar.tar');
    RenameFile(workdir + 'content.tar', workdir + 'ipktar.tar');
    Result := psUntrusted;
    //Now check signature
    sign := TGPGSignWrapper.Create;
    sign.FileName := workdir + 'ipktar.tar';
    if sign.Verify(workdir + 'signature.asc') then
      Result := psTrusted;
    sign.Free;
  end;
  mnarc.Free;
  signChecked := true;
end;

function TLiUnpacker.UnpackFile(fname: String): Boolean;
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
  arc.TarArchive := workdir + 'ipktar.tar';
  arc.BaseDir := workdir;
  arc.Compression:=cmXZ;
  //Create dir struct
  //ForceDirectories(ExtractFilePath(fdest));
  //Check if package has signature
  if arc.ExtractFile(fname) = 0 then
    Result := true;

  arc.Free;
end;

{ TLiUpdateBit }

constructor TLiUpdateBit.Create;
begin
  inherited;
  xz := TTarArchive.Create;
  xz.Compression:=cmLZMA;
end;

destructor TLiUpdateBit.Destroy;
begin
  xz.Free;
  inherited;
end;

procedure TLiUpdateBit.Compress(infile: String; outfile: String);
begin
  xz.BaseDir:=ExtractFilePath(infile);
  xz.TarArchive:=outfile;
  xz.AddFile(infile);
  xz.Finalize;
end;

procedure TLiUpdateBit.Decompress(infile: String; outfile: String);
begin
  //NEEDS WORK!
  xz.TarArchive:=infile;
  xz.BaseDir:=ExtractFilePath(outfile);
  xz.ExtractFile('*');
end;

end.

