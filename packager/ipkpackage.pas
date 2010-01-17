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
  Classes, SysUtils, LibTar, liBasic,
  ULZMAEncoder, ULZMADecoder, UBufferedFS, ULZMACommon;

type
 //** Creates IPK packages from preprocessed source files
 TLiPackager = class
  private
   OutFileName: String;
   pkrandom: String;
   basename: String;
   mntar: TTarWriter;
   finalized: Boolean;
   algorithm: Integer;
   function RandomID: String;
  public
   constructor Create(aIPKFile: String);
   destructor  Destroy;override;

   //** Add a new file to the IPK structure @return False if already finalized
   function  AddFile(fname: String;todir: String): Boolean;
   //** Finalize the base file for signing
   procedure Finalize;
   //** Sign the package
   procedure SignPackage;
   //** Compress package and copy it to output @returns Success of operation
   function ProduceIPKPackage: Boolean;
 end;

 //** Unpacks IPK package structure
 TLiUnpacker = class
 private
 public
  constructor Create(aIPKFile: String);
  destructor  Destroy;override;
 end;

implementation

{ TLiPackager }

constructor TLiPackager.Create(AFile: String);
begin
 inherited Create;
 randomize;
 pkrandom:='-'+RandomID+RandomID+RandomID;
 finalized:=false;
 basename:=tmpdir+'/'+ExtractFileName(AFile)+pkrandom+'.tar';
 mntar:=TTarWriter.Create(basename);
 OutFileName:=AFile;
 algorithm:=2;
end;

destructor TLiPackager.Destroy;
begin
 if Assigned(mntar) then mntar.Free;
 inherited;
end;

function TLiPackager.RandomID: String;
begin
 Result:=IntToStr(random(99));
end;

function TLiPackager.AddFile(fname: String;todir: String): Boolean;
begin
if finalized then
 Result:=false
else
begin
 mntar.AddFile(fname,todir+'/'+ExtractFileName(fname));
 Result:=true;
end;
end;

procedure TLiPackager.Finalize;
begin
 mntar.Finalize;
 FreeAndNil(mntar);
 finalized:=true;
end;

procedure TLiPackager.SignPackage;
begin
 if (not Finalized) then raise Exception.Create('IPK file was not finalized before signing.');
 //Impossible at time
end;

function TLiPackager.ProduceIPKPackage: Boolean;
var encoder: TLZMAEncoder;
    inStream:TBufferedFS;
    outStream:TBufferedFS;
    eos: Boolean=false; //Don't write end marker
    fileSize: Int64;
    i: Integer;
begin
 Result:=true;
 if FileExists(OutFileName) then Exception.Create('Output file already exists!');
 if (not Finalized) then raise Exception.Create('IPK file was not finalized before compressing.');

 inStream:=TBufferedFS.Create(basename,fmOpenRead or fmShareDenyNone);
 outStream:=TBufferedFS.Create(OutFileName,fmcreate);

 encoder:=TLZMAEncoder.Create;
 if not encoder.SetAlgorithm(algorithm) then
  raise Exception.Create('Incorrect compression mode');
 if not encoder.SetDictionarySize(1 shl 23) then
  raise Exception.Create('Incorrect dictionary size');

 if not encoder.SeNumFastBytes(128) then
  raise Exception.Create('Incorrect -fb value');
 if not encoder.SetMatchFinder(1) then
  raise Exception.Create('Incorrect -mf value');
 if not encoder.SetLcLpPb(3, 0, 2) then
  raise Exception.Create('Incorrect -lc or -lp or -pb value');


  encoder.SetEndMarkerMode(eos);
  encoder.WriteCoderProperties(outStream);

  if eos then
   fileSize:=-1
  else fileSize:=inStream.Size;

  for i := 0 to 7 do
   WriteByte(outStream,(fileSize shr (8 * i)) and $FF);

  encoder.Code(inStream, outStream, -1, -1);

  encoder.free;
  outStream.Free;
  inStream.Free;
end;

end.

