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
  Classes, SysUtils, LibTar, liBasic, ULZMAEncoder, ULZMADecoder,
  UBufferedFS, ULZMACommon, gpgsign, CallbackProcess, liTypes;

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
   function  AddFile(todir: String;fname: String): Boolean;
   //** Finalize the base file for signing
   procedure Finalize;
   //** Sign the package
   function SignPackage: Boolean;
   //** Compress package and copy it to output @returns Success of operation
   function ProduceIPKPackage: Boolean;
 end;

 //** Unpacks IPK package structure

 { TLiUnpacker }

 TLiUnpacker = class
   procedure decoderProgress(const Action: TLZMAProgressAction;
     const Value: int64);
 private
  ipkfile: String;
  workdir: String;
  signChecked: Boolean;
  FProgress: TProgressEvent;
  one: Double;
 public
  constructor Create(aIPKFile: String);
  destructor  Destroy;override;

  //** Decompress IPK tar file
  procedure Decompress;
  //** Verify signature (if there is any)
  function  CheckSignature: Boolean;
  //** Unpack file @returns Success of operation
  function UnpackFile(fname: String;fdest: String): Boolean;
  //** Unpacker's working dir
  property WDir: String read workdir;
  //** On unpackger decompress progress
  property OnProgress: TProgressEvent read FProgress write FProgress;
 end;

 //** Create small LZMA compressed files for update sources
 TLiUpdateBit = class
  private
   encoder: TLZMAEncoder;
   decoder: TLZMADecoder;
   algorithm: Integer;
  public
   constructor Create;
   destructor  Destroy;override;

   //** Compress files to XZ
   procedure Compress(infile: String;outfile: String);
   //** Decompress a file
   procedure Decompress(infile: String;outfile: String);
 end;

implementation

{ TLiPackager }

constructor TLiPackager.Create(aIPKFile: String);
begin
 inherited Create;
 randomize;
 pkrandom:='-'+RandomID+RandomID+RandomID;
 finalized:=false;
  OutFileName:=aIPKFile;
 basename:=tmpdir+'/'+ExtractFileName(OutFileName)+pkrandom+'.tar';
 mntar:=TTarWriter.Create(basename);
 algorithm:=2;
end;

destructor TLiPackager.Destroy;
begin
 if not finalized then mntar.Free;
 inherited;
end;

function TLiPackager.RandomID: String;
begin
 Result:=IntToStr(random(99));
end;

function TLiPackager.AddFile(todir: String;fname: String): Boolean;
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
 mntar.Free;
 finalized:=true;
end;

function TLiPackager.SignPackage: Boolean;
var sign: TGPGSignWrapper;oldbase: String;
begin
 Result:=false;
 if (not Finalized) then raise Exception.Create('IPK file was not finalized before signing.');

 oldbase:=basename;
 sign:=TGPGSignWrapper.Create;
 sign.FileName:=oldbase;
 Result:=true;
 if not sign.Signfile(ExtractFilePath(oldbase)+'/signature.asc') then
 begin
   Result:=false;
   sign.Free;
   exit;
 end;
 sign.Free;

 pkrandom:='-'+RandomID+RandomID+RandomID;
 basename:=tmpdir+'/'+ExtractFileName(OutFileName)+pkrandom+'.tar';
 mntar:=TTarWriter.Create(basename);
 mntar.AddFile(oldbase,'content.tar');
 mntar.AddFile(ExtractFilePath(oldbase)+'/signature.asc','signature.asc');
 mntar.Finalize;
 mntar.Free;

 DeleteFile(ExtractFilePath(oldbase)+'/signature.asc');
 DeleteFile(oldbase);
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
  DeleteFile(basename);
end;

{ TLiUnpacker }

constructor TLiUnpacker.Create(aIPKFile: String);
begin
 inherited Create;
 ipkfile:=aIPKFile;
 workdir:=tmpdir+'/listaller/'+ExtractFileName(ipkfile)+'/';
 SysUtils.ForceDirectories(workdir);
 signChecked:=false;
end;

destructor TLiUnpacker.Destroy;
begin
 inherited;
end;

procedure TLiUnpacker.decoderProgress(const Action: TLZMAProgressAction;
  const Value: int64);
begin
  if Assigned(FProgress) then
  begin
   if Action=LPAMax then
    one:=100/value
   else
    FProgress(Round(value*one),nil);
  end;
end;

procedure TLiUnpacker.Decompress;
var inStream:TBufferedFS;
    outStream:TBufferedFS;
    decoder: TLZMADecoder;
    properties:array[0..4] of byte;
    filesize,outSize:Int64;
    i: Integer;
    v: byte;
const propertiessize=5;
begin
 if not FileExists(ipkfile) then Exception.Create('IPK file does not exists!');

 try
  inStream:=TBufferedFS.Create(ipkfile, fmOpenRead or fmShareDenyNone);
 try
  outStream:=TBufferedFS.Create(workdir+'ipktar.tar', fmCreate);

  decoder:=TLZMADecoder.Create;
  decoder.OnProgress:=@decoderProgress;
  inStream.position:=0;
    with decoder do
    begin
      if inStream.read(properties, propertiesSize) <> propertiesSize then
       raise Exception.Create('input .lzma file is too short');
      if not SetDecoderProperties(properties) then
       raise Exception.Create('Incorrect stream properties');

      outSize := 0;
      for i := 0 to 7 do
      begin
       v := inStream.ReadByte;
       if v < 0 then
        raise Exception.Create('Can''t read stream size');
       outSize := outSize or (v shl ((8 *i) and 31));
      end;
      if not Code(inStream, outStream, outSize) then
       raise Exception.Create('Error in data stream');
     end;
     decoder.Free;

 finally
  outStream.Free;
 end;
 finally
  inStream.Free;
 end;
end;

function TLiUnpacker.CheckSignature: Boolean;
var mnarc: TTarArchive;
    dir: TTarDirRec;
    hasSignature: Boolean;
    sign: TGPGSignWrapper;
begin
 Result:=true;
 hasSignature:=false;
 mnarc:=TTarArchive.Create(workdir+'ipktar.tar');
 mnarc.Reset;
 //Check if package has signature
 while mnarc.FindNext(dir) do
 begin
  if dir.Name='signature.asc' then
  begin
   hasSignature:=true;
   break;
  end;
 end;
 if hasSignature then
 begin

  mnarc.Reset;
  while mnarc.FindNext(dir) do
  begin
   if dir.Name='signature.asc' then
    mnarc.ReadFile(workdir+'signature.asc');
   if dir.Name='content.tar' then
    mnarc.ReadFile(workdir+'content.tar');
  end;
  DeleteFile(workdir+'ipktar.tar');
  RenameFile(workdir+'content.tar',workdir+'ipktar.tar');
  //Now check signature
  sign:=TGPGSignWrapper.Create;
  sign.FileName:=workdir+'ipktar.tar';
  Result:=sign.Verify(workdir+'signature.asc');
  sign.Free;
 end;
 mnarc.Free;
 signChecked:=true;
end;

function TLiUnpacker.UnpackFile(fname: String;fdest: String): Boolean;
var arc: TTarArchive;
    dir: TTarDirRec;
begin
if not signChecked then CheckSignature;
Result:=false;
if length(fname)<2 then exit;
if fname[1]<>'.' then fname:='./'+fname;
 fname:=StringReplace(fname,'//','/',[rfReplaceAll]);

 arc:=TTarArchive.Create(workdir+'ipktar.tar');
 arc.Reset;
 //Create dir struct
 ForceDirectories(ExtractFilePath(fdest));
 //Check if package has signature
 while arc.FindNext(dir) do
 begin
 if pos('rubber',fname)>0 then
  p_debug(dir.name+' # '+fname+' >> '+fdest);
  if StringReplace(dir.name,'//','/',[rfReplaceAll])=fname then
  begin
   arc.ReadFile(fdest);
   Result:=true;
   break;
  end;
 end;
 arc.Free;
end;

{ TLiUpdateBit }

constructor TLiUpdateBit.Create;
begin
 inherited;
 algorithm:=2;
 encoder:=TLZMAEncoder.Create;
 decoder:=TLZMADecoder.Create;
end;

destructor TLiUpdateBit.Destroy;
begin
 encoder.Free;
 decoder.Free;
 inherited;
end;

procedure TLiUpdateBit.Compress(infile: String;outfile: String);
var inStream:TBufferedFS;
    outStream:TBufferedFS;
    eos: Boolean=false; //Don't write end marker
    fileSize: Int64;
    i: Integer;
begin
 inStream:=TBufferedFS.Create(infile,fmOpenRead or fmShareDenyNone);
 outStream:=TBufferedFS.Create(outfile,fmcreate);

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

procedure TLiUpdateBit.Decompress(infile: String;outfile: String);
 var inStream:TBufferedFS;
    outStream:TBufferedFS;
    properties:array[0..4] of byte;
    filesize,outSize:Int64;
    i: Integer;
    v: byte;
const propertiessize=5;
begin
 if not FileExists(infile) then Exception.Create('UpdateBit does not exists!!');

 try
  inStream:=TBufferedFS.Create(infile, fmOpenRead or fmShareDenyNone);
 try
  if FileExists(outfile) then DeleteFile(outfile);
  outStream:=TBufferedFS.Create(outfile, fmCreate);

  decoder:=TLZMADecoder.Create;
  inStream.position:=0;
    with decoder do
    begin
      if inStream.read(properties, propertiesSize) <> propertiesSize then
       raise Exception.Create('input .lzma file is too short');
      if not SetDecoderProperties(properties) then
       raise Exception.Create('Incorrect stream properties');

      outSize := 0;
      for i := 0 to 7 do
      begin
       v := inStream.ReadByte;
       if v < 0 then
        raise Exception.Create('Can''t read stream size');
       outSize := outSize or (v shl ((8 *i) and 31));
      end;
      if not Code(inStream, outStream, outSize) then
       raise Exception.Create('Error in data stream');
     end;
     decoder.Free;

 finally
  outStream.Free;
 end;
 finally
  inStream.Free;
 end;
end;

end.

