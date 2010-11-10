(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 *  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License v3
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
//** Generate hashes for files and streams
unit lihash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HashFunc, LiUtils;

const
  maxBufSize = 1024 * 4;  // Buffersize for File, Stream-Access

type
  TLiHash = class
  private
    hash: TSHA1Hash;
    digest: T160BitDigest;
  public
    constructor Create;
    destructor Destroy; override;

    function HashFromStream(const Stream: TStream; StreamSize: Integer): String;
    function HashFromFile(FName: String): String;
    function HashesEqual(const HexHash: Ansistring; FileName: String): Boolean;
  end;

implementation

constructor TLiHash.Create;
begin
  inherited;
  hash := TSHA1Hash.Create;
end;

destructor TLiHash.Destroy;
begin
  hash.Free;
  inherited;
end;

function TLiHash.HashFromStream(const Stream: TStream; StreamSize: Integer): String;
var
  Buf: Pointer;
  BufSize: Integer;
  last: Boolean;
  Size: Integer;
begin
  last := false;
  hash.Init(@digest, '');
  try
    Buf := AllocMem(maxBufSize);
    if StreamSize < 0 then
 {if Size < 0 then reset the Position, otherwise, calc with the specific
  Size and from the aktual Position in the Stream}
    begin
      Stream.Position := 0;
      StreamSize := Stream.Size;
    end;
    Size := StreamSize;
    repeat
      BufSize := StreamSize;
      if BufSize > maxBufSize then
        BufSize := maxBufSize;
      BufSize := Stream.Read(Buf^, BufSize);
      if BufSize <= 0 then
        Break;
      if (StreamSize - BufSize <= 0) then
        last := true;
      hash.HashBuf(Buf^, BufSize, last);
      Dec(StreamSize, BufSize);
    until BufSize <= 0;
    Result := SHA1DigestToHex(digest);
  finally
    ReallocMem(Buf, 0);
  end;
end;

function TLiHash.HashFromFile(FName: String): String;
var
  S: TFileStream;
begin
  S := TFileStream.Create(FName, fmOpenRead or fmShareDenyNone);
  try
    Result := HashFromStream(S, S.Size);
  finally
    S.Free;
  end;
end;

function TLiHash.HashesEqual(const HexHash: Ansistring; FileName: String): Boolean;
begin
  Result := false;
  if HashFromFile(FileName) = HexHash then
    Result := true;
end;

end.

