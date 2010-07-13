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
  Classes, LiUtils, Process, FileUtil, SysUtils, TarArchive, CallbackProcess;

type
  TDEBConverter = class
  private
    FName: String;
    WDir: String;
  public
    constructor Create(debfile: String);
    destructor Destroy; override;

    function GetDataArchive: TTarArchive;
    function UnpackDataTo(ar: TTarArchive; path: String): Integer;
    function UnpackDataTo(path: String): Integer;
    property FileName: String read FName;
    property WorkDir: String read WDir;
  end;

  TLibManager = class
  private
  public
    constructor Create;
    destructor Destroy; override;
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

function TDEBConverter.GetDataArchive: TTarArchive;
var
  pr: TProcess;
  data: TTarArchive;
  sl: TStringList;
  i: Integer;
  s: String;
  comp: CompressionMethod;
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

  i := sl.IndexOf('data.tar');
  if i >= 0 then
    comp := cmNone
  else
  begin
    i := sl.IndexOf('data.tar.gz');
    if i >= 0 then
      comp := cmGZip
    else
    begin
      i := sl.IndexOf('data.tar.bz2');
      if i >=0 then
        comp := cmBZip2
      else
      begin
        i := sl.IndexOf('data.tar.lzma');
        if i >= 0 then
         comp := cmLZMA
        else
        begin
          Result := nil;
          exit;
        end;
      end;
    end;
  end;

  s := WDir + sl[i];
  sl.Free;
  pr.CommandLine := FindBinary('ar') + ' x ''' + FName + '''';
  pr.Execute;

  data := TTarArchive.Create;
  data.TarArchive := s;
  data.Compression := comp;
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

{ TLibManager }

constructor TLibManager.Create;
begin

end;

destructor TLibManager.Destroy;
begin
  inherited;
end;

end.

