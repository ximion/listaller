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
//** Allows signing of files via simple wrapper class
unit gpgsign;

{$mode objfpc}{$H+}

interface

uses
  Classes, liTypes, liUtils, LiFileUtil,
  SysUtils, CallbackProcess;

type
  //** Class to sign files using GPG
  TGPGSignWrapper = class
  private
    CBProcess: TCallBackProcess;
    FFileName: String;
    status: TLiStatusData;
    FStatusEvent: StatusChangeEvent;
    gpg: String;

    procedure OnProcessEvent(pcChannel: TProcessChannel; strData: String);
    procedure ChangeStatus(ty: LiProcStatus; msg: String);
  public
    constructor Create;
    destructor Destroy; override;

   {** Sign file
       @param ascFile File for signature
       @returns Success of operation}
    function Signfile(ascFile: String): Boolean;
    //** Verify a signature
    function Verify(ascFile: String): Boolean;
    //** (Binary) file to sign
    property FileName: String read FFileName write FFileName;
    //** Get the status of the current action
    property OnStatus: StatusChangeEvent read FStatusEvent write FStatusEvent;
  end;

//** Check if GPG was found
function GPGFound: Boolean;

implementation

{ TGPGSignWrapper }

constructor TGPGSignWrapper.Create;
begin
  inherited;
  CBProcess := TCallbackProcess.Create;
  CBProcess.CallBackEvent := @OnProcessEvent;
  CBProcess.WaitOnExit := true;
  status.lastresult := prNone;
  gpg := FindBinary('gpg')+' --no-tty --batch ';
end;

destructor TGPGSignWrapper.Destroy;
begin
  CBProcess.Free;
  inherited;
end;

procedure TGPGSignWrapper.ChangeStatus(ty: LiProcStatus; msg: String);
begin
  status.lastresult := ty;
  status.msg := PChar(msg);
  if Assigned(FStatusEvent) then
  begin
    FStatusEvent(scMessage, status, nil);
  end
  else
  begin
    pinfo(status.msg);
  end;
end;

procedure TGPGSignWrapper.OnProcessEvent(pcChannel: TProcessChannel; strData: String);
begin
  if pcChannel = pcStdError then
    ChangeStatus(prError, strData);
  if pcChannel = pcStdOut then
    ChangeStatus(prInfo, strData);
  if pcChannel = pcFinished then
    ChangeStatus(prFinished, strData);
  if pcChannel = pcError then
    ChangeStatus(prError, strData);
end;

function TGPGSignWrapper.Signfile(ascFile: String): Boolean;
var
  resfile: String;
begin
  Result := true;
  if FileExists(ascFile) then
  begin
    Result := false;
    ChangeStatus(prError, 'File "'+ascFile+'" already exists!');
    exit;
  end;

  CBProcess.CommandLine := gpg+'--detach-sign -a "'+FFileName+'"';
  CBProcess.RunCommand;
  while CBProcess.Running do ;
  resfile := ExtractFilePath(FFileName)+'/'+ExtractFileName(FFileName)+'.asc';
  if not FileExists(resfile) then
  begin
    Result := false;
    exit;
  end;
  LiFileUtil.RenameFileUTF8(resfile, ascFile);
end;

function TGPGSignWrapper.Verify(ascFile: String): Boolean;
begin
  CBProcess.CommandLine := gpg+'--verify "'+ascFile+'" "'+FFileName+'"';
  CBProcess.RunCommand;
  Result := CBProcess.ExitCode = 0;
end;

function GPGFound: Boolean;
begin
  if LiFileUtil.FindDefaultExecutablePath('gpg2') = '' then
    Result := false
  else
    Result := true;
end;

end.

