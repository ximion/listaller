{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Thomas Dieffenbach
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** Contains TCallbackProgress to catch output from cmdline-tools
unit callbackprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, pipes;

type

  TProcessChannel=(pcStdOut, pcStdError, pcFinished, pcError);

  TCallBackEvent=procedure(pcChannel: TProcessChannel; strData: String) of object;

  TCallbackProcess=class(TComponent)
  private
    FProcess: TProcess;
    FCallBackEvent: TCallBackEvent;
    FCommandLine: String;
    FExitCode: Integer;
    FCancel: Boolean;
    procedure CreateProcess;
    function ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    procedure Execute;
    property CallBackEvent: TCallBackEvent write FCallBackEvent;
  published
    property Cancel: Boolean read FCancel write FCancel;
    property CommandLine: String read FCommandLine write FCommandLine;
    property ExitCode: Integer read FExitCode;
  end;

implementation

constructor TCallbackProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCallbackProcess.Destroy;
begin
  inherited Destroy;
end;

function TCallbackProcess.ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
var
  M: TMemoryStream;
  BytesRead: Int64;
  n: Integer;
begin
  M := TMemoryStream.Create;
  BytesRead := 0;
  try
    repeat
      M.SetSize(BytesRead + AStream.NumBytesAvailable);
      n := AStream.Read((M.Memory + BytesRead)^, AStream.NumBytesAvailable);
      Inc(BytesRead, n);
    until (n=0);
    if BytesRead>0 then
    begin
      SetLength(AString,BytesRead);
      M.Read(AString[1],BytesRead);
    end;
  finally
    M.Free;
    Result := BytesRead;
  end;
end;

procedure TCallbackProcess.Execute;
var
  strTemp: String;
begin
  FExitCode:=0;
  try
    strTemp := '';
    CreateProcess;
    FCancel := False;
    FProcess.CommandLine := FCommandLine;
    FProcess.Execute;
    while (FProcess.Running) do
    begin
      Sleep(10);
      if FCancel then FProcess.Terminate(0);
      if ReadFromPipeStream(FProcess.Stderr,strTemp)>0 then
        FCallBackEvent(pcStdError, strTemp);
    end;
    if ReadFromPipeStream(FProcess.Stderr,strTemp)>0 then
      FCallBackEvent(pcStdError, strTemp);
    if ReadFromPipeStream(FProcess.Output,strTemp)>0 then
      FCallBackEvent(pcStdOut, strTemp);
  except
    on E:EProcess do
      FCallBackEvent(pcError, 'Process-Error ' + IntToStr(FProcess.ExitStatus));
    else
      FCallBackEvent(pcError, IntToStr(FProcess.ExitStatus));
  end;
  FExitCode:=FProcess.ExitStatus;
  FreeAndNil(FProcess);
  FCallBackEvent(pcFinished, '');
end;

procedure TCallbackProcess.CreateProcess;
begin
  FProcess := TProcess.Create(nil);
  FProcess.Options :=  [poUsePipes,poNoConsole];
end;

end.

