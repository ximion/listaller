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
//** Process with callbacks
unit callbackprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, Pipes, Process, SysUtils;

type

  TProcessChannel = (pcStdOut, pcStdError, pcFinished, pcError);

  TCallBackEvent = procedure(pcChannel: TProcessChannel; strData: AnsiString) of object;

  TCallbackProcess = class(TThread)
  private
    FProcess: TProcess;
    FCallBackEvent: TCallBackEvent;
    FCommandLine: String;
    FCancel: Boolean;
    FProcessOptions: TProcessOptions;
    FOutString: WideString;
    FReplaceBreaks: Boolean;
    FErrorMsg: String;
    FChannel: TProcessChannel;
    FWait: Boolean;
    procedure SetProcessOptions(Value: TProcessOptions);
    function ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
    procedure SyncState;
    function GetRunning: Boolean;
    function GetExitCode: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunCommand;
    property CallBackEvent: TCallBackEvent write FCallBackEvent;
    property Cancel: Boolean read FCancel write FCancel;
    property CommandLine: String read FCommandLine write FCommandLine;
    property Options: TProcessOptions read FProcessOptions write SetProcessOptions;
    property Running: Boolean read GetRunning;
    property WaitOnExit: Boolean read FWait write FWait;
    property ExitCode: Integer read GetExitCode;
  end;

implementation

constructor TCallbackProcess.Create;
begin
  inherited Create(true);
  FProcessOptions := [poUsePipes, poNoConsole];
  FProcess := TProcess.Create(nil);
  FProcess.Options := FProcessOptions;
  FReplaceBreaks := true;
end;

destructor TCallbackProcess.Destroy;
begin
  Terminate;
  FProcess.Free;
  inherited;
end;

function TCallbackProcess.ReadFromPipeStream(AStream: TInputPipeStream;
  var AString: String): Integer;
var
  M: TMemoryStream;
  BytesRead: int64;
  n: Integer;
begin
  M := TMemoryStream.Create;
  BytesRead := 0;
  try
    repeat
      M.SetSize(BytesRead + AStream.NumBytesAvailable);
      n := AStream.Read((M.Memory + BytesRead)^, AStream.NumBytesAvailable);
      Inc(BytesRead, n);
    until (n = 0);
    if BytesRead > 0 then
    begin
      SetLength(AString, BytesRead);
      M.Read(AString[1], BytesRead);
    end;
  finally
    M.Free;
    Result := BytesRead;
  end;
end;

procedure TCallbackProcess.Execute;
var
  NoMoreOutPut: Boolean;

  function ReadOutputToStr(Process: TProcess): String;
  var
    Buffer: String;
    BytesAvailable: DWord;
    BytesRead: longint;
  begin
    if Process.Running then
    begin
      BytesAvailable := Process.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        Result := Result + copy(Buffer, 1, BytesRead);
        BytesAvailable := Process.Output.NumBytesAvailable;
        NoMoreOutput := false;
      end;
    end;
  end;

var
  strTemp: String;
begin
  while not Terminated do
  begin
    try
      repeat
        FOutString := FOutString + ReadOutputToStr(FProcess);
        if pos(#10, FOutString) > 0 then
        begin
          strTemp := FOutString;
          FOutString := copy(strTemp, 1, pos(#10, strTemp));
          strTemp := copy(strTemp, pos(#10, strTemp)+1, length(strTemp));
          FChannel := pcStdOut;
          Synchronize(@SyncState);
          FOutString := strTemp;
        end;

        if FCancel then
        begin
          Terminate;
          exit;
        end;
      until NoMoreOutput;

      if ReadFromPipeStream(FProcess.Stderr, strTemp) > 0 then
      begin
        while pos(#10, strTemp)>0 do
        begin
          FOutString := copy(strTemp, 1, pos(#10, strTemp));
          strTemp := copy(strTemp, pos(#10, strTemp)+1, length(strTemp));
          FChannel := pcStdOut;
          Synchronize(@SyncState);
          FOutString := strTemp;
        end;
      end;

    except
      on E: EProcess do
      begin
        FErrorMsg := 'Process-Error ' + IntToStr(FProcess.ExitStatus);
        FChannel := pcError;
        Synchronize(@SyncState);
      end
      else
      begin
        FErrorMsg := IntToStr(FProcess.ExitStatus);
        FChannel := pcError;
        Synchronize(@SyncState);
      end;
    end;

    if not FProcess.Running then
    begin
      FChannel := pcFinished;
      Synchronize(@SyncState);
      Terminate;
    end;
  end;
end;

procedure TCallbackProcess.SyncState;
begin
  if FChannel = pcStdOut then
  begin
  if FReplaceBreaks then
    FOutString := StringReplace(FOutString, #10, '', [rfReplaceAll]);
  FCallBackEvent(pcStdOut, FOutString);
  FOutString := '';
  end else if FChannel = pcError then
  begin
    FCallBackEvent(pcError, FErrorMsg);
    FErrorMsg := '';
  end else
   FCallBackEvent(FChannel, '');
end;

function TCallbackProcess.GetRunning: Boolean;
begin
  Result := FProcess.Running;
end;

procedure TCallbackProcess.RunCommand;
begin
  FCancel := false;
  FOutString := '';
  FerrorMsg := '';
  FProcess.CommandLine := FCommandLine;
  FProcess.Execute;
  if not FWait then
   Resume
  else
   Execute;
end;

procedure TCallbackProcess.SetProcessOptions(Value: TProcessOptions);
begin
  if not (poUsePipes in Value) then
    Include(Value, poUsePipes);
  //FProcess.Options := Value;
end;

function TCallbackProcess.GetExitCode: Integer;
begin
  Result := FProcess.ExitStatus;
end;

end.

