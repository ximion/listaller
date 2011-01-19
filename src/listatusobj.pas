(* Copyright (C) 2010-2011 Matthias Klumpp
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
//** Class to provide all derivative classes simple access to Listaller's message types
unit listatusobj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LiTypes, LiUtils;

type
  TLiStatusObject = class
  private
  protected
    // Events
    FStatus: LiStateEvent;
    FMessage: LiMessageEvent;
    // Callback userdata
    status_udata: Pointer;
    message_udata: Pointer;
    //State data
    sdata: LiStatusData;

    procedure EmitInfoMsg(s: String);
    procedure EmitStageMsg(s: String);
    procedure EmitError(msg: String);
    function EmitUserRequestYesNo(s: String): LI_REQUEST_RES;
    function EmitUserRequestAbortContinue(s: String): LI_REQUEST_RES;
    procedure EmitStatusChange(state: LI_STATUS; const msg: String = ''; const errcode: Integer = 0);
    procedure EmitProgress(i: Integer);
    procedure EmitExProgress(i: Integer);
  public
    constructor Create;

    procedure RegisterOnStatus(call: LiStateEvent; udata: Pointer);
    procedure RegisterOnMessage(call: LiMessageEvent; udata: Pointer);
  end;


implementation

{ TLiStatusObject }

constructor TLiStatusObject.Create;
begin
  inherited;
  status_udata := nil;
  message_udata := nil;
end;

procedure TLiStatusObject.RegisterOnStatus(call: LiStateEvent; udata: Pointer);
begin
  if CheckPtr(call, 'LiStateEvent') then
  begin
    FStatus := call;
    status_udata := udata;
  end;
end;

procedure TLiStatusObject.RegisterOnMessage(call: LiMessageEvent; udata: Pointer);
begin
  if CheckPtr(call, 'LiMessageEvent') then
  begin
    FMessage := call;
    message_udata := udata;
  end;
end;

procedure TLiStatusObject.EmitInfoMsg(s: String);
begin
  if Assigned(FMessage) then
    FMessage(LIM_Info, PChar(s), message_udata)
  else
    pinfo(s);
end;

procedure TLiStatusObject.EmitStageMsg(s: String);
begin
  sdata.text := PChar(s);
  if Assigned(FStatus) then
    FStatus(LIS_Stage, sdata, status_udata)
  else
    pinfo(s);
  sdata.text := ''; //Zero
end;

procedure TLiStatusObject.EmitError(msg: String);
begin
  sdata.text := PChar(msg);
  if Assigned(FStatus) then
    FStatus(LIS_Failed, sdata, status_udata)
  else
    perror(msg);
  sdata.text := ''; //Zero

  // Send failed message
  EmitStatusChange(LIS_Finished, '', 1);
end;

function TLiStatusObject.EmitUserRequestYesNo(s: String): LI_REQUEST_RES;
begin
  if Assigned(FMessage) then
    Result := FMessage(LIM_Question_YesNo, PChar(s), message_udata)
  else
  begin
    Result := LIRQS_No;
    pwarning('Tried to display user question, but no callback was attached.');
    pwarning('Using defaulr option "No"');
  end;
end;

function TLiStatusObject.EmitUserRequestAbortContinue(s: String): LI_REQUEST_RES;
begin
  if Assigned(FMessage) then
    Result := FMessage(LIM_Question_AbortContinue, PChar(s), message_udata)
  else
  begin
    Result := LIRQS_No;
    pwarning('Tried to display warning dialog, but no callback was attached.');
    pwarning('Using defaulr option "No"');
  end;
end;

procedure TLiStatusObject.EmitProgress(i: Integer);
begin
  sdata.mnprogress := i;
  if Assigned(FStatus) then
    FStatus(LIS_Progress, sdata, status_udata);
end;

procedure TLiStatusObject.EmitExProgress(i: Integer);
begin
  sdata.exprogress := i;
  if Assigned(FStatus) then
    FStatus(LIS_Progress, sdata, status_udata);
end;

procedure TLiStatusObject.EmitStatusChange(state: LI_STATUS; const msg: String = ''; const errcode: Integer = 0);
begin
  sdata.Text := PChar(msg);
  sdata.Error_Code := errcode;
  if state = LIS_Started then
    pdebug('LISTATUS::CHANGED >> LIS_Started!');
  if state = LIS_Finished then
    pdebug('LISTATUS::CHANGED >> LIS_Finished!');
  if Assigned(FStatus) then
    FStatus(state, sdata, status_udata);
end;

end.

