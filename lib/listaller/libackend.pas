(* Copyright (C) 2010 Matthias Klumpp
 *
 * Authors:
 *  Matthias Klumpp
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
//** Defines backend interface class
unit libackend;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LiTypes;

type
  TLiBackend = class
  private
    FStatus: StatusChangeEvent;

    statechange_udata: Pointer;
  protected
    procedure EmitMessage(s: widestring);
    procedure EmitProgress(pos: integer);
  public
    function Initialize(ai: LiAppInfo): boolean; virtual; abstract;
    function CanBeUsed: boolean; virtual; abstract;
    function Run: boolean; virtual; abstract;
    procedure SetMessageHandler(status: StatusChangeEvent; udata: Pointer);
  end;

implementation

{ TLiBackend }

procedure TLiBackend.SetMessageHandler(status: StatusChangeEvent; udata: Pointer);
begin
  FStatus := status;
  statechange_udata := udata;
end;

procedure TLiBackend.EmitMessage(s: widestring);
var
  sdata: LiStatusData;
begin
  sdata.msg := PChar(s);
  if Assigned(FStatus) then
    FStatus(scMessage, sdata, statechange_udata);
end;

procedure TLiBackend.EmitProgress(pos: integer);
var
  sdata: LiStatusData;
begin
  sdata.mnprogress := pos;
  if Assigned(FStatus) then
    FStatus(scMnProgress, sdata, statechange_udata);
end;

end.

