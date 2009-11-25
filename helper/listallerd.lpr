{ Copyright (C) 2009 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This unit is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This unit is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this unit. If not, see <http://www.gnu.org/licenses/>.}
//** The Listaller DBus daemon tool
program listallerd;

{$mode objfpc}{$H+}

uses
  cthreads, Interfaces, Classes, SysUtils, CustApp, dbus, djobs,
  Contnrs, cTypes, liBasic, LCLIntf;

type

  { TLiDaemon }

  //** The Listaller helper daemon for root actions
  TLiDaemon = class(TCustomApplication)
  protected
    err: DBusError;
    procedure DoRun; override;
  private
    JobList: TObjectList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure ListenForCall;
  end;

const
 Timeout_Seconds = 18; //Quit daemon after 18 seconds of inactivity
{ TLiDaemon }

procedure TLiDaemon.ListenForCall;
var
  msg: PDBusMessage;
  smsg: PDBusMessage;
  ret: cint;
  startTick: Integer;
  lastJobCount: Integer=0; //Hold the last number of active jobs
begin
  WriteLn('Daemon started.');

  // Request the name of the bus
  ret:=dbus_bus_request_name(conn, 'org.freedesktop.Listaller', DBUS_NAME_FLAG_REPLACE_EXISTING, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    p_error('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;

  if ret<>DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then exit;

  //Set tick count
  startTick:=DateTimeToTimeStamp(Now).Time;

  // loop, testing for new messages
  while (true) do
  begin

    //Check tick count
    if (InstallWorkers+ManagerWorkers)<>lastJobCount then
     startTick:=DateTimeToTimeStamp(Now).Time;

    lastJobCount:=InstallWorkers+ManagerWorkers;

     if (InstallWorkers=0)and(ManagerWorkers=0)and(((DateTimeToTimeStamp(Now).Time-startTick)/1000)=Timeout_Seconds) then
      break; //Exit loop -> terminate daemon

    // non blocking read of the next available message
    dbus_connection_read_write(conn, 0);
    msg:=dbus_connection_pop_message(conn);

    // loop again if we haven't got a message
    if (msg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    // check this is a method call for the right interface & method
    if (dbus_message_is_method_call(msg, 'org.freedesktop.Listaller.Install', CALL_RUNSETUP) <> 0) then
    begin
      if InstallWorkers = 0 then
      begin
       sMsg:=msg;
        JobList.Add(TDoAppInstall.Create(sMsg));
       Inc(InstallWorkers);
      end;
    end else
    if (dbus_message_is_method_call(msg, 'org.freedesktop.Listaller.Manage', CALL_APPREMOVE) <> 0) then
    begin
      if InstallWorkers = 0 then
      begin
       sMsg:=msg;
        JobList.Add(TDoAppRemove.Create(sMsg));
       Inc(ManagerWorkers);
      end;
    end else
     // free the message
     dbus_message_unref(msg);
  end;
end;

procedure TLiDaemon.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ListenForCall;

  // stop program loop
  Terminate;
end;

constructor TLiDaemon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

if IsRoot then
begin
  dbus_error_init(@err);
  conn := dbus_bus_get_private(DBUS_BUS_SYSTEM, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('DBus connection Error: '+err.message);
    dbus_error_free(@err);
  end;

  if conn = nil then Exit;

  JobList:=TObjectList.Create;
end else
begin
 writeLn('Please run listallerd as root!');
 halt(1);
end;

end;

destructor TLiDaemon.Destroy;
begin
 if conn<>nil then
  dbus_connection_close(conn);
  inherited Destroy;
end;

procedure TLiDaemon.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TLiDaemon;

{$IFDEF WINDOWS}{$R listallerd.rc}{$ENDIF}

begin
  Application:=TLiDaemon.Create(nil);
  Application.Title:='Listaller Daemon';
  Application.Run;
  Application.Free;
end.

