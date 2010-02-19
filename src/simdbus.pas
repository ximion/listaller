{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This library is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Make DBus access a little bit easier
unit simdbus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBus, liBasic, CTypes;

type
 TDBusClient = class
 private
  err: DBusError;
  conn: PDBusConnection;
  error: Boolean;
  connected: Boolean;
  args: DBusMessageIter;
  argInit: Boolean;

  bname, bintf: String;
  procedure ShowError(info: String);
 public
  constructor Create(bustype: DBusBusType);
  destructor Destroy;override;

  function Connect(name: String;path: String;intf:String;method: String): PDBusMessage;
  function ReplyMessageAddString(msg: PDBusMessage;str: String): Boolean;
  function SendReplyAndWait(msg: PDBusMessage): PDBusMessage;

  function ReadMessageParamStr(msg: PDBusMessage): String;
  function ReadMessageParamBool(msg: PDBusMessage): Boolean;

  function StartListening(objpath: String): Boolean;
  function ReadSignalMessage: PDBusMessage;

  function ReceivedSignal(msg: PDBusMessage;name: String): Boolean;
  function ReadSignalStr(msg: PDBusMessage): String;
  function ReadSignalInt(msg: PDBusMessage): Integer;
  function ReadSignalBool(msg: PDBusMessage): Boolean;

  procedure FreeMessage(msg: PDBusMessage);

  property Failed: Boolean read error;
 end;

implementation

{ TSimDBus }

constructor TDBusClient.Create(bustype: DBusBusType);
begin
 dbus_error_init(@err);
 error:=false;
 argInit:=false;
 conn := dbus_bus_get_private(bustype, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    ShowError('Connection Error: '#10+err.message);
    dbus_error_free(@err);
    exit;
  end;
 if conn=nil then
  error:=true;
end;

destructor TDBusClient.Destroy;
begin
 inherited;
end;

procedure TDBusClient.ShowError(info: String);
begin
 p_error(info);
 error:=true;
end;

function TDBusClient.Connect(name: String;path: String;intf:String;method: String): PDBusMessage;
begin
 error:=false;
 bname:=name;
 bintf:=intf;
 Result:=dbus_message_new_method_call(PChar(name),
                                      PChar(path),
                                      PChar(intf),
                                      PChar(method));
 if (Result = nil) then
  begin
    ShowError('Message Null');
    exit;
  end;

  //append arguments
  dbus_message_iter_init_append(Result, @args);
end;

function TDBusClient.ReplyMessageAddString(msg: PDBusMessage;str: String): Boolean;
var val: PChar;
begin
 Result:=true;
 val:=PChar(str);
 if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @val) = 0) then
  begin
    ShowError('Out Of Memory!');
    exit;
  end;
end;

function TDBusClient.SendReplyAndWait(msg: PDBusMessage): PDBusMessage;
var pending: PDBusPendingCall;
begin
 // send message and get a handle for a reply
  if (dbus_connection_send_with_reply(conn, msg, @pending, -1) = 0) then // -1 is default timeout
  begin
    ShowError('Out Of Memory!');
    exit;
  end;
  if (pending = nil) then
  begin
    ShowError('Pending Call Null');
    exit;
  end;
  dbus_connection_flush(conn);

  // free message
  dbus_message_unref(msg);

  // block until we recieve a reply
  dbus_pending_call_block(pending);

  // get the reply message
  Result := dbus_pending_call_steal_reply(pending);
  if (Result = nil) then
  begin
    ShowError('Reply Null');
    exit;
  end;
  // free the pending message handle
  dbus_pending_call_unref(pending);
  if (dbus_message_iter_init(msg, @args) = 0) then
   ShowError('Message has no arguments!');

  argInit:=false;
end;

function TDBusClient.ReadMessageParamBool(msg: PDBusMessage): Boolean;
var boolv: cbool;
begin
 Result:=false;
 if argInit then
 begin
  if (dbus_message_iter_next(@args) = 0) then
  begin
     ShowError('Message has too few arguments!');
     exit;
  end;
 end else if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
     ShowError('Argument is not boolean!')
  else
  begin
   dbus_message_iter_get_basic(@args, @boolv);
   Result:=boolv;
  end;
 argInit:=true;
end;

function TDBusClient.ReadMessageParamStr(msg: PDBusMessage): String;
var strv: PChar;
begin
 Result:='';
 if argInit then
 begin
  if (dbus_message_iter_next(@args) = 0) then
  begin
     ShowError('Message has too few arguments!');
     exit;
  end;
 end else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
     ShowError('Argument is no string!')
  else
  begin
     dbus_message_iter_get_basic(@args, @strv);
     Result:=strv;
     writeln(strv);
  end;
 argInit:=true;
end;

procedure TDBusClient.FreeMessage(msg: PDBusMessage);
begin
 dbus_message_unref(msg);
end;

function TDBusClient.StartListening(objpath: String): Boolean;
begin
 Result:=true;
 // add a rule for which messages we want to see
  dbus_bus_add_match(conn,
  PChar('type=''signal'',sender='''+PChar(bname)+''', interface='''+PChar(bintf)+''', path='''+PChar(objpath)+'''')
  , @err);

  dbus_connection_flush(conn);
  if (dbus_error_is_set(@err) <> 0) then
  begin
    ShowError('Match Error ('+err.message+')');
    Result:=false;
    exit;
  end;
  p_info('Match rule sent');
end;

function TDBusClient.ReadSignalMessage: PDBusMessage;
begin
 Result:=nil;
 // non blocking read of the next available message
 dbus_connection_read_write(conn, 0);
 Result:=dbus_connection_pop_message(conn);
end;

function TDBusClient.ReceivedSignal(msg: PDBusMessage;name: String): Boolean;
begin
 Result:=false;
 if (dbus_message_is_signal(msg, PChar(bintf), PChar(bname)) <> 0) then
    Result:=true;
end;

function TDBusClient.ReadSignalStr(msg: PDBusMessage): String;
var strv: PChar;
begin
 Result:='';
 // read the parameters
 if (dbus_message_iter_init(msg, @args) = 0) then
  ShowError('Message Has No Parameters')
 else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
  ShowError('Argument is no string!')
 else
 begin
  dbus_message_iter_get_basic(@args, @strv);
  Result:=strv;
 end;
end;

function TDBusClient.ReadSignalInt(msg: PDbusMessage): Integer;
var intv: cuint32;
begin
 Result:=-1;
 // read the parameters
 if (dbus_message_iter_init(msg, @args) = 0) then
  ShowError('Message Has No Parameters')
 else if (DBUS_TYPE_UINT32 <> dbus_message_iter_get_arg_type(@args)) then
  ShowError('Argument is no integer!')
 else
 begin
  dbus_message_iter_get_basic(@args, @intv);
  Result:=intv;
 end;
end;

function TDBusClient.ReadSignalBool(msg: PDBusMessage): Boolean;
var boolv: cbool;
begin
 Result:=false;
 // read the parameters
 if (dbus_message_iter_init(msg, @args) = 0) then
  ShowError('Message Has No Parameters')
 else if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
  ShowError('Argument is no boolean!')
 else
 begin
  dbus_message_iter_get_basic(@args, @boolv);
  Result:=boolv;
 end;
end;

end.

