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
  Classes, SysUtils, DBus, liUtils, CTypes;

type
 TDBusClient = class
 private
  err: DBusError;
  conn: PDBusConnection;
  error: Boolean;
  args: DBusMessageIter;

  bname, bintf: String;
  procedure ShowError(info: String);
 public
  constructor Create(bustype: DBusBusType);
  destructor Destroy;override;

  function Connect(name: String;path: String;intf:String;method: String): PDBusMessage;

  function ReplyMessageAddString(msg: PDBusMessage;str: String): Boolean;
  function ReplyMessageAddBool(msg: PDBusMessage;bool: Boolean): Boolean;
  function ReplyMessageAddInt(msg: PDBusMessage;int: Integer): Boolean;
  function SendReplyAndWait(msg: PDBusMessage): PDBusMessage;

  function MessageIterNext: Boolean;
  function ReadMessageParamStr(msg: PDBusMessage): String;
  function ReadMessageParamBool(msg: PDBusMessage): Boolean;

  function StartListening(objpath: String): Boolean;
  function ReadSignalMessage: PDBusMessage;
  function ReceivedSignalIs(msg: PDBusMessage;sigName: String): Boolean;
  function ReadSignalStr(msg: PDBusMessage): String;
  function ReadSignalInt(msg: PDBusMessage): Integer;
  function ReadSignalBool(msg: PDBusMessage): Boolean;

  procedure FreeMessage(msg: PDBusMessage);

  property Failed: Boolean read error;
 end;

 TDBusServer = class
 private
  conn: PDBusConnection;
  err: DBusError;
  args: DBusMessageIter;
  error: Boolean;
  procedure ShowError(info: String);
 public
  constructor Create(bname: String;ty: DBusBusType;flags: cuint);
  constructor Create(dConn: PDBusConnection);
  destructor  Destroy;override;

  function ReadMessage: PDBusMessage;
  function MessageIsMethodCall(msg: PDBusMessage;intf: String;call: String): Boolean;

  function InitMessageIter(msg: PDBusMessage): Boolean;
  procedure MessageIterNext;
  function ReadMessageParamStr: String;
  function ReadMessageParamBool: Boolean;
  function ReadMessageParamInt: Integer;

  function CreateReturnMessage(msg: PDBusMessage): PDBusMessage;

  function AppendBool(val: Boolean): Boolean;
  function AppendStr(val: String): Boolean;
  function AppendUInt(val: Integer): Boolean;
  function SendMessage(msg: PDBusMessage): Boolean;

  function GetMessageSender(msg: PDBusMessage): String;
  procedure FreeMessage(msg: PDBusMessage);

  function CreateNewSignal(objName: String;intfName: String;sigName: String): PDBusMessage;

  property Failed: Boolean read error;
  property Connection: PDBusConnection read conn;
 end;

implementation

{ TSimDBus }

constructor TDBusClient.Create(bustype: DBusBusType);
begin
 dbus_error_init(@err);
 error:=false;
 conn := dbus_bus_get_private(bustype, @err);
 dbus_connection_set_exit_on_disconnect(conn,0);

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
 if conn<>nil then
  dbus_connection_close(conn);
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
    Result:=false;
  end;
end;

function TDBusClient.ReplyMessageAddBool(msg: PDBusMessage;bool: Boolean): Boolean;
var val: cbool;
begin
 Result:=true;
 val:=bool;
 if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @val) = 0) then
  begin
    ShowError('Out Of Memory!');
    Result:=false;
  end;
end;

function TDBusClient.ReplyMessageAddInt(msg: PDBusMessage;int: Integer): Boolean;
var val: cuint32;
begin
 Result:=true;
 val:=int;
 if (dbus_message_iter_append_basic(@args, DBUS_TYPE_UINT32, @val) = 0) then
  begin
    ShowError('Out Of Memory!');
    Result:=false;
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
end;

function TDBusClient.MessageIterNext: Boolean;
begin
 Result:=true;
 if (dbus_message_iter_next(@args) = 0) then
  begin
     ShowError('Message has too few arguments!');
     Result:=false;
  end;
end;

function TDBusClient.ReadMessageParamBool(msg: PDBusMessage): Boolean;
var boolv: cbool;
begin
 Result:=false;
 if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
     ShowError('Argument is not boolean!')
  else
  begin
   dbus_message_iter_get_basic(@args, @boolv);
   Result:=boolv;
  end;
end;

function TDBusClient.ReadMessageParamStr(msg: PDBusMessage): String;
var strv: PChar;
begin
 Result:='';
 if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
     ShowError('Argument is no string!')
  else
  begin
     dbus_message_iter_get_basic(@args, @strv);
     Result:=strv;
  end;
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

function TDBusClient.ReceivedSignalIs(msg: PDBusMessage;sigName: String): Boolean;
begin
 Result:=false;
 if (dbus_message_is_signal(msg, PChar(bintf), PChar(sigName)) <> 0) then
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

{ TDBusServer }

constructor TDBusServer.Create(bname: String;ty: DBusBusType;flags: CUInt);
var ret: cint;
begin
  error:=false;
  conn := dbus_bus_get_private(ty, @err);
  if dbus_error_is_set(@err) <> 0 then
  begin
    ShowError('DBus connection Error: '+err.message);
    dbus_error_free(@err);
  end;
  if conn = nil then Exit;

  dbus_error_init(@err);
  //Request the name of the bus
  ret:=dbus_bus_request_name(conn, PChar(bname), flags, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    p_error('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;

  if ret<>DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then error:=true;
end;

constructor TDBusServer.Create(dConn: PDBusConnection);
begin
 error:=false;
 dbus_error_init(@err);
 conn:=dConn;
end;

destructor TDBusServer.Destroy;
begin
 if conn<>nil then
  dbus_connection_close(conn);
 inherited;
end;

procedure TDBusServer.ShowError(info: String);
begin
 p_error(info);
 error:=true;
end;

function TDBusServer.ReadMessage: PDBusMessage;
begin
  //non blocking read of the next available message
  dbus_connection_read_write(conn, 0);
  Result:=dbus_connection_pop_message(conn);
end;

function TDBusServer.MessageIsMethodCall(msg: PDBusMessage;intf: String;call: String): Boolean;
begin
 if dbus_message_is_method_call(msg,PChar(intf),PChar(call)) <> 0 then
  Result:=true
 else
  Result:=false;
end;

procedure TDBusServer.FreeMessage(msg: PDBusMessage);
begin
 dbus_message_unref(msg);
end;

function TDBusServer.InitMessageIter(msg: PDBusMessage): Boolean;
begin
 if (dbus_message_iter_init(msg, @args) = 0) then
 begin
  ShowError('Message has no arguments!');
  Result:=false;
 end else
  Result:=true;
end;

procedure TDBusServer.MessageIterNext;
begin
 dbus_message_iter_next(@args);
end;

function TDBusServer.ReadMessageParamStr: String;
var strv: PChar;
begin
 Result:='';
 if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
      ShowError('Argument is no string!')
   else
   begin
      dbus_message_iter_get_basic(@args, @strv);
      Result:=strv;
   end;
end;

function TDBusServer.ReadMessageParamBool: Boolean;
var boolv: cbool;
begin
 Result:=false;
 if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
      ShowError('Argument is no boolean!')
 else
 begin
      dbus_message_iter_get_basic(@args, @boolv);
      Result:=boolv;
 end;
end;

function TDBusServer.ReadMessageParamInt: Integer;
var intv: cuint;
begin
 Result:=-1;
 if (DBUS_TYPE_UINT32 <> dbus_message_iter_get_arg_type(@args)) then
      ShowError('Argument is no integer!')
 else
 begin
      dbus_message_iter_get_basic(@args, @intv);
      Result:=intv;
 end;
end;

function TDBusServer.GetMessageSender(msg: PDBusMessage): String;
begin
 Result:=dbus_message_get_sender(msg)
end;

function TDBusServer.CreateReturnMessage(msg: PDBusMessage): PDBusMessage;
begin
 Result:=dbus_message_new_method_return(msg);
 dbus_message_iter_init_append(Result, @args);
end;

function TDBusServer.AppendBool(val: Boolean): Boolean;
var boolv: cbool;
begin
 boolv:=val;
 Result:=true;
 if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @boolv) = 0) then
   begin
     ShowError('Out Of Memory!');
     Result:=false;
   end;
end;

function TDBusServer.AppendStr(val: String): Boolean;
var strv: PChar;
begin
 strv:=PChar(val);
 Result:=true;
 if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @strv) = 0) then
   begin
     ShowError('Out Of Memory!');
     Result:=false;
   end;
end;

function TDBusServer.SendMessage(msg: PDBusMessage): Boolean;
begin
 Result:=true;
 //Send the reply & flush the connection
 if (dbus_connection_send(conn, msg, nil) = 0) then
 begin
     ShowError('Out Of Memory!');
     Result:=false;
     exit;
 end;
 dbus_connection_flush(conn);

 //Free the reply
 dbus_message_unref(msg);
end;

function TDBusServer.CreateNewSignal(objName: String;intfName: String;sigName: String): PDBusMessage;
begin
 Result:=dbus_message_new_signal(PChar(objName),PChar(intfName),PChar(sigName));

 if (Result = nil) then
 begin
  ShowError('Message Null');
  exit;
 end;
  //Prepare argument inter
  dbus_message_iter_init_append(Result, @args);
end;

function TDBusServer.AppendUInt(val: Integer): Boolean;
var uintv: cuint;
begin
 uintv:=val;
 Result:=true;
 if (dbus_message_iter_append_basic(@args, DBUS_TYPE_UINT32, @uintv) = 0) then
  begin
    ShowError('Out Of Memory!');
    Result:=false;
  end;
end;

end.

