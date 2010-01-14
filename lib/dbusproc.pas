unit dbusproc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, liTypes, DBus, ctypes, liBasic;

type
 //** Which detail of the process was changed?
 TLiProcDetail = (pdProgress, pdMessage, pdInfo, pdStatus, pdError);

 //** Record which contains data about the current thread
 TLiProcData = record
   changed: TLiProcDetail;
   progress: Integer;
   jobID: String;
   msg: String;
   info: String;
 end;

 //** Event for thread signals
 TLiDAction = procedure(ty: TProcStatus;data: TLiProcData) of Object;

 //** Actions Listaller can perform on DBus root interface
 TListallerBusAction = (lbaUninstallApp);

 //** A command Listaller should do on DBus
 ListallerBusCommand = record
   cmdtype: TListallerBusAction;
   appinfo: TAppInfo;
 end;

 //** Thread which can perform various Listaller-DBus actions
 TLiDBusAction = class(TThread)
 private
  cmd: TListallerBusAction;
  FStatus: TLiDAction;
  appinfo: TAppInfo;

  status: TProcStatus;
  procinfo: TLiProcData;
  procedure SyncProcStatus;
  procedure SendError(msg: String);
  procedure SendProgress(val: Integer);
  //** Remove app as root via remote DBus connection
  procedure UninstallAppAsRoot(obj: TAppInfo);
 public
  constructor Create(action: ListallerBusCommand);
  destructor Destroy;override;

  procedure Execute;override;
  property OnStatus: TLiDAction read FStatus write FStatus;
 end;

implementation

{ TDBusAction }

constructor TLiDBusAction.Create(action: ListallerBusCommand);
begin
 inherited Create(true);
 FreeOnTerminate:=true;
 cmd:=action.cmdtype;
 appinfo:=action.appinfo;
 Resume;
end;

destructor TLiDBusAction.Destroy;
begin
 status:=prFinished;
 procinfo.changed:=pdStatus;
 Synchronize(@SyncProcStatus);
 inherited;
end;

procedure TLiDBusAction.SyncProcStatus;
begin
 if Assigned(FStatus) then FStatus(status,procinfo);
 procinfo.msg:='~?error';
 procinfo.info:='~?error';
end;

procedure TLiDBusAction.Execute;
begin
 if cmd=lbaUninstallApp then UninstallAppAsRoot(appinfo);
end;

procedure TLiDBusAction.SendError(msg: String);
begin
 procinfo.msg:=msg;
 procinfo.changed:=pdError;
 Synchronize(@SyncProcStatus);
end;

procedure TLiDBusAction.SendProgress(val: Integer);
begin
 procinfo.changed:=pdProgress;
 procinfo.progress:=val;
 Synchronize(@SyncProcStatus);
end;

procedure TLiDBusAction.UninstallAppAsRoot(obj: TAppInfo);
var
  dmsg: PDBusMessage;
  args: DBusMessageIter;
  pending: PDBusPendingCall;
  stat: Boolean;
  rec: PChar;
  err: DBusError;
  conn: PDBusConnection;
  action_finished: Boolean;

  intvalue: cuint32;
  strvalue: PChar;
  boolvalue: Boolean;
begin

  dbus_error_init(@err);

  //New DBus connection
  conn := dbus_bus_get_private(DBUS_BUS_SYSTEM, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    p_error('Connection Error: '#10+err.message);
    dbus_error_free(@err);

    SendError('An error occured during remove request.');
    Terminate;
  end;

  if conn = nil then exit;

  p_info('Calling listaller-daemon...');
  // create a new method call and check for errors
  dmsg := dbus_message_new_method_call('org.freedesktop.Listaller', // target for the method call
                                      '/org/freedesktop/Listaller/Manager1', // object to call on
                                      'org.freedesktop.Listaller.Manage', // interface to call on
                                      'RemoveApp'); // method name
  if (dmsg = nil) then
  begin
    p_error('Message Null');
    exit;
  end;

  //append arguments
  dbus_message_iter_init_append(dmsg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @obj.Name) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @obj.UId) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  // send message and get a handle for a reply
  if (dbus_connection_send_with_reply(conn, dmsg, @pending, -1) = 0) then // -1 is default timeout
  begin
    p_error('Out Of Memory!');
    exit;
  end;
  if (pending = nil) then
  begin
    p_error('Pending Call Null');
    exit;
  end;
  dbus_connection_flush(conn);

  p_info('AppRemove request sent');

  // free message
  dbus_message_unref(dmsg);

  // block until we recieve a reply
  dbus_pending_call_block(pending);

  // get the reply message
  dmsg := dbus_pending_call_steal_reply(pending);
  if (dmsg = nil) then
  begin
    p_error('Reply Null');
    exit;
  end;
  // free the pending message handle
  dbus_pending_call_unref(pending);

  // read the parameters
  if (dbus_message_iter_init(dmsg, @args) = 0) then
     p_error('Message has no arguments!')
  else if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
     p_error('Argument is not boolean!')
  else
     dbus_message_iter_get_basic(@args, @stat);

  if (dbus_message_iter_next(@args) = 0) then
     p_error('Message has too few arguments!')
  else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
     p_error('Argument is no string!')
  else
     dbus_message_iter_get_basic(@args, @rec);

  if (stat = false) then
  begin
   if rec = 'blocked' then
    SendError('You are not authorized to do this action!')
   else
    SendError('An error occured during install request.');
   Synchronize(@SyncProcStatus);
   Terminate;
   exit;
  end;
  // free reply
  dbus_message_unref(dmsg);

  //////////////////////////////////////////////////////////////////////

  //Now start listening to Listaller dbus signals and forward them to
  //native functions until the "Finished" signal is received

  // add a rule for which messages we want to see
  dbus_bus_add_match(conn,
  'type=''signal'',sender=''org.freedesktop.Listaller'', interface=''org.freedesktop.Listaller.Manage'', path=''/org/freedesktop/Listaller/Manager1'''
  , @err);

  dbus_connection_flush(conn);
  if (dbus_error_is_set(@err) <> 0) then
  begin
    p_error('Match Error ('+err.message+')');
    exit;
  end;
  p_info('Match rule sent');

  procinfo.progress:=0;
  action_finished:=false;
  // loop listening for signals being emmitted
  while (not action_finished) do
  begin
    // non blocking read of the next available message
    dbus_connection_read_write(conn, 0);
    dmsg:=dbus_connection_pop_message(conn);

    // loop again if we haven't read a message
    if (dmsg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    //Convert all DBus signals into standard callbacks

    //Change of progress
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Manage', 'ProgressChange') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_UINT32 <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no integer!')
      else
      begin
         dbus_message_iter_get_basic(@args, @intvalue);

         SendProgress(intvalue);
      end;
    end;

    //Receive new message
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Manage', 'Message') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no string!')
      else
      begin
         dbus_message_iter_get_basic(@args, @strvalue);
         procinfo.changed:=pdInfo;
         procinfo.info:=strvalue;
         Synchronize(@SyncProcStatus);
      end;
    end;

    //Break on error signal
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Manage', 'Error') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no string!')
      else
      begin
         dbus_message_iter_get_basic(@args, @strvalue);
         //The action failed. Diplay error and leave the loop
          SendError(strvalue);
          Terminate;
          action_finished:=true;
      end;
    end;

    //Check if the installation has finished
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Manage', 'Finished') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no boolean!')
      else
      begin
         dbus_message_iter_get_basic(@args, @boolvalue);
         action_finished:=boolvalue;
         if (not boolvalue) then
         begin
          //The action failed. Leave the loop and display message
          SendError('The action failed.');
          Terminate;
          action_finished:=true;
         end;
      end;
    end;

    // free the message
    dbus_message_unref(dmsg);
  end;

end;

end.

