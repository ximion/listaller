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
//** Class which can perform various actions on Listaller's DBus interface
unit lidbusproc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, liTypes, SimDBus, DBus, ctypes, liBasic;

type
 //** Which detail of the process was changed?
 TLiProcDetail = (pdMainProgress, pdMessage,
                  pdInfo, pdStatus, pdError,
                  pdStepMessage, pdExtraProgress);

 //** Record which contains data about the current thread
 TLiProcData = record
   changed: TLiProcDetail;
   mnprogress, exprogress: Integer;
   jobID: String;
   msg: String;
 end;

 //** Event for thread signals
 TLiDAction = procedure(ty: LiProcStatus;data: TLiProcData) of Object;

 //** Actions Listaller can perform on DBus root interface
 TListallerBusAction = (lbaUninstallApp,lbaInstallPack);

 //** A command Listaller should do on DBus
 ListallerBusCommand = record
   cmdtype: TListallerBusAction;
   appinfo: TAppInfo;
   pkgname: String;
   addsrc: Boolean;
 end;

 //** Object which can perform various actions on Listaller's DBus interface
 TLiDBusAction = class
 private
  cmd: TListallerBusAction;
  FStatus: TLiDAction;
  cmdinfo: ListallerBusCommand;

  status: LiProcStatus;
  procinfo: TLiProcData;
  done: Boolean;
  procedure SyncProcStatus;
  procedure SendError(msg: String);
  procedure SendProgress(val: Integer;const ty: TLiProcDetail=pdMainProgress);
  procedure SendMessage(msg: String;const ty: TLiProcDetail=pdInfo);
  //** Remove app as root via remote DBus connection
  procedure UninstallAppAsRoot(obj: TAppInfo);
  //** Execute installation as root using dbus daemon & PolicyKit
  function  DoInstallationAsRoot(pkgpath: String;addsrc: Boolean): Boolean;
 public
  constructor Create(action: ListallerBusCommand);
  destructor Destroy;override;

  procedure ExecuteAction;
  property OnStatus: TLiDAction read FStatus write FStatus;
  property Finished: Boolean read done;
 end;

implementation

{ TDBusAction }

constructor TLiDBusAction.Create(action: ListallerBusCommand);
begin
 cmd:=action.cmdtype;
 cmdinfo:=action;
 done:=false;
end;

destructor TLiDBusAction.Destroy;
begin
 status:=prFinished;
 procinfo.changed:=pdStatus;
 SyncProcStatus;
 p_debug('DBus action done.');
 inherited;
end;

procedure TLiDBusAction.SyncProcStatus;
begin
 if Assigned(FStatus) then FStatus(status,procinfo);
 {procinfo.msg:='~?error';
 procinfo.info:='~?error';}
end;

procedure TLiDBusAction.ExecuteAction;
begin
 case cmd of
  lbaUninstallApp: UninstallAppAsRoot(cmdinfo.appinfo);
  lbaInstallPack : DoInstallationAsRoot(cmdinfo.pkgname,cmdinfo.addsrc);
 end;
end;

procedure TLiDBusAction.SendError(msg: String);
begin
 procinfo.msg:=msg;
 procinfo.changed:=pdError;
 SyncProcStatus;
 procinfo.msg:='#';
end;

procedure TLiDBusAction.SendMessage(msg: String;const ty: TLiProcDetail=pdInfo);
begin
 procinfo.msg:=msg;
 procinfo.changed:=ty;
 SyncProcStatus;
 procinfo.msg:='#';
end;

procedure TLiDBusAction.SendProgress(val: Integer;const ty: TLiProcDetail=pdMainProgress);
begin
 procinfo.changed:=ty;
 if ty=pdMainprogress then
  procinfo.mnprogress:=val
 else
  procinfo.exprogress:=val;

 SyncProcStatus;
end;

procedure TLiDBusAction.UninstallAppAsRoot(obj: TAppInfo);
var
  bus: TDBusClient;
  dmsg: PDBusMessage;
  stat: Boolean;
  rec: PChar;
  action_finished: Boolean;

  jobID: String;
begin

  //New DBus connection
  bus:=TDBusClient.Create(DBUS_BUS_SYSTEM);
  if bus.Failed then
  begin
    SendError('An error occured during remove request.'#10'Unable to connect to DBus!');
    exit;
  end;

  p_info('Calling listaller-daemon...');
  // create a new method call and check for errors
  dmsg := bus.Connect('org.freedesktop.Listaller', // target for the method call
                                      '/org/freedesktop/Listaller/Manager', // object to call on
                                      'org.freedesktop.Listaller.Manage', // interface to call on
                                      'RemoveApp'); //Method
  if bus.Failed then
  begin
   SendError('Could not get reply from DBus!');
   exit;
  end;

  bus.ReplyMessageAddString(dmsg,obj.Name);
  bus.ReplyMessageAddString(dmsg,obj.UId);

  p_info('Sending AppRemove request...');
  dmsg:=bus.SendReplyAndWait(dmsg);
  if not bus.Failed then
   p_info('Got reply.');

  stat:=false;
  // read the parameters
  stat:=bus.ReadMessageParamBool(dmsg);
  rec:=PChar(bus.ReadMessageParamStr(dmsg));
  p_debug(rec);

  jobID:=bus.ReadMessageParamStr(dmsg);

  if (not stat)or(rec = 'blocked')or(jobID='') then
  begin
   if rec = 'blocked' then
    SendError('You are not authorized to do this action!')
   else
    SendError('An error occured during install request.');
   exit;
  end;
  // free reply
  bus.FreeMessage(dmsg);

  p_debug('Got job '+jobID);
  //////////////////////////////////////////////////////////////////////

  //Now start listening to Listaller dbus signals and forward them to
  //native functions until the "Finished" signal is received

  bus.StartListening('/org/freedesktop/Listaller/'+jobID);

  p_debug(jobID);

  procinfo.mnprogress:=0;
  action_finished:=false;
  // loop listening for signals being emmitted
  while (not action_finished) do
  begin
    dmsg:=bus.ReadSignalMessage;
    //loop again if we haven't read a message
    if (dmsg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    //Convert all DBus signals into standard callbacks

    //Change of progress
    if bus.ReceivedSignal(dmsg,'ProgressChange') then
     SendProgress(bus.ReadSignalInt(dmsg));

    //Receive new message
    if bus.ReceivedSignal(dmsg,'Message') then
      SendMessage(bus.ReadSignalStr(dmsg));

    //Break on error signal
    if bus.ReceivedSignal(dmsg,'Error') then
    begin
         //The action failed. Diplay error and leave the loop
          SendError(bus.ReadSignalStr(dmsg));
          action_finished:=true;
    end;

    //Check if the installation has finished
    if bus.ReceivedSignal(dmsg, 'Finished') then
    begin
       action_finished:=true;
       if (not bus.ReadSignalBool(dmsg)) then
        //The action failed. Leave the loop and display message
        SendError('The action failed.');
    end;

    // free the message
    bus.FreeMessage(dmsg);
  end;
end;

//This method submits all actions to the DBus daemon
function TLiDBusAction.DoInstallationAsRoot(pkgpath: String;addsrc: Boolean): Boolean;
var
  dmsg: PDBusMessage;
  args: DBusMessageIter;
  pending: PDBusPendingCall;
  stat: Boolean;
  rec: PChar;
  param: PChar;
  err: DBusError;
  conn: PDBusConnection;

  install_finished: Boolean;
  intvalue: cuint;
  strvalue: PChar;
  boolvalue: Boolean;
begin
  Result:=false;
  dbus_error_init(@err);

  //New DBus connection
  conn := dbus_bus_get_private(DBUS_BUS_SYSTEM, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    p_error('Connection Error: '#10+err.message);
    dbus_error_free(@err);
    SendError('An error occured during install request.');
  end;

  if conn = nil then exit;

  p_info('Calling listaller-daemon...');
  // create a new method call and check for errors
  dmsg := dbus_message_new_method_call('org.freedesktop.Listaller', // target for the method call
                                      '/org/freedesktop/Listaller/Installer1', // object to call on
                                      'org.freedesktop.Listaller.Install', // interface to call on
                                      'ExecuteSetup'); // method name
  if (dmsg = nil) then
  begin
    p_error('Message Null');
    exit;
  end;

  // append arguments
  param:=PChar(pkgpath);
  dbus_message_iter_init_append(dmsg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @param) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  //Append true if update source should be registered
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @addsrc) = 0) then
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

  p_info('InstallPkg request sent');

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
   exit;
  end;
  // free reply
  dbus_message_unref(dmsg);

  ////////////////////////////////////////////////////////////////////////////////////

  //Now start listening to Listaller dbus signals and forward them to
  //native functions until the "Finished" signal is received

  // add a rule for which messages we want to see
  dbus_bus_add_match(conn,
  'type=''signal'',sender=''org.freedesktop.Listaller'', interface=''org.freedesktop.Listaller.Install'', path=''/org/freedesktop/Listaller/Installer1'''
  , @err);

  dbus_connection_flush(conn);
  if (dbus_error_is_set(@err) <> 0) then
  begin
    p_error('Match Error ('+err.message+')');
    exit;
  end;
  p_info('Match rule sent');

  install_finished:=false;
  SendProgress(0);
  // loop listening for signals being emmitted
  while (not install_finished) do
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

    //Change of the main progress bar
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Install', 'MainProgressChange') <> 0) then
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

    //Change of the extra progress bar
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Install', 'ExtraProgressChange') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_UINT32 <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no integer!')
      else
      begin
         dbus_message_iter_get_basic(@args, @intvalue);
         SendProgress(intvalue,pdExtraProgress);
      end;
    end;

    //Receive new message
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Install', 'Message') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no string!')
      else
      begin
         dbus_message_iter_get_basic(@args, @strvalue);
         SendMessage(strvalue);
      end;
    end;

    //Receive current state of installation progress
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Install', 'StateMessage') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no string!')
      else
      begin
         dbus_message_iter_get_basic(@args, @strvalue);
         SendMessage(strvalue,pdStepMessage);
      end;
    end;

    //Break on error signal
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Install', 'Error') <> 0) then
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
          install_finished:=true;
      end;
    end;

    //Check if the installation has finished
    if (dbus_message_is_signal(dmsg, 'org.freedesktop.Listaller.Install', 'Finished') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(dmsg, @args) = 0) then
         p_error('Message Has No Parameters')
      else if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
         p_error('Argument is no string!')
      else
      begin
         dbus_message_iter_get_basic(@args, @boolvalue);
         install_finished:=boolvalue;
      end;
    end;


    // free the message
    dbus_message_unref(dmsg);
  end;

 //If we are here, the installation was successful
 Result:=true;

end;

end.

