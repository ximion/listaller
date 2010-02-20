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
  Classes, SysUtils, liTypes, SimDBus, DBus, liBasic;

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
  bus.MessageIterNext;
  rec:=PChar(bus.ReadMessageParamStr(dmsg));

  bus.MessageIterNext;
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
    if bus.ReceivedSignalIs(dmsg,'ProgressChange') then
     SendProgress(bus.ReadSignalInt(dmsg));

    //Receive new message
    if bus.ReceivedSignalIs(dmsg,'Message') then
      SendMessage(bus.ReadSignalStr(dmsg));

    //Break on error signal
    if bus.ReceivedSignalIs(dmsg,'Error') then
    begin
         //The action failed. Diplay error and leave the loop
          SendError(bus.ReadSignalStr(dmsg));
          action_finished:=true;
    end;

    //Check if the installation has finished
    if bus.ReceivedSignalIs(dmsg, 'Finished') then
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
  bus: TDBusClient;
  stat: Boolean;
  rec: String;
  jobID: String;

  install_finished: Boolean;
begin
  Result:=false;
  bus:=TDBusClient.Create(DBUS_BUS_SYSTEM);

  if bus.Failed then exit;

  p_info('Calling listaller-daemon...');
  dmsg := bus.Connect('org.freedesktop.Listaller','/org/freedesktop/Listaller/Installer',
                                      'org.freedesktop.Listaller.Install',
                                      'ExecuteSetup');
  if (dmsg = nil) then
  begin
    p_error('Message Null');
    exit;
  end;

  //Append arguments

  bus.ReplyMessageAddString(dmsg,pkgpath);
  //Append true if update source should be registered
  bus.ReplyMessageAddBool(dmsg,addsrc);

  dmsg:=bus.SendReplyAndWait(dmsg);

  p_info('InstallPkg request sent');

  stat:=bus.ReadMessageParamBool(dmsg);
  bus.MessageIterNext;
  rec:=bus.ReadMessageParamStr(dmsg);
  bus.MessageIterNext;
  jobID:=bus.ReadMessageParamStr(dmsg);

  if (stat = false)or(jobID='') then
  begin
   if rec = 'blocked' then
    SendError('You are not authorized to do this action!')
   else
    SendError('An error occured during install request.');
   exit;
  end;
  // free reply
  bus.FreeMessage(dmsg);

  ////////////////////////////////////////////////////////////////////////////////////

  //Now start listening to Listaller dbus signals and forward them to
  //native functions until the "Finished" signal is received

  //Add a rule for which messages we want to see
  bus.StartListening('/org/freedesktop/Listaller/'+jobID);

  install_finished:=false;
  SendProgress(0);
  // loop listening for signals being emmitted
  while (not install_finished) do
  begin
    dmsg:=bus.ReadSignalMessage;
    //loop again if we haven't read a message
    if (dmsg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    //Convert all DBus signals into standard Listaller callbacks

    //Change of the main progress bar
    if bus.ReceivedSignalIs(dmsg,'MainProgressChange') then
         SendProgress(bus.ReadSignalInt(dmsg));

    //Change of the extra progress bar
    if bus.ReceivedSignalIs(dmsg,'ExtraProgressChange') then
         SendProgress(bus.ReadSignalInt(dmsg),pdExtraProgress);

    //Receive new message
    if bus.ReceivedSignalIs(dmsg,'Message') then
         SendMessage(bus.ReadSignalStr(dmsg));

    //Receive current state of installation progress
    if bus.ReceivedSignalIs(dmsg,'StateMessage') then
         SendMessage(bus.ReadSignalStr(dmsg),pdStepMessage);

    //Break on error signal
    if bus.ReceivedSignalIs(dmsg,'Error') then
    begin
         //The action failed. Diplay error and leave the loop
          SendError(bus.ReadSignalStr(dmsg));
          install_finished:=true;
    end;

    //Check if the installation has finished
    if bus.ReceivedSignalIs(dmsg,'Finished') then
    begin
         install_finished:=bus.ReadSignalBool(dmsg);
    end;

    //Free the message
    bus.FreeMessage(dmsg);
  end;

 //If we are here, the installation was successful
 Result:=true;
end;

end.

