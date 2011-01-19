(* Copyright (C) 2008-2011 Matthias Klumpp
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
//** Class which can perform various actions on Listaller's DBus interface
//** !This unit is deprecated!
unit lidbusproc;

{$mode objfpc}{$H+}

interface

uses
  Classes, DBus, liUtils, liTypes, SimDBus, SysUtils, StrLocale, LiApp;

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
  TLiDAction = procedure(ty: LI_STATUS; Data: TLiProcData) of object;

  //** Actions Listaller can perform on DBus root interface
  TListallerBusAction = (lbaUninstallApp, lbaInstallPack, lbaUpdateApp);

  //** A command Listaller should do on DBus
  ListallerBusCommand = record
    cmdtype: TListallerBusAction;
    appinfo: TLiAppItem;
    pkgname: String;
    overrides: String;
    updid: Integer;
    addsrc: Boolean;
  end;

  //** Object which can perform various actions on Listaller's DBus interface (currently NO Thread anymore)
  TLiDBusAction = class
  private
    cmd: TListallerBusAction;
    FStatus: TLiDAction;
    cmdinfo: ListallerBusCommand;

    status: LI_STATUS;
    procinfo: TLiProcData;
    done: Boolean;
    procedure SyncProcStatus;
    procedure SendError(msg: String);
    procedure SendProgress(val: Integer; const ty: TLiProcDetail = pdMainProgress);
    procedure SendMessage(msg: String; const ty: TLiProcDetail = pdInfo);
    //** Remove app as root via remote DBus connection
    procedure UninstallAppAsRoot(obj: TLiAppItem);
    //** Execute installation as root using dbus daemon & PolicyKit
    function DoInstallationAsRoot(pkgpath: String; overrides: String;
      addsrc: Boolean): Boolean;
    //** Update shared application via dbus daemon & PolicyKit
    function DoUpdateAsRoot(uid: Integer): Boolean;
  public
    constructor Create(action: ListallerBusCommand);
    destructor Destroy; override;

    procedure ExecuteAction;
    property OnStatus: TLiDAction read FStatus write FStatus;
    property Finished: Boolean read done;
  end;

implementation

{ TDBusAction }

constructor TLiDBusAction.Create(action: ListallerBusCommand);
begin
  //FreeOnTerminate:=true;
  cmd := action.cmdtype;
  cmdinfo := action;
  done := false;
end;

destructor TLiDBusAction.Destroy;
begin
 {if Assigned(FatalException) then
  raise FatalException; }

  status := LIS_Finished;
  procinfo.changed := pdStatus;
  SyncProcStatus;
  pdebug('DBus action done.');
  inherited;
end;

procedure TLiDBusAction.SyncProcStatus;
begin
  if Assigned(FStatus) then
    FStatus(status, procinfo);
  procinfo.msg := '~?error';
end;

procedure TLiDBusAction.ExecuteAction;
begin
  done := false;
  //if Terminated then exit;
  case cmd of
    lbaUninstallApp: UninstallAppAsRoot(cmdinfo.appinfo);
    lbaInstallPack: DoInstallationAsRoot(cmdinfo.pkgname, cmdinfo.overrides,
        cmdinfo.addsrc);
    lbaUpdateApp: DoUpdateAsRoot(cmdinfo.updid);
  end;
  done := true;
  pdebug('Job done.');
end;

procedure TLiDBusAction.SendError(msg: String);
begin
  procinfo.msg := msg;
  procinfo.changed := pdError;
  SyncProcStatus;
  procinfo.msg := '#';
end;

procedure TLiDBusAction.SendMessage(msg: String; const ty: TLiProcDetail = pdInfo);
begin
  procinfo.msg := msg;
  procinfo.changed := ty;
  SyncProcStatus;
  procinfo.msg := '#';
end;

procedure TLiDBusAction.SendProgress(val: Integer;
  const ty: TLiProcDetail = pdMainProgress);
begin
  procinfo.changed := ty;
  if ty = pdMainprogress then
    procinfo.mnprogress := val
  else
    procinfo.exprogress := val;

  SyncProcStatus;
end;

procedure TLiDBusAction.UninstallAppAsRoot(obj: TLiAppItem);
var
  bus: TDBusClient;
  dmsg: PDBusMessage;
  stat: Boolean;
  rec: PChar;
  action_finished: Boolean;

  jobID: String;
begin

  //New DBus connection
  bus := TDBusClient.Create(DBUS_BUS_SYSTEM);
  if bus.Failed then
  begin
    SendError('An error occured during remove request.'#10'Unable to connect to DBus!');
    exit;
  end;

  pinfo('Calling listaller-daemon...');
  // create a new method call and check for errors
  dmsg := bus.Connect('org.nlinux.Listaller', // target for the method call
    '/org/nlinux/Listaller/Manager',
    // object to call on
    'org.nlinux.Listaller.Manage',
    // interface to call on
    'RemoveApp'); //Method
  if bus.Failed then
  begin
    SendError('Could not get reply from DBus!');
    exit;
  end;

  bus.ReplyMessageAddString(dmsg, obj.AName);
  bus.ReplyMessageAddString(dmsg, obj.AId);

  pinfo('Sending AppRemove request...');
  dmsg := bus.SendReplyAndWait(dmsg);
  if not bus.Failed then
    pinfo('Got reply.');

  stat := false;
  // read the parameters
  stat := bus.ReadMessageParamBool(dmsg);
  bus.MessageIterNext;
  rec := PChar(bus.ReadMessageParamStr(dmsg));

  bus.MessageIterNext;
  jobID := bus.ReadMessageParamStr(dmsg);

  if (not stat) or (rec = 'blocked') or (jobID = '') then
  begin
    if rec = 'blocked' then
      SendError('You are not authorized to do this action!')
    else
      SendError('An error occured during install request.');
    exit;
  end;
  // free reply
  bus.FreeMessage(dmsg);

  pdebug('Got job ' + jobID);
  //////////////////////////////////////////////////////////////////////

  //Now start listening to Listaller dbus signals and forward them to
  //native functions until the "Finished" signal is received

  procinfo.mnprogress := 0;
  action_finished := false;
  //Add a rule for which messages we want to see
  bus.StartListening('/org/nlinux/Listaller/' + jobID);
  // loop listening for signals being emmitted
  while (not action_finished) do
  begin
    dmsg := bus.ReadSignalMessage;
    //loop again if we haven't read a message
    if (dmsg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    //Convert all DBus signals into standard callbacks

    //Check if the installation has finished
    if bus.ReceivedSignalIs(dmsg, 'Finished') then
    begin
      action_finished := true;
      if (not bus.ReadSignalBool(dmsg)) then
        //The action failed. Leave the loop and display message
        SendError(rsRMerror);
    end;

    //Change of progress
    if bus.ReceivedSignalIs(dmsg, 'ProgressChange') then
      SendProgress(bus.ReadSignalInt(dmsg));

    //Receive new message
    if bus.ReceivedSignalIs(dmsg, 'Message') then
      SendMessage(bus.ReadSignalStr(dmsg));

    //Break on error signal
    if bus.ReceivedSignalIs(dmsg, 'Error') then
    begin
      //The action failed. Diplay error and leave the loop
      SendError(bus.ReadSignalStr(dmsg));
      action_finished := true;
    end;

    // free the message
    bus.FreeMessage(dmsg);
  end;
end;

//This method submits all actions to the DBus daemon
function TLiDBusAction.DoInstallationAsRoot(pkgpath: String;
  overrides: String; addsrc: Boolean): Boolean;
var
  dmsg: PDBusMessage;
  bus: TDBusClient;
  stat: Boolean;
  rec: String;
  jobID: String;

  install_finished: Boolean;
begin
  Result := false;
  bus := TDBusClient.Create(DBUS_BUS_SYSTEM);

  if bus.Failed then
    exit;

  pinfo('Calling listaller-daemon...');
  dmsg := bus.Connect('org.nlinux.Listaller', '/org/nlinux/Listaller/Installer',
    'org.nlinux.Listaller.Install', 'ExecuteSetup');
  if (dmsg = nil) then
  begin
    perror('Message Null');
    exit;
  end;

  //Append arguments

  //Add path to package
  bus.ReplyMessageAddString(dmsg, pkgpath);
  //Add overrides
  bus.ReplyMessageAddString(dmsg, overrides);
  //Append true if update source should be registered
  bus.ReplyMessageAddBool(dmsg, addsrc);

  dmsg := bus.SendReplyAndWait(dmsg);

  pinfo('InstallPkg request sent');

  stat := bus.ReadMessageParamBool(dmsg);
  bus.MessageIterNext;
  rec := bus.ReadMessageParamStr(dmsg);
  bus.MessageIterNext;
  jobID := bus.ReadMessageParamStr(dmsg);

  if (stat = false) or (jobID = '') then
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
  bus.StartListening('/org/nlinux/Listaller/' + jobID);

  install_finished := false;
  SendProgress(0);
  // loop listening for signals being emmitted
  while (not install_finished) do
  begin
    dmsg := bus.ReadSignalMessage;
    //loop again if we haven't read a message
    if (dmsg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    //Convert all DBus signals into standard Listaller callbacks

    //Change of the main progress bar
    if bus.ReceivedSignalIs(dmsg, 'MainProgressChange') then
      SendProgress(bus.ReadSignalInt(dmsg));

    //Change of the extra progress bar
    if bus.ReceivedSignalIs(dmsg, 'ExtraProgressChange') then
      SendProgress(bus.ReadSignalInt(dmsg), pdExtraProgress);

    //Receive new message
    if bus.ReceivedSignalIs(dmsg, 'Message') then
      SendMessage(bus.ReadSignalStr(dmsg));

    //Receive current state of installation progress
    if bus.ReceivedSignalIs(dmsg, 'StepMessage') then
      SendMessage(bus.ReadSignalStr(dmsg), pdStepMessage);

    //Break on error signal
    if bus.ReceivedSignalIs(dmsg, 'Error') then
    begin
      //The action failed. Diplay error and leave the loop
      SendError(bus.ReadSignalStr(dmsg));
      install_finished := true;
    end;

    //Check if the installation has finished
    if bus.ReceivedSignalIs(dmsg, 'Finished') then
    begin
      install_finished := true;
      if (not bus.ReadSignalBool(dmsg)) then
        //The action failed. Leave the loop and display message
        SendError(rsInstFailed);
    end;

    //Free the message
    bus.FreeMessage(dmsg);
  end;

  //If we are here, the installation was successful
  Result := true;
end;

//Execute an update over DBus
function TLiDBusAction.DoUpdateAsRoot(uid: Integer): Boolean;
var
  bus: TDBusClient;
  dmsg: PDBusMessage;
  stat: Boolean;
  rec: PChar;
  action_finished: Boolean;

  jobID: String;
begin
  Result := true;
  //New DBus connection
  bus := TDBusClient.Create(DBUS_BUS_SYSTEM);
  if bus.Failed then
  begin
    SendError('An error occured during update request.'#10'Unable to connect to DBus!');
    exit;
  end;

  pinfo('Calling listaller-daemon...');
  // create a new method call and check for errors
  dmsg := bus.Connect('org.nlinux.Listaller', // target for the method call
    '/org/nlinux/Listaller/Manager',
    // object to call on
    'org.nlinux.Listaller.Manage',
    // interface to call on
    'UpdateApp'); //Method
  if bus.Failed then
  begin
    SendError('Could not get reply from DBus!');
    exit;
  end;

  bus.ReplyMessageAddInt(dmsg, uid);

  pinfo('Sending AppUpdate request...');
  dmsg := bus.SendReplyAndWait(dmsg);
  if not bus.Failed then
    pinfo('Got reply.');

  stat := false;
  // read the parameters
  stat := bus.ReadMessageParamBool(dmsg);
  bus.MessageIterNext;
  rec := PChar(bus.ReadMessageParamStr(dmsg));

  bus.MessageIterNext;
  jobID := bus.ReadMessageParamStr(dmsg);

  if (not stat) or (rec = 'blocked') or (jobID = '') then
  begin
    if rec = 'blocked' then
      SendError('You are not authorized to execute an update of this application!')
    else
      SendError('An error occured during update request.');
    exit;
  end;
  // free reply
  bus.FreeMessage(dmsg);

  pdebug('Got job ' + jobID);
  //////////////////////////////////////////////////////////////////////

  //Now start listening to Listaller dbus signals and forward them to
  //native functions until the "Finished" signal is received

  procinfo.mnprogress := 0;
  action_finished := false;
  //Add a rule for which messages we want to see
  bus.StartListening('/org/nlinux/Listaller/' + jobID);
  // loop listening for signals being emmitted
  while (not action_finished) do
  begin
    dmsg := bus.ReadSignalMessage;
    //loop again if we haven't read a message
    if (dmsg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    //Convert all DBus signals into standard callbacks

    //Check if the installation has finished
    if bus.ReceivedSignalIs(dmsg, 'Finished') then
    begin
      action_finished := true;
      if (not bus.ReadSignalBool(dmsg)) then
        //The action failed. Leave the loop and display message
        SendError(rsRMerror);
    end;

    //Change of progress
    if bus.ReceivedSignalIs(dmsg, 'ProgressChange') then
      SendProgress(bus.ReadSignalInt(dmsg));

    //Receive new message
    if bus.ReceivedSignalIs(dmsg, 'Message') then
      SendMessage(bus.ReadSignalStr(dmsg));

    //Break on error signal
    if bus.ReceivedSignalIs(dmsg, 'Error') then
    begin
      //The action failed. Diplay error and leave the loop
      SendError(bus.ReadSignalStr(dmsg));
      Result := false;
      action_finished := true;
    end;

    // free the message
    bus.FreeMessage(dmsg);
  end;
end;

end.

