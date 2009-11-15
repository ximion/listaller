{ Copyright (C) 2008-2009 Matthias Klumpp

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
//** Processes which listen on the DBus for new calls
unit djobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbus, polkit, glib2, gExt, Installer, AppMan,
  liBasic, liTypes, CTypes;

type
 TAccessType = (AC_UNKNOWN,AC_AUTHORIZED,AC_NOT_AUTHORIZED);

 //** Base class for all jobs
 TJob = class(TThread)
 protected
  allowed: TAccessType;
  ident:  String;
  loop: PGMainLoop;

  authority: PPolkitAuthority;
  replyMsg: PDBusMessage;
 private
  procedure SendReply(stat: Boolean);virtual;abstract;
 public
  constructor Create(aMsg: PDBusMessage);
  destructor  Destroy;override;

  property Authorization: TAccessType read allowed write allowed;
  property Identifier: String read ident write ident;
  property MainLoop: PGMainLoop read loop write loop;
 end;

 //** Job which can install software packages
 TDoAppInstall = class(TJob)
 private
  FileName: String;
  //** Send reply to client
  procedure SendReply(stat: Boolean);override;
  //** Emit a progress changed signal
  procedure SendProgressSignal(xn: String;sigvalue: cuint);
  //** Emit a message signal with the current message
  procedure SendMessageSignal(xn: String;sigvalue: PChar);

  procedure InstallMainChange(pos: LongInt);cdecl;
  procedure InstallExtraChange(pos: LongInt);cdecl;
  function  InstallUserRequest(mtype: TRqType;info: PChar): TRqResult;cdecl;
  procedure InstallMessage(info: String;imp: TMType);cdecl;
  procedure InstallStepMessage(info: String;imp: TMType);cdecl;
 public
  constructor Create(aMsg: PDBusMessage);
  destructor  Destroy;override;

  procedure Execute;override;
 end;

 //** Job which removes applications
 TDoAppRemove = class(TJob)
 private
  FileName: String;
  //** Send reply to client
  procedure SendReply(stat: Boolean);override;
 public
  constructor Create(aMsg: PDBusMessage);
  destructor  Destroy;override;

  procedure Execute;override;
 end;

const
 CALL_RUNSETUP  = 'ExecuteSetup';
 CALL_APPREMOVE = 'RemoveApp';
var
 InstallWorkers: Integer;
 conn: PDBusConnection;

implementation

{ TJob }

constructor TJob.Create(aMsg: PDBusMessage);
begin
 inherited Create(true);

 replyMsg:=aMsg;

 authority:=polkit_authority_get();
 loop:=g_main_loop_new (nil, false);
 allowed:=AC_UNKNOWN;
 FreeOnTerminate:=true;
end;

destructor TJob.Destroy;
begin
 g_object_unref(authority);
 g_main_loop_unref(loop);
 inherited;
end;

procedure check_authorization_cb(authority: PPolkitAuthority;res: PGAsyncResult;job: TJob);
var
 error: PGError;
 result: PPolkitAuthorizationResult;
 result_str: PGChar;
begin
  error := nil;
  result_str:='';

  result := polkit_authority_check_authorization_finish(authority, res, @error);
  if(error <> nil)then
  begin
      p_error('Error checking authorization: '#10+error^.message);
      g_error_free(error);
      job.Authorization:=AC_NOT_AUTHORIZED;
      g_main_loop_quit(job.MainLoop);
      exit;
  end else
  begin
      if (polkit_authorization_result_get_is_authorized(result)) then
          result_str := 'authorized'
      else if (polkit_authorization_result_get_is_challenge(result)) then
          result_str := 'challenge'
      else
          result_str := 'not authorized';

      p_info('Authorization result for '+job.Identifier+': '+result_str);
  end;
  if result_str = 'authorized' then
   job.Authorization:=AC_AUTHORIZED
  else
   job.Authorization:=AC_NOT_AUTHORIZED;
  g_main_loop_quit(job.MainLoop);
end;

{ TDoAppInstall }

constructor TDoAppInstall.Create(aMsg: PDBusMessage);
var args: DBusMessageIter;param: PChar = '';
begin
 inherited Create(aMsg);
 ident:='DoAppInstall~'+dbus_message_get_sender(aMsg);

 // read the arguments
   if (dbus_message_iter_init(aMsg, @args) = 0) then
      p_error('Message has no arguments!')
   else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
      p_error('Argument is no string!')
   else
      dbus_message_iter_get_basic(@args, @param);

   FileName:=param;

   p_info('RunSetup called with '+FileName);

 //Start work
  Resume();
end;

destructor TDoAppInstall.Destroy;
begin
 Dec(InstallWorkers);
 inherited;
end;

procedure TDoAppInstall.SendProgressSignal(xn: String;sigvalue: cuint);
var
  args: DBusMessageIter;
  msg: PDBusMessage;
begin

  // create a signal & check for errors
  msg := dbus_message_new_signal('/org/freedesktop/Listaller/Installer1', // object name of the signal
                                 'org.freedesktop.Listaller.Install', // interface name of the signal
                                 PChar(xn)); // name of the signal

  if (msg = nil) then
  begin
    p_error('Message Null');
    exit;
  end;

  // append arguments onto signal
  dbus_message_iter_init_append(msg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_UINT32, @sigvalue) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  // send the message and flush the connection
  if (dbus_connection_send(conn, msg, nil) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  dbus_connection_flush(conn);

  // free the message and close the connection
  dbus_message_unref(msg);
end;

procedure TDoAppInstall.SendMessageSignal(xn: String;sigvalue: PChar);
var
  args: DBusMessageIter;
  msg: PDBusMessage;
begin

  // create a signal & check for errors
  msg := dbus_message_new_signal('/org/freedesktop/Listaller/Installer1', // object name of the signal
                                 'org.freedesktop.Listaller.Install', // interface name of the signal
                                 PChar(xn)); // name of the signal

  if (msg = nil) then
  begin
    p_error('Message Null');
    exit;
  end;

  // append arguments onto signal
  dbus_message_iter_init_append(msg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @sigvalue) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  // send the message and flush the connection
  if (dbus_connection_send(conn, msg, nil) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  dbus_connection_flush(conn);

  // free the message and close the connection
  dbus_message_unref(msg);
end;

procedure TDoAppInstall.SendReply(stat: Boolean);
var
  reply: PDBusMessage;
  args: DBusMessageIter;
  auth: PChar = '';
  serial: dbus_uint32_t = 0;
begin
 p_info('Send reply called');

  // create a reply from the message
   reply := dbus_message_new_method_return(replyMsg);

   // add the arguments to the reply
   dbus_message_iter_init_append(reply, @args);
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @stat) = 0) then
   begin
     p_error('Out Of Memory!');
     exit;
   end;
   auth:='';
   if allowed = AC_AUTHORIZED then auth:='authorized'
   else if allowed = AC_NOT_AUTHORIZED then auth:='blocked';
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @auth) = 0) then
   begin
     p_error('Out Of Memory!');
     exit;
   end;

   // send the reply & flush the connection
   if (dbus_connection_send(conn, reply, @serial) = 0) then
   begin
     p_error('Out Of Memory!');
     Exit;
   end;
   dbus_connection_flush(conn);

   // free the reply
   dbus_message_unref(reply);

   // free the message
   dbus_message_unref(replyMsg);
end;

procedure TDoAppInstall.InstallMainChange(pos: LongInt);cdecl;
begin
 SendProgressSignal('MainProgressChange',pos);
end;

procedure TDoAppInstall.InstallExtraChange(pos: LongInt);cdecl;
begin
 SendProgressSignal('ExtraProgressChange',pos);
end;

function TDoAppInstall.InstallUserRequest(mtype: TRqType;info: PChar): TRqResult;cdecl;
begin
writeLn(info);
{with IWizFrm do
begin
case mtype of
rqError: begin
  ShowMessage(msg);
  Result:=rqsOK;
  if Assigned(InfoMemo) then
  begin
   InfoMemo.Lines.Add(rsInstFailed);
   InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.GetAppName+'.log');
  end;
  setup.Free;
  Application.Terminate;
  exit;
end;
rqWarning: begin
  if Application.MessageBox(PAnsiChar(msg),PAnsiChar(Format(rsInstOf,[setup.GetAppName])),MB_YESNO)<>IDYES then
  begin
   ShowMessage(rsINClose);
   Result:=rqsNo;
   InfoMemo.Lines.Add('Installation aborted by user.');
   InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.GetAppName+'.log');
   setup.Free;
   Application.Terminate;
   FreeAndNil(IWizFrm);
  end;
end;
rqQuestion: begin
  if Application.MessageBox(PAnsiChar(msg),PAnsiChar(Format(rsInstOf,[setup.GetAppName])),MB_YESNO)<>IDYES then
   Result:=rqsNo else Result:=rqsYes;
end;
rqInfo: begin
  ShowMessage(msg);
  Result:=rqsOK;
end;
 end;
end; }

end;

procedure TDoAppInstall.InstallMessage(info: String;imp: TMType);cdecl;
begin
 SendMessageSignal('Message',PChar(info));
end;

procedure TDoAppInstall.InstallStepMessage(info: String;imp: TMType);cdecl;
begin
 SendMessageSignal('StepMessage',PChar(info));
end;

procedure TDoAppInstall.Execute;
var
 action_id: PGChar;
 subject: PPolkitSubject;
 target: String;
 setup: TInstallPack;

 args: DBusMessageIter;
 msg: PDBusMessage;
 btrue: Boolean = true;
begin
  action_id:='org.freedesktop.listaller.execute-installation';
  target:=dbus_message_get_sender(replyMsg);

  subject:=polkit_system_bus_name_new(PGChar(target));
  polkit_authority_check_authorization(authority,subject,action_id,
                                        nil,
                                        POLKIT_CHECK_AUTHORIZATION_FLAGS_ALLOW_USER_INTERACTION,
                                        nil,
                                        TGAsyncReadyCallback(@check_authorization_cb),
                                        self);



  g_object_unref(subject);
  g_main_loop_run(loop);

  //If not authorized, quit the job
  if allowed = AC_NOT_AUTHORIZED then
  begin
   p_error('Not authorized to call this action.');
   Terminate;
   exit;
  end;

 p_info('New app install job for '+dbus_message_get_sender(replyMsg)+' started.');

 if not FileExists(FileName) then
 begin
  p_error('Installation package not found!');
  p_debug(FileName);
  SendReply(false);
  Terminate;
  exit;
 end;

 //We got authorization!
 SendReply(true);

 { This is a fast install. Should never be called directly! }
 setup:=TInstallPack.Create;

 setup.SetMainChangeCall(TProgressCall(TMethod(@InstallMainChange).Code));
 setup.SetExtraChangeCall(TProgressCall(TMethod(@InstallExtraChange).Code));
 setup.SetMessageCall(TMessageCall(TMethod(@InstallMessage).Code));
 setup.SetStepMessageCall(TMessageCall(TMethod(@InstallStepMessage).Code));
 setup.SetUserRequestCall(TRequestCall(TMethod(@InstallUserRequest).Code));

 setup.Initialize(FileName);
 setup.StartInstallation;

 setup.Free;

 p_info('AppInstall job '+'???'+ ' completed.');

 //Now emit action finished signal:

  // create a signal & check for errors
  msg := dbus_message_new_signal('/org/freedesktop/Listaller/Installer1', // object name of the signal
                                 'org.freedesktop.Listaller.Install', // interface name of the signal
                                 'Finished'); // name of the signal

  if (msg = nil) then
  begin
    p_error('Message Null');
    exit;
  end;

  // append arguments onto signal
  dbus_message_iter_init_append(msg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @btrue) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  // send the message and flush the connection
  if (dbus_connection_send(conn, msg, nil) = 0) then
  begin
    p_error('Out Of Memory!');
    exit;
  end;

  dbus_connection_flush(conn);

  // free the message and close the connection
  dbus_message_unref(msg);

 Terminate;
end;

{ TDoAppRemove }

constructor TDoAppRemove.Create(aMsg: PDBusMessage);
var args: DBusMessageIter;param: PChar = '';
begin
 inherited Create(aMsg);
 ident:='DoAppRemove~'+dbus_message_get_sender(aMsg);

 // read the arguments
   if (dbus_message_iter_init(aMsg, @args) = 0) then
      p_error('Message has no arguments!')
   else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
      p_error('Argument is no string!')
   else
      dbus_message_iter_get_basic(@args, @param);

   FileName:=param;

   p_info('Remove application called with '+FileName);

 //Start work
  Resume();
end;

destructor TDoAppRemove.Destroy;
begin
 inherited;
end;

procedure TDoAppRemove.SendReply(stat: Boolean);
var
  reply: PDBusMessage;
  args: DBusMessageIter;
  auth: PChar = '';
  serial: dbus_uint32_t = 0;
begin
 p_info('Send reply called');

  // create a reply from the message
   reply := dbus_message_new_method_return(replyMsg);

   // add the arguments to the reply
   dbus_message_iter_init_append(reply, @args);
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @stat) = 0) then
   begin
     p_error('Out Of Memory!');
     exit;
   end;
   auth:='';
   if allowed = AC_AUTHORIZED then auth:='authorized'
   else if allowed = AC_NOT_AUTHORIZED then auth:='blocked';
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @auth) = 0) then
   begin
     p_error('Out Of Memory!');
     exit;
   end;

   // send the reply & flush the connection
   if (dbus_connection_send(conn, reply, @serial) = 0) then
   begin
     p_error('Out Of Memory!');
     Exit;
   end;
   dbus_connection_flush(conn);

   // free the reply
   dbus_message_unref(reply);

   // free the message
   dbus_message_unref(replyMsg);
end;

procedure TDoAppRemove.Execute;
var
 action_id: PGChar;
 subject: PPolkitSubject;
 target: String;
begin
  action_id:='org.freedesktop.listaller.remove-application';
  target:=dbus_message_get_sender(replyMsg);

  subject:=polkit_system_bus_name_new(PGChar(target));
  polkit_authority_check_authorization(authority,subject,action_id,
                                        nil,
                                        POLKIT_CHECK_AUTHORIZATION_FLAGS_ALLOW_USER_INTERACTION,
                                        nil,
                                        TGAsyncReadyCallback(@check_authorization_cb),
                                        self);



  g_object_unref(subject);
  g_main_loop_run(loop);

  //If not authorized, quit the job
  if allowed = AC_NOT_AUTHORIZED then
  begin
   p_error('Not authorized to call this action.');
   Terminate;
   exit;
  end;

 p_info('New app uninstallation job for '+dbus_message_get_sender(replyMsg)+' started.');

end;

end.

