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
  liBasic;

type
 TAccessType = (AC_UNKNOWN,AC_AUTHORIZED,AC_NOT_AUTHORIZED);

 //** Base class for all jobs
 TJob = class(TThread)
 protected
  allowed: TAccessType;
  ident:  String;
  loop: PGMainLoop;

  authority: PPolkitAuthority;
  msg: PDBusMessage;
  conn: PDBusConnection;
 private
  procedure SendReply;virtual;abstract;
 public
  constructor Create(aMsg: PDBusMessage;aConn: PDBusConnection);
  destructor  Destroy;override;

  property Authorization: TAccessType read allowed write allowed;
  property Identifier: String read ident write ident;
  property MainLoop: PGMainLoop read loop write loop;
 end;

 //** Job which can install software packages
 TDoAppInstall = class(TJob)
 private
  procedure SendReply;override;
 public
  constructor Create(aMsg: PDBusMessage;aConn: PDBusConnection);
  destructor  Destroy;override;

  procedure Execute;override;
 end;

const
 CALL_RUNSETUP = 'ExecuteSetup';

implementation

{ TJob }

constructor TJob.Create(aMsg: PDBusMessage;aConn: PDBusConnection);
begin
 inherited Create(true);

 msg:=aMsg;
 conn:=aConn;

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

constructor TDoAppInstall.Create(aMsg: PDBusMessage;aConn: PDBusConnection);
begin
 inherited Create(aMsg,aConn);
 ident:='DoAppInstall~'+dbus_message_get_sender(msg);
 //Start work
 Resume();
end;

destructor TDoAppInstall.Destroy;
begin
 inherited;
end;

procedure TDoAppInstall.SendReply;
var
  reply: PDBusMessage;
  args: DBusMessageIter;
  stat: Boolean = true;
  level: dbus_uint32_t = 21614;
  serial: dbus_uint32_t = 0;
begin
 p_info('Send reply called');

// create a reply from the message
   reply := dbus_message_new_method_return(msg);

   // add the arguments to the reply
   dbus_message_iter_init_append(reply, @args);
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @stat) = 0) then
   begin
     p_error('Out Of Memory!');
     exit;
   end;
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_UINT32, @level) = 0) then
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
end;

procedure TDoAppInstall.Execute;
var
 action_id: PGChar;
 subject: PPolkitSubject;
 target: String;
begin
  action_id:='org.freedesktop.listaller.execute-installation';
  target:=dbus_message_get_sender(msg);

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

  SendReply;
 p_info('New app install job for '+dbus_message_get_sender(msg)+' started.');

 { Do install stuff here }

 p_info('AppInstall job '+dbus_message_get_sender(msg)+ ' completed.');


 Terminate;
end;

end.

