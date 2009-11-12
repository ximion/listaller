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
  liBasic, liTypes;

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
  FileName: String;
  procedure SendReply;override;
  procedure InstallMainChange(pos: LongInt);cdecl;
  procedure InstallExtraChange(pos: LongInt);cdecl;
  function  InstallUserRequest(mtype: TRqType;info: PChar): TRqResult;cdecl;
  procedure InstallMessage(info: String;imp: TMType);cdecl;
  procedure InstallStepMessage(info: String;imp: TMType);cdecl;
 public
  constructor Create(aMsg: PDBusMessage;aConn: PDBusConnection);
  destructor  Destroy;override;

  procedure Execute;override;
 end;

const
 CALL_RUNSETUP = 'ExecuteSetup';
var
 InstallWorkers: Integer;

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
var args: DBusMessageIter;param: PChar = '';obj: Pointer;
begin
 inherited Create(aMsg,aConn);
 ident:='DoAppInstall~'+dbus_message_get_sender(msg);

 // read the arguments
   if (dbus_message_iter_init(msg, @args) = 0) then
      dbus_message_iter_get_basic(@args, @obj);
   FileName:=param;

 //Start work
 Resume();
end;

destructor TDoAppInstall.Destroy;
begin
 Dec(InstallWorkers);
 inherited;
end;

procedure TDoAppInstall.SendReply;
var
  reply: PDBusMessage;
  args: DBusMessageIter;
  stat: Boolean = true;
  auth: PChar = '';
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
end;

procedure TDoAppInstall.InstallMainChange(pos: LongInt);cdecl;
begin
 //IWizFrm.InsProgress.Position:=pos;
end;

procedure TDoAppInstall.InstallExtraChange(pos: LongInt);cdecl;
begin
// ExProgress.Position:=pos;
end;

function TDoAppInstall.InstallUserRequest(mtype: TRqType;info: PChar): TRqResult;cdecl;
begin
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
 //writeLn(info);
end;

procedure TDoAppInstall.InstallStepMessage(info: String;imp: TMType);cdecl;
begin
// IWizFrm.Label9.Caption:=info;
end;

procedure TDoAppInstall.Execute;
var
 action_id: PGChar;
 subject: PPolkitSubject;
 target: String;
 setup: TInstallPack;
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

 p_info('New app install job for '+dbus_message_get_sender(msg)+' started.');

 //We got authorization!
 SendReply;

 { This is a fast install. Should never be called directly! }
 setup:=TInstallPack.Create;
 setup.Initialize(FileName);
 setup.SetMainChangeCall(TProgressCall(TMethod(@InstallMainChange).Code));
 setup.SetExtraChangeCall(TProgressCall(TMethod(@InstallExtraChange).Code));
 setup.SetMessageCall(TMessageCall(TMethod(@InstallMessage).Code));
 setup.SetStepMessageCall(TMessageCall(TMethod(@InstallStepMessage).Code));
 setup.SetUserRequestCall(TRequestCall(TMethod(@InstallUserRequest).Code));
 setup.StartInstallation;
 setup.Free;

 p_info('AppInstall job '+dbus_message_get_sender(msg)+ ' completed.');

 Terminate;
end;

end.

