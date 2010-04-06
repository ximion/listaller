{ Copyright (C) 2009-2010 Matthias Klumpp

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
//** The Listaller DBus helper daemon
program listallerd;

{$mode objfpc}{$H+}

uses
  cthreads, Interfaces, Classes, SysUtils, CustApp, dbus, djobs,
  Contnrs, SimDBus, liBasic, LCLIntf, SyncObjs;

type

  //** The Listaller helper daemon for root actions
  TLiDaemon = class(TCustomApplication)
  protected
    bs: TDBusServer;
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
   startTick: integer;
   lastTime: integer;
begin
 lastTime := 0;
 if bs.Failed then
  exit;

 //Set tick count
 startTick := DateTimeToTimeStamp(Now).Time;

 //Init critical section for usage in threads
 InitCriticalSection(critsec);

 // loop, testing for new messages
 while (true) do
 begin
   //Check tick count
   if (InstallWorkers + ManagerWorkers) > 0 then
     startTick := DateTimeToTimeStamp(Now).Time;

   if (InstallWorkers = 0) and (ManagerWorkers = 0) and
      (lastTime >= Timeout_Seconds) then
        break; //Exit loop -> terminate daemon

   if lastTime <> Round(((DateTimeToTimeStamp(Now).Time - startTick) / 1000)) then
   begin
     lastTime := Round(((DateTimeToTimeStamp(Now).Time - startTick) / 1000));
     if (InstallWorkers = 0) and (ManagerWorkers = 0) then
       p_debug(IntToStr(lastTime) + ' seconds idle');
   end;

   msg := bs.ReadMessage;

   // loop again if we haven't got a message
   if (msg = nil) then
   begin
     sleep(1);
     Continue;
   end;

   // check this is a method call for the right interface & method
   if bs.MessageIsMethodCall(msg, 'org.nlinux.Listaller.Install', CALL_RUNSETUP) then
   begin
     JobList.Add(TDoAppInstall.Create(msg, bs.Connection));
     Inc(InstallWorkers);
     JobList.Delete(0); //Can be removed if threading is enabled
   end
   else
     if bs.MessageIsMethodCall(msg, 'org.nlinux.Listaller.Manage',
      CALL_APPREMOVE) then
     begin
       JobList.Add(TDoAppRemove.Create(msg, bs.Connection));
       Inc(ManagerWorkers);
       JobList.Delete(0); //Can be removed if threading is enabled
     end
     else
       if bs.MessageIsMethodCall(msg, 'org.nlinux.Listaller.Manage',
         CALL_UPDATEAPP) then
       begin
         JobList.Add(TDoAppUpdate.Create(msg, bs.Connection));
         Inc(ManagerWorkers);
         JobList.Delete(0); //Can be removed if threading is enabled
       end
       else
         //Free the message
         bs.FreeMessage(msg);
 end;

 //Finalize and remove critical section
 DoneCriticalSection(critsec);
end;

procedure TLiDaemon.DoRun;
var
  ErrorMsg: string;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
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
  StopOnException := true;

  if IsRoot then
  begin
    bs := TDBusServer.Create('org.nlinux.Listaller', DBUS_BUS_SYSTEM,
      DBUS_NAME_FLAG_REPLACE_EXISTING);

    JobList := TObjectList.Create;
  end
  else
  begin
    writeLn('Please run listallerd as root!');
    halt(1);
  end;
  WriteLn('Daemon started.');

end;

destructor TLiDaemon.Destroy;
begin
  bs.Free;
  inherited Destroy;
end;

procedure TLiDaemon.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TLiDaemon;

{$R *.res}

begin
  Application := TLiDaemon.Create(nil);
  Application.Title := 'Listaller Daemon';
  Application.Run;
  Application.Free;
end.

