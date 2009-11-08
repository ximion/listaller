program listallerd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, dbus, polkit, installer, appman;

type

  { TLiDaemon }

  TLiDaemon = class(TCustomApplication)
  protected
    conn: PDBusConnection;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TLiDaemon }

procedure TLiDaemon.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TLiDaemon.Create(TheOwner: TComponent);
var
  err: DBusError;
  ret: cint;
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  dbus_error_init(@err);
  conn := dbus_bus_get_private(DBUS_BUS_SYSTEM, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('DBus connection Error: '+err.message);
    dbus_error_free(@err);
  end;

  if conn = nil then Exit;
end;

destructor TLiDaemon.Destroy;
begin
  dbus_connection_close(conn);
  inherited Destroy;
end;

procedure TLiDaemon.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TLiDaemon;

{$IFDEF WINDOWS}{$R listallerd.rc}{$ENDIF}

begin
  Application:=TLiDaemon.Create(nil);
  Application.Title:='Listaller Daemon';
  Application.Run;
  Application.Free;
end.

