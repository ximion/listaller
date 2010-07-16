//Created by Matthias Klumpp, licensed under GPLv3
//Simple tool to test connection to DDE and the dependency loader & unpacker
program ddetest;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  dderesolve,
  liUtils,
  depmanage;

type

  { TDDELoader }

  TDDELoader = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure Execute;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDDELoader }

procedure TDDELoader.DoRun;
var
  ErrorMsg: String;
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

  Execute;

  // stop program loop
  Terminate;
end;

procedure TDDELoader.Execute;
var
  depmgr: TDepManager;
begin
  if ParamStr(1) = '' then
  begin
    writeLn('Please add library name as parameter!');
    halt(1);
  end;
  depmgr := TDepManager.Create;
  writeLn(depmgr.InstallDependency(paramstr(1)));
  depmgr.Free;
end;

constructor TDDELoader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := true;
end;

destructor TDDELoader.Destroy;
begin
  inherited Destroy;
end;

procedure TDDELoader.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TDDELoader;

{$R *.res}

begin
  Application := TDDELoader.Create(nil);
  Application.Title := 'DDECheck';
  Application.Run;
  Application.Free;
end.

