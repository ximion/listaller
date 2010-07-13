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
  slibmanage;

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
  solver: TDDEResolver;
  dc: TDEBConverter;
  error: Boolean;
begin
  if ParamStr(1) = '' then
  begin
    writeLn('Please add library name as parameter!');
    halt(1);
  end;
  error := false;
  solver := TDDEResolver.Create;
  if solver.ResolveString(ParamStr(1)) then
  begin
    p_info(' Package: ' + solver.Pack.PkName);
    p_info(' Distro: ' + solver.Pack.Distro);
    if not solver.DownloadPackage then
    begin
      writeLn('Download failed :o');
      error := true;
    end;
  end
  else
  begin
    writeLn('Resolving failed :(');
    error := true;
  end;
  if error then
    halt(2);

  dc := TDEBConverter.Create(solver.Pack.Path);
  solver.Free;
  ForceDirectories('junk/test');
  dc.UnpackDataTo('junk/test');
  dc.Free;
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

