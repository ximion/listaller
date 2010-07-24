{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** Command-line application for IPK-package building
program libuild;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Interfaces, //We need an widgetset (NoGUI) for graphic handling
  Classes, SysUtils, CustApp, liUtils, Process, IPKBuild, StrLocale,
  LiTranslator, UniBuild, GExt;

type

 { TLiBuild }

 TLiBuild = class(TCustomApplication)
 private
   procedure OnProgress(pos: Integer; user_data: Pointer);
 protected
   procedure DoRun; override;
 public
   constructor Create(TheOwner: TComponent); override;
   destructor Destroy; override;
   procedure WriteHelp; virtual;
   //** Exception handler
   procedure OnExeception(Sender: TObject; E: Exception);
 end;

 { TLiBuild }

procedure TLiBuild.OnProgress(pos: Integer; user_data: Pointer);
begin
 write(#13+' Progress: '+IntToStr(pos)+'%');
end;

procedure TLiBuild.DoRun;
var
  a, b: String;
  i: Integer;
  x: Boolean;
  pki: TPackInfo;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h?b:uv',['help', 'build:', 'gen-update', 'version', 'noquietcrash', 'deb', 'rpm', 'dpack',
                                          'generate-button', 'sign']);
  if ErrorMsg<>'' then
  begin
    writeLn(ErrorMsg);
    Terminate;
    Exit;
  end;

if not HasOption('noquietcrash') then
    OnException := @OnExeception;

  CheckOptions('h', 'help');
  CheckOptions('?', 'help');

  CheckOptions('b', 'build');
  CheckOptions('u', 'gen-update');
  CheckOptions('v', 'version');

  if paramstr(1) = '' then
  begin
    writeLn('Listaller''s package builder');
    writeln('Usage: ', ExeName, ' <option> [file] ...');
    halt;
  end;

  //Parse parameters
  i := 1;
  a := '';
  b := '';

  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Halt(0);
  end;

  if HasOption('?', 'help') then
  begin
    WriteHelp;
    Halt(0);
  end;

  if HasOption('v', 'version') then
  begin
    writeLn('Version: ' + LiVersion);
    Halt(0);
  end;

  if ParamStr(1) = '' then
  begin
    writeln('Usage: ', ExeName, ' <option> [file] ...');
    Terminate;
  end;

  while ParamStr(i) <> '' do
  begin
    if ParamStr(i)[1] <> '-' then
      if a = '' then
        a := ParamStr(i)
      else
       if b='' then
        b := ParamStr(i);
    Inc(i);
  end;

  if a <> '' then
    a := ExpandFileName(a);
  if b <> '' then
    b := ExpandFileName(b);

  if HasOption('b', 'build') then
  begin
    writeln;
    if HasOption('deb') or HasOption('rpm') or HasOption('dpack') then
    begin
      pki := ReadInformation(ParamStr(2));
      pki.out := ExtractFilePath(ParamStr(2));
      pki.path := ExtractFilePath(ParamStr(2));
      writeLn('== Creating application ==');
      BuildApplication(pki);
      if HasOption('deb') or HasOption('dpack') then
      begin
        if FileExists('/usr/bin/dpkg') then
        begin
          writeLn('== Creating DEB package ==');
          CreateDEB(pki);
        end
        else
        begin
          writeLn('ERROR:');
          writeLn('Cannot build DEB package. You need to install "deb" and "dpkg" before.');
        end;
      end;
      if HasOption('rpm') or HasOption('dpack') then
      begin
        if FileExists('/usr/bin/rpmbuild') then
        begin
          writeLn('== Creating RPM package ==');
          CreateRPM(pki);
        end
        else
        begin
          writeLn('error:');
          writeLn(' Cannot build RPM package. You need to install "rpmbuild" before.');
        end;
      end;
    end;

 if HasOption('generate-button') then
      x := true
    else
      x := false;

 if (FileExists(a)) then
    begin
      if b <> '' then
      begin
        if (not FileExists(b)) and (LowerCase(ExtractFileExt(b)) = '.ipk') and
          (DirectoryExists(ExtractFilePath(b))) then
        begin
          BuildPackage(a, b, x, HasOption('sign'));
        end
        else
        begin
          writeln('Can''t build file.');
          writeLn('- Does the input-file exist?');
          writeLn('- Has the output-file parameter the extension .IPK?');
          writeln('- Does the IPK-File already exists?');
          halt(10);
        end;
      end
      else
      begin
        BuildPackage(a, '', x, HasOption('sign'));
      end;
    end;
    halt;
  end;

  if HasOption('u', 'gen-update') and (FileExists(ParamStr(2))) and
    (DirectoryExists(ParamStr(3))) then
    CreateUpdateSource(ParamStr(2), ParamStr(3) + '/');

  //Stop program loop
  Terminate;
end;

constructor TLiBuild.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := true;
end;

destructor TLiBuild.Destroy;
begin
  inherited Destroy;
end;

procedure TLiBuild.OnExeception(Sender: TObject; E: Exception);
begin
  writeLn;
  writeLn('error:');
  writeLn(' ' + rsInternalError);
  writeLn(' :: ' + E.Message);
  halt(8);
end;

procedure TLiBuild.WriteHelp;
begin
  writeLn('Listaller''s package builder');
  writeln('Usage: ', ExeName, ' <option> [file] ...');
  writeLn('Commands:');
  writeln('-?, --help                                 Show this help');
  writeln('-v, --version                              Show Listaller version');
  writeLn(rsLiBuildInfoA);
  writeLn('-u, --gen-update [IPS-File] [Repo-Path]    Create/Update update-repository');
  writeLn('-b, --build [IPS-File]                     Create DEB and RPM file from IPS');
  writeLn('  Options:');
  writeLn('    --generate-button                      Generates the "Linux-compatible" PNG button for this package');
  writeLn('  Enables these options:');
  writeLn('    --dpack                                Generates DEB/RPM package from IPS file');
  writeLn('    --deb                                  Create DEB package');
  writeLn('    --rpm                                  Create RPM package');
  writeLn('');
end;

var
  Application: TLiBuild;

{$R *.res}

begin
  Application := TLiBuild.Create(nil);
  //Make GType work for us
  InitializeGType();
  Application.Run;
  Application.Free;
end.

