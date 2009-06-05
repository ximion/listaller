{ lipa.lpr
  Copyright (C) Listaller Project 2008-2009

  lipa.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  lipa.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Command-line application for IPK-package handling
program lipa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, //Needed as workaround
  Classes, SysUtils, CustApp,
  Process, ipkbuild, LiCommon;

type

  { TLipa }

  TLipa = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    //** Exception handler
    procedure OnExeception(Sender : TObject;E : Exception);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TLipa }


////////////////////////////////////////////////////////////////////////////////
///////////////////This will be done if "lipa" is started///////////////////////
////////////////////////////////////////////////////////////////////////////////
procedure TLipa.DoRun;
var
  ErrorMsg,a,b: String;
  t: TProcess;
  i: Integer;
  x: Boolean;
begin
  // quick check parameters

  ErrorMsg:=CheckOptions('h','help');
  ErrorMsg:=CheckOptions('?','help');
  //
  ErrorMsg:=CheckOptions('b','build');
  ErrorMsg:=CheckOptions('u','gen-update');
  ErrorMsg:=CheckOptions('v','version');
 { if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Halt;
  end;  }

  // parse parameters
  i:=1;
  a:='';
  b:='';

  if HasOption('h','help') then begin
    WriteHelp;
    Halt(0);
  end;
  
  if HasOption('?','help') then begin
    WriteHelp;
    Halt(0);
  end;

  if HasOption('v','version') then begin
    writeLn('Version: '+LiVersion);
    Halt(0);
  end;

  if HasOption('s','solve') then begin
    writeLn(SyblToPath('$'+paramstr(2)));
    halt(0);
  end;


 while paramstr(i)<>'' do begin
   if paramstr(i)[1]='/' then
    if a = '' then a := paramstr(i)
    else b:=paramstr(i);
    Inc(i);
  end;

  a:=ExpandFileName(a);
  b:=ExpandFileName(b);

  if HasOption('b','build') then begin
  if HasOption('deb')or HasOption('rpm')or HasOption('dpack') then begin
    if FileExists(ExtractFilePath(ExeName)+'unibuild') then begin
      t:=tprocess.create(nil);
      t.Options:=[poUsePipes];
      t.CommandLine:=ExtractFilePath(ExeName)+'unibuild '+paramstr(3);
      sleep(10);
      t.Free;
     end else begin
      writeLn('Cannot execute this action.');
      writeLn('You need to install ''listaller-tools'' first');
      halt(1);
     end;
  end;

  if HasOption('generate-button') then x:=true else x:=false;
   if (FileExists(a))
   then begin
   if b<>'' then begin
   if (not FileExists(b))
   and (LowerCase(ExtractFileExt(b))='.ipk')
   and (DirectoryExists(ExtractFilePath(b))) then begin
   BuildPackage(a,b,x);
   end else begin
   writeln('Can''t build file.');
   writeLn('- Does the input-file exist?');
   writeLn('- Has the output-file parameter the extension .IPK?');
   writeLn('- Is the path to the ouput-file available?');
   writeln('- Does the IPK-File already exists?');
   halt(10);
   end;
   end else begin
   if not FileExists(ChangeFileExt(a,'.ipk')) then
   BuildPackage(a,ChangeFileExt(a,'.ipk'),x)else begin
   writeLn('The package "'+ChangeFileExt(a,'.ipk')+'" already exists!');
   halt(10);
   end;
   end;
   end;
   halt;
  end;
   
  if HasOption('u','gen-update') and(FileExists(paramstr(2)))and(DirectoryExists(paramstr(3)))
  then CreateUpdateSource(paramstr(2),paramstr(3)+'/');

  // stop program loop
  Terminate;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////Help & Main functions////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TLipa.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TLipa.Destroy;
begin
  inherited Destroy;
end;

procedure TLipa.WriteHelp;
begin
writeln('Usage: ',ExeName,' [options] file [...]');

writeLn('Listaller main tool to handle ipk-packages');
writeLn('Usage:');
writeln('-?, --help                                 Show this help');
writeln('-v, --version                              Show Listaller version');
writeLn('-b, --build [IPS-File] [Output-IPK]        Build ipk-package');
writeLn('-u, --gen-update [IPS-File] [Repo-Path]    Create/Update update-repository');
writeLn('-b, --build [IPS-File]                     Create DEB and RPM file from IPS');
if FileExists(ExtractFilePath(ExeName)+'unibuild') then
writeLn('   Enables these options:')
else
writeLn('   Enables these options: (! "unibuild" needed)');
writeLn('    --dpack                                Generates DEB/RPM package from IPS file');
writeLn('    --deb                                  Create DEB package');
writeLn('    --rpm                                  Create RPM package');
writeLn('Options:');
writeLn('--generate-button                          Generates the "Linux-compatible" PNG button for this package');
end;

procedure TLipa.OnExeception(Sender : TObject;E : Exception);
begin
writeLn('An internal error occured');
writeLn('[Message]: '+E.Message);
writeLn('(Aborted)');
halt(8);
end;

var
  Application: TLipa;

{$IFDEF WINDOWS}{$R lipa.rc}{$ENDIF}

begin
  Application:=TLipa.Create(nil);
  Application.OnException:=@Application.OnExeception;
  Application.Run;
  Application.Free;
end.
