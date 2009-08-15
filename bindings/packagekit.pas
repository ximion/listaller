{ packagekit.pas
  Copyright (C) Listaller Project 2009

  packagekit.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  packagekit.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Contains Listaller's PackageKit-DBus implementation
unit packagekit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Forms, glib2;


//** PackageKit wrapper
type
 PPackageKit = ^TPackageKit;

TPackageKit = class
private
 //PkClient connection
 pkclient: Pointer;
 //Resulting list
 result: TStringList;
 //True if transaction finished
 finaction: Boolean;
 //True if output should be assigned to list
 asstolist: Boolean;
 //Catch the exitcode
 exitcode: Integer;
 //Current progress
 prog: Integer;
 //Last error message
 ErrorMsg: String;
 //Function to get PackageKit version from pkcon
 function GetPkVersion: String;
public
 constructor Create;
 destructor Destroy; override;
 {** Check if package is installed @param pkg Name of the package
     @returns True if the daemon queued the transaction}
 function Resolve(pkg: String): Boolean;
 {Returns the reverse dependencies of a package
 @param pkg Name of the package}
 function GetRequires(pkg: String): Boolean;
 {** Removes a package @param pkg Name of the package
    @returns True if the daemon queued the transaction}
 function RemovePkg(pkg: String): Boolean;
 {** Installs a package from repo @param pkg Name of the package
    @returns True if the daemon queued the transaction}
 function InstallPkg(pkg: String): Boolean;
 {** Get the name of the package, the file belongs to (!for installed pkgs only!) @param fname Name of the file
    @returns True if the daemon queued the transaction}
 function PkgNameFromFile(fname: String): Boolean;
 {** Installs a package from file @param fname Name of the package file
      @returns True if the daemon queued the transaction}
 function InstallLocalPkg(fname: String): Boolean;
 {** Get the name of the package, the file belongs to (!for not installed pkgs too!) @param fname Name of the file
      @returns True if the daemon queued the transaction}
 function FindPkgForFile(fname: String): Boolean;
 //** Grab the resulting package list
 property RsList: TStringList read result write result;
 //** Check if the last transaction was finished
 property PkFinished: Boolean read finaction;
 //** Read finish code
 property PkFinishCode: Integer read exitcode;
 //** Reads the current Packagekit version as string
 property Version: String read GetPkVersion;
 //** Internal: True if a list is recieved
 property AssignToList: Boolean read asstolist;
 //** Current progress of the operation (in %)
 property Progress: Integer read prog;
 //** Read the last error message
 property LastErrorMessage: String read ErrorMsg;
end;

PPkDetailsObj = ^PkDetailsObj;
PkDetailsObj = record
 id: PChar;
 license: PChar;
 //group: PPkGroupEnum;
 description: PChar;
 url: PChar;
 size: Guint64;
end;

PPkPackageId = ^PkPackageID;
PkPackageId = record
 name: PChar;
 version: PChar;
 arch: PChar;
 data: PChar;
end;

//** Needed for use with Qt4, initializes the GType
procedure InitializeGType;

const pklib = 'libpackagekit-glib.so';

//Bitfield
function pk_filter_bitfield_from_text(filters: PChar): guint64; cdecl; external pklib name 'pk_filter_bitfield_from_text';
//Package obj conversion
function pk_package_obj_to_string(obj: GPointer): PChar;cdecl; external pklib name 'pk_package_obj_to_string';
function pk_package_obj_get_id(obj: GPointer): PPkPackageID;cdecl; external pklib name 'pk_package_obj_get_id';
//Actions
function pk_client_reset(client: Pointer;error: PPGError): GBoolean;cdecl;external pklib name 'pk_client_reset';
function pk_client_install_packages(client: Pointer;package_ids: PPChar;error: PPGError): GBoolean;cdecl;external pklib name 'pk_client_install_packages';
function pk_client_new:Pointer;cdecl;external pklib name 'pk_client_new';
function pk_client_resolve(client: Pointer;filters: guint64;packages: PPChar;error: PPGError): GBoolean;cdecl;external pklib name 'pk_client_resolve';
function pk_client_get_requires(client: Pointer;filters: Guint64;package_ids: PPChar;recursive: GBoolean;error: PPGerror): GBoolean;cdecl;external pklib name 'pk_client_get_requires';
function pk_client_remove_packages(client: Pointer;package_ids: PPChar;allow_deps: GBoolean;autoremove: GBoolean;error: PPGerror): GBoolean;cdecl;external pklib name 'pk_client_remove_packages';
function pk_client_search_file(client: Pointer;filters: Guint64;search: PChar;error: PPGError): GBoolean;cdecl;external pklib name 'pk_client_search_file';
function pk_client_install_files(client: Pointer;trusted: GBoolean;files_rel:PPChar;error: PPGerror): GBoolean;cdecl;external pklib name 'pk_client_install_files';

implementation

procedure InitializeGType;
begin
 //Needed for use with Qt4
 g_type_init();
end;

procedure OnProgChange(client: Pointer;percentage: guint;subpercentage: guint;elapsed: guint;remaining: guint;user_data: gpointer);cdecl;
begin
 if percentage = 101 then
  TPackageKit(user_data).prog:=0
 else
  TPackageKit(user_data).prog:=percentage;
end;

procedure OnPackage(client: Pointer;obj: GPointer;user_data: Pointer);cdecl;
var s: String;pk: PPkPackageID;
begin
if Assigned(TPackageKit(user_data).RsList) then
begin
 if (obj<>nil)and(TPackageKit(user_data).AssignToList) then
 begin
 pk:=pk_package_obj_get_id(obj);
 s:=pk^.name;
 TPackageKit(user_data).RsList.Add(s);
 pk:=nil;
 end;
end;
end;

procedure OnFinish(client: Pointer;exit: guint;runtime:guint;user_data: Pointer);cdecl;
begin
if user_data<>nil then
begin
 TPackageKit(user_data).finaction:=true;
 TPackageKit(user_data).exitcode:=exit;
end;
end;

procedure OnMessage(client: Pointer;message: Guint;details: PChar;user_data: Pointer);cdecl;
begin
 writeLn(details);
end;

{ TPackageKit }
constructor TPackageKit.Create;
begin
  inherited Create;
  //Create new PackageKit client
  pkclient := pk_client_new;

  //Assign signals
  g_signal_connect(pkclient,'progress-changed',TGCallback(@OnProgChange),self);
  g_signal_connect(pkclient,'package',TGCallback(@OnPackage),self);
  g_signal_connect(pkclient,'finished',TGCallback(@OnFinish),self);
  g_signal_connect(pkclient,'message',TGCallback(@OnMessage),self);

  asstolist:=false;
end;

destructor TPackageKit.Destroy;
begin
  inherited Destroy;
  pkclient:=nil;
end;

function TPackageKit.GetPkVersion: String;
var s: TStringList;t: TProcess;
begin
 s:=TStringList.Create;
 t:=TProcess.create(nil);
 t.Options:=[poUsePipes];
 t.CommandLine:='pkcon --version';
 try
  t.Execute;
  while t.Running do Application.ProcessMessages;
  s.LoadFromStream(t.Output);
 finally
 t.Free;
 end;
if s.Count>=0 then
Result:=s[0]
else Result:='?';
s.Free;
end;

function TPackageKit.Resolve(pkg: String): Boolean;
var filter: guint64;
    arg: PPChar;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  Result:=true;
  finaction:=false;
  filter:=pk_filter_bitfield_from_text('installed');
  arg := StringToPPchar(pkg, 0);
  Result:=pk_client_resolve(pkclient,filter,arg,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    g_error_free(error);
  end;
end;

function TPackageKit.GetRequires(pkg: String): Boolean;
var filter: guint64;
    ast: String;
    arg: PPChar;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  finaction:=false;
  asstolist:=true;
  filter:=pk_filter_bitfield_from_text('installed');
  ast := pkg+';;;';
  arg := StringToPPchar(ast, 0);

  Result:=pk_client_get_requires(pkclient,filter,arg,true,@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end;

function TPackageKit.RemovePkg(pkg: String): Boolean;
var ast: String;
    arg: PPChar;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  finaction:=false;
  asstolist:=false;
  ast := pkg+';;;';
  arg := StringToPPchar(ast, 0);

  Result:=pk_client_remove_packages(pkclient,arg,true,true,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end;

function TPackageKit.InstallPkg(pkg: String): Boolean;
var ast: String;
    arg: PPChar;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  finaction:=false;
  asstolist:=false;
  ast := pkg+';;;';
  arg := StringToPPchar(ast, 0);

  Result:=pk_client_install_packages(pkclient,arg,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end;

function TPackageKit.PkgNameFromFile(fname: String): Boolean;
var filter: guint64;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  finaction:=false;
  asstolist:=true;
  filter:=pk_filter_bitfield_from_text('installed');

  Result:=pk_client_search_file(pkclient,filter,PChar(fname),@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end;

function TPackageKit.InstallLocalPkg(fname: String): Boolean;
var arg: PPChar;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  finaction:=false;
  asstolist:=false;
  arg:=StringToPPchar(fname, 0);

  Result:=pk_client_install_files(pkclient,true,arg,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end;

function TPackageKit.FindPkgForFile(fname: String): Boolean;
var filter: guint64;
    error: PGError=nil;
begin
  pk_client_reset(pkclient,nil);
  finaction:=false;
  asstolist:=true;
  filter:=pk_filter_bitfield_from_text('none');

  Result:=pk_client_search_file(pkclient,filter,PChar(fname),@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end;

initialization
 InitializeGType;

end.

