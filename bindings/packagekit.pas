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
//** Contains Listaller's PackageKit-GLib2 bindings
unit packagekit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, glib2, distri, Dialogs, liTypes;

type

//** PackageKit progress type
PK_PROGRESS_TYPE =
(PK_PROGRESS_TYPE_PACKAGE_ID,
 PK_PROGRESS_TYPE_PERCENTAGE,
 PK_PROGRESS_TYPE_SUBPERCENTAGE,
 PK_PROGRESS_TYPE_ALLOW_CANCEL,
 PK_PROGRESS_TYPE_STATUS,
 PK_PROGRESS_TYPE_ROLE,
 PK_PROGRESS_TYPE_CALLER_ACTIVE,
 PK_PROGRESS_TYPE_INVALID);

TPkProgressCallback = procedure(progress: Pointer;ptype: PK_PROGRESS_TYPE;user_data: GPointer);
TGAsyncReadyCallback = procedure(source_object: PGObject;res: Pointer;user_data: GPointer);
PGAsyncReadyCallback = ^TGAsyncReadyCallback;

//** Pointer to TPackageKit object
PPackageKit = ^TPackageKit;

//** PackageKit wrapper
TPackageKit = class
private
 //PkClient connection
 pkclient: Pointer;
 //Resulting list
 result: TStringList;
 //True if transaction finished
 done: Boolean;
 //Catch the exitcode
 exitcode: Integer;
 //Current progress
 prog: Integer;
 //Last error message
 ErrorMsg: String;
 //ProgressChange event
 FProg: TProgressEvent;
 //Set new progress
 procedure SetProgress(i: Integer);
 //Function to get PackageKit version from pkcon
 function GetPkVersion: String;
 //Quit a main loop
 procedure LoopQuit();
public
 loop: Pointer;
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
 property PkFinished: Boolean read done write done;
 //** Read finish code
 property PkFinishCode: Integer read exitcode;
 //** Reads the current Packagekit version as string
 property Version: String read GetPkVersion;
 //** Progress change callback (progress in %)
 property OnProgress: TProgressEvent read FProg write FProg;
 //** Read the last error message
 property LastErrorMessage: String read ErrorMsg;
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
const pklib2 = 'libpackagekit-glib2.so';
var loop: PGMainLoop; //GLib main loop to catch signals on idle

//GLib-GCancellable
function g_cancellable_new: Pointer;cdecl;external gliblib name 'g_cancellable_new';
function g_cancellable_is_cancelled(cancellable: Pointer): GBoolean;cdecl;external gliblib name 'g_cancellable_is_cancelled';
function g_cancellable_set_error_if_cancelled(cancellable: Pointer;error: PPGError): GBoolean;cdecl;external gliblib name 'g_cancellable_set_error_if_cancelled';

//Bitfield
function pk_filter_bitfield_from_text(filters: PChar): guint64; cdecl; external pklib2 name 'pk_filter_bitfield_from_text';
//Package obj conversion
function pk_package_obj_to_string(obj: GPointer): PChar;cdecl; external pklib2 name 'pk_package_obj_to_string';
function pk_package_obj_get_id(obj: GPointer): PPkPackageID;cdecl; external pklib2 name 'pk_package_obj_get_id';
//Actions
function pk_client_new:Pointer;cdecl;external pklib2 name 'pk_client_new';
procedure pk_client_resolve_async(client: Pointer;filters: GuInt64;
                                          packages: PPChar;cancellable: PGObject;
                                          progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                          callback_ready: TGAsyncReadyCallback;user_data: GPointer);
                                          cdecl;external pklib2 name 'pk_client_resolve_async';
procedure pk_client_install_packages_async(client: Pointer;only_trusted: GBoolean;
                                                   package_ids: PPGChar;cancellable: PGObject;
                                                   progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                                   callback_ready: TGAsyncReadyCallback;user_data: GPointer);
                                                   cdecl;external pklib2 name 'pk_client_install_packages_async';
procedure pk_client_get_requires_async(client: Pointer;filters: GuInt64;
                                               package_ids: PPGChar;recursive: GBoolean;
                                               cancellable: PGObject;
                                               progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                               callback_ready: TGAsyncReadyCallback;user_data: GPointer);
                                               cdecl;external pklib2 name 'pk_client_get_requires_async';


function pk_client_remove_packages(client: Pointer;package_ids: PPChar;allow_deps: GBoolean;autoremove: GBoolean;error: PPGerror): GBoolean;cdecl;external pklib name 'pk_client_remove_packages';
function pk_client_search_file(client: Pointer;filters: Guint64;search: PChar;error: PPGError): GBoolean;cdecl;external pklib name 'pk_client_search_file';
function pk_client_install_files(client: Pointer;trusted: GBoolean;files_rel:PPChar;error: PPGerror): GBoolean;cdecl;external pklib name 'pk_client_install_files';

implementation

procedure InitializeGType;
begin
 //Needed for use with Qt4
 {$IFNDEF LCLGTK2}
  g_type_init();
 {$ENDIF}
end;

procedure OnPkActionFinished(source_object: PGObject;res: Pointer;user_data: GPointer);
begin
 TPackageKit(user_data).LoopQuit();
 TPackageKit(user_data).done:=true;
end;

procedure OnPkProgress(progress: Pointer;ptype: PK_PROGRESS_TYPE;user_data: GPointer);
var pk: TPackageKit;pid: PGChar;percentage: GuInt;
begin
pk:=TPackageKit(user_data);

  case ptype of
   PK_PROGRESS_TYPE_PACKAGE_ID:
   begin
    if Assigned(pk.RsList) then
    begin
     g_object_get(progress,'package-id', @pid,nil);
     if (pid<>'') then
      pk.RsList.Add(copy(pid,0,pos(';',pid)-1));
    end;
   end;
   PK_PROGRESS_TYPE_PERCENTAGE:
   begin
    g_object_get(progress,'percentage', @percentage,nil);
    ShowMessage('Percentage: '+IntToStr(percentage));
    if percentage = 101 then
     pk.Setprogress(0)
    else
     pk.SetProgress(percentage);
   end;
  end;
end;

{ TPackageKit }

constructor TPackageKit.Create;
begin
  inherited Create;
  //Create new PackageKit client
  pkclient := pk_client_new;

  loop:=g_main_loop_new(nil,false);
end;

destructor TPackageKit.Destroy;
begin
  g_object_unref(pkclient);
  g_main_loop_unref(loop);
  inherited Destroy;
end;

procedure TPackageKit.LoopQuit();
begin
 g_main_loop_quit(loop);
end;

procedure TPackageKit.SetProgress(i: Integer);
begin
 prog:=i;
 if Assigned(FProg) then FProg(i);
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
  while t.Running do begin end;
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
    gcan: Pointer;
begin
  Result:=true;
  done:=false;
  filter:=pk_filter_bitfield_from_text('installed');
  arg := StringToPPchar(pkg, 0);

  gcan:=g_cancellable_new;

  pk_client_resolve_async(pkclient,filter,arg,gcan,@OnPkProgress,self,@OnPkActionFinished,self);

  g_main_loop_run(loop);

  g_cancellable_set_error_if_cancelled(gcan,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    g_error_free(error);
  end;
  g_object_unref(gcan);
end;

function TPackageKit.GetRequires(pkg: String): Boolean;
var filter: guint64;
    ast: String;
    arg: PPChar;
    error: PGError=nil;
    cancellable: Pointer;
begin
  done:=false;
  filter:=pk_filter_bitfield_from_text('installed');
  ast := pkg+';;;';
  arg := StringToPPchar(ast, 0);

  cancellable:=g_cancellable_new;
  pk_client_get_requires_async(pkclient,filter,arg,true,cancellable,@OnPkProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  g_cancellable_set_error_if_cancelled(cancellable,@error);
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
  done:=false;
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
    gcan: Pointer;
begin

  done:=false;
  ast := pkg+';;;';
  arg := StringToPPchar(ast, 0);

  gcan:=g_cancellable_new;
  pk_client_install_packages_async(pkclient,false,arg,gcan,@OnPkProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  g_cancellable_set_error_if_cancelled(gcan,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
  g_object_unref(gcan);
end;

function TPackageKit.PkgNameFromFile(fname: String): Boolean;
var filter: guint64;
    error: PGError=nil;
begin

  done:=false;
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

  done:=false;
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
    DInfo: TDistroInfo;
    p: TProcess;
    s: TStringList;
begin
DInfo:=GetDistro;
if DInfo.PackageSystem<>'DEB' then
begin
  writeLn('DEBUG: Using native pkit backend.');
  done:=false;
  filter:=pk_filter_bitfield_from_text('none');

  Result:=pk_client_search_file(pkclient,filter,PChar(fname),@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
  end;
end else
begin
 // We need to use apt-file, because the PackageKit
 // APT backend does not support searching for not-installed packages
  done:=false;
  s:=TStringList.Create;
  p:=TProcess.Create(nil);
  p.Options:=[poUsePipes];
  p.CommandLine:='apt-file -l -N search '+fname;
 try
  p.Execute;
  while p.Running do begin end;
   s.LoadFromStream(p.Output);
 finally
 p.Free;
 end;
 RsList.Assign(s);
 if s.Count>=0 then
  Result:=true
 else Result:=false;
 s.Free;
 done:=true;
end;

end;

initialization
 InitializeGType;

end.

