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
  Classes, SysUtils, Process, glib2, distri, Dialogs, liTypes, gExt;

type

//** PackageKit progress type
PK_PROGRESS_TYPE = (PK_PROGRESS_TYPE_PACKAGE_ID,
  PK_PROGRESS_TYPE_PERCENTAGE,
  PK_PROGRESS_TYPE_SUBPERCENTAGE,
  PK_PROGRESS_TYPE_ALLOW_CANCEL,
  PK_PROGRESS_TYPE_STATUS,
  PK_PROGRESS_TYPE_ROLE,
  PK_PROGRESS_TYPE_CALLER_ACTIVE,
  PK_PROGRESS_TYPE_INVALID);

//** How the backend exits
PkExitEnum = (PK_EXIT_ENUM_UNKNOWN,
	PK_EXIT_ENUM_SUCCESS,
	PK_EXIT_ENUM_FAILED,
	PK_EXIT_ENUM_CANCELLED,
	PK_EXIT_ENUM_KEY_REQUIRED,
	PK_EXIT_ENUM_EULA_REQUIRED,
	PK_EXIT_ENUM_KILLED,
	PK_EXIT_ENUM_MEDIA_CHANGE_REQUIRED,
	PK_EXIT_ENUM_NEED_UNTRUSTED,
	PK_EXIT_ENUM_LAST);

TPkProgressCallback = procedure(progress: Pointer;ptype: PK_PROGRESS_TYPE;user_data: GPointer);

//** Pointer to TPackageKit object
PPackageKit = ^TPackageKit;

//** PackageKit wrapper
TPackageKit = class
private
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
 //** GLib main loop
 loop: Pointer;
 //** Pointer to a PkClient
 pkclient: Pointer;
 constructor Create;
 destructor Destroy; override;
 {** Check if package is installed @param pkg Name of the package
     @returns True if action was not cancelled}
 function Resolve(pkg: String): Boolean;
 {** Returns the reverse dependencies of a package
     @param pkg Name of the package
     @returns True if action was not cancelled}
 function GetRequires(pkg: String): Boolean;
 {** Removes a package @param pkg Name of the package
    @returns True if action was not cancelled}
 function RemovePkg(pkg: String): Boolean;
 {** Installs a package from repo @param pkg Name of the package
    @returns True if action was not cancelled}
 function InstallPkg(pkg: String): Boolean;
 {** Get the name of the package, the file belongs to (!for installed pkgs only!) @param fname Name of the file
    @returns True if action was not cancelled}
 function PkgNameFromFile(fname: String): Boolean;
 {** Installs a package from file @param fname Name of the package file
      @returns True if action was not cancelled}
 function InstallLocalPkg(fname: String): Boolean;
 {** Get the name of the package, the file belongs to (!for not installed pkgs too!) @param fname Name of the file
      @returns True if action was not cancelled}
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

const pklib2 = 'libpackagekit-glib2.so';

//Bitfield
function pk_filter_bitfield_from_text(filters: PChar): GuInt64; cdecl; external pklib2 name 'pk_filter_bitfield_from_text';
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
procedure pk_client_remove_packages_async(client: Pointer;package_ids: PPGChar;
                                                  allow_deps: GBoolean;autoremove: GBoolean;
                                                  cancellable: PGObject;
                                                  progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                                  callback_ready: TGAsyncReadyCallback;user_data: GPointer);
                                                  cdecl;external pklib2 name 'pk_client_remove_packages_async';
procedure pk_client_search_file_async(client: Pointer;filters: GuInt64;
                                              values: PPGChar;cancellable: PGObject;
                                              progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                              callback_ready: TGAsyncReadyCallback;user_data: GPointer);
                                              cdecl;external pklib2 name 'pk_client_search_file_async';
procedure pk_client_install_files_async(client: Pointer;only_trusted: GBoolean;
                                                files: PPGChar;cancellable: PGObject;
                                                progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                                callback_ready: TGAsyncReadyCallback;user_data: GPointer);
                                                cdecl;external pklib2 name 'pk_client_install_files_async';

function  pk_client_generic_finish(client: Pointer;res: Pointer;error: PPGError): Pointer;cdecl;external pklib2 name 'pk_client_generic_finish';
function  pk_results_get_exit_code(results: Pointer): PkExitEnum;cdecl;external pklib2 name 'pk_client_generic_finish';
function  pk_client_error_quark(): GQuark;cdecl;external name 'pk_client_error_quark';
function  pk_client_get_type(): GType;cdecl;external name 'pk_client_get_type';

implementation

procedure InitializeGType;
begin
 //Needed for use with Qt4
 {$IFNDEF LCLGTK2}
  g_type_init();
 {$ENDIF}
end;

function PK_CLIENT(o: GPointer): PGTypeInstance;
begin
  G_TYPE_CHECK_INSTANCE_CAST(o,pk_client_get_type());
end;

procedure OnPkActionFinished(source_object: PGObject;res: Pointer;user_data: GPointer);
var result: Pointer;
    rcode: PkExitEnum;
    error: PGError=nil;
begin
 result:=pk_client_generic_finish(source_object,res,@error);

 if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    g_error_free(error);
    TPackageKit(user_data).exitcode:=8;
    exit;
  end;

 if result<>nil then
 begin
 rcode:=pk_results_get_exit_code(result);
  writeLn(rcode);
  TPackageKit(user_data).exitcode:=LongInt(rcode);
 g_object_unref(result);
 end else TPackageKit(user_data).exitcode:=8;
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
 if Assigned(FProg) then FProg(i,nil);
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
    cancellable: Pointer;
begin
  done:=false;
  filter:=pk_filter_bitfield_from_text('installed');
  arg := StringToPPchar(pkg, 0);

  cancellable:=g_cancellable_new;
   pk_client_resolve_async(pkclient,filter,arg,cancellable,@OnPkProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  Result:=true;
  g_cancellable_set_error_if_cancelled(cancellable,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(cancellable);
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

  Result:=true;
  g_cancellable_set_error_if_cancelled(cancellable,@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(cancellable);
end;

function TPackageKit.RemovePkg(pkg: String): Boolean;
var ast: String;
    arg: PPChar;
    error: PGError=nil;
    cancellable: Pointer;
begin
  writeLn('Remove package called!');
  done:=false;
  ast := pkg+';;;';
  arg := StringToPPchar(ast, 0);

  cancellable:=g_cancellable_new;
  writeLn(pkg);
   pk_client_remove_packages_async(pkclient,arg,true,false,cancellable,@OnPKProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  Result:=true;
  g_cancellable_set_error_if_cancelled(cancellable,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(cancellable);
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

  Result:=true;
  g_cancellable_set_error_if_cancelled(gcan,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(gcan);
end;

function TPackageKit.PkgNameFromFile(fname: String): Boolean;
var filter: guint64;
    error: PGError=nil;
    cancellable: Pointer;
begin
  done:=false;
  filter:=pk_filter_bitfield_from_text('installed');

  cancellable:=g_cancellable_new;
   pk_client_search_file_async(pkclient,filter,StringToPPchar(fname, 0),cancellable,@OnPkProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  Result:=true;
  g_cancellable_set_error_if_cancelled(cancellable,@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(cancellable);
end;

function TPackageKit.InstallLocalPkg(fname: String): Boolean;
var arg: PPChar;
    error: PGError=nil;
    cancellable: Pointer;
begin
  done:=false;
  arg:=StringToPPchar(fname, 0);

  cancellable:=g_cancellable_new;
   pk_client_install_files_async(pkclient,false,arg,cancellable,@OnPkProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  Result:=true;
  g_cancellable_set_error_if_cancelled(cancellable,@error);
  if error<>nil then
  begin
    Result:=false;
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(cancellable);
end;

function TPackageKit.FindPkgForFile(fname: String): Boolean;
var filter: guint64;
    error: PGError=nil;
    DInfo: TDistroInfo;
    p: TProcess;
    s: TStringList;
    cancellable: Pointer;
begin
DInfo:=GetDistro;
if DInfo.PackageSystem<>'DEB' then
begin
  writeLn('DEBUG: Using native pkit backend.');
  done:=false;
  filter:=pk_filter_bitfield_from_text('none');

  cancellable:=g_cancellable_new;
   pk_client_search_file_async(pkclient,filter,StringToPPchar(fname, 0),cancellable,@OnPkProgress,self,@OnPkActionFinished,self);
  g_main_loop_run(loop);

  Result:=true;
  g_cancellable_set_error_if_cancelled(cancellable,@error);
  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    ErrorMsg:=error^.message;
    g_error_free(error);
    Result:=false;
  end;
  g_object_unref(cancellable);
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

