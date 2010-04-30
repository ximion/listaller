{ Copyright (C) 2008-2010 Matthias Klumpp

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
  gExt, glib2, distri, Classes, liUtils, liTypes, Process, SysUtils, pkEnum;

type
  //** The pkBitfield var
  PkBitfield = GUInt64;

  TPkProgressCallback = procedure(progress: Pointer; ptype: PkProgressType;
    user_data: GPointer);cdecl;

  //** Pointer to PkClient GObject
  PPkClient = Pointer;
  PPkResults = Pointer;
  PPkError = Pointer;

  TPkPackage = record
    PkDesc: String;
    PkLicense: String;
    PkSummary: String;
    PkUrl: String;
    PkSize: Int64;
    PkGroup: Integer;
    PkPackageId: String;
  end;

  //** Pointer to TPackageKit object
  PPackageKit = ^TPackageKit;

  //** Custom PackageKit wrapper
  TPackageKit = class
  private
    //Resulting list
    Result: TStringList;
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
    //List of received package ids
    pkglist: TStringList;
    //Processes actions asyncronous
    doasync: Boolean;
    //Last detected package
    lpkg: TPkPackage;
    //Set new progress
    procedure SetProgress(i: Integer);
    //Function to get PackageKit version from pkcon
    function GetPkVersion: String;
    //Run the main loop if doasync=false
    procedure RunLoop();
    //Quit a main loop
    procedure LoopQuit();
    //Check if error is set (if it is, print a console message)
    function IsErrorSet(aError: PGError): Boolean;
    //Resolve for internal use
    function INTERN_Resolve(Name: String; filt: String; toPublic: Boolean): Boolean;
  public
    //** GLib main loop
    loop: Pointer;
    //** Pointer to a PkClient
    pkclient: PPkClient;
    constructor Create;
    destructor Destroy; override;
    {** Resolve a package name
        @returns True if action was not cancelled}
    function Resolve(pkg: String): Boolean;
    {** Check if package is installed @param pkg Name of the package
        @returns True if action was not cancelled}
    function ResolveInstalled(pkg: String): Boolean;
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
    {** Get details about a package @param pkg Name of the package
        @returns True if action was not cancelled}
    function GetPkgDetails(pkg: String): Boolean;
    {** Get the name of the package, the file belongs to (!for installed pkgs only!) @param fname Name of the file
        @returns True if action was not cancelled}
    function PkgNameFromFile(fname: String; const desktopfile: Boolean = false): Boolean;
    {** Installs a package from file @param fname Name of the package file
        @returns True if action was not cancelled}
    function InstallLocalPkg(fname: String): Boolean;
    {** Get the name of the package, the file belongs to (!for not installed pkgs too!) @param fname Name of the file
        @returns True if action was not cancelled}
    function FindPkgForFile(fname: String): Boolean;
    //** Grab the resulting package list
    property RsList: TStringList read Result write Result;
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
    //** Read if object is idle
    property Finished: Boolean read done;
    //** If true, the instance won't wait until the action completes
    property NoWait: Boolean read doasync write doasync;
    //** Package information from the last call
    property LastPackage: TPkPackage read lpkg;
  end;

  PPkPackageId = ^PkPackageID;

  PkPackageId = record
    Name: PChar;
    version: PChar;
    arch: PChar;
    Data: PChar;
  end;

const pklib2 = 'libpackagekit-glib2.so';

//Bitfield
function pk_filter_bitfield_from_string(roles: PChar): GuInt64; cdecl; external pklib2 name 'pk_filter_bitfield_from_text';
//Package obj conversion
function pk_package_obj_to_string(obj: GPointer): PChar;cdecl; external pklib2;
function pk_package_obj_get_id(obj: GPointer): PPkPackageID;cdecl; external pklib2;
//Actions
function pk_client_new:Pointer;cdecl;external pklib2;
procedure pk_client_resolve_async(client: PPkClient;filters: GuInt64;
                                          packages: PPChar;cancellable: PGObject;
                                          progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                          callback_ready: GAsyncReadyCallback;user_data: GPointer);
                                          cdecl;external pklib2;
procedure pk_client_install_packages_async(client: PPkClient;only_trusted: GBoolean;
                                                   package_ids: PPGChar;cancellable: PGObject;
                                                   progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                                   callback_ready: GAsyncReadyCallback;user_data: GPointer);
                                                   cdecl;external pklib2;
procedure pk_client_get_requires_async(client: PPkClient;filters: GuInt64;
                                               package_ids: PPGChar;recursive: GBoolean;
                                               cancellable: PGObject;
                                               progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                               callback_ready: GAsyncReadyCallback;user_data: GPointer);
                                               cdecl;external pklib2;
procedure pk_client_remove_packages_async(client: PPkClient;package_ids: PPGChar;
                                                  allow_deps: GBoolean;autoremove: GBoolean;
                                                  cancellable: PGObject;
                                                  progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                                  callback_ready: GAsyncReadyCallback;user_data: GPointer);
                                                  cdecl;external pklib2;
procedure pk_client_search_files_async(client: PPkClient;filters: PkBitfield;values: PPGChar;
                                                  cancellable: PGCancellable;
                                                  progress_callback: TPKProgressCallback;progress_user_data: GPointer;
                                                  callback_ready: GAsyncReadyCallback;user_data: GPointer);
                                                  cdecl;external pklib2;
procedure pk_client_install_files_async(client: PPkClient;only_trusted: GBoolean;
                                                files: PPGChar;cancellable: PGObject;
                                                progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                                callback_ready: GAsyncReadyCallback;user_data: GPointer);
                                                cdecl;external pklib2;
procedure pk_client_get_details_async(client: PPkClient;package_ids: PPGChar;cancellable: PGCancellable;
                                              progress_callback: TPkProgressCallback;progress_user_data: GPointer;
                                              callback_ready: GAsyncReadyCallback;user_data: GPointer);cdecl; external pklib2;

function  pk_client_generic_finish(client: PPkClient;res: Pointer;error: PPGError): Pointer;cdecl;external pklib2;
function  pk_results_get_exit_code(results: PPkResults): PkExitEnum;cdecl;external pklib2;
function  pk_results_get_error_code(results: PPkResults): PPkError;cdecl;external pklib2;
function  pk_results_get_details_array(results: PPkResults): PGPtrArray;cdecl;external pklib2;

function  pk_client_error_quark(): GQuark;cdecl;external pklib2;
function  pk_client_get_type(): GType;cdecl;external pklib2;

implementation

uses PkDesktop;

function PK_CLIENT(o: GPointer): PGTypeInstance;
begin
  Result := G_TYPE_CHECK_INSTANCE_CAST(o, pk_client_get_type());
end;

procedure OnPkActionFinished(source_object: PGObject; res: Pointer; user_data: GPointer);cdecl;
var
  results: Pointer;
  error: PGError = nil;
  detArr: PGPtrArray;
  str: PGChar;
  pkg: Pointer;
  role: PkRoleEnum;
  pk: TPackageKit;
begin
  results := pk_client_generic_finish(source_object, res, @error);

  if error<>nil then
  begin
    g_warning('failed: %s', [error^.message]);
    g_error_free(error);
    TPackageKit(user_data).exitcode := 8;
    exit;
  end;

  pk := TPackageKit(user_data);
  if results = nil then
  begin
   pk.LoopQuit();
   pk.done := true;
   exit;
  end;

  //pk.exitcode := pk_results_get_error_code(results);

  //Get the role
  g_object_get(G_OBJECT(results), 'role', @role, nil);

  if role  = PK_ROLE_ENUM_GET_DETAILS then
  begin
    detArr := pk_results_get_details_array(results);
    if detArr <> nil then
    if detArr^.len = 1 then
    begin
      pkg := g_ptr_array_index(detArr, 0);
      g_object_get(pkg, 'description', @str, nil);
      pk.lpkg.PkDesc := str;
      g_object_get(pkg, 'package-id', @str, nil);
      pk.lpkg.PkPackageId := str;
      g_object_get(pkg, 'url', @str, nil);
      pk.lpkg.PkUrl := str;
      g_object_get(pkg, 'license', @str, nil);
      pk.lpkg.PkLicense := str;
    end else writeLn('No one entry found!');
     g_ptr_array_unref(detArr);
  end;
    pk.exitcode := Integer(pk_results_get_exit_code(results));
    g_object_unref(results);
  pk.LoopQuit();
  pk.done := true;
end;

procedure OnPkProgress(progress: Pointer; ptype: PkProgressType; user_data: GPointer);cdecl;
var
  pk: TPackageKit;
  pid: PGChar;
  percentage: GuInt;
begin
  pk := TPackageKit(user_data);

  case ptype of
    PK_PROGRESS_TYPE_PACKAGE_ID:
    begin
      if Assigned(pk.RsList) then
      begin
        g_object_get(progress, 'package-id', @pid, nil);
        if (pid<>'') then
          pk.RsList.Add(copy(pid, 0, pos(';', pid)-1));
      end;
    end;
    PK_PROGRESS_TYPE_PERCENTAGE:
    begin
      g_object_get(progress, 'percentage', @percentage, nil);
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

  loop := g_main_loop_new(nil, false);
  doasync := false;
  pkglist := TStringList.Create;
end;

destructor TPackageKit.Destroy;
begin
  pkglist.Free;
  g_object_unref(pkclient);
  g_main_loop_unref(loop);
  inherited Destroy;
end;

procedure TPackageKit.LoopQuit();
begin
  if g_main_loop_is_running(loop) then
    g_main_loop_quit(loop);
end;

procedure TPackageKit.RunLoop();
begin
  if not doasync then
    g_main_loop_run(loop);
end;

function TPackageKit.IsErrorSet(aError: PGError): Boolean;
begin
  Result := false;
  if aError<>nil then
  begin
    Result := true;
    errorMsg:=aError^.message;
    g_warning('action failed: %s', [errorMsg]);
    exitcode:=88;
    g_error_free(aError);
  end;
end;

procedure TPackageKit.SetProgress(i: Integer);
begin
  prog := i;
  if Assigned(FProg) then
    FProg(i, nil);
end;

function TPackageKit.GetPkVersion: String;
var
  s: TStringList;
  t: TProcess;
begin
  s := TStringList.Create;
  t := TProcess.Create(nil);
  t.Options := [poUsePipes];
  t.CommandLine := 'pkcon --version';
  try
    t.Execute;
    while t.Running do
    begin
    end;
    s.LoadFromStream(t.Output);
  finally
    t.Free;
  end;
  if s.Count>=0 then
    Result := s[0]
  else
    Result := '?';
  s.Free;
end;

procedure INTERNAL_OnPkProgress(progress: Pointer; ptype: PkProgressType;
  user_data: GPointer);cdecl;
var
  pk: TPackageKit;
  pid: PGChar;
  percentage: GuInt;
begin
  pk := TPackageKit(user_data);

  case ptype of
    PK_PROGRESS_TYPE_PACKAGE_ID:
    begin
      if Assigned(pk.pkglist) then
      begin
        g_object_get(progress, 'package-id', @pid, nil);
        pk.pkglist.Add(pid);
      end;
    end;
    PK_PROGRESS_TYPE_PERCENTAGE:
    begin
      g_object_get(progress, 'percentage', @percentage, nil);
      if percentage = 101 then
        pk.Setprogress(0)
      else
        pk.SetProgress(percentage);
    end;
  end;
end;

function TPackageKit.INTERN_Resolve(Name: String; filt: String;
  toPublic: Boolean): Boolean;
var
  filter: guint64;
  arg: PPChar;
  error: PGError = nil;
  cancellable: Pointer;
begin
  done := false;
  filter := pk_filter_bitfield_from_string(PChar(filt));
  arg := StringToPPchar(Name, 0);

  cancellable := g_cancellable_new;
  if toPublic then
    pk_client_resolve_async(pkclient, filter, arg, cancellable, @OnPkProgress,
      self, @OnPkActionFinished, self)
  else
    pk_client_resolve_async(pkclient, filter, arg, cancellable,
      @INTERNAL_OnPkProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);
  g_object_unref(cancellable);

  if Result then
   RunLoop();
end;

function TPackageKit.ResolveInstalled(pkg: String): Boolean;
begin
  Result := INTERN_Resolve(pkg, 'installed', true);
end;

function TPackageKit.Resolve(pkg: String): Boolean;
begin
  Result := INTERN_Resolve(pkg, 'available', true);
end;

function TPackageKit.GetRequires(pkg: String): Boolean;
var
  filter: guint64;
  arg: PPChar;
  error: PGError = nil;
  cancellable: Pointer;
begin
  done := false;
  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'installed', false);
  while not done do ;
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  pkg := pkglist[0];

  filter := pk_filter_bitfield_from_string('installed');
  arg := StringToPPchar(pkg, 0);

  cancellable := g_cancellable_new;
  pk_client_get_requires_async(pkclient, filter, arg, true, cancellable,
    @OnPkProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);
  g_object_unref(cancellable);

  if Result then
   RunLoop();
end;

function TPackageKit.RemovePkg(pkg: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
  cancellable: Pointer;
begin
  done := false;
  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'none', false);
  while not done do ;
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  pkg := pkglist[0];
  done := false;
  arg := StringToPPchar(pkg, 0);

  cancellable := g_cancellable_new;
  pk_client_remove_packages_async(pkclient, arg, true, false, cancellable,
    @OnPKProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);
  g_object_unref(cancellable);

  if Result then
   RunLoop();
end;

function TPackageKit.InstallPkg(pkg: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
  gcan: Pointer;
begin
  done := false;
  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'none', false);
  while not done do ;
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  pkg := pkglist[0];
  done := false;
  arg := StringToPPchar(pkg, 0);

  gcan := g_cancellable_new;
  pk_client_install_packages_async(pkclient, false, arg, gcan, @OnPkProgress,
    self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(gcan, @error);
  Result := not IsErrorSet(error);
  g_object_unref(gcan);
  if Result then
   RunLoop();
end;

function TPackageKit.GetPkgDetails(pkg: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
  gcan: Pointer;
begin
  done := false;
  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'none', false);
  while not done do ;
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  pkg := pkglist[0];
  done := false;
  arg := StringToPPchar(pkg, 0);

  gcan := g_cancellable_new;

  pk_client_get_details_async(pkclient, arg, gcan, @OnPkProgress,
    self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(gcan, @error);
  Result := not IsErrorSet(error);
  g_object_unref(gcan);
  if Result then
   RunLoop();
end;

function TPackageKit.PkgNameFromFile(fname: String;
  const desktopfile: Boolean = false): Boolean;
var
  filter: guint64;
  error: PGError = nil;
  cancellable: Pointer;
  pkg: String;
  pkdesk: Pointer;
begin
  done := false;
  error := nil;

  pkglist.Clear;
  pkg := '';
  //Do not use this for _every_ desktop file.
  //Sometimes it might be better to do a complete search.
  // Using PkDesktop can be set by var desktopfile
  if (desktopfile = true)and(LowerCase(ExtractFileExt(fname)) = '.desktop') then
  begin
    pkdesk := pk_desktop_new();
    try
      pk_desktop_open_database(pkdesk, @error);
      if not IsErrorSet(error) then
      begin
        pkg := pk_desktop_get_package_for_file(pkdesk, PGChar(fname), @error);
        pkglist.Add(pkg);
      end;

      exitcode := 1;
    finally
      g_object_unref(pkdesk);
    end;
    Result := true;
  end;

  Result := not IsErrorSet(error);
  //If package was not found yet or PkDesktop-mode is not activated
  if StrSubst(pkg, ' ', '') = '' then
  begin
    pkglist.Clear;
    Result := false;
    if error<>nil then
      error := nil;

    filter := pk_filter_bitfield_from_string('installed');

    cancellable := g_cancellable_new;
    pk_client_search_files_async(pkclient, filter, StringToPPchar(fname,
      0), cancellable, @OnPkProgress, self, @OnPkActionFinished, self);

    Result := true;
    g_cancellable_set_error_if_cancelled(cancellable, @error);
    g_object_unref(cancellable);
    Result := not IsErrorSet(error);
    if Result then
     RunLoop();
  end;
end;

function TPackageKit.InstallLocalPkg(fname: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
  cancellable: Pointer;
begin
  done := false;
  arg := StringToPPchar(fname, 0);

  cancellable := g_cancellable_new;
  pk_client_install_files_async(pkclient, false, arg, cancellable,
    @OnPkProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);
  g_object_unref(cancellable);

  if Result then
   RunLoop();
end;

function TPackageKit.FindPkgForFile(fname: String): Boolean;
var
  filter: guint64;
  error: PGError = nil;
  DInfo: TDistroInfo;
  p: TProcess;
  s: TStringList;
  cancellable: Pointer;
begin
  DInfo := GetDistro;
  if DInfo.PackageSystem<>'DEB' then
  begin
    writeLn('DEBUG: Using native pkit backend.');
    done := false;
    filter := pk_filter_bitfield_from_string('none');

    cancellable := g_cancellable_new;
    pk_client_search_files_async(pkclient, filter, StringToPPchar(fname,
      0), cancellable, @OnPkProgress, self, @OnPkActionFinished, self);

    Result := true;
    g_cancellable_set_error_if_cancelled(cancellable, @error);
    Result := not IsErrorSet(error);
    g_object_unref(cancellable);
    if Result then
     RunLoop();
  end
  else
  begin
    // We need to use apt-file, because the PackageKit
    // APT backend does not support searching for not-installed packages
    done := false;
    s := TStringList.Create;
    p := TProcess.Create(nil);
    p.Options := [poUsePipes];
    if not FileExists('/usr/bin/apt-file') then
    begin
      writeLn('error:');
      writeLn(' The apt-file utility was not found!');
      writeLn(' Please install apt-file and run this setup again.');
      writeLn(' [Emergency halt.]');
      halt(4);
    end;
    p.CommandLine := 'apt-file -l -N search '+fname;
    try
      p.Execute;
      while p.Running do
      begin
      end;
      s.LoadFromStream(p.Output);
    finally
      p.Free;
    end;
    RsList.Assign(s);
    if s.Count>=0 then
      Result := true
    else
      Result := false;
    s.Free;
    done := true;
  end;

end;

end.

