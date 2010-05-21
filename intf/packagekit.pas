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
  gExt, glib2, distri, Classes, liTypes, liUtils, Process, SysUtils, pktypes;

type
  //** The pkBitfield var
  PkBitfield = GUInt64;

  TPkProgressCallback = procedure(progress: Pointer; ptype: PkProgressType;
    user_data: GPointer); cdecl;

  //** Pointer to PkClient GObject
  PPkClient = Pointer;
  PPkResults = Pointer;
  PPkError = Pointer;
  PPkPackageSack = Pointer;
  PPkPackage = Pointer;

  PPkPackageId = ^PkPackageID;

  PkPackageId = record
    Name: PChar;
    version: PChar;
    arch: PChar;
    Data: PChar;
  end;

  //** Pointer to TPackageKit object
  PPackageKit = ^TPackageKit;

  //** Custom PackageKit wrapper
  TPackageKit = class
  private
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
    pkglist: TPackageList;
    //Processes actions asyncronous
    doasync: Boolean;
    //Our GCancellable
    cancellable: PGObject;
    //Tag
    tagid: Integer;
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
    //** Cancel current action
    procedure Cancel;
    {** Get the name of the package, the file belongs to (!for not installed pkgs too!) @param fname Name of the file
        @returns True if action was not cancelled}
    function FindPkgForFile(fname: String): Boolean;
    //** Grab the resulting package list
    property RList: TPackageList read PkgList write PkgList;
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
    //** Tag
    property Tag: Integer read tagid write tagid;
  end;

  {$I pkclient.inc}
  {$I pkdesktop.inc}

implementation

function PK_CLIENT(o: GPointer): PGTypeInstance;
begin
  Result := G_TYPE_CHECK_INSTANCE_CAST(o, pk_client_get_type());
end;

procedure OnPkActionFinished(source_object: PGObject; res: Pointer;
  user_data: GPointer); cdecl;
var
  results: Pointer;
  error: PGError = nil;
  detArr: PGPtrArray;
  str: PGChar;
  pkg: Pointer;
  role: PkRoleEnum;
  pk: TPackageKit;
  newpkg: TPkPackage;
  sack: PPkPackageSack;
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
  if not (pk is TPackageKit) then exit;

  if results = nil then
  begin
    pk.LoopQuit();
    pk.done := true;
    exit;
  end;

  //pk.exitcode := pk_results_get_error_code(results);

  //Get the role
  g_object_get(G_OBJECT(results), 'role', @role, nil);

  if role = PK_ROLE_ENUM_GET_DETAILS then
  begin
    detArr := pk_results_get_details_array(results);
    sack := pk_results_get_package_sack(results);
    if detArr <> nil then
      if detArr^.len = 1 then
      begin
        pkg := g_ptr_array_index(detArr, 0);
        newpkg := TPkPackage.Create;
        g_object_get(pkg, 'description', @str, nil);
        newpkg.Description := str;
        g_object_get(pkg, 'package-id', @str, nil);
        newpkg.PackageId := str;
        g_object_get(pkg, 'url', @str, nil);
        newpkg.Url := str;
        g_object_get(pkg, 'license', @str, nil);
        newpkg.License := str;

        pkg := pk_package_sack_find_by_id(sack, PChar(newpkg.PackageId));
        if pkg <> nil then
        begin
         g_object_get(pkg, 'info', @newpkg.Status, nil);
         g_object_unref(pkg);
        end;

        pk.pkglist.Add(newpkg);
      end
      else
        writeLn('No one entry found!');
    g_object_unref(sack);
    g_ptr_array_unref(detArr);
  end;
  pk.exitcode := integer(pk_results_get_exit_code(results));
  g_object_unref(results);
  pk.LoopQuit();
  pk.done := true;
end;

procedure OnPkProgress(progress: Pointer; ptype: PkProgressType;
  user_data: GPointer); cdecl;
var
  pk: TPackageKit;
  pid: PGChar;
  percentage: GuInt;
begin
  pk := TPackageKit(user_data);
  if not (pk is TPackageKit) then exit;

  case ptype of
    PK_PROGRESS_TYPE_PACKAGE_ID:
    begin
      if Assigned(pk.RList) then
      begin
        g_object_get(progress, 'package-id', @pid, nil);
        if (pid<>'') then
          pk.RList.Add(TPkPackage.Create(copy(pid, 0, pos(';', pid)-1)));
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

  cancellable := g_cancellable_new();
  loop := g_main_loop_new(nil, false);
  doasync := false;
  pkglist := TPackageList.Create(true);
  done := true; //TPackageKit is idle
end;

destructor TPackageKit.Destroy;
begin
  pkglist.Free;
  g_object_unref(cancellable);
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

procedure TPackageKit.Cancel;
begin
  if not done then
  if cancellable <> nil then
   g_cancellable_cancel(cancellable);
end;

function TPackageKit.IsErrorSet(aError: PGError): Boolean;
begin
  Result := false;
  if aError<>nil then
  begin
    Result := true;
    errorMsg := aError^.message;
    g_warning('action failed: %s', [errorMsg]);
    exitcode := 88;
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
  user_data: GPointer); cdecl;
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
        pk.pkglist.Add(TPkPackage.Create(pid));
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
begin
  if toPublic then
  begin
   if not done then
   begin
     Result := false;
     exit;
   end;
  done := false;
  end;

  filter := pk_filter_bitfield_from_string(PChar(filt));
  arg := StringToPPchar(Name, 0);

  if toPublic then
    pk_client_resolve_async(pkclient, filter, arg, cancellable, @OnPkProgress,
      self, @OnPkActionFinished, self)
  else
    pk_client_resolve_async(pkclient, filter, arg, cancellable,
      @INTERNAL_OnPkProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);

  if Result then
   if not toPublic then
    g_main_loop_run(loop)
   else
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
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
  done := false;

  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'installed', false);
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  pkg := pkglist[0].PackageId;
  pkglist.Clear;

  filter := pk_filter_bitfield_from_string('installed');
  arg := StringToPPchar(pkg, 0);


  pk_client_get_requires_async(pkclient, filter, arg, true, cancellable,
    @OnPkProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);

  if Result then
    RunLoop();
end;

function TPackageKit.RemovePkg(pkg: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
  done := false;

  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'none', false);
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  pkg := pkglist[0].PackageId;
  pkglist.Clear;
  done := false;
  arg := StringToPPchar(pkg, 0);

  pk_client_remove_packages_async(pkclient, arg, true, false,
    cancellable, @OnPKProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);

  if Result then
    RunLoop();
end;

function TPackageKit.InstallPkg(pkg: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
  done := false;

  pkglist.Clear;
  Result := INTERN_Resolve(pkg, 'none', false);
  if not Result then
    exit;
  if pkglist.Count<=0 then
  begin
    Result := false;
    exit;
  end;
  done := false;
  pkg := pkglist[0].PackageId;
  pkglist.Clear;
  arg := StringToPPchar(pkg, 0);

  pk_client_install_packages_async(pkclient, false, arg, cancellable, @OnPkProgress,
    self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);
  if Result then
    RunLoop();
end;

function TPackageKit.GetPkgDetails(pkg: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
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
  done := false;
  pkg := pkglist[0].PackageId;
  pkglist.Clear;
  arg := StringToPPchar(pkg, 0);

  pk_client_get_details_async(pkclient, arg, cancellable, @OnPkProgress,
    self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);
  if Result then
    RunLoop();
end;

function TPackageKit.PkgNameFromFile(fname: String;
  const desktopfile: Boolean = false): Boolean;
var
  filter: guint64;
  error: PGError = nil;
  pkg: String;
  pkdesk: Pointer;
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
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
        pkglist.Add(TPkPackage.Create(pkg));
      end;

      exitcode := 1;
    finally
      g_object_unref(pkdesk);
    end;
    Result := true;
  end;

  Result := not IsErrorSet(error);
  //If package was not found yet or PkDesktop-mode is not activated
  if trim(pkg) = '' then
  begin
    pkglist.Clear;
    Result := false;
    if error<>nil then
      error := nil;

    filter := pk_filter_bitfield_from_string('installed');

    pk_client_search_files_async(pkclient, filter, StringToPPchar(fname, 0),
      cancellable, @OnPkProgress, self, @OnPkActionFinished, self);

    Result := true;
    g_cancellable_set_error_if_cancelled(cancellable, @error);
    Result := not IsErrorSet(error);
    if Result then
      RunLoop();
  end;
end;

function TPackageKit.InstallLocalPkg(fname: String): Boolean;
var
  arg: PPChar;
  error: PGError = nil;
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
  done := false;

  arg := StringToPPchar(fname, 0);

  pk_client_install_files_async(pkclient, false, arg, cancellable,
    @OnPkProgress, self, @OnPkActionFinished, self);

  Result := true;
  g_cancellable_set_error_if_cancelled(cancellable, @error);
  Result := not IsErrorSet(error);

  if Result then
    RunLoop();
end;

function TPackageKit.FindPkgForFile(fname: String): Boolean;
var
  filter: guint64;
  error: PGError = nil;
  DInfo: TDistroInfo;
begin
  if not done then
  begin
    Result := false;
    exit;
  end;
  done := false;

  DInfo := GetDistro;
  if DInfo.PackageSystem = 'DEB' then
  begin
    if not FileExists('/usr/bin/apt-file') then
      begin
        writeLn('error:');
        writeLn(' The apt-file utility was not found!');
        writeLn(' Please install apt-file and run this setup again.');
        writeLn(' [Emergency halt.]');
        halt(4);
      end;
  end;
    done := false;
    filter := pk_filter_bitfield_from_string('none');

    pk_client_search_files_async(pkclient, filter, StringToPPchar(fname, 0),
      cancellable, @OnPkProgress, self, @OnPkActionFinished, self);

    Result := true;
    g_cancellable_set_error_if_cancelled(cancellable, @error);
    Result := not IsErrorSet(error);
    if Result then
      RunLoop();


  //Callback processes do not work in this lib, because their threads use Synchronize()
  {else
  begin
    // We need to use apt-file, because the PackageKit
    // APT backend does not support searching for not-installed packages
    done := false;
    if not Assigned(cbproc) then
    begin
      cbproc := TCallbackProcess.Create;
      cbproc.Options := [poUsePipes];
      if not FileExists('/usr/bin/apt-file') then
      begin
        writeLn('error:');
        writeLn(' The apt-file utility was not found!');
        writeLn(' Please install apt-file and run this setup again.');
        writeLn(' [Emergency halt.]');
        halt(4);
      end;
    end;
    cbproc.CommandLine := 'apt-file -l -N search '+fname;
    cbproc.CallBackEvent := @pCallBackEvent;
    cbproc.RunCommand;
  end;  }
end;

end.

