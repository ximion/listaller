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
//** LibInstaller functions to perform software installations
unit liinstaller;

{$MODE objfpc}{$H+}

interface

uses
  Classes, LiTypes, SysUtils;

type
  TInstallPack = class
  private
    ins: Pointer;
    ForcedActn: String;
    procedure SetForced(s: String);
    function IsRootMode: Boolean;
    procedure SetTestmode(b: Boolean);
    function IsTestmode: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(pkname: String);
    procedure SetStatusEvent(call: LiStateEvent; const userdata: Pointer = nil);
    procedure SetMessageEvent(call: LiMessageEvent; const userdata: Pointer = nil);
    function PkType: LiPkgType;
    function GetDisallows: String;
    function GetSupDistris: String;
    function GetAppName: String;
    function GetAppVersion: String;
    function GetAppID: String;
    procedure ReadLongDescription(lst: TStringList);
    function GetWizardImagePath: String;
    procedure ReadLicense(lst: TStringList);
    procedure ReadProfiles(lst: TStringList);
    procedure ReadDeps(lst: TStringList);
    function GetAppIcon: String;
    function GetDesktopFiles: String;
    function GetAppCMD: String;
    function GetFileList: String;
    function StartInstallation: Boolean;
    function GetSignatureState: PkgSignatureState;
    procedure EnableUSource(b: Boolean);
    procedure SetProfileID(i: Integer);
    procedure SetRootMode(b: Boolean);
    property Forced: String read ForcedActn write SetForced;
    property SUMode: Boolean read IsRootMode write SetRootMode;
    property TestMode: Boolean read IsTestMode write SetTestMode;
    property RemoteObject: Pointer read ins;
  end;

function IsIPKAppInstalled(appname: String; appid: String; sumode: Boolean): Boolean;

//Import library functions
{@Begin:Installer}

function li_setup_new: LiInstallation; cdecl;external liblistaller;
function li_setup_init(setup: LiInstallation; const pkname: PChar): Boolean;cdecl;external liblistaller;
function li_setup_register_status_call(setup: LiInstallation; call: LiStateEvent;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_setup_register_message_call(setup: LiInstallation; call: LiMessageEvent;user_data: Pointer): Boolean;cdecl;external liblistaller;
function li_setup_pkgtype(setup: LiInstallation): LiPkgType;cdecl;external liblistaller;
procedure li_setup_set_testmode(setup: LiInstallation;st: Boolean);cdecl;external liblistaller;
function li_setup_testmode(setup: LiInstallation): Boolean;cdecl;external liblistaller;
procedure li_setup_set_overrides(setup: LiInstallation; const str: PChar);cdecl;external liblistaller;
procedure li_setup_set_sumode(setup: LiInstallation; b: Boolean);cdecl;external liblistaller;
function li_setup_sumode(setup: LiInstallation): Boolean;cdecl;external liblistaller;
function li_setup_disallows(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_supported_distros(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_ipk_app_is_installed(appname: PChar;appid: PChar;sumode: Boolean): Boolean;cdecl;external liblistaller;
function li_setup_appitem(setup: LiInstallation): LiAppItem;cdecl;external liblistaller;
function li_setup_pkgid(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_signature_state(setup: LiInstallation): PkgSignatureState;cdecl;external liblistaller;
function li_setup_long_description(setup: LiInstallation; list: TStringList): Boolean;cdecl;external liblistaller;
function li_setup_long_description_as_string(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_wizard_image_path(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_license(setup: LiInstallation; list: TStringList): Boolean;cdecl;external liblistaller;
function li_setup_profiles_list(setup: LiInstallation; list: TStringList): Boolean;cdecl;external liblistaller;
procedure li_setup_set_profileid(setup: LiInstallation;id: SmallInt);cdecl;external liblistaller;
procedure li_setup_enable_usource_registering(setup: LiInstallation;b: Boolean);cdecl;external liblistaller;
function li_setup_appicon(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_desktopfiles(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_app_exec_command(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_current_profile_filelist(setup: LiInstallation): PChar;cdecl;external liblistaller;
function li_setup_execute(setup: LiInstallation): Boolean;cdecl;external liblistaller;
procedure li_setup_exec_by_daemon(setup: LiInstallation;b: Boolean);cdecl;external liblistaller;
function li_setup_dependencies(setup: LiInstallation; list: TStringList): Boolean;cdecl;external liblistaller;

{@End:Installer}

implementation

{ TInstallPack }

constructor TInstallPack.Create;
begin
  inherited Create;
  ins := li_setup_new;
end;

destructor TInstallPack.Destroy;
begin
  li_setup_free(@ins);
  inherited Destroy;
end;

procedure TInstallPack.Initialize(pkname: String);
begin
  li_setup_init(@ins, PChar(pkname));
end;

procedure TInstallPack.SetStatusEvent(call: LiStateEvent;
  const userdata: Pointer = nil);
begin
  li_setup_register_status_call(@ins, call, userdata);
end;

function TInstallPack.PkType: LiPkgType;
begin
  Result := li_setup_pkgtype(@ins);
end;

function TInstallPack.IsTestmode: Boolean;
begin
  Result := li_setup_testmode(@ins);
end;

procedure TInstallPack.SetTestmode(b: Boolean);
begin
  li_setup_set_testmode(@ins, b);
end;

function TInstallPack.GetDisallows: String;
begin
  Result := li_setup_disallows(@ins);
end;

function TInstallPack.GetSupDistris: String;
begin
  Result := li_setup_supported_distros(@ins);
end;

function TInstallPack.GetAppName: String;
begin
  Result := li_setup_appname(@ins);
end;

function TInstallPack.GetAppVersion: String;
begin
  Result := li_setup_appversion(@ins);
end;

function TInstallPack.GetAppID: String;
begin
  Result := li_setup_pkgid(@ins);
end;

procedure TInstallPack.ReadLongDescription(lst: TStringList);
begin
  li_setup_long_description(@ins, @lst);
end;

function TInstallPack.GetWizardImagePath: String;
begin
  Result := li_setup_wizard_image_path(@ins);
end;

procedure TInstallPack.ReadLicense(lst: TStringList);
begin
  li_setup_license(@ins, @lst);
end;

procedure TInstallPack.ReadProfiles(lst: TStringList);
begin
  li_setup_profiles_list(@ins, @lst);
end;

function TInstallPack.GetAppIcon: String;
begin
  Result := li_setup_appicon(@ins);
end;

function TInstallPack.GetDesktopFiles: String;
begin
  Result := li_setup_desktopfiles(@ins);
end;

function TInstallPack.GetAppCMD: String;
begin
  Result := li_setup_app_exec_command(@ins);
end;

function TInstallPack.GetFileList: String;
begin
  Result := li_setup_current_profile_filelist(@ins);
end;

procedure TInstallPack.SetMessageEvent(call: LiMessageEvent;
  const userdata: Pointer = nil);
begin
  li_setup_register_message_call(@ins, call, userdata);
end;

procedure TInstallPack.ReadDeps(lst: TStringList);
begin
  li_setup_dependencies(@ins, @lst);
end;

function TInstallPack.StartInstallation: Boolean;
begin
  Result := li_setup_execute(@ins);
end;

procedure TInstallPack.SetRootMode(b: Boolean);
begin
  li_setup_set_sumode(@ins, b);
end;

function TInstallPack.IsRootMode: Boolean;
begin
  Result := li_setup_sumode(@ins);
end;

procedure TInstallPack.EnableUSource(b: Boolean);
begin
  li_setup_enable_usource_registering(@ins, b);
end;

procedure TInstallPack.SetProfileID(i: Integer);
begin
  li_setup_set_profileid(@ins, i);
end;

procedure TInstallPack.SetForced(s: String);
begin
  ForcedActn := s;
  li_setup_set_overrides(@ins, PChar(s));
end;

function TInstallPack.GetSignatureState: PkgSignatureState;
begin
  Result := li_setup_signature_state(@ins);
end;

function IsIPKAppInstalled(appname: String; appid: String; sumode: Boolean): Boolean;
begin
  Result := li_ipk_app_is_installed(PChar(appname), PChar(appid), sumode);
end;

end.

