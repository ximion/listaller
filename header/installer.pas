unit installer;
 
{$MODE objfpc}{$H+}
 
interface
 
uses
  Classes, SysUtils;

type

 TListallerPackageType = (lptLinstall, lptDLink, lptContainer);
 TProgressChange = function(max: Longint;pos: Longint): Boolean; cdecl;

 TInstallPack = class
 private
  ins: Pointer;
 public
  constructor Create;
  destructor  Destroy;
  procedure Initialize(pkname: String);
  procedure SetMainChangeCall(call: TProgressChange);
  procedure SetExtraChangeCall(call: TProgressChange);
  function  PkType: TListallerPackageType;
  procedure SetTestmode(b: Boolean);
  function  GetDisallows: String;
  function  GetSupDistris: String;
  function  GetAppName: String;
  function  GetAppVersion: String;
  function  GetAppID: String;
  procedure ReadLongDescription(lst: TStringList);
  function  GetWizardImagePath: String;
  procedure ReadLicense(lst: TStringList);
 end;

 const libinst = 'libinstaller.so';

 function IsIPKAppInstalled(appname: String;appid: String): Boolean;

implementation

//Import library functions
function remove_ipk_installed_app(appname, appid: PChar;log: Pointer;poschange: TProgressChange;fastmode: Boolean): Boolean; cdecl; external libinst name 'remove_ipk_installed_app';

function new_installation: Pointer; cdecl; external libinst name 'new_installation';

function free_installation(setup: Pointer): Boolean; external libinst name 'free_installation';

function init_installation(setup: Pointer;pkname: PChar): PChar; cdecl; external libinst name 'init_installation';

function ins_register_main_prog_change_call(setup: Pointer;call: TProgressChange): Boolean; cdecl; external libinst name 'ins_register_main_prog_change_call';

function ins_register_extra_prog_change_call(setup: Pointer;call: TProgressChange): Boolean; cdecl; external libinst name 'ins_register_extra_prog_change_call';

function ins_pkgtype(setup: Pointer): TListallerPackageType; cdecl; external libinst name 'ins_pkgtype';

function set_testmode(st: Boolean): Boolean; cdecl; external libinst name 'set_testmode';

function ins_disallows(setup: Pointer): PChar; cdecl; external libinst name 'ins_disallows';

function ins_supported_distributions(setup: Pointer): PChar; cdecl; external libinst name 'ins_supported_distributions';

function is_ipk_app_installed(appname: PChar;appid: PChar): Boolean; cdecl; external libinst name 'is_ipk_app_installed';

function ins_appname(setup: Pointer): PChar; cdecl; external libinst name 'ins_appname';

function ins_appversion(setup: Pointer): PChar; cdecl; external libinst name 'ins_appversion';

function ins_appid(setup: Pointer): PChar; cdecl; external libinst name 'ins_appid';

function ins_long_description(setup: Pointer; list: Pointer): Boolean; cdecl; external libinst name 'ins_long_description';

function ins_wizard_image_path(setup: Pointer): PChar; cdecl; external libinst name 'ins_wizard_image_path';

function ins_license(setup: Pointer; list: Pointer): Boolean; cdecl; external libinst name 'ins_license';

{ TInstallPack }

constructor TInstallPack.Create;
begin
 inherited Create;
 ins := new_installation;
end;

destructor TInstallPack.Destroy;
begin
 free_installation(@ins);
 inherited Destroy;
end;

procedure TInstallPack.Initialize(pkname: String);
begin
 init_installation(@ins,PChar(pkname))
end;

procedure TInstallPack.SetMainChangeCall(call: TprogressChange);
begin
 ins_register_main_prog_change_call(@ins,call)
end;

procedure TInstallPack.SetExtraChangeCall(call: TprogressChange);
begin
 ins_register_extra_prog_change_call(@ins,call)
end;

function TInstallPack.PkType: TListallerPackageType;
begin
 Result:=ins_pkgtype(@ins);
end;

procedure TInstallPack.SetTestmode(b: Boolean);
begin
 set_testmode(b);
end;

function TInstallPack.GetDisallows: String;
begin
 Result:=ins_disallows(@ins);
end;

function TInstallPack.GetSupDistris: String;
begin
 Result:=ins_supported_distributions(@ins);
end;

function TInstallPack.GetAppName: String;
begin
 Result:=ins_appname(@ins);
end;

function TInstallPack.GetAppVersion: String;
begin
 Result:=ins_appversion(@ins);
end;

function TInstallPack.GetAppID: String;
begin
 Result:=ins_appid(@ins);
end;

procedure TInstallPack.ReadLongDescription(lst: TStringList);
begin
 ins_long_description(@ins,@lst)
end;

function TInstallPack.GetWizardImagePath: String;
begin
 Result:=ins_wizard_image_path(@ins);
end;

procedure TInstallPack.ReadLicense(lst: TStringList);
begin
 ins_license(@ins,@lst)
end;

function IsIPKAppInstalled(appname: String;appid: String): Boolean;
begin
 Result:=is_ipk_app_installed(PChar(appname), PChar(appid));
end;

end.
