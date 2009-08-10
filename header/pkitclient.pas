unit pkitclient;
 
{$MODE objfpc}{$H+}
 
interface
 
uses
  Classes, SysUtils, glib2, dbus;
 
type

  PDBusGConnection = Pointer;
  PDBusGMessage = Pointer;
  PDBusGMethodInvocation = Pointer;
  PDBusGObjectInfo = Pointer;
  PDBusGProxy = Pointer;
  PDBusGProxyCall = Pointer;
  PPkClient  = ^PkClient;
  PkStatusEnum  = Pointer;
  PPkPackageObj  = Pointer;
  PPkTransactionObj  = Pointer;
  PPkUpdateDetailObj  = Pointer;
  PPkDetailsObj  = Pointer;
  PPkPackageId  = Pointer;
  PPkCategoryObj = Pointer;

  PkUpdateStateEnum =(PK_UPDATE_STATE_ENUM_STABLE,PK_UPDATE_STATE_ENUM_UNSTABLE,PK_UPDATE_STATE_ENUM_TESTING,PK_UPDATE_STATE_ENUM_UNKNOWN);
  PkSigTypeEnum = (PK_SIGTYPE_ENUM_GPG,PK_SIGTYPE_ENUM_UNKNOWN);

  PkErrorCodeEnum =(PK_ERROR_ENUM_OOM,
	PK_ERROR_ENUM_NO_NETWORK,
	PK_ERROR_ENUM_NOT_SUPPORTED,
	PK_ERROR_ENUM_INTERNAL_ERROR,
	PK_ERROR_ENUM_GPG_FAILURE,
	PK_ERROR_ENUM_PACKAGE_ID_INVALID,
	PK_ERROR_ENUM_PACKAGE_NOT_INSTALLED,
	PK_ERROR_ENUM_PACKAGE_NOT_FOUND,
	PK_ERROR_ENUM_PACKAGE_ALREADY_INSTALLED,
	PK_ERROR_ENUM_PACKAGE_DOWNLOAD_FAILED,
	PK_ERROR_ENUM_GROUP_NOT_FOUND,
	PK_ERROR_ENUM_GROUP_LIST_INVALID,
	PK_ERROR_ENUM_DEP_RESOLUTION_FAILED,
	PK_ERROR_ENUM_FILTER_INVALID,
	PK_ERROR_ENUM_CREATE_THREAD_FAILED,
	PK_ERROR_ENUM_TRANSACTION_ERROR,
	PK_ERROR_ENUM_TRANSACTION_CANCELLED,
	PK_ERROR_ENUM_NO_CACHE,
	PK_ERROR_ENUM_REPO_NOT_FOUND,
	PK_ERROR_ENUM_CANNOT_REMOVE_SYSTEM_PACKAGE,
	PK_ERROR_ENUM_PROCESS_KILL,
	PK_ERROR_ENUM_FAILED_INITIALIZATION,
	PK_ERROR_ENUM_FAILED_FINALISE,
	PK_ERROR_ENUM_FAILED_CONFIG_PARSING,
	PK_ERROR_ENUM_CANNOT_CANCEL,
	PK_ERROR_ENUM_CANNOT_GET_LOCK,
	PK_ERROR_ENUM_NO_PACKAGES_TO_UPDATE,
	PK_ERROR_ENUM_CANNOT_WRITE_REPO_CONFIG,
	PK_ERROR_ENUM_LOCAL_INSTALL_FAILED,
	PK_ERROR_ENUM_BAD_GPG_SIGNATURE,
	PK_ERROR_ENUM_MISSING_GPG_SIGNATURE,
	PK_ERROR_ENUM_CANNOT_INSTALL_SOURCE_PACKAGE,
	PK_ERROR_ENUM_REPO_CONFIGURATION_ERROR,
	PK_ERROR_ENUM_NO_LICENSE_AGREEMENT,
	PK_ERROR_ENUM_FILE_CONFLICTS,
	PK_ERROR_ENUM_PACKAGE_CONFLICTS,
	PK_ERROR_ENUM_REPO_NOT_AVAILABLE,
	PK_ERROR_ENUM_INVALID_PACKAGE_FILE,
	PK_ERROR_ENUM_PACKAGE_INSTALL_BLOCKED,
	PK_ERROR_ENUM_PACKAGE_CORRUPT,
	PK_ERROR_ENUM_ALL_PACKAGES_ALREADY_INSTALLED,
	PK_ERROR_ENUM_FILE_NOT_FOUND,
	PK_ERROR_ENUM_NO_MORE_MIRRORS_TO_TRY,
	PK_ERROR_ENUM_NO_DISTRO_UPGRADE_DATA,
	PK_ERROR_ENUM_INCOMPATIBLE_ARCHITECTURE,
	PK_ERROR_ENUM_NO_SPACE_ON_DEVICE,
	PK_ERROR_ENUM_MEDIA_CHANGE_REQUIRED,
	PK_ERROR_ENUM_NOT_AUTHORIZED,
	PK_ERROR_ENUM_UNKNOWN);

PkRestartEnum = (
	PK_RESTART_ENUM_NONE,
	PK_RESTART_ENUM_APPLICATION,
	PK_RESTART_ENUM_SESSION,
	PK_RESTART_ENUM_SYSTEM,
	PK_RESTART_ENUM_UNKNOWN);

PkMessageEnum = (PK_MESSAGE_ENUM_BROKEN_MIRROR,
	PK_MESSAGE_ENUM_CONNECTION_REFUSED,
	PK_MESSAGE_ENUM_PARAMETER_INVALID,
	PK_MESSAGE_ENUM_PRIORITY_INVALID,
	PK_MESSAGE_ENUM_BACKEND_ERROR,
	PK_MESSAGE_ENUM_DAEMON_ERROR,
	PK_MESSAGE_ENUM_CACHE_BEING_REBUILT,
	PK_MESSAGE_ENUM_UNTRUSTED_PACKAGE,
	PK_MESSAGE_ENUM_NEWER_PACKAGE_EXISTS,
	PK_MESSAGE_ENUM_COULD_NOT_FIND_PACKAGE,
	PK_MESSAGE_ENUM_CONFIG_FILES_CHANGED,
	PK_MESSAGE_ENUM_PACKAGE_ALREADY_INSTALLED,
	PK_MESSAGE_ENUM_UNKNOWN);

PkExitEnum = (PK_EXIT_ENUM_SUCCESS,
	PK_EXIT_ENUM_FAILED,
	PK_EXIT_ENUM_CANCELLED,
	PK_EXIT_ENUM_KEY_REQUIRED,
	PK_EXIT_ENUM_EULA_REQUIRED,
	PK_EXIT_ENUM_KILLED, // when we forced the cancel, but had to SIGKILL
	PK_EXIT_ENUM_MEDIA_CHANGE_REQUIRED,
	PK_EXIT_ENUM_UNKNOWN);

PkMediaTypeEnum=(
	PK_MEDIA_TYPE_ENUM_CD,
	PK_MEDIA_TYPE_ENUM_DVD,
	PK_MEDIA_TYPE_ENUM_DISC,
	PK_MEDIA_TYPE_ENUM_UNKNOWN);

 PkClient = ^_PkClientClass;
       _PkClientClass = record
            parent: PGObject;
            status_changed : procedure(client:PPkClient; status:PkStatusEnum);cdecl;
            progress_changed : procedure (client:PPkClient; percentage:guint; subpercentage:guint; elapsed:guint; remaining:guint);
            package : procedure (client:PPkClient; obj:PPkPackageObj);
            transaction : procedure (client:PPkClient; obj:PPkTransactionObj);
            distro_upgrade : procedure (client:PPkClient; _type:PkUpdateStateEnum; name:Pgchar; summary:Pgchar);
            update_detail : procedure (client:PPkClient; update_detail:PPkUpdateDetailObj);
            details : procedure (client:PPkClient; package_detail:PPkDetailsObj);
            files : procedure (client:PPkClient; package_id:Pgchar; filelist:Pgchar);
            repo_signature_required : procedure (client:PPkClient; package_id:Pgchar; repository_name:Pgchar; key_url:Pgchar; key_userid:Pgchar; 
                          key_id:Pgchar; key_fingerprint:Pgchar; key_timestamp:Pgchar; _type:PkSigTypeEnum);
            eula_required : procedure (client:PPkClient; eula_id:Pgchar; package_id:Pgchar; vendor_name:Pgchar; license_agreement:Pgchar);
            repo_detail : procedure (client:PPkClient; repo_id:Pgchar; description:Pgchar; enabled:gboolean);
            error_code : procedure (client:PPkClient; code:PkErrorCodeEnum; details:Pgchar);
            require_restart : procedure (client:PPkClient; restart:PkRestartEnum; id:PPkPackageId);
            message : procedure (client:PPkClient; message:PkMessageEnum; details:Pgchar);
            allow_cancel : procedure (client:PPkClient; allow_cancel:gboolean);
            caller_active_changed : procedure (client:PPkClient; is_active:gboolean);
            finished : procedure (client:PPkClient; exit:PkExitEnum; runtime:guint);
            category : procedure (client:PPkClient; obj:PPkCategoryObj);
            media_change_required : procedure (client:PPkClient; _type:PkMediaTypeEnum; media_id:Pgchar; media_text:Pgchar);
            _pk_reserved1 : procedure ;
            _pk_reserved2 : procedure ;
            _pk_reserved3 : procedure ;
            _pk_reserved4 : procedure ;
end;
 
const External_library = 'libpackagekit-glib.so';
const G_TYPE_STRV: GType = 0;
 
function pk_client_install_packages(client: Pointer;package_ids: PPChar;error: PPGError): GBoolean; cdecl; external External_library name 'pk_client_install_packages';
function pk_client_new:Pointer;cdecl;external External_library name 'pk_client_new';

procedure InstallPackage;
 
implementation
 
procedure InstallPackage;
var connection: PDBusGConnection;
  proxy: PDBusGProxy;
  error: PGError =nil;
  ret: gboolean;
  arg: PPchar;
  ast: string;
  client: Pointer;
begin
  client := pk_client_new;
  ast := 'amor;;;';
  arg := StringToPPchar(ast, 0);
  ret := pk_client_install_packages(client,arg,@error);
  if not ret then
  begin
    g_warning('failed: %s', [error^.message]);
    g_error_free(error);
  end;
end;

end.
