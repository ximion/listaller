/* packagekit-plugin.vapi generated by vapigen, do not modify. */

[CCode (cprefix = "Pk", gir_namespace = "PackageKitPlugin", gir_version = "1.0", lower_case_cprefix = "pk__")]
namespace PkPlugin {
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", type_id = "pk_backend_get_type ()")]
	public class Backend : GLib.Object {
		[CCode (cname = "pk_backend_new", has_construct_function = false)]
		public Backend ();
		[CCode (cname = "pk_backend_accept_eula")]
		public void accept_eula (string eula_id);
		[CCode (cname = "pk_backend_bool_to_string")]
		public static unowned string bool_to_string (bool value);
		[CCode (cname = "pk_backend_cancel")]
		public void cancel (PkPlugin.BackendJob job);
		[CCode (cname = "pk_backend_destroy")]
		public void destroy ();
		[CCode (cname = "pk_backend_download_packages")]
		public void download_packages (PkPlugin.BackendJob job, string package_ids, string directory);
		[CCode (cname = "pk_backend_get_accepted_eula_string")]
		public string get_accepted_eula_string ();
		[CCode (cname = "pk_backend_get_author")]
		public unowned string get_author ();
		[CCode (cname = "pk_backend_get_categories")]
		public void get_categories (PkPlugin.BackendJob job);
		[CCode (cname = "pk_backend_get_depends")]
		public void get_depends (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string package_ids, bool recursive);
		[CCode (cname = "pk_backend_get_description")]
		public unowned string get_description ();
		[CCode (cname = "pk_backend_get_details")]
		public void get_details (PkPlugin.BackendJob job, string package_ids);
		[CCode (cname = "pk_backend_get_distro_upgrades")]
		public void get_distro_upgrades (PkPlugin.BackendJob job);
		[CCode (cname = "pk_backend_get_files")]
		public void get_files (PkPlugin.BackendJob job, string package_ids);
		[CCode (cname = "pk_backend_get_filters")]
		public PackageKit.Bitfield get_filters ();
		[CCode (cname = "pk_backend_get_groups")]
		public PackageKit.Bitfield get_groups ();
		[CCode (array_length = false, array_null_terminated = true, cname = "pk_backend_get_mime_types")]
		public string[] get_mime_types ();
		[CCode (cname = "pk_backend_get_name")]
		public unowned string get_name ();
		[CCode (cname = "pk_backend_get_packages")]
		public void get_packages (PkPlugin.BackendJob job, PackageKit.Bitfield filters);
		[CCode (cname = "pk_backend_get_repo_list")]
		public void get_repo_list (PkPlugin.BackendJob job, PackageKit.Bitfield filters);
		[CCode (cname = "pk_backend_get_requires")]
		public void get_requires (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string package_ids, bool recursive);
		[CCode (cname = "pk_backend_get_roles")]
		public PackageKit.Bitfield get_roles ();
		[CCode (cname = "pk_backend_get_update_detail")]
		public void get_update_detail (PkPlugin.BackendJob job, string package_ids);
		[CCode (cname = "pk_backend_get_updates")]
		public void get_updates (PkPlugin.BackendJob job, PackageKit.Bitfield filters);
		[CCode (cname = "pk_backend_implement")]
		public void implement (PackageKit.Role role);
		[CCode (cname = "pk_backend_initialize")]
		public void initialize ();
		[CCode (cname = "pk_backend_install_files")]
		public void install_files (PkPlugin.BackendJob job, PackageKit.Bitfield transaction_flags, string full_paths);
		[CCode (cname = "pk_backend_install_packages")]
		public void install_packages (PkPlugin.BackendJob job, PackageKit.Bitfield transaction_flags, string package_ids);
		[CCode (cname = "pk_backend_install_signature")]
		public void install_signature (PkPlugin.BackendJob job, PackageKit.SigType type, string key_id, string package_id);
		[CCode (cname = "pk_backend_is_eula_valid")]
		public bool is_eula_valid (string eula_id);
		[CCode (cname = "pk_backend_is_implemented")]
		public bool is_implemented (PackageKit.Role role);
		[CCode (cname = "pk_backend_is_online")]
		public bool is_online ();
		[CCode (cname = "pk_backend_load")]
		public bool load () throws GLib.Error;
		[CCode (cname = "pk_backend_refresh_cache")]
		public void refresh_cache (PkPlugin.BackendJob job, bool force);
		[CCode (cname = "pk_backend_remove_packages")]
		public void remove_packages (PkPlugin.BackendJob job, PackageKit.Bitfield transaction_flags, string package_ids, bool allow_deps, bool autoremove);
		[CCode (cname = "pk_backend_repair_system")]
		public void repair_system (PkPlugin.BackendJob job, PackageKit.Bitfield transaction_flags);
		[CCode (cname = "pk_backend_repo_enable")]
		public void repo_enable (PkPlugin.BackendJob job, string repo_id, bool enabled);
		[CCode (cname = "pk_backend_repo_list_changed")]
		public bool repo_list_changed ();
		[CCode (cname = "pk_backend_repo_set_data")]
		public void repo_set_data (PkPlugin.BackendJob job, string repo_id, string parameter, string value);
		[CCode (cname = "pk_backend_reset_job")]
		public void reset_job (PkPlugin.BackendJob job);
		[CCode (cname = "pk_backend_resolve")]
		public void resolve (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string packages);
		[CCode (cname = "pk_backend_search_details")]
		public void search_details (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string search);
		[CCode (cname = "pk_backend_search_files")]
		public void search_files (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string search);
		[CCode (cname = "pk_backend_search_groups")]
		public void search_groups (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string search);
		[CCode (cname = "pk_backend_search_names")]
		public void search_names (PkPlugin.BackendJob job, PackageKit.Bitfield filters, string search);
		[CCode (cname = "pk_backend_start_job")]
		public void start_job (PkPlugin.BackendJob job);
		[CCode (cname = "pk_backend_stop_job")]
		public void stop_job (PkPlugin.BackendJob job);
		[CCode (cname = "pk_backend_supports_parallelization")]
		public bool supports_parallelization ();
		[CCode (cname = "pk_backend_unload")]
		public bool unload ();
		[CCode (cname = "pk_backend_update_packages")]
		public void update_packages (PkPlugin.BackendJob job, PackageKit.Bitfield transaction_flags, string package_ids);
		[CCode (cname = "pk_backend_upgrade_system")]
		public void upgrade_system (PkPlugin.BackendJob job, string distro_id, PackageKit.UpgradeKind upgrade_kind);
		[CCode (cname = "pk_backend_watch_file")]
		public bool watch_file (string filename, PkPlugin.BackendFileChanged func);
		[CCode (cname = "pk_backend_what_provides")]
		public void what_provides (PkPlugin.BackendJob job, PackageKit.Bitfield filters, PackageKit.Provides provides, string search);
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", type_id = "pk_backend_job_get_type ()")]
	public class BackendJob : GLib.Object {
		[CCode (cname = "pk_backend_job_new", has_construct_function = false)]
		public BackendJob ();
		[CCode (cname = "pk_backend_job_category")]
		public void category (string parent_id, string cat_id, string name, string summary, string icon);
		[CCode (cname = "pk_backend_job_details")]
		public void details (string package_id, string license, PackageKit.Group group, string description, string url, ulong size);
		[CCode (cname = "pk_backend_job_distro_upgrade")]
		public void distro_upgrade (PackageKit.DistroUpgradeType type, string name, string summary);
		[CCode (cname = "pk_backend_job_eula_required")]
		public void eula_required (string eula_id, string package_id, string vendor_name, string license_agreement);
		[CCode (cname = "pk_backend_job_files")]
		public void files (string package_id, string filelist);
		[CCode (cname = "pk_backend_job_finished")]
		public void finished ();
		[CCode (cname = "pk_backend_job_get_allow_cancel")]
		public bool get_allow_cancel ();
		[CCode (cname = "pk_backend_job_get_backend")]
		public void* get_backend ();
		[CCode (cname = "pk_backend_job_get_background")]
		public PkPlugin.Hint get_background ();
		[CCode (cname = "pk_backend_job_get_cache_age")]
		public uint get_cache_age ();
		[CCode (cname = "pk_backend_job_get_cmdline")]
		public unowned string get_cmdline ();
		[CCode (cname = "pk_backend_job_get_exit_code")]
		public PackageKit.Exit get_exit_code ();
		[CCode (cname = "pk_backend_job_get_frontend_socket")]
		public string get_frontend_socket ();
		[CCode (cname = "pk_backend_job_get_interactive")]
		public PkPlugin.Hint get_interactive ();
		[CCode (cname = "pk_backend_job_get_is_error_set")]
		public bool get_is_error_set ();
		[CCode (cname = "pk_backend_job_get_is_finished")]
		public bool get_is_finished ();
		[CCode (cname = "pk_backend_job_get_locale")]
		public string get_locale ();
		[CCode (cname = "pk_backend_job_get_locked")]
		public bool get_locked ();
		[CCode (cname = "pk_backend_job_get_no_proxy")]
		public string get_no_proxy ();
		[CCode (cname = "pk_backend_job_get_pac")]
		public string get_pac ();
		[CCode (cname = "pk_backend_job_get_parameters")]
		public GLib.Variant get_parameters ();
		[CCode (cname = "pk_backend_job_get_proxy_ftp")]
		public string get_proxy_ftp ();
		[CCode (cname = "pk_backend_job_get_proxy_http")]
		public string get_proxy_http ();
		[CCode (cname = "pk_backend_job_get_proxy_https")]
		public string get_proxy_https ();
		[CCode (cname = "pk_backend_job_get_proxy_socks")]
		public string get_proxy_socks ();
		[CCode (cname = "pk_backend_job_get_role")]
		public PackageKit.Role get_role ();
		[CCode (cname = "pk_backend_job_get_runtime")]
		public uint get_runtime ();
		[CCode (cname = "pk_backend_job_get_started")]
		public bool get_started ();
		[CCode (cname = "pk_backend_job_get_transaction_flags")]
		public PackageKit.Bitfield get_transaction_flags ();
		[CCode (cname = "pk_backend_job_get_uid")]
		public uint get_uid ();
		[CCode (cname = "pk_backend_job_get_user_data")]
		public void* get_user_data ();
		[CCode (cname = "pk_backend_job_get_vfunc_enabled")]
		public bool get_vfunc_enabled (PkPlugin.BackendJobSignal signal_kind);
		[CCode (cname = "pk_backend_job_has_set_error_code")]
		public bool has_set_error_code ();
		[CCode (cname = "pk_backend_job_media_change_required")]
		public void media_change_required (PackageKit.MediaType media_type, string media_id, string media_text);
		[CCode (cname = "pk_backend_job_not_implemented_yet")]
		public void not_implemented_yet (string method);
		[CCode (cname = "pk_backend_job_package")]
		public void package (PackageKit.Info info, string package_id, string summary);
		[CCode (cname = "pk_backend_job_repo_detail")]
		public void repo_detail (string repo_id, string description, bool enabled);
		[CCode (cname = "pk_backend_job_repo_signature_required")]
		public void repo_signature_required (string package_id, string repository_name, string key_url, string key_userid, string key_id, string key_fingerprint, string key_timestamp, PackageKit.SigType type);
		[CCode (cname = "pk_backend_job_require_restart")]
		public void require_restart (PackageKit.Restart restart, string package_id);
		[CCode (cname = "pk_backend_job_reset")]
		public void reset ();
		[CCode (cname = "pk_backend_job_set_allow_cancel")]
		public void set_allow_cancel (bool allow_cancel);
		[CCode (cname = "pk_backend_job_set_backend")]
		public void set_backend (void* backend);
		[CCode (cname = "pk_backend_job_set_background")]
		public void set_background (PkPlugin.Hint background);
		[CCode (cname = "pk_backend_job_set_cache_age")]
		public void set_cache_age (uint cache_age);
		[CCode (cname = "pk_backend_job_set_cmdline")]
		public void set_cmdline (string cmdline);
		[CCode (cname = "pk_backend_job_set_download_size_remaining")]
		public void set_download_size_remaining (uint64 download_size_remaining);
		[CCode (cname = "pk_backend_job_set_exit_code")]
		public void set_exit_code (PackageKit.Exit exit);
		[CCode (cname = "pk_backend_job_set_frontend_socket")]
		public bool set_frontend_socket (string frontend_socket);
		[CCode (cname = "pk_backend_job_set_interactive")]
		public void set_interactive (PkPlugin.Hint interactive);
		[CCode (cname = "pk_backend_job_set_item_progress")]
		public void set_item_progress (string package_id, PackageKit.Status status, uint percentage);
		[CCode (cname = "pk_backend_job_set_locale")]
		public void set_locale (string code);
		[CCode (cname = "pk_backend_job_set_locked")]
		public void set_locked (bool locked);
		[CCode (cname = "pk_backend_job_set_parameters")]
		public void set_parameters (GLib.Variant @params);
		[CCode (cname = "pk_backend_job_set_percentage")]
		public void set_percentage (uint percentage);
		[CCode (cname = "pk_backend_job_set_proxy")]
		public void set_proxy (string proxy_http, string proxy_https, string proxy_ftp, string proxy_socks, string no_proxy, string pac);
		[CCode (cname = "pk_backend_job_set_role")]
		public void set_role (PackageKit.Role role);
		[CCode (cname = "pk_backend_job_set_speed")]
		public void set_speed (uint speed);
		[CCode (cname = "pk_backend_job_set_started")]
		public void set_started (bool started);
		[CCode (cname = "pk_backend_job_set_status")]
		public void set_status (PackageKit.Status status);
		[CCode (cname = "pk_backend_job_set_transaction_flags")]
		public void set_transaction_flags (PackageKit.Bitfield transaction_flags);
		[CCode (cname = "pk_backend_job_set_uid")]
		public void set_uid (uint uid);
		[CCode (cname = "pk_backend_job_set_user_data")]
		public void set_user_data (void* user_data);
		[CCode (cname = "pk_backend_job_set_vfunc")]
		public void set_vfunc (PkPlugin.BackendJobSignal signal_kind, PkPlugin.BackendJobVFunc vfunc);
		[CCode (cname = "pk_backend_job_thread_create")]
		public bool thread_create (owned PkPlugin.BackendJobThreadFunc func);
		[CCode (cname = "pk_backend_job_update_detail")]
		public void update_detail (string package_id, string updates, string obsoletes, string vendor_urls, string bugzilla_urls, string cve_urls, PackageKit.Restart restart, string update_text, string changelog, PackageKit.UpdateState state, string issued, string updated);
		[CCode (cname = "pk_backend_job_use_background")]
		public bool use_background ();
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", type_id = "pk_conf_get_type ()")]
	public class Conf : GLib.Object {
		[CCode (cname = "pk_conf_new", has_construct_function = false)]
		public Conf ();
		[CCode (cname = "pk_conf_get_bool")]
		public bool get_bool (string key);
		[CCode (cname = "pk_conf_get_filename")]
		public static string get_filename ();
		[CCode (cname = "pk_conf_get_int")]
		public int get_int (string key);
		[CCode (cname = "pk_conf_get_string")]
		public string get_string (string key);
		[CCode (array_length = false, array_null_terminated = true, cname = "pk_conf_get_strv")]
		public unowned string[] get_strv (string key);
		[CCode (cname = "pk_conf_set_bool")]
		public void set_bool (string key, bool value);
		[CCode (cname = "pk_conf_set_string")]
		public void set_string (string key, string value);
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", type_id = "pk_transaction_get_type ()")]
	public class Transaction : GLib.Object {
		[CCode (cname = "pk_transaction_new", has_construct_function = false)]
		public Transaction ();
		[CCode (cname = "pk_transaction_add_supported_content_type")]
		public void add_supported_content_type (string mime_type);
		[CCode (cname = "pk_transaction_cancel_bg")]
		public void cancel_bg ();
		[CCode (cname = "pk_transaction_get_backend_job")]
		public unowned PkPlugin.BackendJob get_backend_job ();
		[CCode (cname = "pk_transaction_get_conf")]
		public unowned PkPlugin.Conf get_conf ();
		[CCode (array_length = false, array_null_terminated = true, cname = "pk_transaction_get_full_paths")]
		public unowned string[] get_full_paths ();
		[CCode (array_length = false, array_null_terminated = true, cname = "pk_transaction_get_package_ids")]
		public unowned string[] get_package_ids ();
		[CCode (cname = "pk_transaction_get_results")]
		public unowned PackageKit.Results get_results ();
		[CCode (cname = "pk_transaction_get_role")]
		public PackageKit.Role get_role ();
		[CCode (cname = "pk_transaction_get_state")]
		public PkPlugin.TransactionState get_state ();
		[CCode (cname = "pk_transaction_get_tid")]
		public unowned string get_tid ();
		[CCode (cname = "pk_transaction_get_transaction_flags")]
		public PackageKit.Bitfield get_transaction_flags ();
		[CCode (cname = "pk_transaction_get_uid")]
		public uint get_uid ();
		[CCode (array_length = false, array_null_terminated = true, cname = "pk_transaction_get_values")]
		public unowned string[] get_values ();
		[CCode (cname = "pk_transaction_is_exclusive")]
		public bool is_exclusive ();
		[CCode (cname = "pk_transaction_is_finished_with_lock_required")]
		public bool is_finished_with_lock_required ();
		[CCode (cname = "pk_transaction_make_exclusive")]
		public void make_exclusive ();
		[CCode (cname = "pk_transaction_reset_after_lock_error")]
		public void reset_after_lock_error ();
		[CCode (cname = "pk_transaction_run")]
		public bool run ();
		[CCode (cname = "pk_transaction_set_backend")]
		public void set_backend (PkPlugin.Backend backend);
		[CCode (cname = "pk_transaction_set_full_paths")]
		public void set_full_paths (string full_paths);
		[CCode (cname = "pk_transaction_set_package_ids")]
		public void set_package_ids (string package_ids);
		[CCode (cname = "pk_transaction_set_plugins")]
		public void set_plugins (GLib.GenericArray<void*> plugins);
		[CCode (cname = "pk_transaction_set_state")]
		public bool set_state (PkPlugin.TransactionState state);
		[CCode (cname = "pk_transaction_set_supported_roles")]
		public void set_supported_roles (GLib.GenericArray<void*> plugins);
		[CCode (cname = "pk_transaction_signals_reset")]
		public void signals_reset (PkPlugin.BackendJob job);
		[CCode (cname = "pk_transaction_skip_auth_checks")]
		public void skip_auth_checks (bool skip_checks);
		[CCode (cname = "pk_transaction_state_to_string")]
		public static unowned string state_to_string (PkPlugin.TransactionState state);
		public signal void finished ();
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", has_type_id = false)]
	public struct Plugin {
		public weak PkPlugin.Backend backend;
		public weak PkPlugin.BackendJob job;
		[CCode (cname = "pk_plugin_destroy")]
		public void destroy ();
		[CCode (cname = "pk_plugin_initialize")]
		public void initialize ();
		[CCode (cname = "pk_plugin_state_changed")]
		public void state_changed ();
		[CCode (cname = "pk_plugin_transaction_content_types")]
		public void transaction_content_types (PkPlugin.Transaction transaction);
		[CCode (cname = "pk_plugin_transaction_finished_end")]
		public void transaction_finished_end (PkPlugin.Transaction transaction);
		[CCode (cname = "pk_plugin_transaction_finished_results")]
		public void transaction_finished_results (PkPlugin.Transaction transaction);
		[CCode (cname = "pk_plugin_transaction_run")]
		public void transaction_run (PkPlugin.Transaction transaction);
		[CCode (cname = "pk_plugin_transaction_started")]
		public void transaction_started (PkPlugin.Transaction transaction);
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cprefix = "PK_BACKEND_SIGNAL_")]
	public enum BackendJobSignal {
		ALLOW_CANCEL,
		DETAILS,
		ERROR_CODE,
		DISTRO_UPGRADE,
		FINISHED,
		MESSAGE,
		PACKAGE,
		ITEM_PROGRESS,
		FILES,
		PERCENTAGE,
		REMAINING,
		SPEED,
		DOWNLOAD_SIZE_REMAINING,
		REPO_DETAIL,
		REPO_SIGNATURE_REQUIRED,
		EULA_REQUIRED,
		MEDIA_CHANGE_REQUIRED,
		REQUIRE_RESTART,
		STATUS_CHANGED,
		LOCKED_CHANGED,
		UPDATE_DETAIL,
		CATEGORY,
		LAST
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "PkHintEnum", cprefix = "PK_HINT_ENUM_")]
	[GIR (name = "HintEnum")]
	public enum Hint {
		FALSE,
		TRUE,
		UNSET,
		INVALID,
		LAST
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cprefix = "PK_PLUGIN_PHASE_")]
	public enum PluginPhase {
		INIT,
		TRANSACTION_CONTENT_TYPES,
		TRANSACTION_RUN,
		TRANSACTION_STARTED,
		TRANSACTION_FINISHED_RESULTS,
		TRANSACTION_FINISHED_END,
		DESTROY,
		STATE_CHANGED,
		UNKNOWN
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cprefix = "PK_TRANSACTION_STATE_")]
	public enum TransactionState {
		NEW,
		WAITING_FOR_AUTH,
		COMMITTED,
		READY,
		RUNNING,
		FINISHED,
		UNKNOWN
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cprefix = "PK_TRANSACTION_ERROR_")]
	public errordomain TransactionError {
		[CCode (cname = "PK_TRANSACTION_ERROR_DENIED")]
		PERMISSIONDENIED,
		[CCode (cname = "PK_TRANSACTION_ERROR_NOT_RUNNING")]
		NOTRUNNING,
		[CCode (cname = "PK_TRANSACTION_ERROR_NO_ROLE")]
		NOROLE,
		[CCode (cname = "PK_TRANSACTION_ERROR_CANNOT_CANCEL")]
		CANNOTCANCEL,
		[CCode (cname = "PK_TRANSACTION_ERROR_NOT_SUPPORTED")]
		NOTSUPPORTED,
		[CCode (cname = "PK_TRANSACTION_ERROR_NO_SUCH_TRANSACTION")]
		NOSUCHTRANSACTION,
		[CCode (cname = "PK_TRANSACTION_ERROR_NO_SUCH_FILE")]
		NOSUCHFILE,
		[CCode (cname = "PK_TRANSACTION_ERROR_NO_SUCH_DIRECTORY")]
		NOSUCHDIRECTORY,
		[CCode (cname = "PK_TRANSACTION_ERROR_TRANSACTION_EXISTS_WITH_ROLE")]
		TRANSACTIONEXISTSWITHROLE,
		[CCode (cname = "PK_TRANSACTION_ERROR_REFUSED_BY_POLICY")]
		REFUSEDBYPOLICY,
		[CCode (cname = "PK_TRANSACTION_ERROR_PACKAGE_ID_INVALID")]
		PACKAGEIDINVALID,
		[CCode (cname = "PK_TRANSACTION_ERROR_SEARCH_INVALID")]
		SEARCHINVALID,
		[CCode (cname = "PK_TRANSACTION_ERROR_SEARCH_PATH_INVALID")]
		SEARCHPATHINVALID,
		[CCode (cname = "PK_TRANSACTION_ERROR_FILTER_INVALID")]
		FILTERINVALID,
		[CCode (cname = "PK_TRANSACTION_ERROR_INPUT_INVALID")]
		INPUTINVALID,
		[CCode (cname = "PK_TRANSACTION_ERROR_INVALID_STATE")]
		INVALIDSTATE,
		[CCode (cname = "PK_TRANSACTION_ERROR_INITIALIZE_FAILED")]
		INITIALIZEFAILED,
		[CCode (cname = "PK_TRANSACTION_ERROR_COMMIT_FAILED")]
		COMMITFAILED,
		[CCode (cname = "PK_TRANSACTION_ERROR_PACK_INVALID")]
		PACKINVALID,
		[CCode (cname = "PK_TRANSACTION_ERROR_MIME_TYPE_NOT_SUPPORTED")]
		MIMETYPENOTSUPPORTED,
		[CCode (cname = "PK_TRANSACTION_ERROR_INVALID_PROVIDE")]
		INVALIDPROVIDE,
		[CCode (cname = "PK_TRANSACTION_ERROR_NUMBER_OF_PACKAGES_INVALID")]
		NUMBEROFPACKAGESINVALID
	}
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", has_target = false)]
	public delegate void BackendFileChanged (PkPlugin.Backend backend, void* data);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", instance_pos = 2.9)]
	public delegate void BackendJobThreadFunc (PkPlugin.BackendJob job, GLib.Variant @params);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", instance_pos = 2.9)]
	public delegate void BackendJobVFunc (PkPlugin.BackendJob job, void* object);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", has_target = false)]
	public delegate void PluginFunc (PkPlugin.Plugin plugin);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", has_target = false)]
	public delegate unowned string PluginGetDescFunc ();
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", has_target = false)]
	public delegate void PluginTransactionFunc (PkPlugin.Plugin plugin, PkPlugin.Transaction transaction);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "PK_BACKEND_PERCENTAGE_INVALID")]
	public const int BACKEND_PERCENTAGE_INVALID;
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "PK_CONF_VALUE_INT_MISSING")]
	public const int CONF_VALUE_INT_MISSING;
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "PK_TRANSACTION_ALL_BACKEND_SIGNALS")]
	public const int TRANSACTION_ALL_BACKEND_SIGNALS;
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "PK_TRANSACTION_NO_BACKEND_SIGNALS")]
	public const int TRANSACTION_NO_BACKEND_SIGNALS;
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_directory_remove_contents")]
	public static bool directory_remove_contents (string directory);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_hint_enum_from_string")]
	public static PkPlugin.Hint hint_enum_from_string (string hint);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_hint_enum_to_string")]
	public static unowned string hint_enum_to_string (PkPlugin.Hint hint);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_load_introspection")]
	public static GLib.DBusNodeInfo load_introspection (string filename) throws GLib.Error;
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_plugin_get_description")]
	public static unowned string plugin_get_description ();
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_strlen")]
	public static uint strlen (string text, uint len);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_strtoint")]
	public static bool strtoint (string text, int value);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_strtouint")]
	public static bool strtouint (string text, uint value);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_strtouint64")]
	public static bool strtouint64 (string text, uint64 value);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_strzero")]
	public static bool strzero (string text);
	[CCode (cheader_filename = "plugin/packagekit-plugin.h", cname = "pk_transaction_error_quark")]
	public static GLib.Quark transaction_error_quark ();
}
