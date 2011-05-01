
[CCode (cprefix = "Pk", lower_case_cprefix = "pk_", gir_namespace = "PackageKitGlib", gir_version = "1.0")]
namespace Pk {
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Catalog : GLib.Object {
		public Pk.CatalogPrivate priv;
		[CCode (has_construct_function = false)]
		public Catalog ();
		public static GLib.Quark error_quark ();
		public async GLib.GenericArray<weak void*> lookup_async (string filename, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		public static void test (void* user_data);
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Category : Pk.Source {
		public Pk.CategoryPrivate priv;
		[CCode (has_construct_function = false)]
		public Category ();
		public unowned string get_icon ();
		public unowned string get_id ();
		public unowned string get_name ();
		public unowned string get_parent_id ();
		public unowned string get_summary ();
		public void set_icon (string icon);
		public void set_id (string cat_id);
		public void set_name (string name);
		public void set_parent_id (string parent_id);
		public void set_summary (string summary);
		[NoAccessorMethod]
		public string cat_id { get; set; }
		public string icon { get; set; }
		public string name { get; set; }
		public string parent_id { get; set; }
		public string summary { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Client : GLib.Object {
		public Pk.ClientPrivate priv;
		[CCode (cname = "pk_client_new", has_construct_function = false)]
		public Client ();
		[CCode (cname = "pk_client_accept_eula")]
		public Pk.Results accept_eula (string eula_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_accept_eula_async")]
		public async void accept_eula_async (string eula_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_adopt")]
		public Pk.Results adopt (string transaction_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_adopt_async")]
		public async void adopt_async (string transaction_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[NoWrapper]
		public virtual void changed ();
		[CCode (cname = "pk_client_download_packages")]
		public Pk.Results download_packages (string package_ids, string directory, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_download_packages_async")]
		public async void download_packages_async (string package_ids, string directory, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_error_quark")]
		public static GLib.Quark error_quark ();
		[CCode (cname = "pk_client_generic_finish")]
		public Pk.Results generic_finish (GLib.AsyncResult res) throws GLib.Error;
		[CCode (cname = "pk_client_get_background")]
		public bool get_background ();
		[CCode (cname = "pk_client_get_cache_age")]
		public uint get_cache_age ();
		[CCode (cname = "pk_client_get_categories")]
		public Pk.Results get_categories (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_categories_async")]
		public async void get_categories_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_depends")]
		public Pk.Results get_depends (Pk.Bitfield filters, string package_ids, bool recursive, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_depends_async")]
		public async void get_depends_async (Pk.Bitfield filters, string package_ids, bool recursive, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_details")]
		public Pk.Results get_details (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_details_async")]
		public async void get_details_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_distro_upgrades")]
		public Pk.Results get_distro_upgrades (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_distro_upgrades_async")]
		public async void get_distro_upgrades_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_files")]
		public Pk.Results get_files (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_files_async")]
		public async void get_files_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_idle")]
		public bool get_idle ();
		[CCode (cname = "pk_client_get_interactive")]
		public bool get_interactive ();
		[CCode (cname = "pk_client_get_locale")]
		public unowned string get_locale ();
		[CCode (cname = "pk_client_get_old_transactions")]
		public Pk.Results get_old_transactions (uint number, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_old_transactions_async")]
		public async void get_old_transactions_async (uint number, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_packages")]
		public Pk.Results get_packages (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_packages_async")]
		public async void get_packages_async (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_progress")]
		public Pk.Progress get_progress (string transaction_id, GLib.Cancellable? cancellable) throws GLib.Error;
		[CCode (cname = "pk_client_get_progress_async")]
		public async Pk.Progress get_progress_async (string transaction_id, GLib.Cancellable? cancellable) throws GLib.Error;
		[CCode (cname = "pk_client_get_repo_list")]
		public Pk.Results get_repo_list (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_repo_list_async")]
		public async void get_repo_list_async (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_requires")]
		public Pk.Results get_requires (Pk.Bitfield filters, string package_ids, bool recursive, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_requires_async")]
		public async void get_requires_async (Pk.Bitfield filters, string package_ids, bool recursive, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_update_detail")]
		public Pk.Results get_update_detail (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_update_detail_async")]
		public async void get_update_detail_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_get_updates")]
		public Pk.Results get_updates (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_get_updates_async")]
		public async void get_updates_async (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_install_files")]
		public Pk.Results install_files (bool only_trusted, string files, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_install_files_async")]
		public async void install_files_async (bool only_trusted, string files, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_install_packages")]
		public Pk.Results install_packages (bool only_trusted, string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_install_packages_async")]
		public async void install_packages_async (bool only_trusted, string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_install_signature")]
		public Pk.Results install_signature (Pk.SigTypeEnum type, string key_id, string package_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_install_signature_async")]
		public async void install_signature_async (Pk.SigTypeEnum type, string key_id, string package_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_refresh_cache")]
		public Pk.Results refresh_cache (bool force, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_refresh_cache_async")]
		public async void refresh_cache_async (bool force, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_remove_packages")]
		public Pk.Results remove_packages (string package_ids, bool allow_deps, bool autoremove, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_remove_packages_async")]
		public async void remove_packages_async (string package_ids, bool allow_deps, bool autoremove, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_repo_enable")]
		public Pk.Results repo_enable (string repo_id, bool enabled, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_repo_enable_async")]
		public async void repo_enable_async (string repo_id, bool enabled, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_repo_set_data")]
		public Pk.Results repo_set_data (string repo_id, string parameter, string value, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_repo_set_data_async")]
		public async void repo_set_data_async (string repo_id, string parameter, string value, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_resolve")]
		public Pk.Results resolve (Pk.Bitfield filters, string packages, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_resolve_async")]
		public async void resolve_async (Pk.Bitfield filters, string packages, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_rollback")]
		public Pk.Results rollback (string transaction_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_rollback_async")]
		public async void rollback_async (string transaction_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_search_details")]
		public Pk.Results search_details (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_search_details_async")]
		public async void search_details_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_search_files")]
		public Pk.Results search_files (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_search_files_async")]
		public async void search_files_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_search_groups")]
		public Pk.Results search_groups (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_search_groups_async")]
		public async void search_groups_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_search_names")]
		public Pk.Results search_names (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_search_names_async")]
		public async void search_names_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_set_background")]
		public void set_background (bool background);
		[CCode (cname = "pk_client_set_cache_age")]
		public void set_cache_age (uint cache_age);
		[CCode (cname = "pk_client_set_interactive")]
		public void set_interactive (bool interactive);
		[CCode (cname = "pk_client_set_locale")]
		public void set_locale (string locale);
		[CCode (cname = "pk_client_simulate_install_files")]
		public Pk.Results simulate_install_files (string files, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_simulate_install_files_async")]
		public async void simulate_install_files_async (string files, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_simulate_install_packages")]
		public Pk.Results simulate_install_packages (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_simulate_install_packages_async")]
		public async void simulate_install_packages_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_simulate_remove_packages")]
		public Pk.Results simulate_remove_packages (string package_ids, bool autoremove, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_simulate_remove_packages_async")]
		public async void simulate_remove_packages_async (string package_ids, bool autoremove, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_simulate_update_packages")]
		public Pk.Results simulate_update_packages (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_simulate_update_packages_async")]
		public async void simulate_update_packages_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_test")]
		public static void test (void* user_data);
		[CCode (cname = "pk_client_update_packages")]
		public Pk.Results update_packages (bool only_trusted, string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_update_packages_async")]
		public async void update_packages_async (bool only_trusted, string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_update_system")]
		public Pk.Results update_system (bool only_trusted, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_update_system_async")]
		public async void update_system_async (bool only_trusted, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_upgrade_system")]
		public Pk.Results upgrade_system (string distro_id, Pk.UpgradeKindEnum upgrade_kind, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_upgrade_system_async")]
		public async void upgrade_system_async (string distro_id, Pk.UpgradeKindEnum upgrade_kind, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_client_what_provides")]
		public Pk.Results what_provides (Pk.Bitfield filters, Pk.ProvidesEnum provides, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback) throws GLib.Error;
		[CCode (cname = "pk_client_what_provides_async")]
		public async void what_provides_async (Pk.Bitfield filters, Pk.ProvidesEnum provides, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		public bool background { get; set; }
		public uint cache_age { get; set; }
		public bool idle { get; }
		public bool interactive { get; set; }
		public string locale { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class ClientHelper : GLib.Object {
		public Pk.ClientHelperPrivate priv;
		[CCode (has_construct_function = false)]
		public ClientHelper ();
		public bool start (string socket_filename, string argv, string envp) throws GLib.Error;
		public bool stop () throws GLib.Error;
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Control : GLib.Object {
		public Pk.ControlPrivate priv;
		[CCode (has_construct_function = false)]
		public Control ();
		public async Pk.AuthorizeEnum can_authorize_async (string action_id, GLib.Cancellable? cancellable) throws GLib.Error;
		[NoWrapper]
		public virtual void connection_changed (bool connected);
		public static GLib.Quark error_quark ();
		public async string get_daemon_state_async (GLib.Cancellable? cancellable) throws GLib.Error;
		public async Pk.NetworkEnum get_network_state_async (GLib.Cancellable? cancellable) throws GLib.Error;
		public bool get_properties (GLib.Cancellable? cancellable) throws GLib.Error;
		public async bool get_properties_async (GLib.Cancellable? cancellable) throws GLib.Error;
		public async string get_tid_async (GLib.Cancellable? cancellable) throws GLib.Error;
		public async uint get_time_since_action_async (Pk.RoleEnum role, GLib.Cancellable? cancellable) throws GLib.Error;
		public string[] get_transaction_list (GLib.Cancellable? cancellable) throws GLib.Error;
		public async string[] get_transaction_list_async (GLib.Cancellable? cancellable) throws GLib.Error;
		[NoWrapper]
		public virtual void network_state_changed ();
		public bool set_proxy (string proxy_http, string proxy_ftp, GLib.Cancellable? cancellable) throws GLib.Error;
		public async bool set_proxy_async (string proxy_http, string proxy_ftp, GLib.Cancellable? cancellable) throws GLib.Error;
		public bool set_root (string root, GLib.Cancellable? cancellable) throws GLib.Error;
		public async bool set_root_async (string root, GLib.Cancellable? cancellable) throws GLib.Error;
		public bool suggest_daemon_quit (GLib.Cancellable? cancellable) throws GLib.Error;
		public async bool suggest_daemon_quit_async (GLib.Cancellable? cancellable) throws GLib.Error;
		public static void test (void* user_data);
		[NoAccessorMethod]
		public string backend_author { get; set; }
		[NoAccessorMethod]
		public string backend_description { get; set; }
		[NoAccessorMethod]
		public string backend_name { get; set; }
		[NoAccessorMethod]
		public bool connected { get; set; }
		[NoAccessorMethod]
		public string distro_id { get; set; }
		[NoAccessorMethod]
		public uint64 filters { get; set; }
		[NoAccessorMethod]
		public uint64 groups { get; set; }
		[NoAccessorMethod]
		public virtual bool locked { get; set; }
		[NoAccessorMethod]
		public string mime_types { get; set; }
		[NoAccessorMethod]
		public uint network_state { get; set; }
		[NoAccessorMethod]
		public uint64 roles { get; set; }
		[NoAccessorMethod]
		public uint version_major { get; }
		[NoAccessorMethod]
		public uint version_micro { get; }
		[NoAccessorMethod]
		public uint version_minor { get; }
		public virtual signal void repo_list_changed ();
		public virtual signal void restart_schedule ();
		public virtual signal void transaction_list_changed (string[] control);
		public virtual signal void updates_changed ();
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Desktop : GLib.Object {
		public Pk.DesktopPrivate priv;
		[CCode (has_construct_function = false)]
		public Desktop ();
		public GLib.GenericArray<void*> get_files_for_package (string package) throws GLib.Error;
		public string get_package_for_file (string filename) throws GLib.Error;
		public GLib.GenericArray<void*> get_shown_for_package (string package) throws GLib.Error;
		public bool open_database () throws GLib.Error;
		public static void test (void* user_data);
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Details : Pk.Source {
		public Pk.DetailsPrivate priv;
		[CCode (has_construct_function = false)]
		public Details ();
		[NoAccessorMethod]
		public string description { get; set; }
		[NoAccessorMethod]
		public uint group { get; set; }
		[NoAccessorMethod]
		public string license { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public uint64 size { get; set; }
		[NoAccessorMethod]
		public string url { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class DistroUpgrade : Pk.Source {
		public Pk.DistroUpgradePrivate priv;
		[CCode (has_construct_function = false)]
		public DistroUpgrade ();
		public static Pk.DistroUpgradeEnum enum_from_string (string upgrade);
		public static unowned string enum_to_string (Pk.DistroUpgradeEnum upgrade);
		public unowned string get_id ();
		public Pk.DistroUpgradeEnum get_state ();
		public unowned string get_summary ();
		[NoAccessorMethod]
		public string name { get; set; }
		public uint state { get; set; }
		public string summary { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Error : Pk.Source {
		public Pk.ErrorPrivate priv;
		[CCode (has_construct_function = false)]
		public Error ();
		public static Pk.ErrorEnum enum_from_string (string code);
		public static unowned string enum_to_string (Pk.ErrorEnum code);
		public Pk.ErrorEnum get_code ();
		public unowned string get_details ();
		public uint code { get; set; }
		public string details { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class EulaRequired : Pk.Source {
		public Pk.EulaRequiredPrivate priv;
		[CCode (has_construct_function = false)]
		public EulaRequired ();
		[NoAccessorMethod]
		public string eula_id { get; set; }
		[NoAccessorMethod]
		public string license_agreement { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public string vendor_name { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Files : Pk.Source {
		public Pk.FilesPrivate priv;
		[CCode (has_construct_function = false)]
		public Files ();
		[NoAccessorMethod]
		public string[] files { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class MediaChangeRequired : Pk.Source {
		public Pk.MediaChangeRequiredPrivate priv;
		[CCode (has_construct_function = false)]
		public MediaChangeRequired ();
		[NoAccessorMethod]
		public string media_id { get; set; }
		[NoAccessorMethod]
		public string media_text { get; set; }
		[NoAccessorMethod]
		public uint media_type { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Message : Pk.Source {
		public Pk.MessagePrivate priv;
		[CCode (has_construct_function = false)]
		public Message ();
		public static Pk.MessageEnum enum_from_string (string message);
		public static unowned string enum_to_string (Pk.MessageEnum message);
		public unowned string get_details ();
		public Pk.MessageEnum get_kind ();
		public string details { get; set; }
		[NoAccessorMethod]
		public uint type { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Package : Pk.Source {
		public Pk.PackagePrivate priv;
		[CCode (has_construct_function = false)]
		public Package ();
		public bool equal (Pk.Package package2);
		public bool equal_id (Pk.Package package2);
		public unowned string get_arch ();
		public unowned string get_data ();
		public unowned string get_id ();
		public Pk.InfoEnum get_info ();
		public unowned string get_name ();
		public unowned string get_summary ();
		public unowned string get_version ();
		public static string id_build (string name, string version, string arch, string data);
		public static bool id_check (string package_id);
		public static bool id_equal_fuzzy_arch (string package_id1, string package_id2);
		public static string[] id_split (string package_id);
		public static void id_test (void* user_data);
		public static string id_to_printable (string package_id);
		public static string[] ids_add_id (string package_ids, string package_id);
		public static string[] ids_add_ids (string package_ids, string package_ids_new);
		public static bool ids_check (string package_ids);
		public static string[] ids_from_id (string package_id);
		public static string[] ids_from_string (string package_id);
		public static bool ids_present_id (string package_ids, string package_id);
		public static string[] ids_remove_id (string package_ids, string package_id);
		public static void ids_test (void* user_data);
		public static string ids_to_string (string package_ids);
		public void print ();
		public bool set_id (string package_id) throws GLib.Error;
		public static void test (void* user_data);
		[NoAccessorMethod]
		public string description { get; set; }
		[NoAccessorMethod]
		public uint group { get; set; }
		public uint info { get; set; }
		[NoAccessorMethod]
		public string license { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public uint64 size { get; set; }
		public string summary { get; set; }
		[NoAccessorMethod]
		public string update_bugzilla_url { get; set; }
		[NoAccessorMethod]
		public string update_changelog { get; set; }
		[NoAccessorMethod]
		public string update_cve_url { get; set; }
		[NoAccessorMethod]
		public string update_issued { get; set; }
		[NoAccessorMethod]
		public string update_obsoletes { get; set; }
		[NoAccessorMethod]
		public uint update_restart { get; set; }
		[NoAccessorMethod]
		public uint update_state { get; set; }
		[NoAccessorMethod]
		public string update_text { get; set; }
		[NoAccessorMethod]
		public string update_updated { get; set; }
		[NoAccessorMethod]
		public string update_updates { get; set; }
		[NoAccessorMethod]
		public string update_vendor_url { get; set; }
		[NoAccessorMethod]
		public string url { get; set; }
		public virtual signal void changed ();
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class PackageSack : GLib.Object {
		public Pk.PackageSackPrivate priv;
		[CCode (cname = "pk_package_sack_new", has_construct_function = false)]
		public PackageSack ();
		[CCode (cname = "pk_package_sack_add_package")]
		public bool add_package (Pk.Package package);
		[CCode (cname = "pk_package_sack_add_package_by_id")]
		public bool add_package_by_id (string package_id) throws GLib.Error;
		[NoWrapper]
		public virtual void changed ();
		[CCode (cname = "pk_package_sack_clear")]
		public void clear ();
		[CCode (cname = "pk_package_sack_filter")]
		public Pk.PackageSack filter (Pk.PackageSackFilterFunc filter_cb);
		[CCode (cname = "pk_package_sack_filter_by_info")]
		public Pk.PackageSack filter_by_info (Pk.InfoEnum info);
		[CCode (cname = "pk_package_sack_find_by_id")]
		public Pk.Package find_by_id (string package_id);
		[CCode (cname = "pk_package_sack_get_array")]
		public GLib.GenericArray<void*> get_array ();
		[CCode (cname = "pk_package_sack_get_details")]
		public bool get_details (GLib.Cancellable? cancellable) throws GLib.Error;
		[CCode (cname = "pk_package_sack_get_details_async")]
		public async void get_details_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_package_sack_get_ids")]
		public string[] get_ids ();
		[CCode (cname = "pk_package_sack_get_size")]
		public uint get_size ();
		[CCode (cname = "pk_package_sack_get_total_bytes")]
		public uint64 get_total_bytes ();
		[CCode (cname = "pk_package_sack_get_update_detail")]
		public bool get_update_detail (GLib.Cancellable? cancellable) throws GLib.Error;
		[CCode (cname = "pk_package_sack_get_update_detail_async")]
		public async void get_update_detail_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_package_sack_merge_generic_finish")]
		public bool merge_generic_finish (GLib.AsyncResult res) throws GLib.Error;
		[CCode (cname = "pk_package_sack_remove_by_filter")]
		public bool remove_by_filter (Pk.PackageSackFilterFunc filter_cb);
		[CCode (cname = "pk_package_sack_remove_package")]
		public bool remove_package (Pk.Package package);
		[CCode (cname = "pk_package_sack_remove_package_by_id")]
		public bool remove_package_by_id (string package_id);
		[CCode (cname = "pk_package_sack_resolve")]
		public bool resolve (GLib.Cancellable? cancellable) throws GLib.Error;
		[CCode (cname = "pk_package_sack_resolve_async")]
		public async void resolve_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_package_sack_sort")]
		public void sort (Pk.PackageSackSortType type);
		[CCode (cname = "pk_package_sack_test")]
		public static void test (void* user_data);
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Progress : GLib.Object {
		public Pk.ProgressPrivate priv;
		[CCode (has_construct_function = false)]
		public Progress ();
		public bool set_allow_cancel (bool allow_cancel);
		public bool set_caller_active (bool caller_active);
		public bool set_elapsed_time (uint elapsed_time);
		public bool set_package (Pk.Package package);
		public bool set_package_id (string package_id);
		public bool set_percentage (int percentage);
		public bool set_remaining_time (uint remaining_time);
		public bool set_role (Pk.RoleEnum role);
		public bool set_speed (uint speed);
		public bool set_status (Pk.StatusEnum status);
		public bool set_subpercentage (int subpercentage);
		public bool set_transaction_id (string package_id);
		public bool set_uid (uint uid);
		public static void test (void* user_data);
		[NoAccessorMethod]
		public bool allow_cancel { get; set; }
		[NoAccessorMethod]
		public bool caller_active { get; set; }
		[NoAccessorMethod]
		public uint elapsed_time { get; set; }
		[NoAccessorMethod]
		public Pk.Package package { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public int percentage { get; set; }
		[NoAccessorMethod]
		public uint remaining_time { get; set; }
		[NoAccessorMethod]
		public uint role { get; set; }
		[NoAccessorMethod]
		public uint speed { get; set; }
		[NoAccessorMethod]
		public uint status { get; set; }
		[NoAccessorMethod]
		public int subpercentage { get; set; }
		[NoAccessorMethod]
		public string transaction_id { get; set; }
		[NoAccessorMethod]
		public uint uid { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class RepoDetail : Pk.Source {
		public Pk.RepoDetailPrivate priv;
		[CCode (has_construct_function = false)]
		public RepoDetail ();
		[NoAccessorMethod]
		public string description { get; set; }
		[NoAccessorMethod]
		public bool enabled { get; set; }
		[NoAccessorMethod]
		public string repo_id { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class RepoSignatureRequired : Pk.Source {
		public Pk.RepoSignatureRequiredPrivate priv;
		[CCode (has_construct_function = false)]
		public RepoSignatureRequired ();
		[NoAccessorMethod]
		public string key_fingerprint { get; set; }
		[NoAccessorMethod]
		public string key_id { get; set; }
		[NoAccessorMethod]
		public string key_timestamp { get; set; }
		[NoAccessorMethod]
		public string key_url { get; set; }
		[NoAccessorMethod]
		public string key_userid { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public string repository_name { get; set; }
		[NoAccessorMethod]
		public uint type { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class RequireRestart : Pk.Source {
		public Pk.RequireRestartPrivate priv;
		[CCode (has_construct_function = false)]
		public RequireRestart ();
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public uint restart { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Results : GLib.Object {
		public Pk.ResultsPrivate priv;
		[CCode (has_construct_function = false)]
		public Results ();
		public bool add_category (Pk.Category item);
		public bool add_details (Pk.Details item);
		public bool add_distro_upgrade (Pk.DistroUpgrade item);
		public bool add_eula_required (Pk.EulaRequired item);
		public bool add_files (Pk.Files item);
		public bool add_media_change_required (Pk.MediaChangeRequired item);
		public bool add_message (Pk.Message item);
		public bool add_package (Pk.Package item);
		public bool add_repo_detail (Pk.RepoDetail item);
		public bool add_repo_signature_required (Pk.RepoSignatureRequired item);
		public bool add_require_restart (Pk.RequireRestart item);
		public bool add_transaction (Pk.TransactionPast item);
		public bool add_update_detail (Pk.UpdateDetail item);
		public GLib.GenericArray<void*> get_category_array ();
		public GLib.GenericArray<void*> get_details_array ();
		public GLib.GenericArray<void*> get_distro_upgrade_array ();
		public Pk.Error get_error_code ();
		public GLib.GenericArray<void*> get_eula_required_array ();
		public Pk.ExitEnum get_exit_code ();
		public GLib.GenericArray<void*> get_files_array ();
		public GLib.GenericArray<void*> get_media_change_required_array ();
		public GLib.GenericArray<void*> get_message_array ();
		public GLib.GenericArray<void*> get_package_array ();
		public Pk.PackageSack get_package_sack ();
		public GLib.GenericArray<void*> get_repo_detail_array ();
		public GLib.GenericArray<void*> get_repo_signature_required_array ();
		public GLib.GenericArray<void*> get_require_restart_array ();
		public Pk.RestartEnum get_require_restart_worst ();
		public GLib.GenericArray<void*> get_transaction_array ();
		public GLib.GenericArray<void*> get_update_detail_array ();
		public bool set_error_code (Pk.Error item);
		public bool set_exit_code (Pk.ExitEnum exit_enum);
		public static void test (void* user_data);
		[NoAccessorMethod]
		public uint inputs { get; set; }
		[NoAccessorMethod]
		public Pk.Progress progress { get; set; }
		[NoAccessorMethod]
		public uint role { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class ServicePack : GLib.Object {
		public Pk.ServicePackPrivate priv;
		[CCode (has_construct_function = false)]
		public ServicePack ();
		public bool check_valid (string filename) throws GLib.Error;
		public async void create_for_package_ids_async (string filename, string package_ids, string package_ids_exclude, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		public async void create_for_updates_async (string filename, string package_ids_exclude, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		public static GLib.Quark error_quark ();
		public bool generic_finish (GLib.AsyncResult res) throws GLib.Error;
		public bool set_temp_directory (string directory);
		public static void test (void* user_data);
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Source : GLib.Object {
		public Pk.SourcePrivate priv;
		[CCode (has_construct_function = false)]
		public Source ();
		[NoAccessorMethod]
		public uint role { get; set; }
		[NoAccessorMethod]
		public string transaction_id { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class Task : Pk.Client {
		public Pk.TaskPrivate priv;
		[CCode (cname = "pk_task_new", has_construct_function = false)]
		public Task ();
		[CCode (cname = "pk_task_download_packages_async")]
		public async void download_packages_async (string package_ids, string directory, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[NoWrapper]
		public virtual void eula_question (uint request, Pk.Results results);
		[CCode (cname = "pk_task_generic_finish")]
		public Pk.Results generic_finish (GLib.AsyncResult res) throws GLib.Error;
		[CCode (cname = "pk_task_get_categories_async")]
		public async void get_categories_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_depends_async")]
		public async void get_depends_async (Pk.Bitfield filters, string package_ids, bool recursive, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_details_async")]
		public async void get_details_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_files_async")]
		public async void get_files_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_interactive")]
		public bool get_interactive ();
		[CCode (cname = "pk_task_get_packages_async")]
		public async void get_packages_async (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_repo_list_async")]
		public async void get_repo_list_async (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_requires_async")]
		public async void get_requires_async (Pk.Bitfield filters, string package_ids, bool recursive, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_simulate")]
		public bool get_simulate ();
		[CCode (cname = "pk_task_get_update_detail_async")]
		public async void get_update_detail_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_get_updates_async")]
		public async void get_updates_async (Pk.Bitfield filters, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_install_files_async")]
		public async void install_files_async (string files, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_install_packages_async")]
		public async void install_packages_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[NoWrapper]
		public virtual void key_question (uint request, Pk.Results results);
		[NoWrapper]
		public virtual void media_change_question (uint request, Pk.Results results);
		[CCode (cname = "pk_task_refresh_cache_async")]
		public async void refresh_cache_async (bool force, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_remove_packages_async")]
		public async void remove_packages_async (string package_ids, bool allow_deps, bool autoremove, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_repo_enable_async")]
		public async void repo_enable_async (string repo_id, bool enabled, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_resolve_async")]
		public async void resolve_async (Pk.Bitfield filters, string packages, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_rollback_async")]
		public async void rollback_async (string transaction_id, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_search_details_async")]
		public async void search_details_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_search_files_async")]
		public async void search_files_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_search_groups_async")]
		public async void search_groups_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_search_names_async")]
		public async void search_names_async (Pk.Bitfield filters, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 4.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_set_interactive")]
		public void set_interactive (bool interactive);
		[CCode (cname = "pk_task_set_simulate")]
		public void set_simulate (bool simulate);
		[NoWrapper]
		public virtual void simulate_question (uint request, Pk.Results results);
		[CCode (cname = "pk_task_test")]
		public static void test (void* user_data);
		[NoWrapper]
		public virtual void untrusted_question (uint request, Pk.Results results);
		[CCode (cname = "pk_task_update_packages_async")]
		public async void update_packages_async (string package_ids, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 3.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_update_system_async")]
		public async void update_system_async (GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 2.1)] Pk.ProgressCallback progress_callback);
		[CCode (cname = "pk_task_user_accepted")]
		public bool user_accepted (uint request);
		[CCode (cname = "pk_task_user_declined")]
		public bool user_declined (uint request);
		[CCode (cname = "pk_task_what_provides_async")]
		public async void what_provides_async (Pk.Bitfield filters, Pk.ProvidesEnum provides, string values, GLib.Cancellable? cancellable, [CCode (delegate_target_pos = 5.1)] Pk.ProgressCallback progress_callback);
		public bool interactive { get; set; }
		public bool simulate { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class TransactionList : GLib.Object {
		public Pk.TransactionListPrivate priv;
		[CCode (has_construct_function = false)]
		public TransactionList ();
		public string[] get_ids ();
		public static void test (void* user_data);
		public virtual signal void added (string tlist);
		public virtual signal void removed (string tlist);
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class TransactionPast : Pk.Source {
		public Pk.TransactionPastPrivate priv;
		[CCode (has_construct_function = false)]
		public TransactionPast ();
		[NoAccessorMethod]
		public string cmdline { get; set; }
		[NoAccessorMethod]
		public string data { get; set; }
		[NoAccessorMethod]
		public uint duration { get; set; }
		[NoAccessorMethod]
		public uint role { get; set; }
		[NoAccessorMethod]
		public bool succeeded { get; set; }
		[NoAccessorMethod]
		public string tid { get; set; }
		[NoAccessorMethod]
		public string timespec { get; set; }
		[NoAccessorMethod]
		public uint uid { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public class UpdateDetail : Pk.Source {
		public Pk.UpdateDetailPrivate priv;
		[CCode (has_construct_function = false)]
		public UpdateDetail ();
		[NoAccessorMethod]
		public string bugzilla_url { get; set; }
		[NoAccessorMethod]
		public string changelog { get; set; }
		[NoAccessorMethod]
		public string cve_url { get; set; }
		[NoAccessorMethod]
		public string issued { get; set; }
		[NoAccessorMethod]
		public string obsoletes { get; set; }
		[NoAccessorMethod]
		public string package_id { get; set; }
		[NoAccessorMethod]
		public uint restart { get; set; }
		[NoAccessorMethod]
		public uint state { get; set; }
		[NoAccessorMethod]
		public string update_text { get; set; }
		[NoAccessorMethod]
		public string updated { get; set; }
		[NoAccessorMethod]
		public string updates { get; set; }
		[NoAccessorMethod]
		public string vendor_url { get; set; }
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	[SimpleType]
	[IntegerType (rank = 0)]
	public struct Bitfield : uint64 {
		public static void test (void* user_data);
	}
	[CCode (type_id = "PK_TYPE_CATALOG_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct CatalogPrivate {
	}
	[CCode (type_id = "PK_TYPE_CATEGORY_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct CategoryPrivate {
	}
	[CCode (type_id = "PK_TYPE_CLIENT_HELPER_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ClientHelperPrivate {
	}
	[CCode (type_id = "PK_TYPE_CLIENT_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ClientPrivate {
	}
	[CCode (type_id = "PK_TYPE_CONTROL_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ControlPrivate {
	}
	[CCode (type_id = "PK_TYPE_DESKTOP_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct DesktopPrivate {
	}
	[CCode (type_id = "PK_TYPE_DETAILS_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct DetailsPrivate {
	}
	[CCode (type_id = "PK_TYPE_DISTRO_UPGRADE_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct DistroUpgradePrivate {
	}
	[CCode (type_id = "PK_TYPE_ENUM_MATCH", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct EnumMatch {
		public uint value;
		public weak global::string string;
	}
	[CCode (type_id = "PK_TYPE_ERROR_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ErrorPrivate {
	}
	[CCode (type_id = "PK_TYPE_EULA_REQUIRED_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct EulaRequiredPrivate {
	}
	[CCode (type_id = "PK_TYPE_FILES_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct FilesPrivate {
	}
	[CCode (type_id = "PK_TYPE_MEDIA_CHANGE_REQUIRED_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct MediaChangeRequiredPrivate {
	}
	[CCode (type_id = "PK_TYPE_MESSAGE_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct MessagePrivate {
	}
	[CCode (type_id = "PK_TYPE_PACKAGE_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct PackagePrivate {
	}
	[CCode (type_id = "PK_TYPE_PACKAGE_SACK_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct PackageSackPrivate {
	}
	[CCode (type_id = "PK_TYPE_PACKAGE_SACK_RESULTS", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct PackageSackResults {
	}
	[CCode (type_id = "PK_TYPE_PROGRESS_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ProgressPrivate {
	}
	[CCode (type_id = "PK_TYPE_REPO_DETAIL_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct RepoDetailPrivate {
	}
	[CCode (type_id = "PK_TYPE_REPO_SIGNATURE_REQUIRED_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct RepoSignatureRequiredPrivate {
	}
	[CCode (type_id = "PK_TYPE_REQUIRE_RESTART_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct RequireRestartPrivate {
	}
	[CCode (type_id = "PK_TYPE_RESULTS_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ResultsPrivate {
	}
	[CCode (type_id = "PK_TYPE_SERVICE_PACK_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct ServicePackPrivate {
	}
	[CCode (type_id = "PK_TYPE_SOURCE_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct SourcePrivate {
	}
	[CCode (type_id = "PK_TYPE_TASK_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct TaskPrivate {
	}
	[CCode (type_id = "PK_TYPE_TRANSACTION_LIST_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct TransactionListPrivate {
	}
	[CCode (type_id = "PK_TYPE_TRANSACTION_PAST_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct TransactionPastPrivate {
	}
	[CCode (type_id = "PK_TYPE_UPDATE_DETAIL_PRIVATE", cheader_filename = "packagekit-glib2/packagekit.h")]
	public struct UpdateDetailPrivate {
	}
	[CCode (cprefix = "PK_AUTHORIZE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum AuthorizeEnum {
		UNKNOWN,
		YES,
		NO,
		INTERACTIVE,
		LAST
	}
	[CCode (cprefix = "PK_CLIENT_ERROR_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum ClientError {
		FAILED,
		FAILED_AUTH,
		NO_TID,
		ALREADY_TID,
		ROLE_UNKNOWN,
		CANNOT_START_DAEMON,
		INVALID_INPUT,
		INVALID_FILE,
		NOT_SUPPORTED,
		DECLINED_SIMULATION,
		LAST
	}
	[CCode (cprefix = "PK_CONTROL_ERROR_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum ControlError {
		FAILED,
		CANNOT_START_DAEMON
	}
	[CCode (cprefix = "PK_DISTRO_UPGRADE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum DistroUpgradeEnum {
		UNKNOWN,
		STABLE,
		UNSTABLE,
		LAST
	}
	[CCode (cprefix = "PK_ERROR_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum ErrorEnum {
		UNKNOWN,
		OOM,
		NO_NETWORK,
		NOT_SUPPORTED,
		INTERNAL_ERROR,
		GPG_FAILURE,
		PACKAGE_ID_INVALID,
		PACKAGE_NOT_INSTALLED,
		PACKAGE_NOT_FOUND,
		PACKAGE_ALREADY_INSTALLED,
		PACKAGE_DOWNLOAD_FAILED,
		GROUP_NOT_FOUND,
		GROUP_LIST_INVALID,
		DEP_RESOLUTION_FAILED,
		FILTER_INVALID,
		CREATE_THREAD_FAILED,
		TRANSACTION_ERROR,
		TRANSACTION_CANCELLED,
		NO_CACHE,
		REPO_NOT_FOUND,
		CANNOT_REMOVE_SYSTEM_PACKAGE,
		PROCESS_KILL,
		FAILED_INITIALIZATION,
		FAILED_FINALISE,
		FAILED_CONFIG_PARSING,
		CANNOT_CANCEL,
		CANNOT_GET_LOCK,
		NO_PACKAGES_TO_UPDATE,
		CANNOT_WRITE_REPO_CONFIG,
		LOCAL_INSTALL_FAILED,
		BAD_GPG_SIGNATURE,
		MISSING_GPG_SIGNATURE,
		CANNOT_INSTALL_SOURCE_PACKAGE,
		REPO_CONFIGURATION_ERROR,
		NO_LICENSE_AGREEMENT,
		FILE_CONFLICTS,
		PACKAGE_CONFLICTS,
		REPO_NOT_AVAILABLE,
		INVALID_PACKAGE_FILE,
		PACKAGE_INSTALL_BLOCKED,
		PACKAGE_CORRUPT,
		ALL_PACKAGES_ALREADY_INSTALLED,
		FILE_NOT_FOUND,
		NO_MORE_MIRRORS_TO_TRY,
		NO_DISTRO_UPGRADE_DATA,
		INCOMPATIBLE_ARCHITECTURE,
		NO_SPACE_ON_DEVICE,
		MEDIA_CHANGE_REQUIRED,
		NOT_AUTHORIZED,
		UPDATE_NOT_FOUND,
		CANNOT_INSTALL_REPO_UNSIGNED,
		CANNOT_UPDATE_REPO_UNSIGNED,
		CANNOT_GET_FILELIST,
		CANNOT_GET_REQUIRES,
		CANNOT_DISABLE_REPOSITORY,
		RESTRICTED_DOWNLOAD,
		PACKAGE_FAILED_TO_CONFIGURE,
		PACKAGE_FAILED_TO_BUILD,
		PACKAGE_FAILED_TO_INSTALL,
		PACKAGE_FAILED_TO_REMOVE,
		UPDATE_FAILED_DUE_TO_RUNNING_PROCESS,
		PACKAGE_DATABASE_CHANGED,
		PROVIDE_TYPE_NOT_SUPPORTED,
		INSTALL_ROOT_INVALID,
		CANNOT_FETCH_SOURCES,
		LAST
	}
	[CCode (cprefix = "PK_EXIT_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum ExitEnum {
		UNKNOWN,
		SUCCESS,
		FAILED,
		CANCELLED,
		KEY_REQUIRED,
		EULA_REQUIRED,
		KILLED,
		MEDIA_CHANGE_REQUIRED,
		NEED_UNTRUSTED,
		LAST
	}
	[CCode (cprefix = "PK_FILTER_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum FilterEnum {
		UNKNOWN,
		NONE,
		INSTALLED,
		NOT_INSTALLED,
		DEVELOPMENT,
		NOT_DEVELOPMENT,
		GUI,
		NOT_GUI,
		FREE,
		NOT_FREE,
		VISIBLE,
		NOT_VISIBLE,
		SUPPORTED,
		NOT_SUPPORTED,
		BASENAME,
		NOT_BASENAME,
		NEWEST,
		NOT_NEWEST,
		ARCH,
		NOT_ARCH,
		SOURCE,
		NOT_SOURCE,
		COLLECTIONS,
		NOT_COLLECTIONS,
		APPLICATION,
		NOT_APPLICATION,
		LAST
	}
	[CCode (cprefix = "PK_GROUP_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum GroupEnum {
		UNKNOWN,
		ACCESSIBILITY,
		ACCESSORIES,
		ADMIN_TOOLS,
		COMMUNICATION,
		DESKTOP_GNOME,
		DESKTOP_KDE,
		DESKTOP_OTHER,
		DESKTOP_XFCE,
		EDUCATION,
		FONTS,
		GAMES,
		GRAPHICS,
		INTERNET,
		LEGACY,
		LOCALIZATION,
		MAPS,
		MULTIMEDIA,
		NETWORK,
		OFFICE,
		OTHER,
		POWER_MANAGEMENT,
		PROGRAMMING,
		PUBLISHING,
		REPOS,
		SECURITY,
		SERVERS,
		SYSTEM,
		VIRTUALIZATION,
		SCIENCE,
		DOCUMENTATION,
		ELECTRONICS,
		COLLECTIONS,
		VENDOR,
		NEWEST,
		LAST
	}
	[CCode (cprefix = "PK_INFO_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum InfoEnum {
		UNKNOWN,
		INSTALLED,
		AVAILABLE,
		LOW,
		ENHANCEMENT,
		NORMAL,
		BUGFIX,
		IMPORTANT,
		SECURITY,
		BLOCKED,
		DOWNLOADING,
		UPDATING,
		INSTALLING,
		REMOVING,
		CLEANUP,
		OBSOLETING,
		COLLECTION_INSTALLED,
		COLLECTION_AVAILABLE,
		FINISHED,
		REINSTALLING,
		DOWNGRADING,
		PREPARING,
		DECOMPRESSING,
		LAST
	}
	[CCode (cprefix = "PK_LICENSE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum LicenseEnum {
		UNKNOWN,
		AAL,
		ADOBE,
		ADSL,
		AFL,
		AGPLV1,
		AMDPLPA,
		AML,
		AMPAS_BSD,
		APSL_2_DOT_0,
		ARL,
		ARPHIC,
		ARTISTIC_2_DOT_0,
		ARTISTIC_CLARIFIED,
		ASL_1_DOT_0,
		ASL_1_DOT_1,
		ASL_2_DOT_0,
		BAEKMUK,
		BEOPEN,
		BITTORRENT,
		BOOST,
		BSD,
		BSD_PROTECTION,
		BSD_WITH_ADVERTISING,
		CATOSL,
		CC0,
		CC_BY,
		CC_BY_SA,
		CDDL,
		CDL,
		CECILL,
		CECILL_B,
		CECILL_C,
		CNRI,
		CONDOR,
		COPYRIGHT_ONLY,
		CPAL,
		CPL,
		CRYSTAL_STACKER,
		DOC,
		DSL,
		DVIPDFM,
		ECL_1_DOT_0,
		ECL_2_DOT_0,
		ECOS,
		EFL_2_DOT_0,
		ENTESSA,
		EPL,
		ERPL,
		EUPL_1_DOT_1,
		EUROSYM,
		EU_DATAGRID,
		FAIR,
		FBSDDL,
		FREE_ART,
		FTL,
		GEOGRATIS,
		GFDL,
		GIFTWARE,
		GL2PS,
		GLIDE,
		GNUPLOT,
		GPLV1,
		GPLV2,
		GPLV2_OR_ARTISTIC,
		GPLV2_PLUS,
		GPLV2_PLUS_OR_ARTISTIC,
		GPLV2_PLUS_WITH_EXCEPTIONS,
		GPLV2_WITH_EXCEPTIONS,
		GPLV3,
		GPLV3_PLUS,
		GPLV3_PLUS_WITH_EXCEPTIONS,
		GPLV3_WITH_EXCEPTIONS,
		GPL_PLUS,
		GPL_PLUS_OR_ARTISTIC,
		GPL_PLUS_WITH_EXCEPTIONS,
		IBM,
		IEEE,
		IJG,
		IMAGEMAGICK,
		IMATIX,
		IMLIB2,
		INTEL_ACPI,
		INTERBASE,
		IPA,
		ISC,
		JABBER,
		JASPER,
		JPYTHON,
		KNUTH,
		LBNL_BSD,
		LDPL,
		LGPLV2,
		LGPLV2_PLUS,
		LGPLV2_PLUS_OR_ARTISTIC,
		LGPLV2_PLUS_WITH_EXCEPTIONS,
		LGPLV2_WITH_EXCEPTIONS,
		LGPLV3,
		LGPLV3_PLUS,
		LGPLV3_PLUS_WITH_EXCEPTIONS,
		LGPLV3_WITH_EXCEPTIONS,
		LIBERATION,
		LIBTIFF,
		LLGPL,
		LOGICA,
		LPL,
		LPPL,
		MECAB_IPADIC,
		MIROS,
		MIT,
		MIT_WITH_ADVERTISING,
		MOD_MACRO,
		MOTOSOTO,
		MPLUS,
		MPLV1_DOT_0,
		MPLV1_DOT_1,
		MS_PL,
		MS_RL,
		NAUMEN,
		NCSA,
		NETCDF,
		NETSCAPE,
		NEWMAT,
		NGPL,
		NOKIA,
		NOSL,
		NOWEB,
		OAL,
		OFL,
		OFSFDL,
		OML,
		OPENLDAP,
		OPENPBS,
		OPENSSL,
		OREILLY,
		OSL_1_DOT_0,
		OSL_1_DOT_1,
		OSL_2_DOT_0,
		OSL_2_DOT_1,
		OSL_3_DOT_0,
		PHORUM,
		PHP,
		PLEXUS,
		POSTGRESQL,
		PSUTILS,
		PTFL,
		PUBLIC_DOMAIN,
		PUBLIC_USE,
		PYTHON,
		QHULL,
		QPL,
		RDISC,
		RICEBSD,
		ROMIO,
		RPSL,
		RUBY,
		SAXPATH,
		SCEA,
		SCRIP,
		SENDMAIL,
		SISSL,
		SLEEPYCAT,
		SLIB,
		SNIA,
		SPL,
		STIX,
		TCL,
		TEEWORLDS,
		TMATE,
		TOSL,
		TPL,
		UCD,
		VIM,
		VNLSL,
		VOSTROM,
		VSL,
		W3C,
		WADALAB,
		WEBMIN,
		WTFPL,
		WXWIDGETS,
		XANO,
		XEROX,
		XINETD,
		XSKAT,
		YPLV1_DOT_1,
		ZEND,
		ZLIB,
		ZLIB_WITH_ACKNOWLEDGEMENT,
		ZPLV1_DOT_0,
		ZPLV2_DOT_0,
		ZPLV2_DOT_1,
		LAST
	}
	[CCode (cprefix = "PK_MEDIA_TYPE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum MediaTypeEnum {
		UNKNOWN,
		CD,
		DVD,
		DISC,
		LAST
	}
	[CCode (cprefix = "PK_MESSAGE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum MessageEnum {
		UNKNOWN,
		BROKEN_MIRROR,
		CONNECTION_REFUSED,
		PARAMETER_INVALID,
		PRIORITY_INVALID,
		BACKEND_ERROR,
		DAEMON_ERROR,
		CACHE_BEING_REBUILT,
		UNTRUSTED_PACKAGE,
		NEWER_PACKAGE_EXISTS,
		COULD_NOT_FIND_PACKAGE,
		CONFIG_FILES_CHANGED,
		PACKAGE_ALREADY_INSTALLED,
		AUTOREMOVE_IGNORED,
		REPO_METADATA_DOWNLOAD_FAILED,
		REPO_FOR_DEVELOPERS_ONLY,
		OTHER_UPDATES_HELD_BACK,
		LAST
	}
	[CCode (cprefix = "PK_NETWORK_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum NetworkEnum {
		UNKNOWN,
		OFFLINE,
		ONLINE,
		WIRED,
		WIFI,
		MOBILE,
		LAST
	}
	[CCode (cprefix = "PK_PACKAGE_SACK_SORT_TYPE_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum PackageSackSortType {
		NAME,
		INFO,
		PACKAGE_ID,
		SUMMARY,
		LAST
	}
	[CCode (cprefix = "PK_PROGRESS_TYPE_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum ProgressType {
		PACKAGE_ID,
		TRANSACTION_ID,
		PERCENTAGE,
		SUBPERCENTAGE,
		ALLOW_CANCEL,
		STATUS,
		ROLE,
		CALLER_ACTIVE,
		ELAPSED_TIME,
		REMAINING_TIME,
		SPEED,
		UID,
		PACKAGE,
		INVALID
	}
	[CCode (cprefix = "PK_PROVIDES_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum ProvidesEnum {
		UNKNOWN,
		ANY,
		MODALIAS,
		CODEC,
		MIMETYPE,
		FONT,
		HARDWARE_DRIVER,
		POSTSCRIPT_DRIVER,
		LAST
	}
	[CCode (cprefix = "PK_RESTART_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum RestartEnum {
		UNKNOWN,
		NONE,
		APPLICATION,
		SESSION,
		SYSTEM,
		SECURITY_SESSION,
		SECURITY_SYSTEM,
		LAST
	}
	[CCode (cprefix = "PK_ROLE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum RoleEnum {
		UNKNOWN,
		CANCEL,
		GET_DEPENDS,
		GET_DETAILS,
		GET_FILES,
		GET_PACKAGES,
		GET_REPO_LIST,
		GET_REQUIRES,
		GET_UPDATE_DETAIL,
		GET_UPDATES,
		INSTALL_FILES,
		INSTALL_PACKAGES,
		INSTALL_SIGNATURE,
		REFRESH_CACHE,
		REMOVE_PACKAGES,
		REPO_ENABLE,
		REPO_SET_DATA,
		RESOLVE,
		ROLLBACK,
		SEARCH_DETAILS,
		SEARCH_FILE,
		SEARCH_GROUP,
		SEARCH_NAME,
		UPDATE_PACKAGES,
		UPDATE_SYSTEM,
		WHAT_PROVIDES,
		ACCEPT_EULA,
		DOWNLOAD_PACKAGES,
		GET_DISTRO_UPGRADES,
		GET_CATEGORIES,
		GET_OLD_TRANSACTIONS,
		SIMULATE_INSTALL_FILES,
		SIMULATE_INSTALL_PACKAGES,
		SIMULATE_REMOVE_PACKAGES,
		SIMULATE_UPDATE_PACKAGES,
		UPGRADE_SYSTEM,
		LAST
	}
	[CCode (cprefix = "PK_SIGTYPE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum SigTypeEnum {
		UNKNOWN,
		GPG,
		LAST
	}
	[CCode (cprefix = "PK_STATUS_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum StatusEnum {
		UNKNOWN,
		WAIT,
		SETUP,
		RUNNING,
		QUERY,
		INFO,
		REMOVE,
		REFRESH_CACHE,
		DOWNLOAD,
		INSTALL,
		UPDATE,
		CLEANUP,
		OBSOLETE,
		DEP_RESOLVE,
		SIG_CHECK,
		ROLLBACK,
		TEST_COMMIT,
		COMMIT,
		REQUEST,
		FINISHED,
		CANCEL,
		DOWNLOAD_REPOSITORY,
		DOWNLOAD_PACKAGELIST,
		DOWNLOAD_FILELIST,
		DOWNLOAD_CHANGELOG,
		DOWNLOAD_GROUP,
		DOWNLOAD_UPDATEINFO,
		REPACKAGING,
		LOADING_CACHE,
		SCAN_APPLICATIONS,
		GENERATE_PACKAGE_LIST,
		WAITING_FOR_LOCK,
		WAITING_FOR_AUTH,
		SCAN_PROCESS_LIST,
		CHECK_EXECUTABLE_FILES,
		CHECK_LIBRARIES,
		COPY_FILES,
		LAST
	}
	[CCode (cprefix = "PK_UPDATE_STATE_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum UpdateStateEnum {
		UNKNOWN,
		STABLE,
		UNSTABLE,
		TESTING,
		LAST
	}
	[CCode (cprefix = "PK_UPGRADE_KIND_ENUM_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public enum UpgradeKindEnum {
		UNKNOWN,
		MINIMAL,
		DEFAULT,
		COMPLETE,
		LAST
	}
	[CCode (cprefix = "CATALOG_ERROR_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public errordomain CatalogError {
		FAILED,
	}
	[CCode (cprefix = "SERVICE_PACK_ERROR_", cheader_filename = "packagekit-glib2/packagekit.h")]
	public errordomain ServicePackError {
		FAILEDSETUP,
		FAILEDDOWNLOAD,
		FAILEDEXTRACTION,
		FAILEDCREATE,
		NOTHINGTODO,
		NOTCOMPATIBLE,
	}
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h", instance_pos = 1.9)]
	public delegate bool PackageSackFilterFunc (Pk.Package package);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h", instance_pos = 2.9)]
	public delegate void ProgressCallback (Pk.Progress progress, Pk.ProgressType type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string CATALOG_FILE_EXTENSION;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int CLIENT_DBUS_METHOD_TIMEOUT;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int CONTROL_DBUS_METHOD_TIMEOUT;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string DBUS_INTERFACE;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string DBUS_INTERFACE_TRANSACTION;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string DBUS_PATH;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string DBUS_SERVICE;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string DESKTOP_DEFAULT_APPLICATION_DIR;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int MAJOR_VERSION;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int MICRO_VERSION;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int MINOR_VERSION;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string PACKAGE_IDS_DELIM;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int PACKAGE_ID_ARCH;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int PACKAGE_ID_DATA;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int PACKAGE_ID_NAME;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const int PACKAGE_ID_VERSION;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public const string SERVICE_PACK_GROUP_NAME;
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.AuthorizeEnum authorize_type_enum_from_string (string authorize_type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string authorize_type_enum_to_string (Pk.AuthorizeEnum authorize_type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void common_test (void* user_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void debug_add_log_domain (string log_domain);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static bool debug_is_verbose ();
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void debug_set_verbose (bool verbose);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string enum_find_string (Pk.EnumMatch table, uint value);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static uint enum_find_value (Pk.EnumMatch table, string string);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void enum_test (void* user_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.ExitEnum exit_enum_from_string (string exit);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string exit_enum_to_string (Pk.ExitEnum exit);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.Bitfield filter_bitfield_from_string (string filters);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string filter_bitfield_to_string (Pk.Bitfield filters);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.FilterEnum filter_enum_from_string (string filter);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string filter_enum_to_string (Pk.FilterEnum filter);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string get_distro_id ();
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.Bitfield group_bitfield_from_string (string groups);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string group_bitfield_to_string (Pk.Bitfield groups);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.GroupEnum group_enum_from_string (string group);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string group_enum_to_string (Pk.GroupEnum group);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.InfoEnum info_enum_from_string (string info);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string info_enum_to_string (Pk.InfoEnum info);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string iso8601_from_date (GLib.Date date);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string iso8601_present ();
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.LicenseEnum license_enum_from_string (string license);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string license_enum_to_string (Pk.LicenseEnum license);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__POINTER_UINT_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__POINTER_UINT_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_BOOLEAN (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_BOXED (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_BOOLEAN (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_BOOLEAN_STRING_UINT_STRING_UINT_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_BOOLEAN_UINT_UINT_STRING_UINT_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_BOOLEAN (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_BOOLEAN_STRING_UINT_STRING_UINT_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_STRING_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_UINT64 (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_STRING_UINT_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_STRING_STRING_STRING_UINT64 (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_UINT_STRING_STRING_UINT64 (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_STRING_UINT_UINT_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_UINT_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__STRING_UINT_UINT_UINT_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__UINT_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__UINT_STRING_STRING (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__UINT_STRING_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__UINT_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static void marshal_VOID__UINT_UINT_UINT_UINT (GLib.Closure closure, GLib.Value return_value, uint n_param_values, GLib.Value param_values, void* invocation_hint, void* marshal_data);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.MediaTypeEnum media_type_enum_from_string (string media_type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string media_type_enum_to_string (Pk.MediaTypeEnum media_type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.NetworkEnum network_enum_from_string (string network);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string network_enum_to_string (Pk.NetworkEnum network);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.ProvidesEnum provides_enum_from_string (string provides);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string provides_enum_to_string (Pk.ProvidesEnum provides);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string[] ptr_array_to_strv (GLib.GenericArray<weak void*> array);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.RestartEnum restart_enum_from_string (string restart);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string restart_enum_to_string (Pk.RestartEnum restart);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.Bitfield role_bitfield_from_string (string roles);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static string role_bitfield_to_string (Pk.Bitfield roles);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.RoleEnum role_enum_from_string (string role);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string role_enum_to_string (Pk.RoleEnum role);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.SigTypeEnum sig_type_enum_from_string (string sig_type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string sig_type_enum_to_string (Pk.SigTypeEnum sig_type);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.StatusEnum status_enum_from_string (string status);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string status_enum_to_string (Pk.StatusEnum status);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.UpdateStateEnum update_state_enum_from_string (string update_state);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string update_state_enum_to_string (Pk.UpdateStateEnum update_state);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static Pk.UpgradeKindEnum upgrade_kind_enum_from_string (string upgrade_kind);
	[CCode (cheader_filename = "packagekit-glib2/packagekit.h")]
	public static unowned string upgrade_kind_enum_to_string (Pk.UpgradeKindEnum upgrade_kind);
}
