/* manager.vala - Manage installed applications (remove them / maintain their dependencies)
 *
 * Copyright (C) 2010-2014 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU Lesser General Public License Version 3
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

using GLib;
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller {

/**
 * Allows managing Listaller applications
 *
 * This class allows managing installed applications as
 * well as performing maintainance tasks to keep applications
 * running.
 * It also allows fetching applications from remote sources.
 */
public class Manager : MessageObject {
	private SetupSettings ssettings;

	public signal void status_changed (StatusItem status);
	public signal void application (AppItem appid);

	public SetupSettings settings {
		get { return ssettings; }
		internal set { ssettings = value; }
	}

	/**
	 * Create a new Listaller application manager
	 *
	 * @param shared_mode Whether we are in shared mode or not.
	 */
	public Manager (bool shared_mode = true) {
		base ();
		ssettings = new SetupSettings ();
		if (shared_mode)
			ssettings.current_mode = IPK.InstallMode.SHARED;
		else
			ssettings.current_mode = IPK.InstallMode.PRIVATE;
	}

	private void emit_status (StatusEnum status, string info) {
		StatusItem item = new StatusItem (status);
		item.info = info;
		status_changed (item);
	}

	private bool is_shared () {
		return ssettings.current_mode == IPK.InstallMode.SHARED;
	}

	private bool init_db (out SoftwareDB sdb, bool writeable = true) {
		SoftwareDB db = new SoftwareDB (ssettings, true);
		// Connect the database events with this application manager
		connect_with_object (db, ObjConnectFlags.NONE);
		db.application.connect ( (a) => { this.application (a); } );

		sdb = db;
		if (writeable) {
			if (!db.open_write ()) {
				emit_error (ErrorEnum.DATABASE_OPEN_FAILED,
					    _("Unable to open software database for reading & writing!"));
				return false;
			}
		} else {
			if (!db.open_read ()) {
				emit_error (ErrorEnum.DATABASE_OPEN_FAILED,
					    _("Unable to open software database for reading only!"));
				return false;
			}
		}

		return true;
	}

	public bool filter_applications (AppState filter, out ArrayList<AppItem> app_list = null) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return false;

		return db.filter_applications (filter, out app_list);
	}

	/**
	 * Find applications which match the strings in values
	 *
	 * @param filter Filter, which is applied on the results
	 * @param values Null-terminated list of strings to search for
	 * @param appList ArrayList of AppItems to store the result, or NULL
	 *                (all applications are also emitted in the "application" signal)
	 */
	public bool find_applications_by_values (string filter,
						 [CCode (array_null_terminated = true, array_length = false)] string[] values,
						 out ArrayList<AppItem> appList = null) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return false;

		ArrayList<AppItem> res = db.find_applications (values);
		// Emit signals for found applications
		foreach (AppItem app in res)
			application (app);

		return true;
	}

	public ArrayList<AppItem>? get_applications_by_fullname (string full_name) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return null;

		var appList = db.get_applications_by_fullname (full_name);
		return appList;
	}

	public AppItem? get_application_by_unique_name (string idname) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return null;

		AppItem? app = db.get_application_by_unique_name (idname);
		return app;
	}

	private bool remove_application_internal (AppItem app) {
		// Emit that we're starting
		emit_status (StatusEnum.ACTION_STARTED,
			     _("Removal of %s started.").printf (app.metainfo.name));

		SoftwareDB db;
		if (!init_db (out db))
			return false;

		// Check if this application exists, if not exit
		if (db.get_application_by_id (app) == null) {
			emit_error (ErrorEnum.REMOVAL_FAILED, _("Could not uninstall application %s. It is not installed.").printf (app.metainfo.name));
			return false;
		}

		// Remove all files which belong to this application
		ArrayList<IPK.FileEntry>? files = db.get_application_filelist (app);
		if (files == null) {
			emit_error (ErrorEnum.REMOVAL_FAILED, _("'%s' has no file-list registered. The software database might be broken.").printf (app.metainfo.name));
			return false;
		}

		foreach (IPK.FileEntry fe in files) {
			string fname = fe.fname_installed;
			if (FileUtils.test (fname, FileTest.EXISTS)) {
				int ret = FileUtils.remove (fname);
				if (ret != 0) {
					emit_error (ErrorEnum.REMOVAL_FAILED, _("Could not remove file %s!").printf (fname));
					return false;
				}
				string dirn = Path.get_dirname (fname);
				// Remove directory if it is empty
				if (dir_is_empty (dirn)) {
					DirUtils.remove (dirn);
				}
			}
		}
		bool ret = db.remove_application (app);

		emit_status (StatusEnum.REMOVAL_FINISHED,
			     _("Removal of %s finished.").printf (app.metainfo.name));

		return ret;
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		if (type == PackageKit.ProgressType.PERCENTAGE)
			change_progress (progress.percentage);
		if (type == PackageKit.ProgressType.ITEM_PROGRESS)
			change_item_progress (progress.item_progress.package_id, progress.item_progress.percentage);
	}

	private bool remove_shared_application_internal (AppItem app) {
		bool ret = true;
		PackageKit.Task pktask = new PackageKit.Task ();

		/* PackageKit will handle all Listaller superuser uninstallations.
		 * Therefore, PackageKit needs to be compiled with Listaller support enabled.
		 */

		PackageKit.Results? pkres;
		pktask.background = false;

		string pklipackage = app.build_pk_package_id ();
		change_progress (0);

		try {
			pkres = pktask.remove_packages_sync ({ pklipackage, null }, false, true, null, pk_progress_cb);
		} catch (Error e) {
			emit_error (ErrorEnum.REMOVAL_FAILED, e.message);
			return false;
		}

		if (pkres.get_exit_code () != PackageKit.Exit.SUCCESS) {
			PackageKit.Error error = pkres.get_error_code ();
			emit_error (ErrorEnum.REMOVAL_FAILED, error.get_details ());
			return false;
		}

		change_progress (100);

		// Emit status message (setup finished)
		emit_status (StatusEnum.REMOVAL_FINISHED,
			     _("Removal of %s finished.").printf (app.metainfo.name));

		return ret;

	}

	public bool remove_application (AppItem app) {
		app.fast_check ();

		IPK.InstallMode mode_old = ssettings.current_mode;
		bool shared = app.state == AppState.INSTALLED_SHARED;
		if (shared != ssettings.shared_mode) {
			if (shared)
				debug (_("Trying to remove shared application, but AppManager is not in superuser mode!\nSetting AppManager to superuse mode now."));
			else
				debug (_("Trying to remove local application, but AppManager is in superuser mode!\nSetting AppManager to local mode now."));
			if (shared)
				ssettings.current_mode = IPK.InstallMode.SHARED;
		}

		bool ret;
		if ((app.state == AppState.INSTALLED_SHARED) && (!is_root ())) {
			// Call PackageKit if we aren't root and want to remove a shared application
			ret = remove_shared_application_internal (app);
		} else {
			// Try normal app removal
			ret = remove_application_internal (app);
		}

		ssettings.current_mode = mode_old;

		return ret;
	}

	public bool refresh_appitem_data (ref AppItem item) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return false;

		// We only want to fetch dependencies from the correct database, so limit DB usage
		if (item.state == AppState.INSTALLED_SHARED)
			db.dbflags = DatabaseFlags.USE_SHARED_INSTALLED;
		else
			db.dbflags = DatabaseFlags.USE_PRIVATE_INSTALLED;

		var tmpItem = db.get_application_by_unique_name (item.unique_id);
		if (tmpItem == null)
			return false;
		item = tmpItem;

		return true;
	}

	[CCode (array_length = false, array_null_terminated = true)]
	public string[]? get_application_filelist (AppItem app) {
		app.fast_check ();

		SoftwareDB db;
		if (!init_db (out db))
			return null;

		// Check if this application exists, if not exit
		if (db.get_application_by_id (app) == null) {
			warning ("Unable to get file-list for application %s - app is not installed!", app.unique_id);
			return null;
		}

		// Remove all files which belong to this application
		ArrayList<IPK.FileEntry>? files = db.get_application_filelist (app);
		if (files == null) {
			critical ("Couldn't retrieve file-list for application %s! All apps should have a filelist, this should never happen!", app.unique_id);
			return null;
		}

		string[] res = {};
		foreach (IPK.FileEntry fe in files)
			res += fe.fname_installed;
		res += null;

		return res;
	}

	/**
	 * Get the LD_LIBRARY_PATH environment for an application.
	 *
	 * @param app AppItem belonging to an Listaller-installed app
	 */
	public string get_app_ld_environment (AppItem app) {
		// get basic library path
		IPK.InstallMode inst_mode = IPK.InstallMode.PRIVATE;
		if (app.state == AppState.INSTALLED_SHARED)
			inst_mode = IPK.InstallMode.SHARED;
		string paths = VarSolver.autosubst_instvars ("%LIB_PRIVATE%", app.unique_id, inst_mode);

		if (app.dependencies.length <= 0) {
			debug ("Application has no dependencies set!");
			debug ("Using standard environment...");
			return paths;
		}


		SoftwareDB db;
		if (!init_db (out db, false)) {
			debug ("Unable to open DB, app environment will be empty!");
			return paths;
		}

		// We only want to fetch dependencies from the correct database, so limit DB usage
		if (app.state == AppState.INSTALLED_SHARED)
			db.dbflags = DatabaseFlags.USE_SHARED_INSTALLED;
		else
			db.dbflags = DatabaseFlags.USE_PRIVATE_INSTALLED;

		// A new DepManager to resolve environment
		DepManager depman = new DepManager (db);

		for (var i = 0; i < app.dependencies.length; i++) {
			Dependency dep = app.dependencies.get (i);

			// Now get paths for library, if possible (if dependency is a library)
			string p = depman.get_absolute_library_path (dep);
			if (p != "")
				paths = "%s;%s".printf (paths, p);
		}

		return paths;
	}

	/**
	 * Update the 3rd-party remote application cache.
	 * This will allow querying for new updates.
	 *
	 * @return TRUE if refresh was successful.
	 */
	public bool refresh_repository_cache () {
		bool ret = false;

		if (is_root ()) {
			var repoMgr = new Repo.Manager ();
			ret = repoMgr.refresh_cache ();
		} else {
			var pktask = new PackageKit.Task ();
			PackageKit.Results? pkres;

			change_progress (0);
			try {
				pkres = pktask.refresh_cache_sync (false, null, pk_progress_cb);
			} catch (Error e) {
				emit_error (ErrorEnum.REFRESH_FAILED, e.message);
				return false;
			}

			if (pkres.get_exit_code () != PackageKit.Exit.SUCCESS) {
				PackageKit.Error error = pkres.get_error_code ();
				emit_error (ErrorEnum.REFRESH_FAILED, error.get_details ());
				return false;
			}
		}

		return ret;
	}

	public Setup? prepare_setup_for_app (string app_idname) {
		var repo_mgr = new Repo.Manager ();
		return repo_mgr.get_setup_for_remote_app_by_id (app_idname);
	}

}

} // End of namespace: Listaller
