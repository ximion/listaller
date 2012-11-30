/* database.vala - Easy-to-use interface to Listaller's software database(s)
 *
 * Copyright (C) 2010-2012 Matthias Klumpp <matthias@tenstral.net>
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

private enum DBType {
	NONE,
	PRIVATE,
	SHARED;
}

private class SoftwareDB : MessageObject {
	private LocalDB? db_shared;
	private LocalDB? db_priv;

	private RemoteCacheDB db_available;

	public DBType force_db { get; set; }
	public SetupSettings setup_settings { get; private set; }

	public signal void application (AppItem appid);

	public SoftwareDB (SetupSettings setup_settings, bool include_shared = true) {
		base ();

		db_shared = null;
		db_priv = null;
		force_db = DBType.NONE;

		if (include_shared) {
			db_shared = new LocalDB (true, setup_settings.test_mode);

			/* If we open a shared DB and have root-access, don't open the
			 * private database. It makes no sense someone is working as root
			 * and installing private stuff into root's home-dir */
			if (!is_root ()) {
				db_priv = new LocalDB (false, setup_settings.test_mode);
			}
		} else {
			// If we only want to open the personal DB, don't touch the shared one
			db_priv = new LocalDB (false, setup_settings.test_mode);
		}

		this.setup_settings = setup_settings;

		// Forward messages of the internal DBs to this meta-db
		if (db_priv != null)
			db_priv.message.connect ( (m) => { this.message (m); } );
		if (db_shared != null)
			db_shared.message.connect ( (m) => { this.message (m); } );
	}

	private void emit_dberror (string details) {
		// Emit a database error
		emit_error (ErrorEnum.DATABASE_FAILURE, details);
	}

	private bool shared_db_canbeused (bool error = false) {
		bool ret = true;
		if (db_shared == null) {
			ret = false;
		}

		if (force_db == DBType.PRIVATE)
			return false;

		/* If shared db does not exist AND we don't have root-access, opening the db will fail.
		 * (so we check for this case here, just to be sure) */
		if (ret)
			if ((!is_root ()) && (!FileUtils.test (db_shared.get_database_name (), FileTest.EXISTS))) {
				db_shared = null;
				ret = false;
			}

		if ((!ret) && (error))
			emit_dberror (_("Tried to perform action on shared software database, but the database is not opened! (maybe a permission problem?)"));

		return ret;
	}

	private bool private_db_canbeused (bool error = false) {
		if (force_db == DBType.SHARED)
			return false;

		if (db_priv == null) {
			if (error)
				emit_dberror (_("Tried to perform action on private software database, but the database is not opened! (this should never happen!)"));
			return false;
		}

		return true;
	}

	public bool open_read () {
		bool ret = false;
		try {
			if (private_db_canbeused ())
				ret = db_priv.open_r ();
			if (shared_db_canbeused ()) {
				if (!ret)
					ret = db_shared.open_r ();
				else
					db_shared.open_r ();
			}
		} catch (Error e) {
			emit_dberror (_("Unable to open database for read-only: %s").printf (e.message));
			ret = false;
		}
		return ret;
	}

	public bool open_write () {
		bool ret = false;
		try {
			if (private_db_canbeused ())
				ret = db_priv.open_rw ();
			if (shared_db_canbeused ()) {
				// We can only have write access to shared DB if we're root
				if (is_root ()) {
					if (!ret)
						ret = db_shared.open_rw ();
					else
						db_shared.open_rw ();
				} else {
					if (!ret)
						ret = db_shared.open_r ();
					else
						db_shared.open_r ();
				}
			}
		} catch (Error e) {
			emit_dberror (_("Unable to open database for writing: %s").printf (e.message));
			ret = false;
		}
		return ret;
	}

	public bool database_locked () {
		bool ret = false;
		if (shared_db_canbeused ())
			ret = db_shared.database_locked ();
		if (ret)
			return true;
		if (private_db_canbeused ())
			ret = db_priv.database_locked ();

		return ret;
	}

	public bool database_writeable () {
		bool ret = false;
		if (shared_db_canbeused ())
			ret = db_shared.database_writeable ();
		if (ret)
			return true;
		if (private_db_canbeused ())
			ret = db_priv.database_writeable ();

		return ret;
	}

	public bool add_application (AppItem app) {
		bool ret;
		try {
		if (app.state == AppState.INSTALLED_SHARED) {
			if (shared_db_canbeused (true))
				ret = db_shared.add_application (app);
			else
				return false;
		} else {
			if (private_db_canbeused (true))
				ret = db_priv.add_application (app);
			else
				return false;
		}
		} catch (Error e) {
			emit_dberror (_("Unable to add application to database: %s").printf (e.message));
			ret = false;
		}
		return ret;
	}

	public bool add_application_filelist (AppItem app, Collection<IPK.FileEntry> flist) {
		bool ret;
		try {
			if (app.state == AppState.INSTALLED_SHARED) {
				if (shared_db_canbeused (true))
					ret = db_shared.add_application_filelist (app, flist);
				else
					return false;
			} else {
				if (private_db_canbeused (true))
					ret = db_priv.add_application_filelist (app, flist);
				else
					return false;
			}
		} catch (Error e) {
			emit_dberror (_("Unable to add application file list to database: %s").printf (e.message));
		}

		return ret;
	}

	public ArrayList<IPK.FileEntry>? get_application_filelist (AppItem app) {
		ArrayList<IPK.FileEntry>? res;
		try {
			if (app.state == AppState.INSTALLED_SHARED) {
				if (shared_db_canbeused (true))
					res = db_shared.get_application_filelist (app);
				else
					return null;
			} else {
				if (private_db_canbeused (true))
					res = db_priv.get_application_filelist (app);
				else
					return null;
			}
		} catch (Error e) {
			emit_dberror (_("Unable to fetch application file list: %s").printf (e.message));
			res = null;
		}

		return res;
	}

	public int get_applications_count () {
		int cnt = 0;
		try {
			if (shared_db_canbeused ())
				cnt += db_shared.get_applications_count ();
			if (private_db_canbeused ())
				cnt += db_priv.get_applications_count ();
		} catch (Error e) {
			emit_dberror (_("Unable to count applications: %s").printf (e.message));
		}
		return cnt;
	}

	public AppItem? get_application_by_idname (string appIdName) {
		AppItem? app = null;

		try {
			// It is important that we prefer the personal over the shared database!
			// (so command-line tools allow removing the personal app first)
			if (private_db_canbeused ())
					app = db_priv.get_application_by_idname (appIdName);

			if (app == null)
				if (shared_db_canbeused ())
					app = db_shared.get_application_by_idname (appIdName);

		} catch (Error e) {
			emit_dberror (_("Unable to fetch application by id: %s").printf (e.message));
		}

		return app;
	}

	public AppItem? get_application_by_id (AppItem app) {
		AppItem? resApp;
		resApp = get_application_by_idname (app.idname);
		return resApp;
	}

	public ArrayList<AppItem>? get_applications_by_fullname (string appFullName) {
		ArrayList<AppItem>? appList = null;
		try {
			if (shared_db_canbeused ())
				appList = db_shared.get_applications_by_fullname (appFullName);

			if (appList == null)
				if (private_db_canbeused ())
					appList = db_priv.get_applications_by_fullname (appFullName);
		} catch (Error e) {
			emit_dberror (_("Unable to fetch application by name: %s").printf (e.message));
		}

		return appList;
	}

	public bool remove_application (AppItem app) {
		bool ret;
		try {
			if (app.state == AppState.INSTALLED_SHARED) {
				if (shared_db_canbeused (true))
					ret = db_shared.remove_application (app);
				else
					return false;
			} else {
				if (private_db_canbeused (true))
					ret = db_priv.remove_application (app);
				else
					return false;
			}
		} catch (Error e) {
			emit_dberror (_("Unable to remove application from database: %s").printf (e.message));
			ret = false;
		}

		return ret;
	}

	public ArrayList<AppItem> find_applications ([CCode (array_null_terminated = true, array_length = false)] string[] values) {
		var list = new ArrayList<AppItem> ();

		try {
			if (shared_db_canbeused ()) {
				var tmp = db_shared.find_applications (values);
				list.add_all (tmp);
			}
			if (private_db_canbeused ()) {
				var tmp = db_priv.find_applications (values);
				list.add_all (tmp);
			}
		} catch (Error e) {
			emit_dberror (_("Unable to search application database: %s").printf (e.message));
		}

		return list;
	}

	public void _internal_emit_dbapps (double one, ref ArrayList<AppItem> appList) {
		uint i = 1;
		foreach (AppItem app in appList) {
			application (app);
			change_progress ((int) Math.round (one * i));
			i++;
		}
	}

	public bool find_all_applications (AppOrigin filter, out ArrayList<AppItem> appList = null) {
		ArrayList<AppItem> alist = new ArrayList<AppItem> ();

		double one = 100d / get_applications_count ();

		if (private_db_canbeused ()) {
			alist = db_priv.get_applications_all ();
		}
		if (shared_db_canbeused ()) {
			if (alist == null)
				alist = db_shared.get_applications_all ();
			else
				alist.add_all (db_shared.get_applications_all ());
		}
		if (alist == null)
			return false;
		_internal_emit_dbapps (one, ref alist);
		appList = alist;

		return true;
	}

	public bool set_application_dependencies (string appName, ArrayList<IPK.Dependency> deps) {
		if (is_root ()) {
			if (shared_db_canbeused (true))
				return db_shared.set_application_dependencies (appName, deps);
			else
				return false;
		} else {
			if (private_db_canbeused (true))
				return db_priv.set_application_dependencies (appName, deps);
			else
				return false;
		}
	}

	/* Dependency stuff */

	public bool add_dependency (IPK.Dependency dep) {
		bool ret = false;
		try {
			if (is_root ()) {
				if (shared_db_canbeused (true))
					ret = db_shared.add_dependency (dep);
				else
					return false;
			} else {
				if (private_db_canbeused (true))
					ret = db_priv.add_dependency (dep);
				else
					return false;
			}
		} catch (Error e) {
			emit_dberror (_("Unable to add a dependency to database: %s").printf (e.message));
			return false;
		}
		return ret;
	}

	public IPK.Dependency? get_dependency_by_id (string depIdName) {
		IPK.Dependency? dep = null;
		if (shared_db_canbeused ())
			dep = db_shared.get_dependency_by_id (depIdName);

		if (dep == null)
			if (private_db_canbeused ())
				dep = db_priv.get_dependency_by_id (depIdName);
		return dep;
	}

	public bool set_dependency_environment (string depIdName, string env) {
		bool ret = false;
		if (shared_db_canbeused ())
			ret = db_shared.set_dependency_environment (depIdName, env);

		if (!ret)
			if (private_db_canbeused ())
				ret = db_priv.set_dependency_environment (depIdName, env);

		return ret;
	}
}

} // End of namespace

