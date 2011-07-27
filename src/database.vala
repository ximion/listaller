/* database.vala - Easy-to-use interface to Listaller's software database(s)
 *
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@nlinux.org>
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

public enum AppSource {
	ALL,
	EXTERN,
	NATIVEPKG,
	UNKNOWN;
}

private class SoftwareDB : Object {
	private InternalDB? db_shared;
	private InternalDB? db_priv;
	private Settings conf;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void application (AppItem appid);
	public signal void progress_changed (int progress);

	public SoftwareDB (Settings liconf, bool include_shared = true) {
		db_shared = null;
		db_priv = null;
		// We just store the settings, so other objects can fetch them
		conf = liconf;

		if (include_shared) {
			db_shared = new InternalDB (true);

			/* If we open a shared DB and have root-access, don't open the
			 * private database. It makes no sense someone is working as root
			 * and installing private stuff into root's home-dir */
			if (!is_root ()) {
				db_priv = new InternalDB (false);
			}
		} else {
			// If we only want to open the personal DB, don't touch the shared one
			db_priv = new InternalDB (false);
		}

		if (db_priv != null) {
			db_priv.message.connect ( (m) => { this.message (m); } );
			db_priv.error_code.connect ( (e) => { this.error_code (e); } );
		}
		if (db_shared != null) {
			db_shared.message.connect ( (m) => { this.message (m); } );
			db_shared.error_code.connect ( (e) => { this.error_code (e); } );
		}
	}

	private void emit_dberror (string details) {
		// Emit error
		ErrorItem item = new ErrorItem(ErrorEnum.DATABASE_FAILURE);
		item.details = details;
		error_code (item);
	}

	private void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private bool shared_db_canbeused (bool error = false) {
		bool ret = true;
		if (db_shared == null) {
			ret = false;
		}

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
		if (db_priv == null) {
			if (error)
				emit_dberror (_("Tried to perform action on private software database, but the database is not opened! (this should never happen!)"));
			return false;
		}

		return true;
	}

	public Settings get_liconf () {
		return conf;
	}

	public bool open_read () {
		bool ret = true;
		if (db_priv != null)
			ret = db_priv.open_r ();
		if (db_shared != null) {
			if (shared_db_canbeused ()) {
				if (!ret)
					ret = db_shared.open_r ();
				else
					db_shared.open_r ();
			}
		}
		return ret;
	}

	public bool open_write () {
		bool ret = true;
		if (db_priv != null)
			ret = db_priv.open_rw ();
		if (db_shared != null) {
			if (shared_db_canbeused ()) {
				if (!ret)
					ret = db_shared.open_rw ();
				else
					db_shared.open_rw ();
			}
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

	public bool add_application (AppItem app) {
		if (app.shared) {
			if (shared_db_canbeused (true))
				return db_shared.add_application (app);
			else
				return false;
		} else {
			if (private_db_canbeused (true))
				return db_priv.add_application (app);
			else
				return false;
		}
	}

	public bool add_application_filelist (AppItem app, Collection<IPK.FileEntry> flist) {
		if (app.shared) {
			if (shared_db_canbeused (true))
				return db_shared.add_application_filelist (app, flist);
			else
				return false;
		} else {
			if (private_db_canbeused (true))
				return db_priv.add_application_filelist (app, flist);
			else
				return false;
		}
	}

	public ArrayList<string>? get_application_filelist (AppItem app) {
		if (app.shared) {
			if (shared_db_canbeused (true))
				return db_shared.get_application_filelist (app);
			else
				return null;
		} else {
			if (private_db_canbeused (true))
				return db_priv.get_application_filelist (app);
			else
				return null;
		}
	}

	public int get_applications_count () {
		int cnt = 0;
		if (shared_db_canbeused ())
			cnt += db_shared.get_applications_count ();
		if (private_db_canbeused ())
			cnt += db_priv.get_applications_count ();

		return cnt;
	}

	public AppItem? get_application_by_idname (string appIdName) {
		AppItem? app = null;

		if (shared_db_canbeused ())
			app = db_shared.get_application_by_idname (appIdName);

		if (app == null)
			if (private_db_canbeused ())
				app = db_priv.get_application_by_idname (appIdName);

		return app;
	}

	public AppItem? get_application_by_id (AppItem app) {
		if (app.shared) {
			if (shared_db_canbeused (true))
				return db_shared.get_application_by_idname (app.idname);
			else
				return null;
		} else {
			if (private_db_canbeused (true))
				return db_priv.get_application_by_idname (app.idname);
			else
				return null;
		}
	}

	public AppItem? get_application_by_fullname (string appFullName) {
		AppItem? app = null;
		if (shared_db_canbeused ())
			app = db_shared.get_application_by_fullname (appFullName);

		if (app == null)
			if (private_db_canbeused ())
				app = db_priv.get_application_by_fullname (appFullName);

		return app;
	}

	public bool remove_application (AppItem app) {
		if (app.shared) {
			if (shared_db_canbeused (true))
				return db_shared.remove_application (app);
			else
				return false;
		} else {
			if (private_db_canbeused (true))
				return db_priv.remove_application (app);
			else
				return false;
		}
	}

	public ArrayList<AppItem> find_applications ([CCode (array_null_terminated = true, array_length = false)] string[] values) {
		var list = new ArrayList<AppItem> ();
		if (shared_db_canbeused ()) {
			var tmp = db_shared.find_applications (values);
			list.add_all (tmp);
		}
		if (private_db_canbeused ()) {
			var tmp = db_priv.find_applications (values);
			list.add_all (tmp);
		}

		return list;
	}

	public void _internal_process_dbapps (InternalDB db, double one, ref ArrayList<AppItem> appList) {
		uint i = 1;
		AppItem? capp = db.get_application_by_dbid (i);
		while (capp != null) {
			application (capp);
			appList.add (capp);
			progress_changed ((int) Math.round (one * i));

			i++;
			capp = db.get_application_by_dbid (i);
		}
	}

	public bool find_all_applications (AppSource filter, out ArrayList<AppItem> appList = null) {
		ArrayList<AppItem> alist = new ArrayList<AppItem> ();

		double one = 100d / get_applications_count ();

		if (private_db_canbeused ()) {
			_internal_process_dbapps (db_priv, one, ref alist);
		}
		if (shared_db_canbeused ()) {
			_internal_process_dbapps (db_shared, one, ref alist);
		}
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
		if (is_root ()) {
			if (shared_db_canbeused (true))
				return db_shared.add_dependency (dep);
			else
				return false;
		} else {
			if (private_db_canbeused (true))
				return db_priv.add_dependency (dep);
			else
				return false;
		}
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

	/* Testing stuff */

	public void _remove_db_lock () {
		if (shared_db_canbeused ())
			db_shared._remove_db_lock ();
		if (private_db_canbeused ())
			db_priv._remove_db_lock ();
	}
}

} // End of namespace

