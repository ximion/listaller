/* database-internal.vala - The Listaller application & dependency information storage
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
using Sqlite;
using Listaller;
using Listaller.Utils;

namespace Listaller {

private const string DATABASE = ""
		+ "CREATE TABLE IF NOT EXISTS applications ("
		+ "id INTEGER PRIMARY KEY, "
		+ "name TEXT UNIQUE NOT NULL, "
		+ "full_name TEXT NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "desktop_file TEXT,"
		+ "author TEXT, "
		+ "publisher TEXT, "
		+ "categories TEXT, "
		+ "description TEXT, "
		+ "homepage TEXT, "
		+ "architecture TEXT NOT NULL, "
		+ "install_time INTEGER, "
		+ "dependencies TEXT, "
		+ "origin TEXT NOT NULL"
		+ "); "
		+ "CREATE TABLE IF NOT EXISTS dependencies ("
		+ "id INTEGER PRIMARY KEY, "
		+ "name TEXT UNIQUE NOT NULL, "
		+ "full_name TEXT NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "description TEXT, "
		+ "author TEXT, "
		+ "homepage TEXT, "
		+ "architecture TEXT NOT NULL, "
		+ "install_time INTEGER, "
		+ "components TEXT NOT NULL,"
		+ "environment TEXT"
		+ ");" +
		"";

private const string appcols = "name, full_name, version, desktop_file, author, publisher, categories, " +
			"description, homepage, architecture, install_time, dependencies, origin";
private const string depcols = "name, full_name, version, description, author, homepage, architecture, " +
			"install_time, components, environment";

private enum AppRow {
	DBID = 0,
	IDNAME = 1,
	FULLNAME = 2,
	VERSION = 3,
	DESKTOPFILE = 4,
	AUTHOR = 5,
	PUBLISHER = 6,
	CATEGORIES = 7,
	DESCRIPTION = 8,
	HOMEPAGE = 9,
	ARCHITECTURE = 10,
	INST_TIME = 11,
	DEPS = 12,
	ORIGIN = 13;
}

private enum DepRow {
	DBID = 0,
	IDNAME = 1,
	FULLNAME = 2,
	VERSION = 3,
	DESCRIPTION = 4,
	AUTHOR = 5,
	HOMEPAGE = 6,
	ARCHITECTURE = 7,
	INST_TIME = 8,
	COMPONENTS = 9,
	ENVIRONMENT = 10;
}

public errordomain DatabaseError {
	ERROR,
	BACKING,
	MEMORY,
	ABORT,
	LIMITS,
	TYPESPEC,
	LOCKED
}

public enum DatabaseStatus {
	OPENED,
	LOCKED,
	UNLOCKED,
	SUCCESS,
	FAILURE,
	CLOSED;

	public string to_string() {
		switch (this) {
			case OPENED:
				return _("Software database opened");

			case LOCKED:
				return _("Database locked");

			case UNLOCKED:
				return _("Database unlocked");

			case SUCCESS:
				return _("Database action successful");

			case FAILURE:
				return _("Database action failed");

			case CLOSED:
				return _("Software database closed");

			default:
				return _("Software database message (%d)").printf((int) this);
		}
	}
}

private class InternalDB : Object {
	private Database db;

	private bool locked;
	private string regdir;
	private string dbname;
	private bool shared_db;
	private bool writeable;

	private Sqlite.Statement insert_app;
	private Sqlite.Statement insert_dep;

	public signal void message (MessageItem message);

	public InternalDB (bool sumode, bool _testmode = false) {
		// Create internal dummy configuration to fetch required data
		var tmpConf = new Listaller.Settings (sumode);
		tmpConf.testmode = _testmode;

		// Path with additional data (e.g. the file-list or icons) which is not stored in the SQLite DB
		regdir = Path.build_filename (tmpConf.appregister_dir (), "info", null);
		// The database filename
		dbname = tmpConf.database_file ();
		// Whether we fetch data from the "shared" or "private" application database
		shared_db = sumode;
		writeable = false;
	}

	~InternalDB () {
		// Delete the lock
		remove_db_lock ();
	}

	public bool database_locked () {
		// If no DB is found, it can't be locked, right?
		if (!FileUtils.test (dbname, FileTest.IS_REGULAR)) {
			locked = false;
			return locked;
		}

		Database tmpDB;
		int rc = Database.open_v2 (dbname, out db);
		if (rc == Sqlite.BUSY) {
			locked = true;
		} else {
			locked = false;
		}
		return locked;
	}

	public bool database_writeable () {
		return (!database_locked ()) && (writeable);
	}

	private void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	public string get_database_name () {
		return dbname;
	}

	private bool open_db () throws DatabaseError {
		bool create_db = false;
		int rc;

		if (!FileUtils.test (dbname, FileTest.IS_REGULAR)) {
			emit_message ("Software database does not exist - will be created.");
			create_db = true;
		}

		rc = Database.open_v2 (dbname, out db);
		if (rc == Sqlite.BUSY) {
			locked = true;
			throw new DatabaseError.LOCKED (_("Tried to access locked database! This shouldn't happen."));
		}
		if (rc != Sqlite.OK) {
			string msg = "Can't open database! (Message: %d, %s)".printf (rc, db.errmsg ());
			stderr.printf (msg);
			throw new DatabaseError.ERROR (msg);
		}

		create_dir_parents (regdir);

		// Ensure the database is okay and all tables are created
		if ((create_db) && (!update_db_tables ())) {
			throw new DatabaseError.ERROR (_("Could not create/update software database!"));
		}

		// Prepare statements

		// InsterApp statement
		try {
			db_assert (db.prepare_v2 ("INSERT INTO applications (" + appcols + ") "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
					-1, out insert_app), "prepare app insert statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		// InsertDep statement
		try {
			db_assert (db.prepare_v2 ("INSERT INTO dependencies (" + depcols + ") "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
					-1, out insert_dep), "prepare dependency insert statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	public bool open_rw () throws DatabaseError {
		bool ret;

		// If database is locked, we should not try to write on it
		if (database_locked ()) {
			// We set locked to false, because this DB is NOT locked (because it can't be opened)
			locked = false;
			return false;
		}

		ret = open_db ();
		return_if_fail (ret == true);

		// Lock the database
		try {
			db_assert (db.exec ("PRAGMA locking_mode = RESERVED"));
		} catch (Error e) {
			throw e;
		}


		// DB is now locked
		locked = true;
		// ... and writeable
		writeable = true;

		return true;
	}

	public bool open_r () throws DatabaseError {
		bool ret;
		try {
			ret = open_db ();
		} catch (DatabaseError e) {
			throw e;
		}

		return ret;
	}

	protected void remove_db_lock () throws DatabaseError {
		if (db == null)
			return;

		if (locked) {
			// Set db locking back to normal
			try {
				db_assert (db.exec ("PRAGMA locking_mode = NORMAL"));
			} catch (Error e) {
				throw e;
			}
			locked = false;
		}
	}

	/*
	 * This method will throw an error on an SQLite return code unless it's OK, DONE, or ROW, which
	 * are considered normal results.
	 */
	protected void db_assert (int result, string action_name = "") throws DatabaseError {
		if (action_name == "")
			action_name = "generic action";
		string msg = _("Database action '%s' failed: %s").printf (action_name, db.errmsg ());

		switch (result) {
			case Sqlite.OK:
			case Sqlite.DONE:
			case Sqlite.ROW:
				return;

			case Sqlite.PERM:
			case Sqlite.BUSY:
				throw new DatabaseError.LOCKED (_("Tried to access locked database! This shouldn't happen. Message: %s").printf (msg));
			case Sqlite.READONLY:
			case Sqlite.IOERR:
			case Sqlite.CORRUPT:
			case Sqlite.CANTOPEN:
			case Sqlite.NOLFS:
			case Sqlite.AUTH:
			case Sqlite.FORMAT:
			case Sqlite.NOTADB:
				throw new DatabaseError.BACKING (msg);

			case Sqlite.NOMEM:
				throw new DatabaseError.MEMORY (msg);

			case Sqlite.ABORT:
			case Sqlite.LOCKED:
			case Sqlite.INTERRUPT:
				throw new DatabaseError.ABORT (msg);

			case Sqlite.FULL:
			case Sqlite.EMPTY:
			case Sqlite.TOOBIG:
			case Sqlite.CONSTRAINT:
			case Sqlite.RANGE:
				throw new DatabaseError.LIMITS (msg);

			case Sqlite.SCHEMA:
			case Sqlite.MISMATCH:
				throw new DatabaseError.TYPESPEC (msg);

			case Sqlite.ERROR:
			case Sqlite.INTERNAL:
			case Sqlite.MISUSE:
			default:
				throw new DatabaseError.ERROR (msg);
		}
	}

	protected bool has_table (string table_name) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("PRAGMA table_info(%s)".printf(table_name), -1, out stmt);

		try {
			db_assert (res, "prepare db");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		res = stmt.step ();

		return (res != Sqlite.DONE);
	}

	protected bool update_db_tables () throws DatabaseError {
		Sqlite.Statement stmt;

		int res = db.exec (DATABASE);
		try {
			db_assert (res);
		} catch (Error e) {
			warning (_("Unable to create/update database tables: %s").printf (db.errmsg ()));
			return false;
		}
		try {
			db_assert (db.exec ("PRAGMA synchronous = OFF"));
			db_assert (db.exec ("PRAGMA temp_store = MEMORY"));
			db_assert (db.exec ("PRAGMA journal_mode = MEMORY"));
			db_assert (db.exec ("PRAGMA count_changes = OFF"));
		} catch (Error e) {
			warning (e.message);
			return false;
		}

		// db_assert (db.exec ("PRAGMA user_version = %d".printf (SUPPORTED_VERSION)));

		return true;
	}

	/* Application stuff */

	public bool add_application (AppItem item) throws DatabaseError {
		if (!database_writeable ()) {
			throw new DatabaseError.ERROR (_("Tried to write on readonly database! (This should never happen)"));
		}

		// Set install timestamp
		DateTime dt = new DateTime.now_local ();
		item.install_time = dt.to_unix ();

		// Assign values
		try {
			db_assert (insert_app.bind_text (AppRow.IDNAME, item.idname), "assign value");

			db_assert (insert_app.bind_text (AppRow.VERSION, item.version), "assign value");

			db_assert (insert_app.bind_text (AppRow.FULLNAME, item.full_name), "assign value");

			db_assert (insert_app.bind_text (AppRow.DESKTOPFILE, item.desktop_file), "assign value");

			db_assert (insert_app.bind_text (AppRow.AUTHOR, item.author), "assign value");

			db_assert (insert_app.bind_text (AppRow.PUBLISHER, item.publisher), "assign value");

			db_assert (insert_app.bind_text (AppRow.CATEGORIES, item.categories), "assign value");

			db_assert (insert_app.bind_text (AppRow.DESCRIPTION, "%s\n\n%s".printf (item.summary, item.description)), "assign value");

			db_assert (insert_app.bind_text (AppRow.HOMEPAGE, item.website), "assign value");

			//TODO: Handle arch field
			db_assert (insert_app.bind_text (AppRow.ARCHITECTURE, "current"), "assign value");

			db_assert (insert_app.bind_int64 (AppRow.INST_TIME, item.install_time), "assign value");

			db_assert (insert_app.bind_text (AppRow.ORIGIN, item.origin.to_string ()), "assign value");

			db_assert (insert_app.bind_text (AppRow.DEPS, item.dependencies), "assign value");

			db_assert (insert_app.step (), "execute app insert");

			db_assert (insert_app.reset (), "reset statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	public bool add_application_filelist (AppItem aid, Collection<IPK.FileEntry> flist) {
		if (!database_writeable ()) {
			throw new DatabaseError.ERROR (_("Tried to write on readonly database! (This should not happen)"));
		}
		string metadir = Path.build_filename (regdir, aid.idname, null);
		create_dir_parents (metadir);

		try {
			var file = File.new_for_path (Path.build_filename (metadir, "files.list", null));
			{
				var file_stream = file.create (FileCreateFlags.NONE);

				if (!file.query_exists ())
					return false;

				var data_stream = new DataOutputStream (file_stream);
				data_stream.put_string ("# File list for " + aid.full_name + "\n\n");
				// Now write file list to file
				foreach (IPK.FileEntry fe in flist) {
					if (fe.is_installed ()) {
						string fname = fe.fname_installed;
						if (!shared_db)
							fname = fold_user_dir (fname);
						data_stream.put_string (fname + "\n");
					}
				}
			}
		} catch (Error e) {
			throw new DatabaseError.ERROR (_("Unable to write application file list! Message: %s").printf (e.message));
		}
		return true;
	}

	public ArrayList<string>? get_application_filelist (AppItem app) throws DatabaseError {
		string metadir = Path.build_filename (regdir, app.idname, null);

		var file = File.new_for_path (Path.build_filename (metadir, "files.list", null));
		if (!file.query_exists ()) {
			return null;
		}

		ArrayList<string> flist = new ArrayList<string> ();
		try {
			var dis = new DataInputStream (file.read ());
			string line;
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				if ((!line.has_prefix ("#")) && (line.strip () != "")) {
					string fname = line;
					if (!shared_db)
						fname = expand_user_dir (fname);
					flist.add (fname);
				}
			}
		} catch (Error e) {
			throw new DatabaseError.ERROR (_("Unable to fetch application file list! Message: %s").printf (e.message));
		}
		return flist;
	}

	public int get_applications_count () throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT Count(*) FROM applications", -1, out stmt);

		try {
			db_assert (res, "get applications count");
			db_assert (stmt.step ());
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		int count = stmt.column_int (0);
		return count;
	}

	private AppItem? retrieve_app_item (Sqlite.Statement stmt) {
		AppItem item = new AppItem.blank ();

		item.dbid = stmt.column_int (AppRow.DBID);
		item.idname = stmt.column_text (AppRow.IDNAME);
		item.full_name = stmt.column_text (AppRow.FULLNAME);
		item.version = stmt.column_text (AppRow.VERSION);
		item.desktop_file = stmt.column_text (AppRow.DESKTOPFILE);
		item.author = stmt.column_text (AppRow.AUTHOR);
		item.publisher = stmt.column_text (AppRow.PUBLISHER);
		item.categories = stmt.column_text (AppRow.CATEGORIES);
		item.website = stmt.column_text (AppRow.HOMEPAGE);

		string s = stmt.column_text (AppRow.DESCRIPTION);
		string[] desc = s.split ("\n\n", 2);
		if (desc[0] != null) {
			item.summary = desc[0];
			if (desc[1] != null)
				item.description = desc[1];
		} else {
			item.summary = s;
		}

		item.install_time = stmt.column_int (AppRow.INST_TIME);
		item.set_origin_from_string (stmt.column_text (AppRow.ORIGIN));
		item.dependencies = stmt.column_text (AppRow.DEPS);
		item.shared = shared_db;

		return item;
	}

	public AppItem? get_application_by_idname (string appIdName) throws DatabaseError {
		Sqlite.Statement stmt;
		try {
			db_assert (db.prepare_v2 ("SELECT * FROM applications WHERE name=?", -1, out stmt),
				   "prepare find app by idname statement");

			db_assert (stmt.bind_text (1, appIdName), "bind value");

			int res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_fullname (string appFullName) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM applications WHERE full_name=?", -1, out stmt);

		try {
			db_assert (res, "get application (by full_name)");
			db_assert (stmt.bind_text (1, appFullName), "bind value");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_dbid (uint databaseId) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM applications WHERE id=?", -1, out stmt);

		try {
			db_assert (res, "get application (by database-id)");
			db_assert (stmt.bind_int64 (1, databaseId), "bind value");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_name_version (string appName, string appVersion) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM applications WHERE name=? AND version=?", -1, out stmt);
		try {
			db_assert (res, "get application (by full_name and version)");
			db_assert (stmt.bind_text (1, appName), "bind value");
			db_assert (stmt.bind_text (2, appVersion), "bind value");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public bool remove_application (AppItem app) throws DatabaseError {
		bool ret = true;
		string metadir = Path.build_filename (regdir, app.idname, null);
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("DELETE FROM applications WHERE name=?", -1, out stmt);

		try {
			db_assert (res, "delete application (prepare statement)");
			db_assert (stmt.bind_text (1, app.idname), "bind text");
			db_assert (stmt.step(), "execute action");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		ret = delete_dir_recursive (metadir);
		if (!ret)
			warning ("Could not remove metadata directory for application-id %s!".printf (app.idname));
		return ret;
	}

	private bool string_in_app_item (AppItem item, string s) {
		string str = s.down ();
		if (item.full_name.down ().index_of (str) > -1)
			return true;
		if (item.summary.down ().index_of (str) > -1)
			return true;
		if (item.description.down ().index_of (str) > -1)
			return true;
		return false;
	}

	public ArrayList<AppItem> find_applications ([CCode (array_null_terminated = true, array_length = false)] string[] values) {
		ArrayList<AppItem> resList = new ArrayList<AppItem> ();
		if (values[0] == null)
			return resList;

		HashSet<AppItem> tmpList = new HashSet<AppItem> ();

		Sqlite.Statement stmt;
		try {
			db_assert (db.prepare_v2 ("SELECT * FROM applications", -1, out stmt),
				   "prepare list all applications statement");

			int rc = 0;
			do {
				rc = stmt.step();
				switch (rc) {
					case Sqlite.DONE:
						break;
					case Sqlite.ROW:
						// Fetch a new AppItem
						AppItem? tmpApp = retrieve_app_item (stmt);
						// Fast sanity checks
						if (tmpApp != null)
							tmpApp.fast_check ();
						else
							throw new DatabaseError.ERROR ("Unable to retrieve an application from database! DB might be in an inconstistent state!");

						// Now check if app matches our criteroa
						for (int j = 0; values[j] != null; j++) {
							if (string_in_app_item (tmpApp, values[j])) {
								tmpList.add (tmpApp);
								break;
							}
						}

						break;
					default:
						db_assert (rc, "execute");
						break;
				}
			} while (rc == Sqlite.ROW);

		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		resList.add_all (tmpList.read_only_view);

		return resList;
	}

	public bool set_application_dependencies (string appName, ArrayList<IPK.Dependency> deps) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("UPDATE applications SET dependencies=? WHERE name=?", -1, out stmt);

		string depstr = "";
		foreach (IPK.Dependency d in deps)
			depstr += d.idname + "\n";

		try {
			db_assert (res, "update application deps (by name)");
			db_assert (stmt.bind_text (1, depstr), "bind text value");
			db_assert (stmt.bind_text (2, appName), "bind text value");
			db_assert (stmt.step ());
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	/* Dependency stuff */

	public bool add_dependency (IPK.Dependency dep) throws DatabaseError {
		if (!database_writeable ()) {
			throw new DatabaseError.ERROR (_("Tried to write on readonly database! (This should never happen)"));
		}

		// Set install timestamp
		DateTime dt = new DateTime.now_local ();
		dep.install_time = dt.to_unix ();

		try {
			// Assign values
			db_assert (insert_dep.bind_text (DepRow.IDNAME, dep.idname), "bind value");

			db_assert (insert_dep.bind_text (DepRow.FULLNAME, dep.full_name), "bind value");

			db_assert (insert_dep.bind_text (DepRow.VERSION, dep.version), "bind value");

			db_assert (insert_dep.bind_text (DepRow.DESCRIPTION, "%s\n\n%s".printf (dep.summary, dep.description)), "bind value");

			db_assert (insert_dep.bind_text (DepRow.HOMEPAGE, dep.homepage), "bind value");

			db_assert (insert_dep.bind_text (DepRow.AUTHOR, dep.author), "bind value");

			db_assert (insert_dep.bind_int64 (DepRow.INST_TIME, dep.install_time), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ARCHITECTURE, dep.architecture), "bind value");

			db_assert (insert_dep.bind_text (DepRow.COMPONENTS, dep.get_installdata_as_string ()), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ENVIRONMENT, dep.environment), "bind value");

			db_assert (insert_dep.step (), "add dependency");

			db_assert (insert_dep.reset (), "reset insert_dep statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	private IPK.Dependency? retrieve_dependency (Sqlite.Statement stmt) {
		IPK.Dependency dep = new IPK.Dependency.blank ();

		dep.idname = stmt.column_text (DepRow.IDNAME);
		dep.full_name = stmt.column_text (DepRow.FULLNAME);
		dep.version = stmt.column_text (DepRow.VERSION);

		string s = stmt.column_text (DepRow.DESCRIPTION);
		string[] desc = s.split ("\n\n", 2);
		if (desc[0] != null) {
			dep.summary = desc[0];
			if (desc[1] != null)
				dep.description = desc[1];
		} else {
			dep.summary = s;
		}

		dep.homepage = stmt.column_text (DepRow.HOMEPAGE);
		dep.author = stmt.column_text (DepRow.AUTHOR);
		dep.install_time = stmt.column_int (DepRow.INST_TIME);
		dep.architecture = stmt.column_text (DepRow.ARCHITECTURE);
		dep.set_installdata_from_string (stmt.column_text (DepRow.COMPONENTS));
		dep.environment = stmt.column_text (DepRow.ENVIRONMENT);
		// It's in the db, so this dependency is certainly satisfied
		dep.satisfied = true;

		return dep;
	}

	public IPK.Dependency? get_dependency_by_id (string depIdName) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM dependencies WHERE name=?", -1, out stmt);

		try {
			db_assert (res, "get dependency (by id)");
			db_assert (stmt.bind_text (1, depIdName), "bind text");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		IPK.Dependency? dep = retrieve_dependency (stmt);
		return dep;
	}

}

} // End of namespace

