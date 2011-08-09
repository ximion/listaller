/* database-int.vala - The Listaller application & dependency information storage
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
using Sqlite;
using Listaller;
using Listaller.Utils;

namespace Listaller {

private const string DATABASE = ""
		+ "CREATE TABLE IF NOT EXISTS applications ("
		+ "id INTEGER PRIMARY KEY, "
		+ "name TEXT UNIQUE NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "full_name TEXT NOT NULL, "
		+ "desktop_file TEXT UNIQUE,"
		+ "author TEXT, "
		+ "publisher TEXT, "
		+ "categories TEXT, "
		+ "description TEXT, "
		+ "install_time INTEGER, "
		+ "origin TEXT NOT NULL, "
		+ "dependencies TEXT"
		+ "); "
		+ "CREATE TABLE IF NOT EXISTS dependencies ("
		+ "id INTEGER PRIMARY KEY, "
		+ "name TEXT UNIQUE NOT NULL, "
		+ "full_name TEXT NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "description TEXT, "
		+ "homepage TEXT, "
		+ "author TEXT, "
		+ "install_time INTEGER, "
		+ "environment TEXT"
		+ ");" +
		"";

private const string appcols = "id, name, version, full_name, desktop_file, author, publisher, categories, " +
			"description, install_time, origin, dependencies";
private const string depcols = "id, name, full_name, version, description, homepage, author, " +
			"install_time, environment";

private enum AppRow {
	DBID = 0,
	IDNAME = 1,
	VERSION = 2,
	FULLNAME = 3,
	DESKTOPFILE = 4,
	AUTHOR = 5,
	PUBLISHER = 6,
	CATEGORIES = 7,
	DESCRIPTION = 8,
	INSTTIME = 9,
	ORIGIN = 10,
	DEPS = 11;
}

public errordomain DatabaseError {
	ERROR,
	BACKING,
	MEMORY,
	ABORT,
	LIMITS,
	TYPESPEC
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
	private string dblockfile;
	private string regdir;
	private string dbname;
	private bool shared_db;

	private Sqlite.Statement insert_app;

	public signal void db_status_changed (DatabaseStatus newstatus, string message);
	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);

	public InternalDB (bool sumode, bool _testmode = false) {
		// Create internal dummy configuration to fetch required data
		var tmpConf = new Listaller.Settings (sumode);
		tmpConf.testmode = _testmode;

		// File indication the database is locked (UGLY solution, we need something better, later)
		dblockfile = tmpConf.appregister_dir () + "/lock";
		// Path with additional data (e.g. the file-list or icons) which is not stored in the SQLite DB
		regdir = Path.build_filename (tmpConf.appregister_dir (), "info", null);
		// The database filename
		dbname = tmpConf.database_file ();
		// Whether we fetch data from the "shared" or "private" application database
		shared_db = sumode;
	}

	~InternalDB () {
		// Delete the lock
		remove_db_lock ();
	}

	public bool database_locked () {
		if (FileUtils.test (dblockfile, FileTest.IS_REGULAR)) {
			locked = true;
		} else {
			locked = false;
		}
		return locked;
	}

	private void dbstatus_changed (DatabaseStatus dbs, string details) {
		if (dbs == DatabaseStatus.FAILURE) {
			// Emit error
			ErrorItem item = new ErrorItem(ErrorEnum.DATABASE_FAILURE);
			item.details = details;
			error_code (item);
		}
		db_status_changed (dbs, details);
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

	private bool open_db () {
		bool create_db = false;
		int rc;

		if (!FileUtils.test (dbname, FileTest.IS_REGULAR)) {
			emit_message ("Software database does not exist - will be created.");
			create_db = true;
		}

		rc = Database.open_v2 (dbname, out db);
		if (rc != Sqlite.OK) {
			string msg = "Can't open database! (Message: %d, %s)".printf (rc, db.errmsg ());
			stderr.printf (msg);
			dbstatus_changed (DatabaseStatus.FAILURE, msg);
			return false;
		}

		create_dir_parents (regdir);
		dbstatus_changed (DatabaseStatus.OPENED, "");

		// Ensure the database is okay and all tables are created
		if ((create_db) && (!update_db_tables ())) {
			dbstatus_changed (DatabaseStatus.FAILURE, _("Could not create/update software database!"));
			return false;
		}

		// Prepare statements

		try {
			db_assert (db.prepare_v2 ("INSERT INTO applications (name, version, full_name, desktop_file, author, publisher, categories, "
				+ "description, install_time, origin, dependencies) "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
					-1, out insert_app), "prepare insert into app statement");
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
		}

		return true;
	}

	public bool open_rw () {
		bool ret;

		// If database is locked, we should not try to write on it
		if (database_locked ()) {
			// We set locked to false, because this DB is NOT locked (because it can't be opened)
			locked = false;
			return false;
		}

		ret = open_db ();
		return_if_fail (ret == true);

		// Drop "lock" file
		File lfile = File.new_for_path (dblockfile);
		try {
			lfile.create (FileCreateFlags.NONE);
		} catch (Error e) {
			stdout.printf ("Error: %s\n", e.message);
			return false;
		}

		// Test for the existence of file
		if (!lfile.query_exists ()) {
			stdout.printf ("Error: Unable to create lock file!");
			return false;
		}
		// DB is now locked
		locked = true;
		dbstatus_changed (DatabaseStatus.LOCKED, "");
		return true;
	}

	public bool open_r () {
		bool ret;
		ret = open_db ();
		return ret;
	}

	protected void remove_db_lock () {
		if (locked) {
			File lfile = File.new_for_path (dblockfile);
			try {
				if (lfile.query_exists ()) {
					lfile.delete ();
				}
			} catch (Error e) {
				li_error (_("Unable to remove database lock! (Message: %s)").printf (e.message));
			}
		}
	}

	public void _remove_db_lock () {
		remove_db_lock ();
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

	protected bool has_table (string table_name) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("PRAGMA table_info(%s)".printf(table_name), -1, out stmt);

		try {
			db_assert (res, "prepare db");
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
		}

		res = stmt.step ();

		return (res != Sqlite.DONE);
	}

	protected bool update_db_tables () {
		Sqlite.Statement stmt;

		int res = db.exec (DATABASE);
		try {
			db_assert (res);
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE,
				  _("Unable to create/update database tables: %s").printf (db.errmsg ()));
			return false;
		}
		try {
			db_assert (db.exec ("PRAGMA synchronous = OFF"));
			db_assert (db.exec ("PRAGMA temp_store = MEMORY"));
			db_assert (db.exec ("PRAGMA journal_mode = MEMORY"));
			db_assert (db.exec ("PRAGMA count_changes = OFF"));
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
		}

		// db_assert (db.exec ("PRAGMA user_version = %d".printf (SUPPORTED_VERSION)));

		return true;
	}

	/* Application stuff */

	public bool add_application (AppItem item) {
		if (!locked) {
			dbstatus_changed (DatabaseStatus.FAILURE,
					  _("Tried to write on unlocked (readonly) database! (This should not happen)"));
			return false;
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

			db_assert (insert_app.bind_int64 (AppRow.INSTTIME, item.install_time), "assign value");

			db_assert (insert_app.bind_text (AppRow.ORIGIN, item.origin.to_string ()), "assign value");

			db_assert (insert_app.bind_text (AppRow.DEPS, item.dependencies), "assign value");

			db_assert (insert_app.step (), "execute app insert");

			db_assert (insert_app.reset (), "reset statement");
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return false;
		}

		return true;
	}

	public bool add_application_filelist (AppItem aid, Collection<IPK.FileEntry> flist) {
		if (!locked) {
			dbstatus_changed (DatabaseStatus.FAILURE,
					  _("Tried to write on unlocked (readonly) database! (This should not happen)"));
			return false;
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
					if (fe.installed) {
						string fname = fe.fname_installed;
						if (!shared_db)
							fname = fold_user_dir (fname);
						data_stream.put_string (fname + "\n");
					}
				}
			}
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE,
					  _("Unable to write application file list! Message: %s").printf (e.message));
			return false;
		}
		return true;
	}

	public ArrayList<string>? get_application_filelist (AppItem app) {
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
			dbstatus_changed (DatabaseStatus.FAILURE,
					  _("Unable to fetch application file list! Message: %s").printf (e.message));
			return null;
		}
		return flist;
	}

	public int get_applications_count () {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT Count(*) FROM applications", -1, out stmt);

		try {
			db_assert (res, "get applications count");
			db_assert (stmt.step ());
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return -1;
		}

		int count = stmt.column_int (0);
		return count;
	}

	private AppItem? retrieve_app_item (Sqlite.Statement stmt) {
		AppItem item = new AppItem.blank ();

		item.dbid = stmt.column_int (AppRow.DBID);
		item.idname = stmt.column_text (AppRow.IDNAME);
		item.version = stmt.column_text (AppRow.VERSION);
		item.full_name = stmt.column_text (AppRow.FULLNAME);
		item.desktop_file = stmt.column_text (AppRow.DESKTOPFILE);
		item.author = stmt.column_text (AppRow.AUTHOR);
		item.publisher = stmt.column_text (AppRow.PUBLISHER);
		item.categories = stmt.column_text (AppRow.CATEGORIES);

		string s = stmt.column_text (AppRow.DESCRIPTION);
		string[] desc = s.split ("\n\n", 2);
		if (desc[0] != null) {
			item.summary = desc[0];
			if (desc[1] != null)
				item.description = desc[1];
		} else {
			item.summary = s;
		}

		item.install_time = stmt.column_int (AppRow.INSTTIME);
		item.set_origin_from_string (stmt.column_text (AppRow.ORIGIN));
		item.dependencies = stmt.column_text (AppRow.DEPS);
		item.shared = shared_db;

		return item;
	}

	public AppItem? get_application_by_idname (string appIdName) {
		Sqlite.Statement stmt;
		try {
			db_assert (db.prepare_v2 ("SELECT " + appcols + " FROM applications WHERE name=?", -1, out stmt),
				   "prepare find app by idname statement");

			db_assert (stmt.bind_text (1, appIdName), "bind value");

			int res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return null;
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_fullname (string appFullName) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT " + appcols + " FROM applications WHERE full_name=?", -1, out stmt);

		try {
			db_assert (res, "get application (by full_name)");
			db_assert (stmt.bind_text (1, appFullName), "bind value");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return null;
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_dbid (uint databaseId) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT " + appcols + " FROM applications WHERE id=?", -1, out stmt);

		try {
			db_assert (res, "get application (by database-id)");
			db_assert (stmt.bind_int64 (1, databaseId), "bind value");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return null;
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_name_version (string appName, string appVersion) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT " + appcols + " FROM applications WHERE name=? AND version=?", -1, out stmt);
		try {
			db_assert (res, "get application (by full_name and version)");
			db_assert (stmt.bind_text (1, appName), "bind value");
			db_assert (stmt.bind_text (2, appVersion), "bind value");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return null;
		}

		AppItem? item = retrieve_app_item (stmt);

		// Fast sanity checks
		if (item != null)
			item.fast_check ();

		return item;
	}

	public bool remove_application (AppItem app) {
		bool ret = true;
		string metadir = Path.build_filename (regdir, app.idname, null);
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("DELETE FROM applications WHERE name=?", -1, out stmt);

		try {
			db_assert (res, "delete application (prepare statement)");
			db_assert (stmt.bind_text (1, app.idname), "bind text");
			db_assert (stmt.step(), "execute action");
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return false;
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

		int i = 1;
		AppItem tmpApp = get_application_by_dbid (i);
		while (tmpApp != null) {
			for (int j = 0; values[j] != null; j++) {
				if (string_in_app_item (tmpApp, values[j])) {
					tmpList.add (tmpApp);
					break;
				}
			}
			i++;
			tmpApp = get_application_by_dbid (i);
		}

		resList.add_all (tmpList.read_only_view);
		return resList;
	}

	public bool set_application_dependencies (string appName, ArrayList<IPK.Dependency> deps) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("UPDATE applications SET dependencies=? WHERE name=?", -1, out stmt);

		string depstr = "";
		foreach (IPK.Dependency d in deps)
			depstr = d.idname + "\n";

		try {
			db_assert (res, "update application deps (by name)");
			db_assert (stmt.bind_text (1, depstr), "bind text value");
			db_assert (stmt.bind_text (2, appName), "bind text value");
			db_assert (stmt.step ());
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return false;
		}

		return true;
	}

	/* Dependency stuff */

	public bool add_dependency (IPK.Dependency dep) {
		if (!locked) {
			dbstatus_changed (DatabaseStatus.FAILURE,
					  _("Tried to write on unlocked (readonly) database! (This should not happen)"));
			return false;
		}
		Sqlite.Statement stmt;
		int res = db.prepare_v2 (
			"INSERT INTO dependencies (name, full_name, version, description, homepage, author, " +
			"install_time, environment) "
			+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
				   -1, out stmt);

		// Set install timestamp
		DateTime dt = new DateTime.now_local ();
		dep.install_time = dt.to_unix ();

		try {
			db_assert (res, "add application");

			// Assign values
			db_assert (stmt.bind_text (1, dep.idname), "bind value");

			db_assert (stmt.bind_text (2, dep.full_name), "bind value");

			db_assert (stmt.bind_text (3, dep.version), "bind value");

			db_assert (stmt.bind_text (4, "%s\n\n%s".printf (dep.summary, dep.description)), "bind value");

			db_assert (stmt.bind_text (5, dep.homepage), "bind value");

			db_assert (stmt.bind_text (6, dep.author), "bind value");

			db_assert (stmt.bind_int64 (7, dep.install_time), "bind value");

			db_assert (stmt.bind_text (8, dep.environment), "bind value");

			db_assert (stmt.step (), "add dependency");
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return false;
		}

		return true;
	}

	private IPK.Dependency? retrieve_dependency (Sqlite.Statement stmt) {
		IPK.Dependency dep = new IPK.Dependency.blank ();

		dep.idname = stmt.column_text (1);
		dep.full_name = stmt.column_text (2);
		dep.version = stmt.column_text (3);

		string s = stmt.column_text (4);
		string[] desc = s.split ("\n\n", 2);
		if (desc[0] != null) {
			dep.summary = desc[0];
			if (desc[1] != null)
				dep.description = desc[1];
		} else {
			dep.summary = s;
		}

		dep.homepage = stmt.column_text (5);
		dep.author = stmt.column_text (6);
		dep.install_time = stmt.column_int (7);
		dep.environment = stmt.column_text (8);
		// It's in the db, so this dependency is certainly satisfied
		dep.satisfied = true;

		return dep;
	}

	public IPK.Dependency? get_dependency_by_id (string depIdName) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT " + depcols + " FROM dependencies WHERE name=?", -1, out stmt);

		try {
			db_assert (res, "get dependency (by id)");
			db_assert (stmt.bind_text (1, depIdName), "bind text");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			dbstatus_changed (DatabaseStatus.FAILURE, e.message);
			return null;
		}

		IPK.Dependency? dep = retrieve_dependency (stmt);
		return dep;
	}

}

} // End of namespace

