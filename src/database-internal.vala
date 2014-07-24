/* database-internal.vala - The Listaller application & dependency information storage
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
using Sqlite;
using Listaller;
using Listaller.Utils;

namespace Listaller {

private const string DATABASE = ""
		+ "CREATE TABLE applications ("
		+ "idname TEXT PRIMARY KEY, "
		+ "full_name TEXT NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "summary TEXT NOT NULL, "
		+ "description TEXT, "
		+ "metadata TEXT,"
		+ "architecture TEXT NOT NULL, "
		+ "origin TEXT NOT NULL, "
		+ "install_time INTEGER, "
		+ "dependencies TEXT"
		+ "); "
		+ "CREATE TABLE dependencies ("
		+ "idname TEXT PRIMARY KEY, "
		+ "full_name TEXT NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "summary TEXT NOT NULL, "
		+ "description TEXT, "
		+ "metadata TEXT, "
		+ "architecture TEXT NOT NULL, "
		+ "origin TEXT NOT NULL, "
		+ "install_time INTEGER, "
		+ "items_installed TEXT NOT NULL, "
		+ "items TEXT NOT NULL, "
		+ "environment TEXT, "
		+ "dependencies TEXT"
		+ ");" +
		"";

private const string appcols = "idname, full_name, version, summary, description, metadata, architecture, origin, install_time, dependencies";
private const string depcols = "idname, full_name, version, summary, description, metadata, architecture, origin, install_time, items_installed, " +
						"items, environment, dependencies";

private enum AppRow {
	IDNAME = 0,
	FULLNAME = 1,
	VERSION = 2,
	SUMMARY = 3,
	DESCRIPTION = 4,
	METADATA = 5,
	ARCHITECTURE = 6,
	ORIGIN = 7,
	INST_TIME = 8,
	DEPENDENCIES = 9;
}

private enum DepRow {
	IDNAME = 0,
	FULLNAME = 1,
	VERSION = 2,
	SUMMARY = 3,
	DESCRIPTION = 4,
	METADATA = 5,
	ARCHITECTURE = 6,
	ORIGIN = 7,
	INST_TIME = 8,
	ITEMS_INSTALLED = 9,
	ITEMS = 10,
	ENVIRONMENT = 11,
	DEPENDENCIES = 12;
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

private abstract class InternalDB : Object {
	private Database db;

	private bool locked;
	private string dbname;
	protected AppState db_type;
	protected bool writeable;

	private Sqlite.Statement insert_app;
	private Sqlite.Statement insert_dep;

	public signal void message (MessageItem message);

	public InternalDB (string fname, AppState dbtype) {
		dbname = fname;

		// Whether we fetch data from the "shared" or "private" application database
		db_type = dbtype;
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
		debug (msg);
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
			debug (msg);
			throw new DatabaseError.ERROR (msg);
		}

		// Ensure the database is okay and all tables are created
		if ((create_db) && (!create_database ())) {
			throw new DatabaseError.ERROR (_("Could not create software database!"));
		}

		// Prepare statements

		// InsertApp statement
		try {
			db_assert (db.prepare_v2 ("INSERT INTO applications (" + appcols + ") "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
					-1, out insert_app), "prepare app insert statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		// InsertDep statement
		try {
			db_assert (db.prepare_v2 ("INSERT INTO dependencies (" + depcols + ") "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
					-1, out insert_dep), "prepare dependency insert statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	public virtual bool open_rw () throws DatabaseError {
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
			action_name = "unknown action";
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

	protected bool create_database () throws DatabaseError {
		Sqlite.Statement stmt;

		int res = db.exec (DATABASE);
		try {
			db_assert (res);
		} catch (Error e) {
			critical (_("Unable to create database tables: %s").printf (db.errmsg ()));
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

		/* create database config */
		res = db.exec ("CREATE TABLE config (" +
				"data TEXT primary key," +
				"value INTEGER);");
		try {
			db_assert (res);
		} catch (Error e) {
			critical (_("Unable to create database config table: %s").printf (db.errmsg ()));
			return false;
		}

		res = db.exec ("INSERT INTO config (data, value) VALUES ('dbversion', 0);");
		try {
			db_assert (res);
		} catch (Error e) {
			critical ("Can't create database version: %s", db.errmsg ());
			return false;
		}

		// db_assert (db.exec ("PRAGMA user_version = %d".printf (SUPPORTED_VERSION)));

		return true;
	}

	/* Application stuff */

	public virtual bool add_application (AppItem item, string arch = "current") throws DatabaseError {
		if (!database_writeable ()) {
			throw new DatabaseError.ERROR (_("Tried to write on readonly database! (This should never happen)"));
		}
		bool ret = true;

		// Set install timestamp
		DateTime dt = new DateTime.now_local ();
		item.install_time = dt.to_unix ();

		// Assign values
		try {
			db_assert (insert_app.bind_text (AppRow.IDNAME +1, item.idname), "assign value");

			db_assert (insert_app.bind_text (AppRow.VERSION +1, item.version), "assign value");

			db_assert (insert_app.bind_text (AppRow.FULLNAME +1, item.info.name), "assign value");

			db_assert (insert_app.bind_text (AppRow.SUMMARY +1, item.info.summary), "assign value");

			db_assert (insert_app.bind_text (AppRow.DESCRIPTION +1, item.info.description), "assign value");

			db_assert (insert_app.bind_text (AppRow.METADATA +1, item.get_metadata_xml ()), "assign value");

			db_assert (insert_app.bind_text (AppRow.ARCHITECTURE +1, arch), "assign value");

			db_assert (insert_app.bind_text (AppRow.ORIGIN +1, item.origin), "assign value");

			db_assert (insert_app.bind_int64 (AppRow.INST_TIME +1, item.install_time), "assign value");

			db_assert (insert_app.bind_text (AppRow.DEPENDENCIES +1, item.dependencies_str), "assign value");

			db_assert (insert_app.step (), "execute app insert");

			db_assert (insert_app.reset (), "reset statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return ret;
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

	protected virtual AppItem? retrieve_app_item (Sqlite.Statement stmt) {
		AppItem item = new AppItem.blank ();

		item.idname = stmt.column_text (AppRow.IDNAME);
		item.info.name = stmt.column_text (AppRow.FULLNAME);
		item.version = stmt.column_text (AppRow.VERSION);
		item.desktop_file = stmt.column_text (AppRow.METADATA);

		item.install_time = stmt.column_int (AppRow.INST_TIME);
		item.dependencies_str = stmt.column_text (AppRow.DEPENDENCIES);
		item.state = db_type;

		item.origin = stmt.column_text (AppRow.ORIGIN);

		return item;
	}

	public string? get_arch_for_app (string app_idname) {
		Sqlite.Statement stmt;
		try {
			db_assert (db.prepare_v2 ("SELECT * FROM applications WHERE idname=?", -1, out stmt),
				   "prepare find app by idname statement");

			db_assert (stmt.bind_text (1, app_idname), "bind value");

			int res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		string? arch = stmt.column_text (AppRow.ARCHITECTURE);

		return arch;
	}

	public AppItem? get_application_by_idname (string app_idname) throws DatabaseError {
		Sqlite.Statement stmt;
		try {
			db_assert (db.prepare_v2 ("SELECT * FROM applications WHERE idname=?", -1, out stmt),
				   "prepare find app by idname statement");

			db_assert (stmt.bind_text (1, app_idname), "bind value");

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

	public ArrayList<AppItem>? get_applications_by_fullname (string appFullName) throws DatabaseError {
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

		ArrayList<AppItem>? itemList = null;
		do {
			switch (res) {
				case Sqlite.DONE:
					break;
				case Sqlite.ROW:
					// Check if we have the container ready
					if (itemList == null)
						itemList = new ArrayList<AppItem> ();

					// Fetch a new AppItem
					AppItem? tmpApp = retrieve_app_item (stmt);
					// Fast sanity checks
					if (tmpApp != null)
						tmpApp.fast_check ();
					else
						throw new DatabaseError.ERROR ("Unable to retrieve an application from database! DB might be in an inconstistent state!");
					itemList.add (tmpApp);
					break;
				default:
					db_assert (res, "execute");
					break;
			}
			res = stmt.step ();
		} while (res == Sqlite.ROW);

		return itemList;
	}

	public ArrayList<AppItem>? get_applications_all () throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM applications", -1, out stmt);

		try {
			db_assert (res, "get all applications");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.ROW)
				return null;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		ArrayList<AppItem>? itemList = null;
		do {
			switch (res) {
				case Sqlite.DONE:
					break;
				case Sqlite.ROW:
					// Check if we have the container ready
					if (itemList == null)
						itemList = new ArrayList<AppItem> ();

					// Fetch a new AppItem
					AppItem? tmpApp = retrieve_app_item (stmt);
					// Fast sanity checks
					if (tmpApp != null)
						tmpApp.fast_check ();
					else
						throw new DatabaseError.ERROR ("Unable to retrieve an application from database! DB might be in an inconstistent state!");

					itemList.add (tmpApp);
					break;
				default:
					db_assert (res, "execute");
					break;
			}
			res = stmt.step ();
		} while (res == Sqlite.ROW);

		return itemList;
	}

	public AppItem? get_application_by_name_version (string appName, string appVersion) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM applications WHERE idname=? AND version=?", -1, out stmt);
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

	public virtual bool remove_application (AppItem app) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("DELETE FROM applications WHERE idname=?", -1, out stmt);

		try {
			db_assert (res, "delete application (prepare statement)");
			db_assert (stmt.bind_text (1, app.idname), "bind text");
			db_assert (stmt.step(), "execute action");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	private bool string_in_app_item (AppItem item, string s) {
		string str = s.down ();
		if (item.idname.down ().index_of (str) > -1)
			return true;
		if (item.info.name.down ().index_of (str) > -1)
			return true;
		if (item.info.summary.down ().index_of (str) > -1)
			return true;
		if (item.info.description.down ().index_of (str) > -1)
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

	public bool set_application_dependencies (string appName, string depstr) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("UPDATE applications SET dependencies=? WHERE idname=?", -1, out stmt);

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

	public bool add_dependency (Dependency dep) throws DatabaseError {
		if (!database_writeable ()) {
			throw new DatabaseError.ERROR (_("Tried to write on readonly database! (This should never happen)"));
		}

		// Set install timestamp
		DateTime dt = new DateTime.now_local ();
		dep.install_time = dt.to_unix ();

		try {
			// Assign values
			db_assert (insert_dep.bind_text (DepRow.IDNAME +1, dep.info.id), "bind value");

			db_assert (insert_dep.bind_text (DepRow.FULLNAME +1, dep.info.name), "bind value");

			db_assert (insert_dep.bind_text (DepRow.VERSION +1, dep.get_version ()), "bind value");

			db_assert (insert_dep.bind_text (DepRow.METADATA +1, dep.get_metadata_xml ()));

			db_assert (insert_dep.bind_int64 (DepRow.INST_TIME +1, dep.install_time), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ARCHITECTURE +1, dep.architecture), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ORIGIN +1, dep.origin), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ITEMS_INSTALLED +1, dep.get_installdata_as_string ()), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ITEMS +1, dep.get_items_as_string ()), "bind value");

			db_assert (insert_dep.bind_text (DepRow.ENVIRONMENT +1, dep.environment), "bind value");

			db_assert (insert_dep.bind_text (DepRow.DEPENDENCIES +1, ""), "bind value");

			db_assert (insert_dep.step (), "add dependency");

			db_assert (insert_dep.reset (), "reset insert_dep statement");
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

	private Dependency? retrieve_dependency (Sqlite.Statement stmt) {
		var dep = new Dependency.blank ();

		dep.info.id = stmt.column_text (DepRow.IDNAME);
		dep.info.name = stmt.column_text (DepRow.FULLNAME);
		dep.set_version (stmt.column_text (DepRow.VERSION));

		dep.install_time = stmt.column_int (DepRow.INST_TIME);
		dep.architecture = stmt.column_text (DepRow.ARCHITECTURE);
		dep.set_items_from_string (stmt.column_text (DepRow.ITEMS));
		dep.set_installdata_from_string (stmt.column_text (DepRow.ITEMS_INSTALLED));
		dep.environment = stmt.column_text (DepRow.ENVIRONMENT);

		// Because dep is in the database already, it has to be satisfied
		dep.installed = true;

		return dep;
	}

	public Dependency? get_dependency_by_id (string depIdName) {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("SELECT * FROM dependencies WHERE idname=?", -1, out stmt);

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

		Dependency? dep = retrieve_dependency (stmt);
		return dep;
	}

	public bool set_dependency_environment (string depIdName, string env) throws DatabaseError {
		Sqlite.Statement stmt;
		int res = db.prepare_v2 ("UPDATE dependencies SET environment=? WHERE idname=?", -1, out stmt);

		try {
			db_assert (res, "get dependency (by id)");
			db_assert (stmt.bind_text (1, env), "bind text");
			db_assert (stmt.bind_text (2, depIdName), "bind text");

			res = stmt.step ();
			db_assert (res, "execute");
			if (res != Sqlite.DONE)
				return false;
		} catch (Error e) {
			throw new DatabaseError.ERROR (e.message);
		}

		return true;
	}

}

/**
 * Provides access to the database of installed software
 *
 * LocalDB is used to access nformation about installed applications
 * on a system. This includes installed files and various other things.
 */
private class LocalDB : InternalDB {
	private string regdir;
	private string regdir_apps;
	private string regdir_deps;

	public LocalDB (bool shared_mode, bool testmode = false) {
		// Create internal dummy configuration to fetch required data
		var tmpSSettings = new SetupSettings ();
		if (shared_mode)
			tmpSSettings.current_mode = IPK.InstallMode.SHARED;
		else
			tmpSSettings.current_mode = IPK.InstallMode.PRIVATE;
		// Test mode last to override previous settings if necessary
		if (testmode)
			tmpSSettings.current_mode = IPK.InstallMode.TEST;

		// The database filename
		string dbfname = tmpSSettings.database_file ();

		AppState dbstate;
		if (shared_mode)
			dbstate = AppState.INSTALLED_SHARED;
		else
			dbstate = AppState.INSTALLED_PRIVATE;

		base (dbfname, dbstate);

		// Path with additional data (e.g. the file-list or icons) which is not stored in the SQLite DB
		regdir = Path.build_filename (tmpSSettings.appregister_dir (), "info", null);
	}

	public override bool open_rw () throws DatabaseError {
		create_dir_structure (regdir);
		bool ret;
		try {
			ret = base.open_rw ();
		} catch (Error e) { throw e; }

		return ret;
	}

	public override bool add_application (AppItem item, string arch = "current") throws DatabaseError {
		bool ret;
		try {
			ret = base.add_application (item, arch);
		} catch (Error e) { throw e; }
		if (!ret)
			return ret;

		string metadir = Path.build_filename (regdir, item.idname, null);
		create_dir_structure (metadir);

		// Save extra package properties
		var data = new IPK.MetaFile ();
		if (item.replaces != null)
			data.add_value ("Replaces", item.replaces);

		ret = data.save_to_file (Path.build_filename (metadir, "properties", null));

		return ret;
	}

	public bool add_application_filelist (AppItem aid, Collection<IPK.FileEntry> flist) {
		if (!database_writeable ()) {
			throw new DatabaseError.ERROR (_("Tried to write on readonly database! (This should never happen)"));
		}

		string metadir = Path.build_filename (regdir, aid.idname, null);

		try {
			var file = File.new_for_path (Path.build_filename (metadir, "files.list", null));
			{
				var file_stream = file.create (FileCreateFlags.NONE);

				if (!file.query_exists ())
					return false;

				var data_stream = new DataOutputStream (file_stream);
				data_stream.put_string ("# File list for " + aid.info.name + "\n\n");
				// Now write file list to file
				foreach (IPK.FileEntry fe in flist) {
					if (fe.is_installed ()) {
						string fname = fe.fname_installed;
						if (db_type == AppState.INSTALLED_PRIVATE)
							fname = fold_user_dir (fname);
						// store hash and filename in compact form
						string hash = fe.hash;
						if (hash == "")
							hash = "NOHASH";
						data_stream.put_string ("%s %s\n".printf (hash, fname));
					}
				}
			}
		} catch (Error e) {
			throw new DatabaseError.ERROR (_("Unable to write application file list! Message: %s").printf (e.message));
		}

		return true;
	}

	public ArrayList<IPK.FileEntry>? get_application_filelist (AppItem app) throws DatabaseError {
		string metadir = Path.build_filename (regdir, app.idname, null);

		var file = File.new_for_path (Path.build_filename (metadir, "files.list", null));
		if (!file.query_exists ()) {
			return null;
		}

		var flist = new ArrayList<IPK.FileEntry> ();
		try {
			var dis = new DataInputStream (file.read ());
			string line;
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				if ((!line.has_prefix ("#")) && (line.strip () != "")) {
					string[] parts = line.split (" ", 2);
					string fname = parts[1];

					if (db_type == AppState.INSTALLED_PRIVATE)
						fname = expand_user_dir (fname);

					var fe = new IPK.FileEntry ();
					fe.fname_installed = fname;
					fe.hash = parts[0];
					flist.add (fe);
				}
			}
		} catch (Error e) {
			throw new DatabaseError.ERROR (_("Unable to fetch application file list: %s").printf (e.message));
		}

		return flist;
	}

	protected override AppItem? retrieve_app_item (Sqlite.Statement stmt) {
		AppItem item;
		item = base.retrieve_app_item (stmt);
		if (item == null)
			return null;

		string metadir = Path.build_filename (regdir, item.idname, null);

		// Load extra package properties
		var data = new IPK.MetaFile ();
		data.open_file (Path.build_filename (metadir, "properties", null));

		item.replaces = data.get_value ("Replaces");

		return item;
	}

	public override bool remove_application (AppItem app) throws DatabaseError {
		bool ret = true;
		string metadir = Path.build_filename (regdir, app.idname, null);

		try {
			base.remove_application (app);
		} catch (Error e) { throw e; }

		ret = delete_dir_recursive (metadir);
		if (!ret)
			warning ("Could not remove metadata directory for application-id %s!".printf (app.idname));
		return ret;
	}
}

/**
 * Provides access to cache of available software on remote sources
 *
 * This class is used to operate on Listaller's repository cache
 */
private class RepoCacheDB : InternalDB {

	public RepoCacheDB (string dbname) {
		base (dbname, AppState.AVAILABLE);
	}
}

} // End of namespace
