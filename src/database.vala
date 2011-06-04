/* database.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using GLib;
using Gee;
using Sqlite;
using Listaller;

namespace Listaller {

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
	FATAL,
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

			case FATAL:
				return _("Fatal database error");

			case CLOSED:
				return _("Software database closed");

			default:
				return _("Software database message (%d)").printf((int) this);
		}
	}
}

private class SoftwareDB : Object {
	private Database *db;
	private Settings conf;
	private bool locked;
	private string dblockfile;
	private string apptables;
	private string regdir;

	public signal void db_status_changed (DatabaseStatus newstatus, string message);

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);

	public SoftwareDB (Settings? settings) {
		conf = settings;
		locked = false;
		if (conf == null)
			conf = new Settings (false);

		dblockfile = conf.appregister_dir () + "/lock";
		regdir = Path.build_filename (conf.appregister_dir (), "info", null);
	}

	~SoftwareDB () {
		// Make sure DB is always closed when DB is freed
		close ();
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
		if ((dbs == DatabaseStatus.FAILURE) ||
			(dbs == DatabaseStatus.FATAL)) {
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

	private bool open_db () {
		string dbname = conf.database_file ();
		int rc;

		if (!FileUtils.test (dbname, FileTest.IS_REGULAR)) {
			emit_message ("Software database does not exist - will be created.");
		}

		rc = Database.open (dbname, out db);

		if (rc != Sqlite.OK) {
			string msg = "Can't open database! (Message: %d, %s)".printf (rc, db->errmsg ());
			stderr.printf (msg);
			dbstatus_changed (DatabaseStatus.FAILURE, msg);
			return false;
		}

		create_dir_parents (regdir);
		dbstatus_changed (DatabaseStatus.OPENED, "");

		// Ensure the database is okay and all tables are created
		if (!update_db_structure ()) {
			dbstatus_changed (DatabaseStatus.FAILURE, _("Could not create/update software database!"));
			return false;
		}

		return true;
	}

	public bool open () {
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
			critical ("Error: %s", e.message);
			return false;
		}

		// Test for the existence of file
		if (!lfile.query_exists ()) {
			critical ("Unable to create lock file!");
			return false;
		}
		// DB is now locked
		locked = true;
		dbstatus_changed (DatabaseStatus.LOCKED, "");
		return true;
	}

	public bool open_readonly () {
		bool ret;
		ret = open_db ();
		return ret;
	}

	public void close () {
		delete db;
		// Delete the lock
		remove_db_lock ();
	}

	public void remove_db_lock () {
		if (locked) {
			File lfile = File.new_for_path (dblockfile);
			try {
				if (lfile.query_exists ()) {
					lfile.delete ();
				}
			} catch (Error e) {
				error (_("Unable to remove database lock! (Message: %s)").printf (e.message));
			}
		}
	}

	/*
	 * This method will throw an error on an SQLite return code unless it's OK, DONE, or ROW, which
	 * are considered normal results.
	 */
	protected void throw_error (string method, int res) throws DatabaseError {
		string msg = "(%s) [%d] - %s".printf (method, res, db->errmsg());

		switch (res) {
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

	private void fatal (string op, int res) {
		if (op == "")
			op = "action";
		string msg = _("Database problem:") + "\n%s: [%d] %s".printf (op, res, db->errmsg());
		dbstatus_changed (DatabaseStatus.FATAL, msg);
	}

	protected bool check_result (int res, string info = "") {
		try {
			throw_error (info, res);
		} catch (Error e) {
			fatal (info, res);
			return false;
		}
		return true;
	}

	public bool has_table (string table_name) {
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("PRAGMA table_info(%s)".printf(table_name), -1, out stmt);
		return_if_fail (check_result (res, "prepare db"));

		res = stmt.step ();

		return (res != Sqlite.DONE);
	}

	protected bool update_db_structure () {
		Sqlite.Statement stmt;

		apptables = "id, name, version, full_name, desktop_file, summary, author, publisher, "
		+ "categories, install_time, origin, dependencies";

		// Create table to store information about applications
		int res = db->prepare_v2 ("CREATE TABLE IF NOT EXISTS applications ("
		+ "id INTEGER PRIMARY KEY, "
		+ "name TEXT UNIQUE NOT NULL, "
		+ "version TEXT NOT NULL, "
		+ "full_name TEXT NOT NULL, "
		+ "desktop_file TEXT UNIQUE,"
		+ "summary TEXT, "
		+ "author TEXT, "
		+ "publisher TEXT, "
		+ "categories TEXT, "
		+ "install_time INTEGER, "
		+ "origin TEXT NOT NULL, "
		+ "dependencies TEXT"
		+ ")", -1, out stmt);
		return_if_fail (check_result (res, "create applications table"));

		res = stmt.step ();
		if (res != Sqlite.DONE) {
			fatal ("create applications table", res);
			return false;
		}

		// Table for all the additional stuff fetched during installation (3rd-party libraries etc.)
		res = db->prepare_v2 ("CREATE TABLE IF NOT EXISTS dependencies ("
		+ "id INTEGER PRIMARY KEY, "
		+ "name TEXT UNIQUE NOT NULL, "
		+ "version TEXT UNIQUE NOT NULL, "
		+ "storage_path TEXT UNIQUE NOT NULL, "
		+ "publisher TEXT, "
		+ "install_time INTEGER"
		+ ")", -1, out stmt);
		return_if_fail (check_result (res, "create dependencies table"));

		res = stmt.step ();
		if (res != Sqlite.DONE) {
			fatal ("create dependencies table", res);
			return false;
		}

		return true;
	}

	public bool add_application (AppItem item) {
		if (!locked) {
			fatal ("write to unlocked database!", Sqlite.ERROR);
			return false;
		}
		Sqlite.Statement stmt;
		int res = db->prepare_v2 (
			"INSERT INTO applications (name, version, full_name, desktop_file, summary, author, publisher, "
			+ "categories, install_time, origin, dependencies) "
			+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
				   -1, out stmt);
			return_if_fail (check_result (res, "add application"));

			ulong time_created = now_sec ();

			// Assign values
			res = stmt.bind_text (1, item.idname);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (2, item.version);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (3, item.full_name);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (4, item.desktop_file);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (5, item.summary);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (6, item.author);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (7, item.publisher);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (8, item.categories);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_int64 (9, item.install_time);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (10, item.origin.to_string ());
			return_if_fail (check_result (res, "assign value"));

			res = stmt.bind_text (11, item.dependencies);
			return_if_fail (check_result (res, "assign value"));

			res = stmt.step();
			if (res != Sqlite.DONE) {
				if (res != Sqlite.CONSTRAINT) {
					fatal ("add application", res);
					return false;
				}
			}

			return true;
	}

	public bool add_application_filelist (AppItem aid, ArrayList<IPK.FileEntry> flist) {
		if (!locked) {
			fatal ("write to readonly database!", Sqlite.ERROR);
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
						if (!conf.sumode)
							fname = fold_user_dir (fname);
						data_stream.put_string (fname + "\n");
					}
				}
			}
		} catch (Error e) {
			error (_("Unable to write application file list! Message: %s").printf (e.message));
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
					if (!conf.sumode)
						fname = expand_user_dir (fname);
					flist.add (fname);
				}
			}
		} catch (Error e) {
			error (_("Unable to fetch application file list! Message: %s").printf (e.message));
			return null;
		}
		return flist;
	}

	public int get_applications_count () {
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("SELECT Count(*) FROM applications", -1, out stmt);
		return_if_fail (check_result (res, "get applications count"));

		if (stmt.step() != Sqlite.ROW)
			return -1;
		int count = stmt.column_int (0);
		return count;
	}

	private AppItem? retrieve_app_item (Sqlite.Statement stmt) {
		AppItem item = new AppItem.blank ();

		item.dbid = stmt.column_int (0);
		item.idname = stmt.column_text (1);
		item.version = stmt.column_text (2);
		item.full_name = stmt.column_text (3);
		item.desktop_file = stmt.column_text (4);
		item.summary = stmt.column_text (5);
		item.author = stmt.column_text (6);
		item.publisher = stmt.column_text (7);
		item.categories = stmt.column_text (8);
		item.install_time = stmt.column_int (9);
		item.set_origin_from_string (stmt.column_text (10));
		item.dependencies = stmt.column_text (11);

		return item;
	}

	public AppItem? get_application_by_idname (string appIdName) {
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("SELECT " + apptables + " FROM applications WHERE name=?", -1, out stmt);
		return_if_fail (check_result (res, "get application (by name)"));

		res = stmt.bind_text (1, appIdName);

		if (stmt.step() != Sqlite.ROW)
			return null;

		AppItem item = retrieve_app_item (stmt);

		// Fast sanity checks
		item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_fullname (string appFullName) {
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("SELECT " + apptables + " FROM applications WHERE full_name=?", -1, out stmt);
		return_if_fail (check_result (res, "get application (by full_name)"));

		res = stmt.bind_text (1, appFullName);

		if (stmt.step() != Sqlite.ROW)
			return null;

		AppItem item = retrieve_app_item (stmt);

		// Fast sanity checks
		item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_dbid (int databaseId) {
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("SELECT " + apptables + " FROM applications WHERE id=?", -1, out stmt);
		return_if_fail (check_result (res, "get application (by db_id)"));

		res = stmt.bind_int (1, databaseId);

		if (stmt.step() != Sqlite.ROW)
			return null;

		AppItem item = retrieve_app_item (stmt);

		// Fast sanity checks
		item.fast_check ();

		return item;
	}

	public AppItem? get_application_by_name_version (string appName, string appVersion) {
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("SELECT " + apptables + " FROM applications WHERE name=? AND version=?", -1, out stmt);
		return_if_fail (check_result (res, "get application (by name_version)"));

		res = stmt.bind_text (1, appName);
		return_if_fail (check_result (res, "assign value"));
		res = stmt.bind_text (2, appVersion);
		return_if_fail (check_result (res, "assign value"));

		if (stmt.step() != Sqlite.ROW)
			return null;

		AppItem item = retrieve_app_item (stmt);

		// Fast sanity checks
		item.fast_check ();

		return item;
	}

	public bool remove_application (AppItem app) {
		bool ret = true;
		string metadir = Path.build_filename (regdir, app.idname, null);
		Sqlite.Statement stmt;
		int res = db->prepare_v2 ("DELETE FROM applications WHERE name=?", -1, out stmt);
		return_if_fail (check_result (res, "delete application"));

		res = stmt.bind_text (1, app.idname);
		return_if_fail (check_result (res, "delete application"));

		res = stmt.step();
		return_if_fail (check_result (res, "delete application"));

		ret = delete_dir_recursive (metadir);
		if (!ret)
			critical ("Could not remove metadata directory for application-id %s!".printf (app.idname));
		return ret;
	}

	public AppItem? get_application_by_id (AppItem aid) {
		return get_application_by_idname (aid.idname);
	}

	private bool string_in_app_item (AppItem item, string s) {
		if (item.full_name.index_of (s) > 0)
			return true;
		if (item.summary.index_of (s) > 0)
			return true;
		return false;
	}

	public ArrayList<AppItem> find_applications ([CCode (array_null_terminated = true, array_length = false)] string[] values) {
		ArrayList<AppItem> resList = new ArrayList<AppItem> ();
		if (values[0] == null)
			return resList;

		HashSet<AppItem> tmpList = new HashSet<AppItem> ();

		int i = 0;
		AppItem tmpApp = null;
		for (tmpApp = get_application_by_dbid (i); tmpApp != null; i++) {
			int j = 0;
			for (string? s = values[j]; s != null; j++) {
				if (string_in_app_item (tmpApp, values[j])) {
					tmpList.add (tmpApp);
					break;
				}
			}
		}
		resList.add_all (tmpList.read_only_view);
		return resList;
	}

}

} // End of namespace
