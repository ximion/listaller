/* database.vala
 *
 * Copyright (C) 2010  Matthias Klumpp
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
using Sqlite;

// Workaround for Vala bug #618931
private const string _PKG_VERSION2 = PkgConfig.VERSION;

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

private class SoftwareDB : Object {
	private Database db;
	private LiConfig conf;
	private bool sumode;
	private string dblockfile;

	public signal void status_changed (DatabaseStatus newstatus, string message);

	public SoftwareDB (bool root) {
		sumode = root;
		conf = new LiConfig (sumode);

		dblockfile = conf.appregister_dir () + "/lock";
	}

	public bool database_locked () {
		if (FileUtils.test (dblockfile, FileTest.IS_REGULAR)) {
			return true;
		} else {
			return false;
		}
	}

	public bool open () {
		string dbname = conf.database_file ();
		int rc;

		// If database is locked, we should not try to read/write on it
		if (database_locked ()) {
			return false;
		}

		if (!FileUtils.test (dbname, FileTest.IS_REGULAR)) {
			string msg = "Software database does not exist or is directory\n";
			stderr.printf (msg);
			status_changed (DatabaseStatus.FAILURE, msg);
			return false;
		}

		rc = Database.open (dbname, out db);

		if (rc != Sqlite.OK) {
			string msg = "Can't open database: %d, %s\n".printf (rc, db.errmsg ());
			stderr.printf (msg);
			status_changed (DatabaseStatus.FAILURE, msg);
			return false;
		}

		// Drop "lock" file
		File lfile = File.new_for_path (dblockfile);
		try {
			lfile.create (FileCreateFlags.NONE);
		} catch (Error e) {
			stderr.printf ("Error: %s\n", e.message);
			return false;
		}

		// Test for the existence of file
		if (!lfile.query_exists ()) {
			stderr.printf ("Unable to create lock file!");
			return false;
		}

		status_changed (DatabaseStatus.LOCKED, "");
		status_changed (DatabaseStatus.OPENED, "");

		return true;
	}

}
