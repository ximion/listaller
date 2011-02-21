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

public class SoftwareDB : Object {
	private Database db;
	private LiConfig conf;
	private bool sumode;

	public SoftwareDB (bool root) {
		sumode = root;
		conf = new LiConfig (sumode);
	}

	public bool open () {
		string dbname = conf.database_file ();
		int rc;

		if (!FileUtils.test (dbname, FileTest.IS_REGULAR)) {
			stderr.printf ("Software database does not exist or is directory\n");
			return false;
		}

		rc = Database.open (dbname, out db);

		if (rc != Sqlite.OK) {
			stderr.printf ("Can't open database: %d, %s\n", rc, db.errmsg ());
			return false;
		}

		return true;
	}

}
