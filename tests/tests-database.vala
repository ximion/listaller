/* tests-database.vala
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

void msg(string s) {
	message (s + "\n");
}

void software_db_status_changed_cb (DatabaseStatus status, string message) {

	if (status == DatabaseStatus.FATAL) {
		msg (message);
		assert (status != DatabaseStatus.FATAL);
	}

	if (status == DatabaseStatus.FAILURE) {
		msg (message);
	}
}

void test_software_db () {
	bool ret = false;

	msg ("Opening new software database connection");
	SoftwareDB sdb = new SoftwareDB (false);
	sdb.status_changed.connect (software_db_status_changed_cb);

	// Do this only in testing environment!
	sdb.remove_db_lock ();
	// Open the DB
	sdb.open ();
	msg ("Software database is ready now!");
	msg ("Constructing fake application and adding it to the DB...");

	{
	LiAppItem item = new LiAppItem ("Test", "0.1");
	ret = sdb.add_application (item);
	assert (ret == true);

	// Close & reopen the DB
	sdb.close ();
	sdb.open ();

	msg ("Retrieving AppItem from database...");
	LiAppItem newItem = sdb.get_application_by_name ("Test");
	assert (newItem != null);
	assert (newItem.dbid == 1);

	msg ("Item is: %s".printf (newItem.to_string ()));
	}

	sdb.close ();
	msg ("Software database closed.");
}

int main (string[] args) {
	stdout.printf ("=== Running tests ===\n");
	Test.init (ref args);
	test_software_db ();
	Test.run ();
	return 0;
}
