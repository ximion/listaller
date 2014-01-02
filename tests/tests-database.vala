/* tests-database.vala
 *
 * Copyright (C) 2010-2014 Matthias Klumpp
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
 * 	Matthias Klumpp <matthias@tenstral.net>
 */

using GLib;
using Listaller;

void msg (string s) {
	stdout.printf (s + "\n");
}

void softwaredb_error_code_cb (ErrorItem item) {
	GLib.error ("DBError: %s", item.details);
}

void softwaredb_message_cb (MessageItem message) {
	msg (message.to_string ());
}

void software_db_status_changed_cb (DatabaseStatus status, string message) {

	if (status == DatabaseStatus.FAILURE) {
		msg (message);
		assert (status != DatabaseStatus.FAILURE);
	} else {
		msg ("DB status change message: %s".printf (message));
	}
}

void test_software_db () {
	bool ret = false;

	var setup_settings = new SetupSettings (IPK.InstallMode.TEST);

	msg ("Opening new software database connection");
	SoftwareDB sdb = new SoftwareDB (setup_settings);
	sdb.error_code.connect (softwaredb_error_code_cb);
	sdb.message.connect (softwaredb_message_cb);

	// Open the DB
	sdb.open_write ();
	msg ("Software database is ready now!");
	msg ("Constructing fake application and adding it to the DB...");

	AppItem item = new AppItem ("Test", "0.1");
	ret = sdb.add_application (item);
	assert (ret == true);

	// Close & reopen the DB
	sdb.open_read ();

	msg ("Retrieving AppItem from database...");
	AppItem newItem = sdb.get_application_by_idname ("test");
	assert (newItem != null);
	assert (newItem.idname == "test");
	assert (newItem.full_name == "Test");

	msg ("Item is: %s".printf (newItem.to_string ()));

	int count = sdb.get_applications_count ();
	assert (count == 1);

	msg ("Software database closed.");
}

int main (string[] args) {
	msg ("=== Running Database Tests ===");

	var tenv = new TestEnvironment ("database");
	tenv.init (ref args);
	tenv.create_environment ();

	test_software_db ();

	tenv.run ();

	return 0;
}
