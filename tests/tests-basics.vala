/* tests-basics.vala
 *
 * Copyright (C) 2011  Matthias Klumpp
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
using Listaller;

private string datadir;
private string foobar_dir;

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_basics_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype != MessageEnum.CRITICAL);
}

void test_basics_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	error (item.details);
}

void test_application_ids () {
	bool ret = false;
	msg ("Testing app-ids");

	// Set up Listaller configuration
	Listaller.Settings conf = new Listaller.Settings ();
	conf.testmode = true;

	AppItem dummy = new AppItem.from_desktopfile (Path.build_filename (foobar_dir, "foobar.desktop", null));
	msg ("Dummy application id: " + dummy.appid);
	string expected_id = "foobar;1.0;" + system_architecture () + ";" +
		string_replace (foobar_dir, "(/usr|share/applications|/home/)", "")
		+ "~unknown";

	assert (dummy.appid == expected_id);

	AppItem item1 = new AppItem.from_id (expected_id);
	assert (item1.name == "foobar");
	assert (item1.full_name == "Listaller FooBar");
	assert (item1.version == "1.0");
	assert (item1.maintainer == "Listaller Project");

	AppItem item2 = new AppItem ("MyApp", "0.1", "amd64/i686");
	item2.origin = AppOrigin.IPK;
	assert (item2.full_name == "MyApp");
	assert (item2.name == "myapp");
	item2.update_with_desktop_file ();
	assert (item2.desktop_file == "");
	assert (item2.appid == "myapp;0.1;amd64/i686;~package_ipk");

	AppItem item3 = new AppItem ("Google Earth", "1.2", "amd64");
	assert (item3.name == "google_earth");
}

int main (string[] args) {
	msg ("=== Running Basic Tests ===");
	datadir = args[1];
	assert (datadir != null);
	foobar_dir = Path.build_filename (datadir, "foobar", null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	Test.init (ref args);
	test_application_ids ();
	Test.run ();
	return 0;
}
