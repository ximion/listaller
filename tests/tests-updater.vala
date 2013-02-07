/* tests-updater.vala
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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
using Gee;
using Listaller;

private string datadir;

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_repo_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype == MessageEnum.INFO);
}

void test_repo_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	error (item.details);
}

void print_app_arraylist (ArrayList<AppItem> appList, string label = "") {
	if (label != "")
		msg ("Application List [%s]:".printf (label));
	else
		msg ("Application List:");

	foreach (AppItem app in appList) {
		stdout.printf ("%s\n", app.to_string ());
	}
	msg ("END");
}

void test_refresh_repo_cache () {
	Repo.Manager repomgr = new Repo.Manager ();
	repomgr.refresh_cache ();
}

void test_updater_available () {
	Updater upd = new Updater (false);
	upd.find_updates ();
}

int main (string[] args) {
	msg ("=== Running Updater Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	Test.init (ref args);
	set_console_mode (true);
	set_verbose_mode (true);
	add_log_domain ("LiTest");

	test_refresh_repo_cache ();
	test_updater_available ();

	Test.run ();
	return 0;
}
