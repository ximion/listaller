/* tests-repository.vala
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

void test_repo_remote () {
	var repo_remote = new IPK.RepoRemote ("http://listaller.tenstral.net/stuff/test-repo");

	repo_remote.load_server_index ();

	ArrayList<AppItem> appList = repo_remote.get_available_applications (false);
	print_app_arraylist (appList, "arch-specific");
	assert (appList.size >= 1);

	appList = repo_remote.get_available_applications (true);
	print_app_arraylist (appList, "arch-indep");
	assert (appList.size >= 0);
}

int main (string[] args) {
	msg ("=== Running IPK Repo Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	var tenv = new TestEnvironment ("repository");
	tenv.init (ref args);
	tenv.create_environment ();

	test_repo_remote ();

	tenv.run ();

	return 0;
}
