/* tests-libuild.vala
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

string libuild_exec;

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_libuild_build () {
	bool ret = false;
	msg ("LiBuild tests");

	// Simple build test
	try {
		int exit_status = 0;
		Process.spawn_command_line_sync (libuild_exec + " -b", null, null, out exit_status);
		assert (exit_status == 0);
	} catch (SpawnError e) {
		error (e.message);
	}

}

int main (string[] args) {
	msg ("=== Running LiBuild Tests ===");
	string datadir = args[1];
	assert (datadir != null);
	libuild_exec = args[2];
	assert (libuild_exec != null);

	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	string curdir = Environment.get_current_dir ();
	Environment.set_current_dir (datadir);
	Test.init (ref args);
	test_libuild_build ();
	Test.run ();
	Environment.set_current_dir (curdir);
	return 0;
}
