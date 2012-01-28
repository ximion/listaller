/* tests-ipkbuild.vala
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
 * 	Matthias Klumpp <matthias@tenstral.net>
 */

using GLib;
using Gee;
using Listaller;
using Listaller.Utils;

string libuild_exec;
string acomp_exec;

string foobar_srcdir;
string datadir;

void msg (string s) {
	stdout.printf (s + "\n");
}

void run_command (string cmd) {
	int exit_status = 0;
	try {
		debug ("Running command: " + cmd);
		Process.spawn_command_line_sync (cmd, null, null, out exit_status);
		assert (exit_status == 0);
	} catch (SpawnError e) {
		error (e.message);
	}
}

/*
 * Test the AutoCompile automatic software compiler tool
 */
void test_autocompile () {
	//! msg ("AppCompile tests");

	// Cleanup
	delete_dir_recursive (Path.build_filename (foobar_srcdir, "ipkinstall", "installtarget", null));

	/*!
	Environment.set_current_dir (foobar_srcdir);

	// Perform autocompile of FooBar sample app
	string cmd = acomp_exec;
	run_command (cmd); */
}

/*
 * Test the LiBuild IPK package builder tool
 */
void test_libuild_build () {
	bool ret = false;
	msg ("LiBuild tests");

	// Cleanup
	FileUtils.remove (Path.build_filename (datadir, "FooBar-1.0_install.ipk", null));

	Environment.set_current_dir (foobar_srcdir);
	// Now create IPK package for FooBar!
	string cmd = "%s %s %s %s".printf (libuild_exec, "-b", "-o", datadir);
	run_command (cmd);
}

int main (string[] args) {
	msg ("=== Running LiBuild Tests ===");
	datadir = args[1];
	assert (datadir != null);
	acomp_exec = libuild_exec = args[2];
	libuild_exec = args[3];
	assert (libuild_exec != null);
	assert (acomp_exec != null);

	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);
	foobar_srcdir = real_path (Path.build_filename (datadir, "..", "foobar", null));
	assert (FileUtils.test (foobar_srcdir, FileTest.EXISTS) != false);

	string curdir = Environment.get_current_dir ();
	Environment.set_current_dir (datadir);
	Test.init (ref args);

	test_autocompile ();
	Environment.set_current_dir (datadir);
	test_libuild_build ();

	Test.run ();
	Environment.set_current_dir (curdir);
	return 0;
}
