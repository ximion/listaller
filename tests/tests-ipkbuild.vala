/* tests-ipkbuild.vala
 *
 * Copyright (C) 2011-2013 Matthias Klumpp
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

string lipkgen_exec;
string acomp_exec;

string foobar_srcdir;
string dummy_srcdir;
string datadir;

void msg (string s) {
	stdout.printf (s + "\n");
}

void run_command (string cmd) {
	int exit_status = 0;
	try {
		stdout.printf ("\nRunning command: %s\n\n", cmd);
		Process.spawn_command_line_sync (cmd, null, null, out exit_status);
		assert (exit_status == 0);
	} catch (SpawnError e) {
		error (e.message);
	}
}

void test_cleanup_srcdir () {
	// we can ignore all errors here, as the project might have not been built before
	Environment.set_current_dir (foobar_srcdir);
	Process.spawn_command_line_sync ("make distclean", null, null, null);
}

/*
 * Test the AutoCompile automatic software compiler tool
 */
void test_autocompile () {
	msg ("AppCompile tests");

	// Cleanup
	test_cleanup_srcdir ();
	delete_dir_recursive (Path.build_filename (foobar_srcdir, "ipkinstall", "inst_target", null));

	Environment.set_current_dir (foobar_srcdir);

	// Perform autocompile of FooBar sample app
	string cmd = acomp_exec;
	run_command (cmd);
}

/*
 * Test the LiBuild IPK package builder tool
 */
void test_lipkgen_build () {
	bool ret = false;
	msg ("Lipkgen tests");

	// Cleanup
	FileUtils.remove (Path.build_filename (datadir, "FooBar-1.0_%s.ipk".printf (Utils.system_machine_generic ()), null));
	FileUtils.remove (Path.build_filename (datadir, "Dummy-0.1_all.ipk", null));

	string cmd;

	// Now create IPK package for FooBar!
	Environment.set_current_dir (foobar_srcdir);
	cmd = "%s %s %s %s %s".printf (lipkgen_exec, "-b", "--verbose", "-o", datadir);
	run_command (cmd);
	// Now build the dummy IPK!
	Environment.set_current_dir (dummy_srcdir);
	cmd = "%s %s %s %s %s".printf (lipkgen_exec, "-b", "--verbose", "-o", datadir);
	run_command (cmd);
}

int main (string[] args) {
	msg ("=== Running LiBuild Tests ===");
	datadir = args[1];
	assert (datadir != null);
	acomp_exec = lipkgen_exec = args[2];
	lipkgen_exec = args[3];
	assert (lipkgen_exec != null);
	assert (acomp_exec != null);

	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	foobar_srcdir = real_path (Path.build_filename (datadir, "..", "foobar", null));
	assert (FileUtils.test (foobar_srcdir, FileTest.EXISTS) != false);

	dummy_srcdir = real_path (Path.build_filename (datadir, "..", "dummy-pack", null));
	assert (FileUtils.test (dummy_srcdir, FileTest.EXISTS) != false);

	string curdir = Environment.get_current_dir ();
	Environment.set_current_dir (datadir);

	var tenv = new TestEnvironment ("ipkbuild");
	tenv.init (ref args);
	tenv.create_environment (false);

	test_autocompile ();
	Environment.set_current_dir (datadir);
	test_lipkgen_build ();

	tenv.run ();
	Environment.set_current_dir (curdir);

	return 0;
}
