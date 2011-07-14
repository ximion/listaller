/* tests-depsolver.vala
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

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_solver_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype != MessageEnum.CRITICAL);
}

void test_solver_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	error (item.details);
}

void test_packagekit_solver () {
	bool ret = false;
	msg ("Dependency solver tests");

	// Set up Listaller configuration
	Listaller.Settings conf = new Listaller.Settings ();
	conf.testmode = true;

	SoftwareDB sdb = new SoftwareDB (conf);
	sdb.error_code.connect (test_solver_error_code_cb);
	sdb.message.connect (test_solver_message_cb);

	// Do this only in testing environment!
	sdb.remove_db_lock ();
	// Open the DB
	sdb.open ();

	ArrayList<IPK.Dependency> deplist = new ArrayList<IPK.Dependency> ();
	IPK.Dependency depMp3Gain = new IPK.Dependency ("Mp3Gain");
	depMp3Gain.files.add ("/usr/bin/mp3gain");

	deplist.add (depMp3Gain);

	Deps.Solver solver = new Deps.Solver (deplist, sdb, conf);
	solver.error_code.connect (test_solver_error_code_cb);
	solver.message.connect (test_solver_message_cb);

	// Run it!
	ret = solver.execute ();
	assert (ret == true);
	assert (depMp3Gain.satisfied == true);
}

void test_packagekit_installer () {
	msg ("PackageKit dependency installer test");
	bool ret = false;

	// Build simple mp3gain dependency
	ArrayList<IPK.Dependency> deplist = new ArrayList<IPK.Dependency> ();
	IPK.Dependency depMp3Gain = new IPK.Dependency ("Mp3Gain");
	depMp3Gain.files.add ("/usr/bin/mp3gain");

	Deps.PkInstaller pkit = new Deps.PkInstaller ();
	pkit.message.connect (test_solver_message_cb);
	ret = pkit.install_dependency (ref depMp3Gain);

	if (!ret) {
		debug (pkit.last_error.details);
		assert (ret == true);
	}
	assert (depMp3Gain.satisfied == true);

	// Now something more advanced
	IPK.Dependency crazy = new IPK.Dependency ("CrazyStuff");
	crazy.files.add ("/bin/bash");
	crazy.files.add ("libpackagekit-glib2.so");
	crazy.files.add ("libc6.so");
	ret = pkit.install_dependency (ref crazy);
	if (!ret) {
		debug (pkit.last_error.details);
		//! assert (ret == true);
	}
	//! assert (crazy.satisfied == true);

	// Now something which fails
	IPK.Dependency fail = new IPK.Dependency ("Fail");
	fail.files.add ("/run/chicken");
	ret = pkit.install_dependency (ref fail);
	if (!ret) {
		debug (pkit.last_error.details);
		assert (ret == false);
	}
	assert (fail.satisfied == false);
}

int main (string[] args) {
	msg ("=== Running Dependency Solver Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	Test.init (ref args);

	test_packagekit_installer ();
	//! test_packagekit_solver ();

	Test.run ();
	return 0;
}
