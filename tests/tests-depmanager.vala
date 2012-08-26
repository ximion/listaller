/* tests-depmanager.vala
 *
 * Copyright (C) 2011-2012 Matthias Klumpp
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
private SetupSettings ssettings;
private SoftwareDB sdb;

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

void test_dependency_installer () {
	msg ("Dependency installer tests");

	bool ret;
	DepInstaller depinst = new DepInstaller (sdb);

	// Test 1
	IPK.Dependency test1 = new IPK.Dependency ("Test:1.gee");
	test1.feed_url = "http://services.sugarlabs.org/libgee";
	//test1.add_component ("libgee.so.2", Dep.ComponentType.SHARED_LIB);
	test1.add_component ("bladada.so.2", Dep.ComponentType.SHARED_LIB);

	ret = depinst.install_dependency (ref test1);
	assert (ret == true);
	assert (test1.satisfied == true);
	debug (test1.full_name);
	assert (test1.full_name == "libgee");

	// Test 2
	IPK.Dependency test2 = new IPK.Dependency ("Test:2.vorbis");
	test2.feed_url = "http://services.sugarlabs.org/libvorbis";
	//test2.add_component ("libvorbis.so.0", Dep.ComponentType.SHARED_LIB);
	test2.add_component ("nobis.so.0", Dep.ComponentType.SHARED_LIB);

	ret = depinst.install_dependency (ref test2);

	/* We already installed this in a previous test, but did no proper
	 * registration. So this has to fail because unknown files would be overwritten.
	 */
	assert (ret == false);
	assert (test2.satisfied == false);
	assert (test2.full_name == "libvorbis");

	// Test 3
	// Look if dependency of test1 is still available :-P
	test1.satisfied = false;
	test1.full_name = "";
	ret = depinst.install_dependency (ref test1);
	assert (ret == true);
	assert (test1.satisfied == true);
	assert (test1.full_name == "libgee");
}

void test_dependency_manager () {
	msg ("Dependency manager tests");

	DepManager depman = new DepManager (sdb);

	IPK.Dependency testA = depman.dependency_from_idname ("libgee-0.5.0-8");
	assert (testA != null);

	string path = depman.get_absolute_library_path (testA);
	string expectedPath = Path.build_filename (ssettings.depdata_dir (), testA.idname, "lib", null);
	debug (path);
	assert (path == expectedPath);

	// We do the whole thing again to check if storing this info in the database worked.
	testA = depman.dependency_from_idname ("libgee-0.5.0-8");
	path = depman.get_absolute_library_path (testA);
	debug (path);
	assert (path == expectedPath);
}

void search_install_pkdep (Dep.PkInstaller pkinst, Dep.PkResolver pksolv, ref IPK.Dependency dep) {
	bool ret;
	ret = pksolv.search_dep_packages (ref dep);
	if (!ret) {
		debug (pksolv.last_error.details);
		assert (ret == true);
	}
	ret = pkinst.install_dependency (ref dep);
	if (!ret) {
		debug (pkinst.last_error.details);
		assert (ret == true);
	}
}

void test_packagekit_installer () {
	msg ("PackageKit dependency installer test");
	bool ret = false;

	// Build simple mp3gain dependency
	ArrayList<IPK.Dependency> deplist = new ArrayList<IPK.Dependency> ();
	IPK.Dependency depMp3Gain = new IPK.Dependency ("Mp3Gain");
	depMp3Gain.add_component ("/usr/bin/mp3gain", Dep.ComponentType.BINARY);

	Dep.PkInstaller pkit = new Dep.PkInstaller (ssettings);
	Dep.PkResolver pkslv = new Dep.PkResolver (ssettings);
	pkit.message.connect (test_solver_message_cb);

	search_install_pkdep (pkit, pkslv, ref depMp3Gain);
	assert (depMp3Gain.satisfied == true);

	// Now something more advanced
	IPK.Dependency crazy = new IPK.Dependency ("CrazyStuff");
	crazy.add_component ("/bin/bash", Dep.ComponentType.BINARY);
	crazy.add_component ("libpackagekit-glib2.so", Dep.ComponentType.SHARED_LIB);
	crazy.add_component ("libc6.so", Dep.ComponentType.SHARED_LIB);

	search_install_pkdep (pkit, pkslv, ref crazy);
	assert (crazy.satisfied == true);

	// Now something which fails
	IPK.Dependency fail = new IPK.Dependency ("Fail");
	fail.add_component ("/run/chicken", Dep.ComponentType.UNKNOWN);
	ret = pkslv.search_dep_packages (ref fail);
	if (!ret) {
		debug (pkit.last_error.details);
		assert (ret == false);
	}
	assert (fail.satisfied == false);
}

void test_feed_installer () {
	msg ("ZI Feed installer tests");
	Dep.FeedInstaller finst = new Dep.FeedInstaller (ssettings);

	IPK.Dependency test1 = new IPK.Dependency ("feedTest:1.vorb");
	test1.feed_url = "http://services.sugarlabs.org/libvorbis";

	bool ret;
	ret = finst.install_dependency (sdb, ref test1);
	if (finst.last_error != null) {
		error (finst.last_error.details);
	}
	assert (ret == true);
	assert (test1.full_name == "libvorbis");

	// Other tests might want to install the same stuff, so avoid conflicts.
	//ssettings.invalidate_tmp_dir ();
}

void test_depsolver () {
	msg ("Dependency solver tests");
	ArrayList<IPK.Dependency> deplist = new ArrayList<IPK.Dependency> ();

	// Create a set of dependencies
	IPK.Dependency dep1 = new IPK.Dependency ("Gee");
	dep1.feed_url = "http://services.sugarlabs.org/libgee";
	deplist.add (dep1);

	IPK.Dependency dep2 = new IPK.Dependency ("SDLMixer1.2");
	dep2.feed_url = "http://repo.roscidus.com/lib_rsl/sdl-mixer1.2";
	deplist.add (dep2);

	IPK.Dependency dep3 = new IPK.Dependency ("Mp3Gain");
	dep3.add_component ("/usr/bin/mp3gain", Dep.ComponentType.BINARY);
	dep3.is_standardlib = true;
	deplist.add (dep3);

	IPK.Dependency dep4 = new IPK.Dependency ("LibXml2");
	dep4.feed_url = "http://services.sugarlabs.org/libxml2";
	deplist.add (dep4);

	bool ret;
	DepInstaller depinst = new DepInstaller (sdb);
	depinst.error_code.connect (test_solver_error_code_cb);
	depinst.message.connect (test_solver_message_cb);

	ret = depinst.dependencies_installable (ref deplist);
	assert (ret == true);
}

int main (string[] args) {
	msg ("=== Running Dependency Solver Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	Test.init (ref args);
	set_console_mode (true);
	set_verbose_mode (true);
	add_log_domain ("LiTest");

	// Set up Listaller configuration & database
	ssettings = new SetupSettings (IPK.InstallMode.TEST);

	sdb = new SoftwareDB (ssettings);
	sdb.error_code.connect (test_solver_error_code_cb);
	sdb.message.connect (test_solver_message_cb);

	// Open the DB
	sdb.open_write ();

	test_feed_installer ();
	//! test_packagekit_installer ();
	test_dependency_installer ();
	test_dependency_manager ();
	test_depsolver ();

	Test.run ();
	return 0;
}
