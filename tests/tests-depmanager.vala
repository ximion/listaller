/* tests-depmanager.vala
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
	Dep.Module test1 = new Dep.Module ("Test:1.gee");
	test1.feed_url = "http://services.sugarlabs.org/libgee";
	//test1.add_item ("libgee.so.2", Dep.ItemType.SHARED_LIB);
	test1.add_item (Dep.ItemType.SHARED_LIB, "bladada.so.2");

	ret = depinst.install_existing_module_dependency (ref test1);
	assert (ret == true);
	assert (test1.installed == true);
	debug (test1.full_name);
	assert (test1.full_name == "libgee");

	// Test 2
	Dep.Module test2 = new Dep.Module ("Test:2.vorbis");
	test2.feed_url = "http://services.sugarlabs.org/libvorbis";
	//test2.add_item ("libvorbis.so.0", Dep.ItemType.SHARED_LIB);
	test2.add_item (Dep.ItemType.SHARED_LIB, "nobis.so.0");

	ret = depinst.install_existing_module_dependency (ref test2);

	/* We already installed this in a previous test, but did no proper
	 * registration. So this has to fail because unknown files would be overwritten.
	 */
	assert (ret == false);
	assert (test2.installed == false);
	assert (test2.full_name == "libvorbis");

	// Test 3
	// Look if dependency of test1 is still available :-P
	test1.installed = false;
	test1.full_name = "";
	ret = depinst.install_existing_module_dependency (ref test1);
	assert (ret == true);
	assert (test1.installed == true);
	assert (test1.full_name == "libgee");
}

void test_dependency_manager () {
	msg ("Dependency manager tests");

	DepManager depman = new DepManager (sdb);

	Dep.Module testA = depman.dependency_from_idname ("libgee-0.5.0-8");
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

void search_install_pkdep (Dep.PkInstaller pkinst, Dep.PkResolver pksolv, ref Dep.Module dep) {
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
	ArrayList<Dep.Module> deplist = new ArrayList<Dep.Module> ();
	Dep.Module depMp3Gain = new Dep.Module ("Mp3Gain");
	depMp3Gain.add_item (Dep.ItemType.BINARY, "/usr/bin/mp3gain");

	Dep.PkInstaller pkit = new Dep.PkInstaller (ssettings);
	Dep.PkResolver pkslv = new Dep.PkResolver (ssettings);
	pkit.message.connect (test_solver_message_cb);

	search_install_pkdep (pkit, pkslv, ref depMp3Gain);
	assert (depMp3Gain.installed == true);

	// Now something more advanced
	Dep.Module crazy = new Dep.Module ("CrazyStuff");
	crazy.add_item (Dep.ItemType.BINARY, "/bin/bash");
	crazy.add_item (Dep.ItemType.SHARED_LIB, "libpackagekit-glib2.so");
	crazy.add_item (Dep.ItemType.SHARED_LIB, "libc6.so");

	search_install_pkdep (pkit, pkslv, ref crazy);
	assert (crazy.installed == true);

	// Now something which fails
	Dep.Module fail = new Dep.Module ("Fail");
	fail.add_item (Dep.ItemType.UNKNOWN, "/run/chicken");
	ret = pkslv.search_dep_packages (ref fail);
	if (!ret) {
		debug (pkit.last_error.details);
		assert (ret == false);
	}
	assert (fail.installed == false);
}

void test_feed_installer () {
	msg ("ZI Feed installer tests");
	Dep.FeedInstaller finst = new Dep.FeedInstaller (ssettings);

	Dep.Module test1 = new Dep.Module ("feedTest:1.vorb");
	test1.feed_url = "http://services.sugarlabs.org/libvorbis";

	bool ret;
	ret = finst.install_dependency (sdb, ref test1);
	if (finst.last_error != null) {
		error (finst.last_error.details);
	}
	assert (ret == true);
	assert (test1.full_name == "libvorbis");
}

void test_depsolver () {
	msg ("Dependency solver tests");
	ArrayList<Dep.Module> deplist = new ArrayList<Dep.Module> ();

	// Create a set of dependencies
	Dep.Module dep1 = new Dep.Module ("Gee");
	dep1.feed_url = "http://services.sugarlabs.org/libgee";
	deplist.add (dep1);

	Dep.Module dep2 = new Dep.Module ("SDLMixer1.2");
	dep2.feed_url = "http://repo.roscidus.com/lib_rsl/sdl-mixer1.2";
	deplist.add (dep2);

	Dep.Module dep3 = new Dep.Module ("Mp3Gain");
	dep3.add_item (Dep.ItemType.BINARY, "/usr/bin/mp3gain");
	deplist.add (dep3);

	Dep.Module dep4 = new Dep.Module ("LibXml2");
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

	var tenv = new TestEnvironment ("depmanager");
	tenv.init (ref args);
	tenv.create_environment ();

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

	tenv.run ();

	return 0;
}
