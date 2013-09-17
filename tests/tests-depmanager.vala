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
	var depinst = new DepInstaller (sdb);
	var cfactory = new Dep.ComponentFactory (ssettings);

	// Test 1
	Dep.Module test1 = new Dep.Module ("Test:1#expat");
	test1.feed_url = "http://repo.roscidus.com/lib/expat1";
	//test1.add_item ("libgee.so.2", Dep.ItemType.SHARED_LIB);
	test1.add_item (Dep.ItemType.SHARED_LIB, "bladada.so.2");

	ret = depinst.install_existing_module_dependency (cfactory, ref test1);
	assert (ret == true);
	assert (test1.installed == true);
	debug (test1.full_name);
	assert (test1.full_name == "libexpat1");

	// Test 2
	Dep.Module test2 = new Dep.Module ("Test:2#GNU_parallel");
	test2.feed_url = "http://git.savannah.gnu.org/cgit/parallel.git/plain/packager/0install/parallel.xml";
	//test2.add_item ("libvorbis.so.0", Dep.ItemType.SHARED_LIB);
	test2.add_item (Dep.ItemType.SHARED_LIB, "nobis.so.0");

	ret = depinst.install_existing_module_dependency (cfactory, ref test2);

	/* We already installed this in a previous test, but did no proper
	 * registration. So this has to fail because unknown files would be overwritten.
	 */
	assert (ret == false);
	assert (test2.installed == false);
	assert (test2.full_name == "GNU parallel");

	// Test 3
	// Look if dependency of test1 is still available :-P
	test1.installed = false;
	test1.full_name = "";
	ret = depinst.install_existing_module_dependency (cfactory, ref test1);
	assert (ret == true);
	assert (test1.installed == true);
	assert (test1.full_name == "libexpat1");
}

void test_dependency_manager () {
	msg ("Dependency manager tests");

	DepManager depman = new DepManager (sdb);

	Dep.Module testA = depman.dependency_from_idname ("libexpat1");
	assert (testA != null);

	string path = depman.get_absolute_library_path (testA);
	string expectedPath = Path.build_filename (ssettings.depdata_dir (), testA.idname, "usr", "lib", null);
	debug (path);
	assert (path == expectedPath);

	// We do the whole thing again to check if storing this info in the database worked.
	testA = depman.dependency_from_idname ("libexpat1");
	path = depman.get_absolute_library_path (testA);
	debug (path);
	assert (path == expectedPath);
}

void search_install_pkdep (Dep.PkInstaller pkinst, Dep.PkResolver pksolv, ref Dep.Module dep) {
	bool ret;
	ret = pksolv.search_dep_packages (dep);
	if (!ret) {
		debug (pksolv.last_error.details);
		assert (ret == true);
	}
	ret = pkinst.install_dependency (dep);
	if (!ret) {
		debug (pkinst.last_error.details);
		assert (ret == true);
	}
}

void test_packagekit_installer () {
	msg ("PackageKit dependency installer test");
	bool ret = false;

	// Build simple Nano dependency
	ArrayList<Dep.Module> deplist = new ArrayList<Dep.Module> ();
	Dep.Module depNano = new Dep.Module ("Nano");
	depNano.add_item (Dep.ItemType.BINARY, "/usr/bin/nano");

	Dep.PkInstaller pkit = new Dep.PkInstaller (ssettings);
	Dep.PkResolver pkslv = new Dep.PkResolver (ssettings);
	pkit.message.connect (test_solver_message_cb);

	search_install_pkdep (pkit, pkslv, ref depNano);
	assert (depNano.installed == true);

	// Now something more advanced
	Dep.Module crazy = new Dep.Module ("CrazyStuff");
	crazy.add_item (Dep.ItemType.BINARY, "/bin/bash");
	crazy.add_item (Dep.ItemType.SHARED_LIB, "libpackagekit-glib2.so");
	crazy.add_item (Dep.ItemType.SHARED_LIB, "libc6.so");

	search_install_pkdep (pkit, pkslv, ref crazy);
	assert (crazy.installed == true);

	// Now something which fails
	Dep.Module fail = new Dep.Module ("Fail");
	fail.add_item (Dep.ItemType.UNKNOWN, "/run/far_away");
	ret = pkslv.search_dep_packages (fail);
	if (!ret) {
		debug (pkit.last_error.details);
		assert (ret == false);
	}
	assert (fail.installed == false);
}

void test_feed_installer () {
	msg ("ZI Feed installer tests");
	Dep.FeedInstaller finst = new Dep.FeedInstaller (ssettings);

	Dep.Module test1 = new Dep.Module ("feedTest:1-gnu_parallel");
	test1.feed_url = "http://git.savannah.gnu.org/cgit/parallel.git/plain/packager/0install/parallel.xml";

	bool ret;
	ret = finst.install_dependency (test1);
	if (finst.last_error != null) {
		error (finst.last_error.details);
	}
	assert (ret == true);
	assert (test1.full_name == "GNU parallel");
	debug ("Selected version (GNU parallel): %s", test1.get_version ());
}

void test_depsolver () {
	msg ("Dependency solver tests");
	ArrayList<Dep.Module> deplist = new ArrayList<Dep.Module> ();

	// Create a set of dependencies
	Dep.Module dep1 = new Dep.Module ("Gee");
	dep1.feed_url = "http://sweets.sugarlabs.org/base/libgee";
	dep1.add_item (Dep.ItemType.SHARED_LIB, "libc.so.6");
	deplist.add (dep1);

	Dep.Module dep2 = new Dep.Module ("SDLMixer1.2");
	dep2.feed_url = "http://repo.roscidus.com/lib_rsl/sdl-mixer1.2";
	dep2.add_item (Dep.ItemType.SHARED_LIB, "libgee.so");
	deplist.add (dep2);

	Dep.Module dep3 = new Dep.Module ("Nano");
	dep3.add_item (Dep.ItemType.BINARY, "/usr/bin/nano");
	dep3.add_item (Dep.ItemType.SHARED_LIB, "libc.so.6");
	deplist.add (dep3);

	Dep.Module dep4 = new Dep.Module ("LibXml2");
	dep4.feed_url = "http://sweets.sugarlabs.org/base/libxml2";
	dep4.add_item (Dep.ItemType.SHARED_LIB, "libglib-2.0.so.0");
	deplist.add (dep4);

	bool ret;
	var cfactory = new Dep.ComponentFactory (ssettings);

	ret = cfactory.modules_installable (ref deplist);
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

	test_depsolver ();
	test_feed_installer ();
	//! test_packagekit_installer ();
	test_dependency_installer ();
	test_dependency_manager ();

	tenv.run ();

	return 0;
}
