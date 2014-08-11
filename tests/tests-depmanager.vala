/* tests-depmanager.vala
 *
 * Copyright (C) 2011-2014 Matthias Klumpp
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

	// Feed installer is currently disabled by default
	// Test 1
	Dependency test1 = new Dependency ();
	test1.unique_name = "Test:1#expat";
	//test1.feed_url = "http://repo.roscidus.com/lib/expat1";
	test1.add_item (Appstream.ProvidesKind.LIBRARY, "libc6.so");
	//test1.add_item (Appstream.ProvidesKind.LIBRARY, "bladada.so.2");

	ret = depinst.install_existing_module_dependency (cfactory, ref test1);
	assert (ret == true);
	assert (test1.installed == true);

	// Test 2
	Dependency test2 = new Dependency ();
	test2.metainfo.id = "Test:2#GNU_parallel";
	test2.feed_url = "http://git.savannah.gnu.org/cgit/parallel.git/plain/packager/0install/parallel.xml";
	//test2.add_item ("libvorbis.so.0", Appstream.ProvidesKind.LIBRARY);
	test2.add_item (Appstream.ProvidesKind.LIBRARY, "nobis.so.0");

	ret = depinst.install_existing_module_dependency (cfactory, ref test2);

	/* We already installed this in a previous test, but did no proper
	 * registration. So this has to fail because unknown files would be overwritten.
	 */
	assert (ret == false);
	assert (test2.installed == false);
	assert (test2.metainfo.name == "GNU parallel");

	// Test 3
	// Look if dependency of test1 is still available :-P
	test1.installed = false;
	test1.metainfo.name = "";
	ret = depinst.install_existing_module_dependency (cfactory, ref test1);
	assert (ret == true);
	assert (test1.installed == true);
	assert (test1.metainfo.name == "libexpat1");
}

void test_dependency_manager () {
	msg ("Dependency manager tests");

	DepManager depman = new DepManager (sdb);

	Dependency testA = depman.dependency_from_idname ("libexpat1");
	assert (testA != null);

	string path = depman.get_absolute_library_path (testA);
	string expectedPath = Path.build_filename (ssettings.depdata_dir (), testA.metainfo.id, "usr", "lib", null);
	debug (path);
	assert (path == expectedPath);

	// We do the whole thing again to check if storing this info in the database worked.
	testA = depman.dependency_from_idname ("libexpat1");
	path = depman.get_absolute_library_path (testA);
	debug (path);
	assert (path == expectedPath);
}

void search_install_pkdep (Dep.PkInstaller pkinst, Dep.PkResolver pksolv, ref Dependency dep) {
	bool ret;
#if 0
	ret = pksolv.search_dep_packages (dep);
	if (!ret) {
		debug (pksolv.last_error.details);
		assert (ret == true);
	}
#endif
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
	ArrayList<Dependency> deplist = new ArrayList<Dependency> ();
	Dependency depNano = new Dependency ();
	depNano.metainfo.id = "Nano";
	depNano.add_item (Appstream.ProvidesKind.BINARY, "/usr/bin/nano");

	Dep.PkInstaller pkit = new Dep.PkInstaller (ssettings);
	Dep.PkResolver pkslv = new Dep.PkResolver (ssettings);
	pkit.message.connect (test_solver_message_cb);

	search_install_pkdep (pkit, pkslv, ref depNano);
	assert (depNano.installed == true);

	// Now something more advanced
	Dependency crazy = new Dependency ();
	crazy.metainfo.id = "CrazyStuff";
	crazy.add_item (Appstream.ProvidesKind.BINARY, "/bin/bash");
	crazy.add_item (Appstream.ProvidesKind.LIBRARY, "libpackagekit-glib2.so");
	crazy.add_item (Appstream.ProvidesKind.LIBRARY, "libc6.so");

	search_install_pkdep (pkit, pkslv, ref crazy);
	assert (crazy.installed == true);

#if 0
	// Now something which fails
	Dependency fail = new Dependency ("Fail");
	fail.add_item (Appstream.ProvidesKind.UNKNOWN, "/run/far_away");
	ret = pkslv.search_dep_packages (fail);
	if (!ret) {
		debug (pkit.last_error.details);
		assert (ret == false);
	}
	assert (fail.installed == false);
#endif
}

void test_feed_installer () {
	msg ("ZI Feed installer tests");
	Dep.FeedInstaller finst = new Dep.FeedInstaller (ssettings);

	Dependency test1 = new Dependency ();
	test1.metainfo.id = "feedTest:1-gnu_parallel";
	test1.feed_url = "http://git.savannah.gnu.org/cgit/parallel.git/plain/packager/0install/parallel.xml";

	bool ret;
	ret = finst.install_dependency (test1);
	if (finst.last_error != null) {
		error (finst.last_error.details);
	}
	assert (ret == true);
	assert (test1.metainfo.name == "GNU parallel");
	debug ("Selected version (GNU parallel): %s", test1.get_version ());
}

void test_depsolver () {
	msg ("Dependency solver tests");
	ArrayList<Dependency> deplist = new ArrayList<Dependency> ();

	// Create a set of dependencies
	Dependency dep1 = new Dependency ();
	dep1.metainfo.id = "Gee";
	dep1.feed_url = "http://sweets.sugarlabs.org/base/libgee";
	dep1.add_item (Appstream.ProvidesKind.LIBRARY, "libc.so.6");
	deplist.add (dep1);

	Dependency dep2 = new Dependency ();
	dep2.metainfo.id = "SDLMixer1.2";
	dep2.feed_url = "http://repo.roscidus.com/lib_rsl/sdl-mixer1.2";
	dep2.add_item (Appstream.ProvidesKind.LIBRARY, "libgee.so");
	deplist.add (dep2);

	Dependency dep3 = new Dependency ();
	dep3.metainfo.id = "Nano";
	dep3.add_item (Appstream.ProvidesKind.BINARY, "/usr/bin/nano");
	dep3.add_item (Appstream.ProvidesKind.LIBRARY, "libc.so.6");
	deplist.add (dep3);

	Dependency dep4 = new Dependency ();
	dep4.metainfo.id = "LibXml2";
	dep4.feed_url = "http://sweets.sugarlabs.org/base/libxml2";
	dep4.add_item (Appstream.ProvidesKind.LIBRARY, "libglib-2.0.so.0");
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
