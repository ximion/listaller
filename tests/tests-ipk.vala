/* tests-ipk.vala
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

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_ipk_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype == MessageEnum.INFO);
}

void test_ipk_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	error (item.details);
}

void test_ipk_package () {
	bool ret = false;
	msg ("Package tests");

	string ipkfilename = Path.build_filename (datadir, "ListallerFooBar-1.0_%s.ipk".printf (Utils.system_machine_generic ()), null);
	msg ("Loading IPK package %s".printf (ipkfilename));

	IPK.Package ipk = new IPK.Package (ipkfilename);
	// Connect signal handlers
	ipk.message.connect (test_ipk_message_cb);
	ipk.error_code.connect (test_ipk_error_code_cb);

	ret = ipk.initialize ();
	assert (ret == true);
	ret = ipk.set_install_mode (IPK.InstallMode.TEST);
	assert (ret == true);

	AppItem app = ipk.control.get_application ();
	assert (app.metainfo.name == "Listaller FooBar");

	Collection<IPK.FileEntry> flist = ipk.get_file_entries ();
	foreach (IPK.FileEntry e in flist) {
		bool inst_ok = ipk.install_file (e);
		assert (inst_ok == true);
	}
}

/*
NOTE: This is an old IPK-Control read/write test
*/
#if 0
void test_ipk_control_file () {
	msg ("Controlfile tests");
	IPK.PackControl ipkc = new IPK.PackControl ();
	//! ipkc.create_new ();
	AppItem a = new AppItem ("echoecho", "123");
	a.unique_name = "echo-123";
	a.desktop_file = "echo.desktop";
	//! ipkc.set_application (a);

	ArrayList<IPK.Dependency> list = new ArrayList<IPK.Dependency> ();
	IPK.Dependency d = null;
	d = new IPK.Dependency ("alpha");
	d.add_component ("/etc/alpha.conf", Deps.ComponentType.FILE);
	list.add (d);

	d = new IPK.Dependency ("beta");
	d.add_component ("/usr/lib/libbeta.so", Deps.ComponentType.SHARED_LIB);
	list.add (d);

	d = new IPK.Dependency ("gamma");
	d.add_component ("%SYS_LIB%/gamma.so.4", Deps.ComponentType.SHARED_LIB);
	list.add (d);

	d = new IPK.Dependency ("delta");
	d.add_component ("/usr/bin/delta", Deps.ComponentType.FILE);
	list.add (d);

	ipkc.set_dependencies (list);

	//! ipkc.test_dump_xml ();

	AppItem app = ipkc.get_application ();
	assert (app.full_name == "echoecho");
	assert (app.unique_name == "echo-123");
	assert (app.desktop_file == "%APP%/echo.desktop");

	list = ipkc.get_dependencies ();
	assert (list[0].full_name == "alpha");
	assert (list[0].has_component ("/etc/alpha.conf", Deps.ComponentType.FILE) == true);
}
#endif

void test_ipk_packcontrol () {
	msg ("IPK PackControl tests");
	string ctrlDir = Utils.real_path (Path.build_filename (datadir, "..", "foobar", "ipkinstall", null));

	IPK.PackControl ipkc = new IPK.PackControl ();

	bool ret;
	// We use the "pkoptions" file instead of the "pksetting" file here, both files have the same layout.
	ret = ipkc.open_control (Path.build_filename (ctrlDir, "pkoptions", null),
				 Path.build_filename (ctrlDir, "foobar.appdata.xml", null));
	assert (ret);

	AppItem app = ipkc.get_application ();
	assert (app.metainfo.name == "Listaller FooBar");
	assert (app.unique_name == "foobar");
}

void test_ipk_filelist_file () {
	msg ("Filelist tests.");
	bool ret = false;

	IPK.FileList flist = new IPK.FileList ();
	string foodir = Utils.real_path (Path.build_filename (datadir, "..", "foobar", null));
	// Add some files to IPK file list
	ret = flist.add_file (Path.build_filename (foodir, "autogen.sh", null), "%INST%/+junk");
	assert (ret == true);
	ret = flist.add_file (Path.build_filename (foodir, "doc", "foo.info", null), "%INST%/+junk");
	assert (ret == true);
	ret = flist.add_file (Path.build_filename (foodir, "foobar.desktop", null), "%APP%");
	assert (ret == true);

	string tmpfile = Path.build_filename (datadir, "files-tmp.list~", null);
	FileUtils.remove (tmpfile);
	flist.save (tmpfile);

	HashSet<IPK.FileEntry> lst = flist.get_files_list ();
	foreach (IPK.FileEntry e in lst) {
		msg (e.to_string ());
	}
}

int main (string[] args) {
	msg ("=== Running IPK Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	var tenv = new TestEnvironment ("ipkp");
	tenv.init (ref args);
	tenv.create_environment ();

	test_ipk_packcontrol ();
	test_ipk_filelist_file ();
	test_ipk_package ();

	tenv.run ();

	return 0;
}
