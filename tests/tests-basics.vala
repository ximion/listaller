/* tests-basics.vala
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
using Listaller.Utils;

private string datadir;
private string foobar_dir;

void msg (string s) {
	stdout.printf (s + "\n");
}

void test_basics_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype != MessageEnum.CRITICAL);
}

void test_basics_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	error (item.details);
}

void test_listaller_config () {
	// Set up Listaller configuration
	var conf = new Listaller.Config ();

	// Set up setup-settings
	var ssettings = new SetupSettings (IPK.InstallMode.TEST);
	assert (ssettings.test_mode == true);

	string tmp = ssettings.get_unique_install_tmp_dir ();
	assert (tmp == ssettings.get_unique_install_tmp_dir ());
	ssettings.invalidate_tmp_dir ();
	assert (tmp != ssettings.get_unique_install_tmp_dir ());
}

void test_application_ids () {
	bool ret = false;
	msg ("Testing app-ids");

	string foobar_mfile = Path.build_filename (foobar_dir, "foobar.appdata.xml", null);
	AppItem dummy = new AppItem ();
	ret = dummy.load_xml_file (foobar_mfile);
	assert (ret == true);

	msg ("Dummy application id: " + dummy.appid);
	string expected_id = "foobar;1.0;" + fold_user_dir (foobar_mfile) + ";" + "unknown";

	//! assert (dummy.appid == expected_id);

	AppItem item1 = new AppItem.from_id (expected_id);
	assert (item1.idname == "foobar");
	assert (item1.metainfo.name == "Listaller FooBar");
	assert (item1.version == "1.0");
	//! assert (item1.metainfo.developer == "Listaller Project");
	//! assert (item1.get_raw_cmd () == "%INST%/foo");
	assert (item1.origin == "unknown");

	var cpt = new Appstream.Component ();
	cpt.name = "MyApp";
	AppItem item2 = new AppItem ();
	item2.metainfo = cpt;
	item2.version = "0.1";
	item2.origin = "http://example.com";
	assert (item2.metainfo.name == "MyApp");
	assert (item2.idname == "myapp");
	item2.metadata_file = Path.build_filename (foobar_dir, "foobar.appdata.xml", null);
	item2.update_with_metadata_file ();
	assert (item2.desktop_file == "%APP%/foobar.desktop");
	debug (item2.appid);
	assert (item2.appid == "myapp;1.0;%s;http://example.com".printf (item2.metadata_file));

	cpt.name = "Google Earth";
	AppItem item3 = new AppItem ();
	item3.metainfo = cpt;
	assert (item3.idname == "google_earth");
}

void test_utils () {
	string xpath = fold_user_dir (datadir);
	xpath = expand_user_dir (xpath);
	assert (datadir == xpath);

	string s;
	s = real_path ("/usr/share/../bin/test");
	assert (s == "/usr/bin/test");
	s = real_path ("/usr/share/./listaller/data/../files");
	assert (s == "/usr/share/listaller/files");

	s = Utils.build_filename ("/usr/bin/", "..", "share", "listaller", ".", "test");
	assert (s == "/usr/share/listaller/test");
}

void test_zfeeds () {
	Dep.Feed feed = new Dep.Feed ();
	feed.open (Path.build_filename (datadir, "libogg.xml", null));

	var dep = new Dependency.blank ();
	feed.update_dependency_data (dep);

	assert (dep.metainfo.name == "libogg");
	assert (dep.metainfo.get_url (Appstream.UrlKind.HOMEPAGE) == "http://xiph.org/ogg/");
	/* Info: It is "libogg-0", because the version is set through "search_matching_dependency ()",
	 * which we don't call here because the libogg feed does not provide implementations
	 * for every platform out there. (This is a default-test, which should not fail, usually.) */
	assert (dep.metainfo.id == "libogg");

	bool ret = feed.search_matching_dependency ();
	assert (ret == true);

	feed.update_dependency_data (dep);
	assert (dep.get_version () == "1.1.4-1");

	assert (feed.package_url != "");
}

int comp_ver (string a, string b) {
	int i = compare_versions (a, b);
	debug ("Comparing versions: %s and %s => [%i]", a, b, i);
	return i;
}

void test_versions () {
	assert (comp_ver ("6", "8") == -1);
	assert (comp_ver ("0.6.12b-d", "0.6.12a") == 1);
	assert (comp_ver ("7.4", "7.4") == 0);
	assert (comp_ver ("ab.d", "ab.f") == -1);
	assert (comp_ver ("0.6.16", "0.6.14") == 1);

	assert (comp_ver ("3.0.rc2", "3.0.0") == -1);
}

void test_appstream_xml () {
	Appstream.Component cpt;
	var mdata = new Appstream.Metadata ();
	string asd = load_file_to_string (Path.build_filename (datadir, "appstream.appdata.xml"));
	try {
		cpt = mdata.parse_data (asd);
	} catch (Error e) {
		error (e.message);
	}

	var item = new AppItem ();
	item.metainfo = cpt;
	item.fast_check ();
	assert (item.idname == "foobar");
}

void test_metafile () {
	var mf = new IPK.MetaFile ();
	bool ret;

	ret = mf.open_file (Path.build_filename (datadir, "test-requirements.list"));
	assert (ret == true);
	mf.open_block_by_value ("name", "vorbis");
	string value = mf.get_value ("libraries");
	assert (value == "libvorbis.so.*\nlibvorbisfile.so.*\nlibvorbisenc.so.*");

	ret = mf.open_block_by_value ("id", "mesagl");
	assert (ret == true);
	mf.add_value ("Test", "Blahfvlshfikdj");
	ret = mf.add_value ("Test2", "hgdufjhbudj\nhugdvh ushda743\nsuhfusdha7wdwe");
	assert (ret == true);
	var val = mf.get_value ("Test2");
	assert (val == "hgdufjhbudj\nhugdvh ushda743\nsuhfusdha7wdwe");
}

void test_field () {
	IPK.InstallMode flags = IPK.InstallMode.NONE;
	assert (flags.is_all_set (IPK.InstallMode.SHARED) == false);
	assert (flags.is_any_set (IPK.InstallMode.SHARED) == false);

	flags = IPK.InstallMode.SHARED;
	assert (flags.is_all_set (IPK.InstallMode.PRIVATE) == false);
	assert (flags.is_all_set (IPK.InstallMode.SHARED) == true);
	assert (flags.is_any_set (IPK.InstallMode.SHARED | IPK.InstallMode.PRIVATE) == true);

	flags = flags.set (IPK.InstallMode.PRIVATE);
	assert (flags.is_all_set (IPK.InstallMode.PRIVATE | IPK.InstallMode.SHARED) == true);
	assert (flags.is_all_set (IPK.InstallMode.SHARED | IPK.InstallMode.TEST) == false);

	flags = flags.unset (IPK.InstallMode.PRIVATE);
	assert (flags.is_all_set (IPK.InstallMode.PRIVATE | IPK.InstallMode.SHARED) == false);
}

void test_components () {
	bool ret;

	var foo_frmw = new Dependency.blank ();
	ret = foo_frmw.load_from_file (Path.build_filename (datadir, "FooTest2.framework.xml", null));
	assert (ret);

	assert (foo_frmw.metainfo.id == "FooTest2");

	foo_frmw.installed = true;
	string version = foo_frmw.get_version ();
	assert (version == "1.0");

	// --------------

	// Test capabilities of resolving version names
	var lilibv_frmw = new Dependency.blank ();
	ret = lilibv_frmw.load_from_file (Path.build_filename (datadir, "ListallerTest1.framework", null));
	assert (ret);
	assert (lilibv_frmw.metainfo.id == "ListallerTest1");

	lilibv_frmw.installed = true;
	version = lilibv_frmw.get_version ();
	debug ("LiVersion: %s", version);
	assert (version == PkgConfig.PACKAGE_VERSION);
}

void test_playground () {
	var conf = new Listaller.Config ();

	// Just try something!
}

int main (string[] args) {
	msg ("=== Running Basic Tests ===");
	datadir = args[1];
	assert (datadir != null);
	foobar_dir = Path.build_filename (datadir, "foobar", null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	var tenv = new TestEnvironment ("basic");
	tenv.init (ref args);
	tenv.create_environment ();
	tenv.enforce_full_verbosity ();

	test_utils ();
	test_listaller_config ();
	test_application_ids ();
	test_versions ();
	test_metafile ();
	test_appstream_xml ();
	test_components ();
	test_zfeeds ();
	test_field ();
	test_playground ();

	tenv.run ();

	return 0;
}
