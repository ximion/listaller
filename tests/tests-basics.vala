/* tests-basics.vala
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

void test_application_ids () {
	bool ret = false;
	msg ("Testing app-ids");

	// Set up Listaller configuration
	Listaller.Settings conf = new Listaller.Settings ();
	conf.testmode = true;

	string foobar_dfile = Path.build_filename (foobar_dir, "foobar.desktop", null);
	AppItem dummy = new AppItem.from_desktopfile (foobar_dfile);
	msg ("Dummy application id: " + dummy.appid);
	string expected_id = "foobar;1.0;" + fold_user_dir (foobar_dfile) + ";" + "unknown";

	assert (dummy.appid == expected_id);

	AppItem item1 = new AppItem.from_id (expected_id);
	assert (item1.idname == "foobar");
	assert (item1.full_name == "Listaller FooBar");
	assert (item1.version == "1.0");
	assert (item1.publisher == "Listaller Project");
	assert (item1.get_raw_cmd () == "$INST/foo");

	AppItem item2 = new AppItem ("MyApp", "0.1");
	item2.origin = AppOrigin.IPK;
	assert (item2.full_name == "MyApp");
	assert (item2.idname == "myapp");
	//item2.desktop_file = Path.build_filename (foobar_dir, "foobar.desktop", null);
	item2.update_with_desktop_file ();
	assert (item2.desktop_file == "");
	assert (item2.appid == "myapp;0.1;;package_ipk");

	AppItem item3 = new AppItem ("Google Earth", "1.2");
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
}

void test_zfeeds () {
	Deps.Feed feed = new Deps.Feed ();
	feed.open (Path.build_filename (datadir, "libogg.xml", null));

	IPK.Dependency dep = new IPK.Dependency ("test");
	feed.update_dependency_data (ref dep);

	assert (dep.full_name == "libogg");
	assert (dep.homepage == "http://xiph.org/ogg/");
	/* Info: It is "libogg-0", because the version is set through "search_matching_dependency ()",
	 * which we don't call here because the libogg feed does not provide implementations
	 * for every platform out there. (This is a default-test, which should not fail, usually.) */
	assert (dep.idname == "libogg-0");

	bool ret = feed.search_matching_dependency ();
	assert (ret == true);

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

void test_doap () {
	DoapData doda = new DoapData ();
	string dd = load_file_to_string (Path.build_filename (datadir, "doap.doap"));
	doda.add_data (dd);
	//doda.add_file (Path.build_filename (datadir, "doap.doap"));
	doda.get_project ();
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
	mf.add_value ("Test2", "hgdufjhbudj\nhugdvh ushda743\nsuhfusdha7wdwe");
}

int main (string[] args) {
	msg ("=== Running Basic Tests ===");
	datadir = args[1];
	assert (datadir != null);
	foobar_dir = Path.build_filename (datadir, "foobar", null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	Test.init (ref args);
	test_utils ();
	test_application_ids ();
	test_versions ();
	test_metafile ();
	test_doap ();
	test_zfeeds ();
	Test.run ();
	return 0;
}
