/* tests-updater.vala
 *
 * Copyright (C) 2012-2014 Matthias Klumpp <matthias@tenstral.net>
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

void test_upd_message_cb (MessageItem item) {
	msg ("Received message:");
	msg (" " + item.to_string ());
	assert (item.mtype == MessageEnum.INFO);
}

void test_upd_error_code_cb (ErrorItem item) {
	msg ("Received error:");
	msg (" " + item.to_string ());
	// skip all permission-errors
	if (item.error != ErrorEnum.OPERATION_NOT_ALLOWED)
		error (item.details);
}

void print_app_arraylist (ArrayList<AppItem> appList, string label = "") {
	if (label != "")
		msg ("Application List [%s]:".printf (label));
	else
		msg ("Application List:");

	foreach (AppItem app in appList) {
		stdout.printf ("%s\n", app.to_string ());
	}
	msg ("END");
}

private string foobar_fake_config_ipath;

void test_foobar_installation () {
	bool ret;
	string ipkfilename = Path.build_filename (datadir, "FooBar-1.0_%s.ipk".printf (Utils.system_machine_generic ()), null);

	Setup setup = new Setup (ipkfilename);
	setup.error_code.connect (test_upd_error_code_cb);
	setup.message.connect (test_upd_message_cb);

	ret = setup.initialize ();
	assert (ret == true);

	ret = setup.set_install_mode (IPK.InstallMode.PRIVATE);
	assert (ret == true);

	ret = setup.run_installation ();
	assert (ret == true);

	AppItem? app = setup.get_current_application ();
	assert (app != null);

	var vs = new VarSolver (app.unique_id);
	foobar_fake_config_ipath = vs.substitute_vars_auto ("%INST%/foo_testconf.conf", setup.settings);
}

void test_manager_installed_apps () {
	Manager mgr = new Manager ();
	mgr.settings.current_mode = IPK.InstallMode.PRIVATE;

	ArrayList<AppItem> app_list;
	mgr.filter_applications (AppState.INSTALLED_SHARED | AppState.INSTALLED_PRIVATE, out app_list);
	print_app_arraylist (app_list, "Installed apps (pre-update)");
	// we should have exactly 1 app installed (FooBar)
	assert (app_list.size == 1);

	// test version
	assert (app_list[0].version == "1.0");

	// test example file
	var fake_conf = new KeyFile ();
	fake_conf.load_from_file (foobar_fake_config_ipath, KeyFileFlags.NONE);
	string str = fake_conf.get_string ("Foobar", "Version");

	assert (str == "1.0");
}

void test_refresh_repo_cache () {
	// refresh app cache
	Repo.Manager repomgr = new Repo.Manager ();
	repomgr.refresh_cache ();

	// check new app info
	Manager mgr = new Manager ();
	mgr.settings.current_mode = IPK.InstallMode.PRIVATE;

	ArrayList<AppItem> app_list;
	mgr.filter_applications (AppState.AVAILABLE, out app_list);
	print_app_arraylist (app_list, "Available apps");
	// we should now have more than one app
	assert (app_list.size > 1);
}

void test_updater_update () {
	Updater upd = new Updater (false);
	upd.settings.current_mode = IPK.InstallMode.PRIVATE;
	upd.find_updates ();

	message ("Found updates: %i", upd.available_updates.size);
	assert (upd.available_updates.size >= 1);

	upd.apply_updates_all ();

	foreach (UpdateItem item in upd.available_updates)
		assert (item.completed == true);

	upd.find_updates ();
	assert (upd.available_updates.size == 0);

	// now test installed apps *after* update (version should have been increased)
	Manager mgr = new Manager ();
	mgr.settings.current_mode = IPK.InstallMode.PRIVATE;

	ArrayList<AppItem> app_list;
	mgr.filter_applications (AppState.INSTALLED_SHARED | AppState.INSTALLED_PRIVATE, out app_list);
	print_app_arraylist (app_list, "Installed apps (post-update)");
	// we should still have exactly 1 app installed
	assert (app_list.size == 1);

	// test version
	assert (app_list[0].version == "1.2");

	// test example file
	var fake_conf = new KeyFile ();
	fake_conf.load_from_file (foobar_fake_config_ipath, KeyFileFlags.NONE);
	string str = fake_conf.get_string ("Foobar", "Version");

	assert (str == "1.2");
}


int main (string[] args) {
	msg ("=== Running Updater Tests ===");
	datadir = args[1];
	assert (datadir != null);
	datadir = Path.build_filename (datadir, "testdata", null);
	assert (FileUtils.test (datadir, FileTest.EXISTS) != false);

	var tenv = new TestEnvironment ("updater");
	tenv.init (ref args);
	tenv.create_environment ();
	tenv.listaller_set_unittestmode (true);

	// prepare updater environment
	test_foobar_installation ();
	test_manager_installed_apps ();

	// perform updater tests
	test_refresh_repo_cache ();
	test_updater_update ();

	tenv.run ();

	return 0;
}
