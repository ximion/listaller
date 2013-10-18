/* testcommon.vala -- Basic testing infrastructure
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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

/**
 * A generic Listaller testing environment
 */
private class TestEnvironment {
	private string test_env_name;
	private string test_tmpdir;

	private string tmp_home_dir;
	private string tmp_home_data_dir;
	private string tmp_home_config_dir;
	private string tmp_home_cache_dir;

	public TestEnvironment (string test_name) {
		test_env_name = test_name;

		// build tmp directory name
		string base_tmp_path = Path.build_filename ("/var", "tmp", "listaller", "unit-tests", null);
		Listaller.Utils.create_dir_structure (base_tmp_path);
		string template = Path.build_filename (base_tmp_path, "%s-XXXXXX".printf (test_env_name), null);

		string res = DirUtils.mkdtemp (template);
		if (res == null) {
			error ("Unable to create temporary test dir! Error: %s", strerror (errno));
		}
		test_tmpdir = res;

		// create fake dirs
		tmp_home_dir = Path.build_filename (test_tmpdir, "home", null);
		tmp_home_data_dir = Path.build_filename (tmp_home_dir, "data", null);
		tmp_home_config_dir = Path.build_filename (tmp_home_dir, "config", null);
		tmp_home_cache_dir = Path.build_filename (tmp_home_dir, "cache", null);
	}

	private void clear_environment_variables () {
		string[] vars = Environment.list_variables ();
		foreach (string s in vars)
			Environment.unset_variable (s);
	}

	public void create_environment (bool clear_standard_env = true) {
		if (clear_standard_env)
			clear_environment_variables ();

		// make sure directories exist
		Listaller.Utils.create_dir_structure (tmp_home_dir);
		Listaller.Utils.create_dir_structure (tmp_home_data_dir);
		Listaller.Utils.create_dir_structure (tmp_home_config_dir);
		Listaller.Utils.create_dir_structure (tmp_home_cache_dir);

		// set environment variables
		Environment.set_variable ("HOME", tmp_home_dir, true);
		Environment.set_variable ("XDG_HOME_DIR", tmp_home_dir, true);
		Environment.set_variable ("XDG_DATA_HOME", tmp_home_data_dir, true);
		Environment.set_variable ("XDG_CONFIG_HOME", tmp_home_config_dir, true);
		Environment.set_variable ("XDG_CACHE_HOME", tmp_home_cache_dir, true);

		// PackageKit limits the use of GIO, and we don't want (and can't) to use it's advanced
		// features in Listaller too.
		Environment.set_variable ("GIO_USE_VFS", "local", true);
	}

	public void listaller_set_unittestmode (bool enabled) {
		Listaller.Utils.__unittestmode = enabled;
	}

	public void enforce_full_verbosity () {
		Environment.set_variable ("G_MESSAGES_DEBUG", "all", true);
	}

	public void init ([CCode (array_length_pos = 0.9)] ref unowned string[] args) {
		Test.init (ref args);

		// set Listaller generic verbosity settings
		Listaller.set_console_mode (true);
		Listaller.set_verbose_mode (true);
		Listaller.add_log_domain ("LiTest");

		// switch Listaller internals to unittestmode
		listaller_set_unittestmode (true);
	}

	public void run () {
		Test.run ();
	}

	public void add_func (string testpath, TestFunc test_funcvoid) {
		Test.add_func ("/%s/%s".printf (test_env_name, testpath), test_funcvoid);
	}

	public void print_environment_vars () {
		string[] vars = Environment.list_variables ();
		string vars_str = "";
		foreach (string s in vars)
			vars_str += "%s = %s\n".printf (s, Environment.get_variable (s));

		stdout.printf ("Environment: %s", vars_str);
	}
}
