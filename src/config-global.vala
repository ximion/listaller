/* config-global.vala -- Store global settings which affect all Listaller modules
 *
 * Copyright (C) 2009-2013 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU Lesser General Public License Version 3
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

using PkgConfig;
using GLib;
using Listaller;

namespace Listaller {

/**
 * Provides Listaller's configuration
 *
 * Provides global Listaller configuration.
 */
internal class Config : Object {
	public  const string tmpdir = "/var/tmp";
	public  const string tmpdir_volatile = "/tmp";
	private const string confdir = "/etc/listaller";
	private const string suworkdir = "/var/lib/listaller";
	private const string sucachedir = "/var/cache/listaller";
	private const string datadir = PkgConfig.DATADIR + "/listaller";
	private const string su_desktopdir = PkgConfig.PREFIXDIR + "/share/applications";
	private const string su_icondir = "/usr/share/icons/hicolor";
	private const string su_pixdir = "/usr/share/pixmaps";
	public  string[] lib_paths = { PkgConfig.PREFIXDIR + "/lib",
					   PkgConfig.PREFIXDIR + "/lib64",
					   "/lib"};
	public  const string keyring_dir = suworkdir + "/keyring";

	// System directories
	internal const string sys_libdir = PkgConfig.LIBDIR;
	internal const string sys_bindir = PkgConfig.PREFIXDIR + "/bin";
	internal const string sys_sharedir = PkgConfig.PREFIXDIR + "/share";
	internal const string sys_etcdir = "/etc";
	internal const string sys_componentdir = sys_sharedir + "/modules";

	private string su_instroot;

	private KeyFile keyf;

	public Config () {
		// Append some Debian-specific multiarch paths
		string triplet = "%s-%s-gnu".printf (Utils.system_machine (), Utils.system_os ());
		lib_paths += Path.build_filename ("/lib", triplet);
		lib_paths += Path.build_filename ("/usr", "lib", triplet);
		lib_paths += Path.build_filename ("/usr", "lib", triplet, "mesa");
		// null-terminate paths list
		lib_paths += null;

		// load Listaller config
		keyf = new KeyFile ();
		try {
			keyf.load_from_file (Path.build_filename (conf_dir (), "Listaller.conf", null),
						KeyFileFlags.NONE);
		} catch (Error e) {}

		// load install-root
		try {
			su_instroot = keyf.get_string ("General", "UseInstallRoot");
		} catch (Error e) {
			su_instroot = "/opt";
		}

		// create some standard dirs which might not have been created already
		if (Utils.is_root ()) {
			Utils.create_dir_structure (sucachedir);
			Utils.create_dir_structure (suworkdir);
		}
	}

	[CCode (array_length = false, array_null_terminated = true)]
	public string[] library_paths () {
		return lib_paths;
	}

	internal string database_filename () {
		return "installed.db";
	}

	public string conf_dir () {
		return confdir;
	}

	public string shared_db_dir () {
		return Path.build_filename (suworkdir, "db", null);
	}

	public string shared_repo_cache_dir () {
		string dir = Path.build_filename (sucachedir, "repo", null);
		Utils.create_dir_structure (dir);
		return dir;
	}

	public string user_repo_cache_dir () {
		string dir = Path.build_filename (user_install_root (), "software", "cache", null);
		Utils.create_dir_structure (dir);
		return dir;
	}

	public string data_dir () {
		return datadir;
	}

	public string user_db_dir ()  {
		return Path.build_filename (user_install_root (), "software", null);
	}

	public string shared_install_root () {
		return su_instroot;
	}

	public string user_install_root () {
		return Utils.get_user_data_dir ();
	}

	public string shared_icon_dir () {
		return su_icondir;
	}

	public string user_icon_dir () {
		return Path.build_filename (user_install_root (), "icons", "hicolor", null);
	}

	public string shared_pixmap_dir () {
		return su_pixdir;
	}

	public string user_pixmap_dir () {
		return Path.build_filename (user_install_root (), "icons", null);
	}

	public string shared_applications_dir () {
		return su_desktopdir;
	}

	public string user_applications_dir () {
		return Path.build_filename (Utils.get_home_dir (), ".local", "share", "applications", null);
	}

	public string tmp_dir () {
		string ret;
		//! ret = Environment.get_tmp_dir ();
		ret = Path.build_filename (tmpdir, "listaller");
		Utils.create_dir_structure (ret);

		return ret;
	}

	public string get_unique_tmp_dir (string prefix = "") {
		string px_str = prefix;
		if (px_str != "")
			px_str = px_str + "-";
		string template = Path.build_filename (tmp_dir (), px_str + "XXXXXX", null);

		string res = DirUtils.mkdtemp (template);
		if (res == null) {
			critical ("Unable to create tmp-dir! Error: %s", strerror (errno));
			res = tmp_dir ();
		}
		return res;
	}

	private bool config_str_in_list (string comma_list, string str) {
		if (comma_list.down () == "All")
			return true;
		if (comma_list.down () == "None")
			return false;
		string[] parts = comma_list.split (",");
		if (str in parts)
			return true;

		return false;
	}

	public string? installer_get_str (string key) {
		string? str;
		try {
			str = keyf.get_string ("Installer", key);
		} catch (Error e) {
			return null;
		}

		return str;
	}

	public bool installer_get_bool (string key) {
		bool ret;
		try {
			ret = keyf.get_boolean ("Installer", key);
		} catch (Error e) {
			return false;
		}

		return ret;
	}

	public bool installer_get_string_in_list (string key, string str) {
		string? res = installer_get_str (key);
		if (res == null)
			return false;
		bool ret = config_str_in_list (res, str);

		return ret;
	}

	public string? manager_get_str (string key) {
		string? str;
		try {
			str = keyf.get_string ("Manager", key);
		} catch (Error e) {
			return null;
		}

		return str;
	}

	public bool manager_get_bool (string key) {
		bool ret;
		try {
			ret = keyf.get_boolean ("Manager", key);
		} catch (Error e) {
			return false;
		}

		return ret;
	}

}

private string? find_library (string libname, Config conf) {
	Posix.Stat? s = null;
	string[] paths = conf.library_paths ();
	for (uint i = 0; paths[i] != null; i++) {
		if (!FileUtils.test (paths[i], FileTest.EXISTS))
			continue;

		if (libname.has_suffix ("*")) {
			var res = Utils.find_files_matching (paths[i], libname);
			if (res == null)
				continue;
			if (res.size > 0)
				return res.to_array ()[0];
		} else {
			// speed-up non-wildcard searches
			s = null;
			string path = Path.build_filename (paths[i], libname);
			Posix.stat (path, out s);
			if (s.st_size != 0)
				return path;
		}
	}

	return null;
}

private bool library_exists (string libname, Config conf) {
	string? res = find_library (libname, conf);

	return res != null;
}

} // End of namespace: Listaller

namespace Listaller.Utils {

	public void enable_translation ()
	{
		// Initialize localisation
		Intl.bindtextdomain (PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset (PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain (PkgConfig.GETTEXT_PACKAGE);
	}

} // End of namespace: Listaller.Utils
