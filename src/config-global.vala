/* config-global.vala -- Store global settings which affect all Listaller modules
 *
 * Copyright (C) 2009-2012 Matthias Klumpp <matthias@tenstral.net>
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

private static const string tmpdir = "/var/tmp";
private static const string tmpdir_small = "/tmp";

/**
 * Provides Listaller's configuration
 *
 * Provides global Listaller configuration.
 */
internal class Config : Object {
	private const string confdir = "/etc/listaller";
	private const string sudbdir = "/var/lib/listaller";
	private const string su_desktopdir = PkgConfig.PREFIXDIR + "/share/applications";
	private const string su_icondir = "/usr/share/icons/hicolor";
	private const string su_pixdir = "/usr/share/pixmaps";
	private string[] lib_paths = { PkgConfig.PREFIXDIR + "/lib",
					   PkgConfig.PREFIXDIR + "/lib64",
					   "/lib"};

	// System directories
	internal const string sys_libdir = PkgConfig.LIBDIR;
	internal const string sys_bindir = PkgConfig.PREFIXDIR + "/bin";
	internal const string sys_sharedir = PkgConfig.PREFIXDIR + "/share";
	internal const string sys_etcdir = "/etc";

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
		return sudbdir;
	}

	public string user_db_dir ()  {
		return Path.build_filename (user_data_dir (), "software", null);
	}

	public string user_data_dir () {
		return Environment.get_user_data_dir ();
	}

	public string shared_install_root () {
		return su_instroot;
	}

	public string user_install_root () {
		return Environment.get_user_data_dir ();
	}

	public string shared_icon_dir () {
		return su_icondir;
	}

	public string user_icon_dir () {
		return Path.build_filename (Environment.get_user_data_dir (), "icons", "hicolor", null);
	}

	public string shared_pixmap_dir () {
		return su_pixdir;
	}

	public string user_pixmap_dir () {
		return Path.build_filename (Environment.get_user_data_dir (), "icons", null);
	}

	public string shared_applications_dir () {
		return su_desktopdir;
	}

	public string user_applications_dir () {
		return Path.build_filename (Environment.get_home_dir (), ".local", "share", "applications", null);
	}

	public string tmp_dir () {
		string ret;
		//! ret = Environment.get_tmp_dir ();
		ret = Path.build_filename (tmpdir, "listaller");
		Utils.create_dir_parents (ret);

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

private bool find_library (string libname, Config conf) {
	Posix.Stat? s = null;
	string[] paths = conf.library_paths ();
	for (uint i = 0; paths[i] != null; i++) {
		s = null;
		string path = Path.build_filename (paths[i], libname);
		Posix.stat (path, out s);
		if (s.st_size != 0)
			return true;
	}

	return false;
}

} // End of namespace: Listaller

public void li_enable_translation ()
{
	// Initialize localisation
	Intl.bindtextdomain (PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
	Intl.bind_textdomain_codeset (PkgConfig.GETTEXT_PACKAGE, "UTF-8");
	Intl.textdomain (PkgConfig.GETTEXT_PACKAGE);
}