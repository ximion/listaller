/* settings.vala
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

using Config;
using GLib;
using Listaller;

namespace Listaller {

private static const string tmpdir = "/var/tmp";
private static const string tmpdir_small = "/tmp";

/**
 * Provides Listaller's settings
 *
 * Provides settings for all other classes.
 */
public class Settings : Object {

	const string confdir = "/etc/listaller";
	const string sudbdir = "/var/lib/listaller";
	const string su_instroot = "/opt";
	const string su_desktopdir = Config.PREFIXDIR + "/share/applications";
	const string su_icondir = "/usr/share/icons/hicolor";
	const string su_pixdir = "/usr/share/pixmaps";
	private string[] lib_paths = { Config.PREFIXDIR + "/lib",
					   Config.PREFIXDIR + "/lib64",
					   "/lib"};

	private bool _sumode;
	private bool _testmode;
	private bool _locked;
	private string uinsttmp = "";


	// System directories
	internal const string sys_libdir = Config.LIBDIR;
	internal const string sys_bindir = Config.PREFIXDIR + "/bin";
	internal const string sys_sharedir = Config.PREFIXDIR + "/share";
	internal const string sys_etcdir = "/etc";

	public bool locked {
		get { return _locked; }
	}

	public bool sumode {
		get { return _sumode; }
		set { if (can_change ()) _sumode = value; }
	}

	public bool testmode {
		get { return _testmode; }
		set { if (can_change ()) _testmode = value; }
	}

	public Settings (bool root = false) {
		sumode = root;
		testmode = false;

		// Append some Debian-specific multiarch paths
		string tripel = "%s-%s-gnu".printf (Utils.system_machine (), Utils.system_os ());
		lib_paths += Path.build_filename ("/lib", tripel);
		lib_paths += Path.build_filename ("/usr", "lib", tripel);
		lib_paths += Path.build_filename ("/usr", "lib", tripel, "mesa");
		// null-terminate paths list
		lib_paths += null;

		unlock (); // Be unlocked by default
	}

	/*
	 * Lock the settings, so no changes can be made anymore
	 */
	public void lock () {
		_locked = true;
	}

	/*
	 * Allow changing the settings again
	 */
	public void unlock () {
		_locked = false;
	}

	/*
	 * @returns: True if we can change values
	 */
	private bool can_change () {
		if (locked) {
			warning ("Tried to write on locked settings object! (Don't try this!)");
			return false;
		} else {
			return true;
		}
	}

	public string database_file () {
		return appregister_dir () + "/software.db";
	}

	[CCode (array_length = false, array_null_terminated = true)]
	public string[] library_paths () {
		return lib_paths;
	}

	private void touch_dir (string dirname, string warnmsg = "Error: %s") {
		if ((sumode && Utils.is_root ()) || (!sumode)) {
			File d = File.new_for_path (dirname);
			try {
				if (!d.query_exists ()) {
					d.make_directory_with_parents ();
				}
			} catch (Error e) {
				warning (warnmsg, e.message);
			}
		}
	}

	public string appregister_dir () {
		string regdir;
		if (sumode) {
			regdir = sudbdir;
		} else {
			regdir = Path.build_filename (Environment.get_user_data_dir (), "software", null);
		}
		if (testmode) {
			// Point to our temporary dir
			regdir = Path.build_filename (get_unique_install_tmp_dir (), "appreg", null);
		}

		touch_dir (regdir, "Unable to create application database directory: %s");

		return regdir;
	}

	public string appdata_dir () {
		string addir;

		if (sumode) {
			addir = Path.build_filename (su_instroot, "apps", null);
		} else {
			addir = Path.build_filename (Environment.get_home_dir (), ".appdata", "apps", null);
		}
		if (testmode) {
			// Point to our temporary dir
			addir = Path.build_filename (get_unique_install_tmp_dir (), "apps", null);
		}

		touch_dir (addir, "Unable to create application data directory: %s");

		return addir;
	}

	public string depdata_dir () {
		string depdir;

		if (sumode) {
			depdir = Path.build_filename (su_instroot, "deps", null);
		} else {
			depdir = Path.build_filename (Environment.get_home_dir (), ".appdata", "deps", null);
		}
		if (testmode) {
			// Point to our temporary dir
			depdir = Path.build_filename (get_unique_install_tmp_dir (), "deps", null);
		}

		touch_dir (depdir, "Unable to create application data directory: %s");

		return depdir;
	}

	public string icon_base_dir () {
		string icodir;

		if (sumode) {
			icodir = su_icondir;
		} else {
			icodir = Path.build_filename (Environment.get_user_data_dir (), "icons", "hicolor", null);
		}
		if (testmode) {
			// Point to our temporary dir
			icodir = Path.build_filename (get_unique_install_tmp_dir (), "icons", null);
		}

		touch_dir (icodir, "Unable to create icon directory: %s");

		return icodir;
	}

	public string icon_size_dir (int size = 0) {
		string icodir;

		bool size_valid = false;
		switch (size) {
			case 16: size_valid = true;
				break;
			case 24: size_valid = true;
				break;
			case 32: size_valid = true;
				break;
			case 48: size_valid = true;
				break;
			case 64: size_valid = true;
				break;
			case 128: size_valid = true;
				break;
			case 265: size_valid = true;
				break;
			default: size_valid = false;
				break;
		}

		if (!size_valid)
			size = 0;

		if (size == 0) {
			if (sumode)
				icodir = su_pixdir;
			else
				icodir = Path.build_filename (Environment.get_user_data_dir (), "icons", null);
		} else {
			icodir = Path.build_filename (icon_base_dir (), size.to_string () + "x" + size.to_string (), "apps", null);
		}

		touch_dir (icodir, "Unable to create icon directory: %s");

		return icodir;
	}

	public string desktop_dir () {
		string dskdir;

		if (sumode) {
			dskdir = su_desktopdir;
		} else {
			dskdir = Path.build_filename (Environment.get_home_dir (), ".local", "share", "applications", null);
		}
		if (testmode) {
			// Point to our temporary dir
			dskdir = Path.build_filename (get_unique_install_tmp_dir (), "app-desktop", null);
		}

		touch_dir (dskdir, "Unable to create application data directory: %s");

		return dskdir;
	}

	public string conf_dir () {
		return confdir;
	}

	public string tmp_dir () {
		string ret;
		//! ret = Environment.get_tmp_dir ();
		ret = tmpdir;
		return ret;
	}

	public string get_unique_tmp_dir (string prefix = "listaller") {
		string template = Path.build_filename (tmp_dir (), prefix + "-XXXXXX", null);

		string res = DirUtils.mkdtemp (template);
		if (res == null)
			res = tmp_dir ();
		return res;
	}

	internal void invalidate_tmp_dir () {
		if (uinsttmp.has_prefix (tmpdir))
			Utils.delete_dir_recursive (uinsttmp);
		uinsttmp = "";
	}

	public string get_unique_install_tmp_dir () {
		if (uinsttmp != "")
			return uinsttmp;

		string template = Path.build_filename (tmp_dir (), "litest-XXXXXX", null);

		string res = DirUtils.mkdtemp (template);
		if (res == null)
			res = tmp_dir ();
		uinsttmp = res;
		return uinsttmp;
	}
}

private bool find_library (string libname, Listaller.Settings conf) {
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

} // End of namespace

public void li_enable_translation ()
{
	// Initialize localisation
	Intl.bindtextdomain (Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
	Intl.bind_textdomain_codeset (Config.GETTEXT_PACKAGE, "UTF-8");
	Intl.textdomain (Config.GETTEXT_PACKAGE);
}
