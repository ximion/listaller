/* setup-settings.vala -- Handle settings of a installation
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

using GLib;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

[Flags]
public enum InstallMode {
	NONE = 0,
	SHARED,
	PRIVATE,
	TEST;

	public inline bool is_all_set (InstallMode flags) {
		return (this & flags) == flags;
	}

	public inline bool is_any_set (InstallMode flags) {
		return (this & flags) != 0;
	}

        public inline InstallMode set (InstallMode mode) {
		return (this | mode);
	}

	public inline InstallMode unset (InstallMode mode) {
		return (this & ~mode);
	}
}

} // End of namespace: Listaller.IPK

namespace Listaller {

/**
 * Class to store settings of a IPK setup.
 */
public class SetupSettings : Object {
	private Config conf;
	private string uinsttmp = "";
	private bool _locked;

	private IPK.InstallMode _current_mode;

	internal SetupSettings (IPK.InstallMode mode = IPK.InstallMode.NONE) {
		conf = new Config ();
		current_mode = mode;

		unlock (); // Be unlocked by default
	}

	public IPK.InstallMode current_mode {
		get {
			return _current_mode;
		}
		internal set {
			if (can_change ())
				_current_mode = value;
		}
	}

	public bool locked {
		get { return _locked; }
	}

	public bool shared_mode {
		get { return current_mode.is_all_set (IPK.InstallMode.SHARED); }
	}

	public bool private_mode {
		get { return current_mode.is_all_set (IPK.InstallMode.PRIVATE); }
	}

	public bool test_mode {
		get { return current_mode.is_all_set (IPK.InstallMode.TEST); }
	}

	/**
	 * Lock the settings, so no changes can be made anymore
	 */
	public void lock () {
		_locked = true;
	}

	/**
	 * Allow changing the settings again
	 */
	public void unlock () {
		_locked = false;
	}

	/**
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

	private void touch_dir (string dirname, string warnmsg = "Error: %s") {
		if ((shared_mode && Utils.is_root ()) || (!shared_mode)) {
			Utils.create_dir_parents (dirname);
		}
	}

	internal string get_unique_install_tmp_dir () {
		if (uinsttmp != "")
			return uinsttmp;

		string template = Path.build_filename (conf.tmp_dir (), "testinst-XXXXXX", null);

		string res = DirUtils.mkdtemp (template);
		if (res == null) {
			critical ("Unable to create tmp-dir! Error: %s", strerror (errno));
			res = Path.build_filename (conf.tmp_dir (), "error-invalid", null);
		}
		uinsttmp = res;
		return uinsttmp;
	}

	internal void invalidate_tmp_dir () {
		debug (conf.tmp_dir ());
		debug (uinsttmp);

		// A little bit of additional security...
		if (uinsttmp.has_prefix (conf.tmp_dir ()))
			Utils.delete_dir_recursive (uinsttmp);
		uinsttmp = "";
	}

	public string database_file () {
		return Path.build_filename (appregister_dir (), conf.database_filename (), null);
	}

	public string appregister_dir () {
		string regdir;
		if (shared_mode) {
			regdir = conf.shared_db_dir ();
		} else {
			regdir = conf.user_db_dir ();
		}
		if (test_mode) {
			// Point to our temporary dir
			regdir = Path.build_filename (get_unique_install_tmp_dir (), "appreg", null);
		}

		touch_dir (regdir, "Unable to create application database directory: %s");

		return regdir;
	}

	public string appdata_dir () {
		string addir;

		if (shared_mode) {
			addir = Path.build_filename (conf.shared_install_root (), "apps", null);
		} else {
			addir = Path.build_filename (conf.user_install_root (), "apps", null);
		}
		if (test_mode) {
			// Point to our temporary dir
			addir = Path.build_filename (get_unique_install_tmp_dir (), "apps", null);
		}

		touch_dir (addir, "Unable to create application data directory: %s");

		return addir;
	}

	public string depdata_dir () {
		string depdir;

		if (shared_mode) {
			depdir = Path.build_filename (conf.shared_install_root (), "deps", null);
		} else {
			depdir = Path.build_filename (conf.user_install_root (), "deps", null);
		}
		if (test_mode) {
			// Point to our temporary dir
			depdir = Path.build_filename (get_unique_install_tmp_dir (), "deps", null);
		}

		touch_dir (depdir, "Unable to create application data directory: %s");

		return depdir;
	}

	public string icon_base_dir () {
		string icodir;

		if (shared_mode) {
			icodir = conf.shared_icon_dir ();
		} else {
			icodir = conf.user_icon_dir ();
		}
		if (test_mode) {
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
			if (shared_mode)
				icodir = conf.shared_pixmap_dir ();
			else
				icodir = conf.user_pixmap_dir ();
		} else {
			icodir = Path.build_filename (icon_base_dir (), size.to_string () + "x" + size.to_string (), "apps", null);
		}

		touch_dir (icodir, "Unable to create icon directory: %s");

		return icodir;
	}

	public string applications_dir () {
		string dskdir;

		if (shared_mode) {
			dskdir = conf.shared_applications_dir ();
		} else {
			dskdir = conf.user_applications_dir ();
		}
		if (test_mode) {
			// Point to our temporary dir
			dskdir = Path.build_filename (get_unique_install_tmp_dir (), "app-desktop", null);
		}

		touch_dir (dskdir, "Unable to create application data directory: %s");

		return dskdir;
	}
}

} // End of namespace: Listaller
