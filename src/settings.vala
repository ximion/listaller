/* settings.vala
 *
 * Copyright (C) 2009-2011  Matthias Klumpp
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
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using Config;
using GLib;

// Workaround for Vala bug #618931
private const string _PKG_VERSION1 = Config.VERSION;

public class LiSettings : Object {
	private bool _sumode;
	const string suconfdir = "/etc/lipa";
	const string suappdatadir = "/opt/apps";
	const string sudesktopdir = "/usr/share/applications";
	private bool _testmode;
	private bool _locked;

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

	public LiSettings (bool root = false) {
		sumode = root;
		testmode = false;
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

	public string appregister_dir () {
		string regdir;
		if (sumode) {
			regdir = suconfdir;
		} else {
			regdir = Path.build_filename (Environment.get_user_config_dir (), "software", null);
		}

		File d = File.new_for_path (regdir);
		try {
			if (!d.query_exists ()) {
				d.make_directory_with_parents ();
			}
		} catch (Error e) {
			warning (_("Unable to create application database directory: %s"), e.message);
		}

		return regdir;
	}

	public string appdata_dir () {
		string addir;

		if (sumode) {
			addir = suappdatadir;
		} else {
			addir = Path.build_filename (Environment.get_home_dir (), ".appdata", null);
		}

		if ((sumode && is_root ()) || (!sumode)) {
			File d = File.new_for_path (addir);
			try {
				if (!d.query_exists ()) {
					d.make_directory_with_parents ();
				}
			} catch (Error e) {
				warning (_("Unable to create application data directory: %s"), e.message);
			}
		}

		return addir;
	}

	public string desktop_dir () {
		string dskdir;

		if (sumode) {
			dskdir = sudesktopdir;
		} else {
			dskdir = Path.build_filename (Environment.get_home_dir (), ".local", "share", "applications", null);
		}

		if ((sumode && is_root ()) || (!sumode)) {
			File d = File.new_for_path (dskdir);
			try {
				if (!d.query_exists ()) {
					d.make_directory_with_parents ();
				}
			} catch (Error e) {
				warning (_("Unable to create application data directory: %s"), e.message);
			}
		}

		return dskdir;
	}

	public string tmp_dir () {
		string ret;
		ret = Environment.get_tmp_dir ();
		return ret;
	}

	public string get_unique_tmp_dir () {
		string template = Path.build_filename (tmp_dir (), "listaller-XXXXXX", null);

		string res = DirUtils.mkdtemp (template);
		if (res == null)
			res = tmp_dir ();
		return res;
	}
}

public void li_enable_translation ()
{
	// Initialize localisation
	Intl.bindtextdomain (Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
	Intl.bind_textdomain_codeset (Config.GETTEXT_PACKAGE, "UTF-8");
	Intl.textdomain (Config.GETTEXT_PACKAGE);
}
