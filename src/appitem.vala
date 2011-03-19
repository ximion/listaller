/* appitem.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
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

using GLib;
using Listaller;

// Workaround for Vala bug #618931
private const string _PKG_VERSION4 = Config.VERSION;

namespace Listaller {

public enum AppOrigin {
	IPK,
	NATIVE,
	UNKNOWN,
	INVALID;

	public string to_string() {
		switch (this) {
			case IPK:
				return ("package_ipk");

			case NATIVE:
				return ("package_native");

			case UNKNOWN:
				return ("unknown");

			default:
				return ("<fatal:origin-not-found>");
		}
	}
}

public class AppItem : Object {
	private string _name;
	private string _version;
	private string _summary;
	private string _author;
	private string _pkgmaintainer;
	private string _categories;
	private string _desktop_file;
	private int64  _install_time;
	private string _dependencies;
	private AppOrigin _origin;
	private int _dbid;
	private string _app_id;

	public string name {
		get { return _name; }
		set { _name = value; }
	}

	public string version {
		get { return _version; }
		set { _version = value; }
	}

	public string summary {
		get { return _summary; }
		set { _summary = value; }
	}

	public string author {
		get { return _author; }
		set { _author = value; }
	}

	public string pkgmaintainer {
		get { return _pkgmaintainer; }
		set { _pkgmaintainer = value; }
	}

	public string categories {
		get { return _categories; }
		set { _categories = value; }
	}

	public string desktop_file {
		get { return _desktop_file; }
		set { _desktop_file = value; }
	}

	public int64 install_time {
		get { return _install_time; }
		set { _install_time = value; }
	}

	public AppOrigin origin {
		get { return _origin; }
		set { _origin = value; }
	}

	public string dependencies {
		get { return _dependencies; }
		set { _dependencies = value; }
	}

	public string appid {
		get { _app_id = generate_appid (); return _app_id; }
		set { _app_id = value; desktop_file = desktop_file_from_appid (); }
	}

	/*
	 * Id the application has in the database
	 */
	public int dbid {
		get { return _dbid; }
		set { _dbid = value; }
	}

	public AppItem (string aname, string aversion) {
		this.empty ();
		name = aname;
		version = aversion;

	}

	public AppItem.empty () {
		install_time = 0;
		categories = "all;";
		dbid = -1;
		origin = AppOrigin.UNKNOWN;
		_app_id = "";
		_desktop_file = "";
	}

	public AppItem.from_desktopfile (string dfile) {
		this.empty ();
		this.load_from_desktop_file (dfile);
	}

	public string to_string () {
		return "[ (" + name + ") "
			+ "(" + version +") "
			+ "! " + appid + " ::" + dbid.to_string () + " "
			+ "]";
	}

	public void fast_check () {
		// Fast sanity checks for AppItem
		assert (name != null);
		assert (version != null);
		assert (appid != "");
	}

	public void set_origin_from_string(string s) {
		switch (s) {
			case ("package_ipk"):
				origin = AppOrigin.IPK;
				break;

			case ("package_native"):
				origin = AppOrigin.NATIVE;
				break;

			case ("unknown"):
				origin = AppOrigin.UNKNOWN;
				break;

			default:
				origin = AppOrigin.INVALID;
				break;
		}
	}

	private string generate_appid () {
		string res = "";
		if (desktop_file.strip () == "") {
			message (_("Processing application without assigned .desktop file!"));
			// If no desktop file was found, use application name and version as ID
			res = name + ";" + version;
			res = res.down ();
			return res;
		} else {
			res = desktop_file;
			res = string_replace (res, "(/usr/|share/applications/|.desktop)", "");
			res = res + ";" + name + ";" + version;
			return res;
		}
	}

	private string desktop_file_from_appid () {
		if (appid == "")
			return "";
		// Trick to detect if the appid is based on a desktop-file
		if (count_str (appid, ";") <= 2)
			return "";
		return "<?>"; //TODO
	}

	private string get_desktop_file_string (KeyFile dfile, string keyword) {
		try {
			if (dfile.has_key ("Desktop File", keyword)) {
				return dfile.get_string ("Desktop Entry", keyword);
			} else {
				return "";
			}
		} catch (Error e) {
			error (_("Could not load desktop file values: %s").printf (e.message));
		}
	}

	public void load_from_desktop_file (string fname) {
		KeyFile dfile = new KeyFile ();
		try {
			dfile.load_from_file (fname, KeyFileFlags.NONE);
		} catch (Error e) {
			error (_("Could not open desktop file: %s").printf (e.message));
		}

		name = get_desktop_file_string (dfile, "Name");
		version = get_desktop_file_string (dfile, "X-AppVersion");
		summary = get_desktop_file_string (dfile, "Comment");
		author = get_desktop_file_string (dfile, "X-Author");
		categories = get_desktop_file_string (dfile, "Categories");

		desktop_file = fname;
	}

}

} // End of namespace
