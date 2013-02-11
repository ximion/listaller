/* application.vala
 *
 * Copyright (C) 2010-2013 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller {

/**
 * Status of an application
 *
 * Indicates if an application is installed (and in which mode
 * it was installed), or if it is in any other, different state.
 */
[Flags]
public enum AppState {
	UNKNOWN,
	INSTALLED_SHARED,
	INSTALLED_PRIVATE,
	AVAILABLE;

	public string to_string () {
		switch (this) {
			case INSTALLED_SHARED:
				return ("installed (shared)");

			case INSTALLED_PRIVATE:
				return ("installed (private)");

			case AVAILABLE:
				return ("available");

			case UNKNOWN:
				return ("unknown");

			default:
				return ("unknown-status");
		}
	}

	public inline bool is_all_set (AppState flags) {
		return (this & flags) == flags;
	}

	public inline bool is_any_set (AppState flags) {
		return (this & flags) != 0;
	}

        public inline AppState set (AppState mode) {
		return (this | mode);
	}

	public inline AppState unset (AppState mode) {
		return (this & ~mode);
	}
}

/**
 * License name and text pair
 *
 * Describes a software license
 */
public struct AppLicense {
	public string name;
	public string text;
}

/**
 * Application entry
 *
 * Objects of this class contain information about
 * an application
 */
public class AppItem : Object {
	private string _idname;
	private string _version;
	private string _appname;
	private string _summary;
	private string _description;
	private string _author;
	private string _pkgmaintainer;
	private AppLicense _license;
	private string _categories;
	private string _desktop_file;
	private string _desktop_file_prefix;
	private string _website;
	private string _icon_name;
	private int64  _install_time;
	private string _dependencies;
	private AppState _state;
	private string _origin;
	private string _app_id;
	private SetupSettings setup_settings;

	/**
	 * Application identifier
	 * */
	public string idname {
		get {
			if (_idname.strip () == "")
				_idname = string_replace (_appname.down (), "( )", "_");
			return _idname;
		}
		internal set {
			_idname = value;
			if (_appname.strip () == "")
				_appname = _idname;
		}
	}

	public string full_name {
		get { return _appname; }
		set { _appname = value; }
	}

	public string version {
		get { return _version; }
		set { _version = value; }
	}

	public string summary {
		get { return _summary; }
		set { _summary = value; }
	}

	public string description {
		get { return _description; }
		set { _description = value; }
	}

	public string author {
		get { return _author; }
		set { _author = value; }
	}

	public string publisher {
		get { return _pkgmaintainer; }
		set { _pkgmaintainer = value; }
	}

	public AppLicense license {
		get { return _license; }
		set { _license = value; }
	}

	public string categories {
		get { return _categories; }
		set { _categories = value; }
	}

	public int size_installed { get; set; } // Installed size of this application in KiB

	public string desktop_file {
		get {
			if (_desktop_file.strip () == "")
				return "";
			string dfile = _desktop_file;
			// Check if $APP prefix needs to be added
			if ( (!dfile.has_prefix ("%")) &&
				(!dfile.has_prefix ("/")) &&
				(!dfile.has_prefix ("~")) ) {
				// If no exact path has been specified, we assume $APP
				dfile = Path.build_filename ("%APP%", dfile, null);
			}
			_desktop_file_prefix = fold_user_dir (dfile);
			return _desktop_file_prefix;
		}
		set {
			_desktop_file = fold_user_dir (value);

			// Only do autodetection if we not already have a shared application
			if (state != AppState.INSTALLED_SHARED) {
				// Desktop file in / ==> shared application
				if (_desktop_file.has_prefix ("/"))
					state = AppState.INSTALLED_SHARED;
				// Desktop-file in %APP%/user's home ==> application not shared
				if (_desktop_file.has_prefix ("~"))
					state = AppState.INSTALLED_PRIVATE;
			}
		}
	}

	public AppState state {
		get { return _state; }
		set {
			_state = value;
			if (_state == AppState.INSTALLED_SHARED)
				setup_settings.current_mode = IPK.InstallMode.SHARED;
			else
				setup_settings.current_mode = IPK.InstallMode.PRIVATE; //! FIXME
		}
	}

	public string icon_name {
		get { return _icon_name; }
		set { _icon_name = value; }
	}

	public string website {
		get { return _website; }
		set { _website = value; }
	}

	public int64 install_time {
		get { return _install_time; }
		set { _install_time = value; }
	}

	public string replaces { get; set; }

	public string origin {
		get { return _origin; }
		set { _origin = value; }
	}

	public string dependencies {
		get { return _dependencies; }
		set { _dependencies = value; }
	}

	public string appid {
		get { _app_id = generate_appid (); return _app_id; }
	}

	public AppItem.blank () {
		_idname = "";
		_appname = "";
		_idname = "";
		_summary = "";
		_description = "";
		install_time = 0;
		categories = "all;";
		origin = "unknown";
		_app_id = "";
		_desktop_file = "";
		_website = "";
		_icon_name = "";
		setup_settings = new SetupSettings ();
		_state = AppState.UNKNOWN;
		_license = AppLicense () {
			name = "",
			text = ""
		};
	}

	public void set_origin_local () {
		origin = "local";
	}

	public AppItem (string afullname, string aversion, string desktop_filename = "") {
		this.blank ();
		full_name = afullname;
		version = aversion;
		desktop_file = desktop_filename;
	}

	public AppItem.from_id (string application_id) {
		this.blank ();
		_app_id = application_id;
		update_with_appid ();
	}

	public AppItem.from_desktopfile (string desktop_filename) {
		this.blank ();
		desktop_file = desktop_filename;
		this.update_with_desktop_file ();
	}

	public string to_string () {
		string str_ownership;
		if (state != AppState.INSTALLED_SHARED)
			str_ownership = _("personal");
		else
			str_ownership = _("shared");

		string app_state_str = "";
		if (state == AppState.AVAILABLE)
			app_state_str = "[%s]".printf (_("AVAILABLE"));
		else
			app_state_str = "[%s|%s]".printf (_("INSTALLED"), str_ownership);

		return "%s %s (%s) :: %s -- %s".printf (app_state_str, idname, version, full_name, summary);
	}

	private void set_license_info (string lName, string lText) {
		_license = AppLicense () {
			name = lName,
			text = lText
		};
	}

	public void set_license_name (string lName) {
		set_license_info (lName, _license.text);
	}

	public void set_license_text (string lText) {
		set_license_info (_license.name, lText);
	}

	public void set_license_from_doap_name (string? name) {
		if ((name == null) || (name.strip () == ""))
			return;
		string licenseName, licenseText = "";
		string lID = name.strip ();
		// First handle the DOAP names
		switch (lID.down ()) {
			case ("http://usefulinc.com/doap/licenses/gpl"):
				licenseName = "GPL";
				break;
			case ("http://usefulinc.com/doap/licenses/lgpl"):
				licenseName = "LGPL";
				break;
			case ("http://usefulinc.com/doap/licenses/mpl"):
				licenseName = "MPL";
				break;

			default:
				licenseName = lID;
				break;
		}
		// Now check if we have a copy of this license stored somewhere (works for Debian at time)
		string licenseDir = "/usr/share/common-licenses";
		if (FileUtils.test (Path.build_filename (licenseDir, licenseName, null), FileTest.EXISTS)) {
			licenseText = load_file_to_string (Path.build_filename (licenseDir, licenseName, null));
		}
		if (licenseText != "") {
			set_license_info (licenseName, licenseText);
			return;
		}

		// TODO: Parse other licenses and download them from the net or fetch them from local dir, if necessary

		if (licenseText == "")
			licenseText = licenseName;

		set_license_info (licenseName, licenseText);
	}

	public void fast_check () {
		// Fast sanity checks for AppItem
		assert (idname != null);
		assert (full_name != null);
		assert (version != null);
		assert (appid != "");
	}

	/** Build a Listaller application-id
	 *
	 * An application ID has the following form:
	 * idname;version;desktop_file;origin
	 * idname usually is the application's .desktop file name
	 * version is the application's version
	 * arch the architecture(s) the app was build for
	 * desktop_file is the application's desktop file
	 * origin is the origin of this app, e.g. a repository-id, "unknown" or "local"
	 *
	 * @return a valid application-id
	 */
	private string generate_appid () {
		string res = "";
		if (validate_appid (_app_id))
			return _app_id;

		if (desktop_file.strip () == "") {
			debug (_("We don't know a desktop-file for application '%s'!").printf (full_name));
			// If no desktop file was found, use application name and version as ID
			res = idname + ";" + version;
			res = res + ";" + ";" + origin;
		} else {
			res = idname + ";" + version + ";" + desktop_file + ";";
			res += origin;
		}
		return res;
	}

	public static bool validate_appid (string application_id) {
		string inv_str = _("Application ID %s is invalid!").printf (application_id);
		// Check if application-id is valid
		if (application_id == "")
			return false;
		if (count_str (application_id, ";") != 3) {
			warning (inv_str);
			return false;
		}
		string[] blocks = application_id.split (";");
		if ((blocks[0] == null) || (blocks[1] == null)) {
			warning (inv_str);
			return false;
		}
		return true;
	}

	public void update_with_appid (bool fast = false) {
		if (!validate_appid (appid))
			return;

		string[] blocks = appid.split (";");
		idname = blocks[0];
		version = blocks[1];
		string dfile = blocks[2];
		// Set application origin
		string orig = blocks[3];
		origin = orig;

		// Rebuild the desktop file
		if (dfile != null) {
			desktop_file = dfile;
			if (!fast)
				update_with_desktop_file ();
		}

	}

	private string get_desktop_file_string (KeyFile dfile, string keyword) {
		try {
			if (dfile.has_key ("Desktop Entry", keyword)) {
				return dfile.get_string ("Desktop Entry", keyword);
			} else {
				return "";
			}
		} catch (Error e) {
			Report.log_warning (_("Could not load desktop file values: %s").printf (e.message));
			return "";
		}
	}

	private string get_desktop_filename_expanded () {
		/* NOTE: Hopefully nobody will ever try to store a .desktop-file in %INST%, because
		 * this might cause problems with AppItem's which don't have the correct idname specified.
		 * (Maybe limit this to %APP% only?)
		 */
		// Resolve variables in desktop_file path
		VarSolver vs = new VarSolver (idname);
		string fname = expand_user_dir (desktop_file);
		fname = vs.substitute_vars_auto (fname, setup_settings);

		// Check if file exists
		if (!FileUtils.test (fname, FileTest.EXISTS))
			return "";

		return fname;
	}

	public void update_with_desktop_file () {
		if (desktop_file == "")
			return;
		// Set idname if no idname was specified
		if (_idname == "") {
			idname = string_replace (Path.get_basename (desktop_file), "(.desktop)", "");
			debug ("Set app-idname from desktop filename: %s".printf (idname));
		}

		// Get complete .desktop-file path
		string fname = get_desktop_filename_expanded ();
		if (fname == "")
			return;

		// Load new values from desktop file
		KeyFile dfile = new KeyFile ();
		try {
			dfile.load_from_file (fname, KeyFileFlags.NONE);
		} catch (Error e) {
			warning (_("Could not open desktop file: %s").printf (e.message));
		}

		full_name = get_desktop_file_string (dfile, "Name");
		if (idname == "")
			idname = full_name;
		version = get_desktop_file_string (dfile, "X-AppVersion");
		summary = get_desktop_file_string (dfile, "Comment");
		icon_name = get_desktop_file_string (dfile, "Icon");
		author = get_desktop_file_string (dfile, "X-Author");
		categories = get_desktop_file_string (dfile, "Categories");
		publisher = get_desktop_file_string (dfile, "X-Publisher");
	}

	public string get_raw_cmd (bool subst_cmd = false) {
		if (desktop_file == "")
			return "";

		string fname = get_desktop_filename_expanded ();
		if (fname == "")
			return "";

		// Load new values from desktop file
		KeyFile dfile = new KeyFile ();
		try {
			dfile.load_from_file (fname, KeyFileFlags.NONE);
		} catch (Error e) {
			warning (_("Could not open desktop file: %s").printf (e.message));
		}

		string cmd = get_desktop_file_string (dfile, "Exec");
		// To get the raw command, we remove the "runapp" call
		if (cmd.has_prefix ("runapp ")) {
			cmd = cmd.substring (7);
			// remove quotation marks, if there are any
			if ((cmd.has_prefix ("\"")) && (cmd.has_suffix ("\""))) {
				cmd = cmd.substring (1, cmd.length -2);
			}
		}
		if (subst_cmd) {
			VarSolver vs = new VarSolver (idname);
			cmd = vs.substitute_vars_auto (cmd, setup_settings);
		}

		return cmd;
	}

	/*  1 == bversion is higher
	 *  0 == equal
	 * -1 == this version is higher
	 */
	public int compare_version_with (string bversion) {
		return compare_versions (bversion, _version);
	}

}

} // End of namespace
