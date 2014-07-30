/* application.vala
 *
 * Copyright (C) 2010-2014 Matthias Klumpp <matthias@tenstral.net>
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
 * an application (installed or not installed) and associated
 * Listaller state data.
 */
public class AppItem : Object {
	private string _idname;
	private string _version;
	private AppLicense _license;
	private int64  _install_time;
	private string _dependencies;
	private AppState _state;
	private string _origin;
	private string _app_id;
	private SetupSettings setup_settings;
	private string _metadata_file;
	public string xmldata { private get; internal set; }
	internal int dbid { get; set; }

	/**
	 * Application identifier
	 * */
	public string idname {
		get {
			if (_idname.strip () == "")
				_idname = string_replace (_metainfo.name.down (), "( )", "_");
			return _idname;
		}
		internal set {
			_idname = value;
			if (_metainfo.name.strip () == "")
			_metainfo.name = _idname;
		}
	}

	public string version {
		get { return _version; }
		set { _version = value; }
	}

	public AppLicense license {
		get { return _license; }
		set { _license = value; }
	}

	private Appstream.Component _metainfo;
	public Appstream.Component metainfo {
		get {
			if (_metainfo == null)
				error ("Fatal: AppItem does not contain AppStream Component.metainfo.mation object!");
			return _metainfo;
		}
		set { _metainfo = value; }
	}

	public int size_installed { get; set; } // Installed size of this application in KiB

	private string _metadata_fname_folded;
	public string metadata_file {
		get {
			if (_metadata_file.strip () == "")
				return "";
			string dfile = _metadata_file;
			// Check if full path needs to be added
			if ( (!dfile.has_prefix ("%")) &&
				(!dfile.has_prefix ("/")) &&
				(!dfile.has_prefix ("~")) ) {
				// If no exact path has been specified, we assume /usr/share/appdata
				dfile = Path.build_filename ("/usr/share/appdata", dfile, null);
			}
			_metadata_fname_folded = fold_user_dir (dfile);
			return _metadata_fname_folded;
		}
		set {
			_metadata_file = fold_user_dir (value);

			// Only do autodetection if we not already have a shared application
			if (state != AppState.INSTALLED_SHARED) {
				// Desktop file in / ==> shared application
				if (_metadata_file.has_prefix ("/"))
					state = AppState.INSTALLED_SHARED;
				// Desktop-file in %APP%/user's home ==> application not shared
				if (_metadata_file.has_prefix ("~"))
					state = AppState.INSTALLED_PRIVATE;
			}
		}
	}

	public string desktop_file { get; set; }

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

	public int64 install_time {
		get { return _install_time; }
		set { _install_time = value; }
	}

	public string replaces { get; set; }

	public string origin {
		get { return _origin; }
		set { _origin = value; }
	}

	public string dependencies_str {
		get { return _dependencies; }
		set { _dependencies = value; }
	}

	public string appid {
		get { _app_id = generate_appid (); return _app_id; }
	}

	public string author { get; set; }

	public AppItem () {
		dbid = -1;
		_idname = "";
		install_time = 0;
		origin = "unknown";
		_app_id = "";
		_version = "";
		desktop_file = "";
		_metadata_file = "";
		setup_settings = new SetupSettings ();
		_state = AppState.UNKNOWN;
		_license = AppLicense () {
			name = "",
			text = ""
		};
		metainfo = new Appstream.Component ();
	}

	public AppItem.from_id (string application_id) {
		this ();
		_app_id = application_id;
		update_with_appid ();
	}

	public bool load_xml_file (string asdata_fname) throws GLib.Error {
		var mdata = new Appstream.Metadata ();
		var f = File.new_for_path (asdata_fname);
		Appstream.Component cpt;
		try {
			cpt = mdata.parse_file (f);
			xmldata = load_file_to_string (asdata_fname);
		} catch (Error e) {
			throw e;
		}
		_metadata_file = asdata_fname;
		metainfo = cpt;

		return true;
	}

	public bool load_xml_data (string xmld) throws GLib.Error {
		var mdata = new Appstream.Metadata ();
		Appstream.Component cpt;
		try {
			cpt = mdata.parse_data (xmld);
		} catch (Error e) {
			throw e;
		}
		_metadata_file = "";
		xmldata = xmld;
		metainfo = cpt;

		return true;
	}

	public void set_origin_local () {
		origin = "local";
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

		return "%s %s (%s) :: %s".printf (app_state_str, idname, version, metainfo.to_string ());
	}

	public void fast_check () {
		// Fast sanity checks for AppItem
		assert (metainfo != null);
		assert (metainfo.name != null);
		assert (metainfo.id != null);
		assert (version != null);
		assert (appid != "");
	}

	private void set_license_info (string lName, string lText) {
		_license = AppLicense () {
			name = lName,
			text = lText
		};
	}

	public void set_license_name (string? name) {
		if ((name == null) || (name.strip () == ""))
			return;
		string licenseName, licenseText = "";
		licenseName = name.strip ();
		// Check if we have a copy of this license stored somewhere (works for Debian at time)
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

	public void set_license_text (string lText) {
		set_license_info (_license.name, lText);
	}

	/** Build a Listaller application-id
	 *
	 * An application ID has the following form:
	 * idname;version;metadata-file;origin
	 * idname usually is the application's .desktop file name
	 * version is the application's version
	 * arch the architecture(s) the app was build for
	 * metadata-file is the application's AppStream metadata file
	 * origin is the origin of this app, e.g. a repository-id, "unknown" or "local"
	 *
	 * @return a valid application-id
	 */
	private string generate_appid () {
		string res = "";
		if (validate_appid (_app_id))
			return _app_id;

		if (metadata_file.strip () == "") {
			debug (_("We don't know a metadata-file for application '%s'!").printf (metainfo.name));
			// If no metadata file was found, we can only use application name and version as ID
			res = "%s;%s;;%s".printf (idname, version, origin);
		} else {
			res = "%s;%s;%s;%s".printf (idname, version, metadata_file, origin);
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
		string asfile = blocks[2];
		// Set application origin
		string orig = blocks[3];
		origin = orig;

		// Rebuild the Appstream metadata file
		if (asfile != null) {
			metadata_file = asfile;
			if (!fast)
				update_with_metadata_file ();
		}

	}

	/**
	* Generate a PackageKit package-id for this application
	*/
	public string build_pk_package_id () {
		string data;
		string package_id;
		if (Utils.str_is_empty (metadata_file))
			data = "local:listaller";
		else
			data = "local:listaller#%s".printf (metadata_file);

		// FIXME: Handle architecture correctly
		package_id = PackageKit.Package.id_build (idname, version, system_machine_generic (), data);

		return package_id;
	}

	private string get_desktop_file_string (KeyFile dfile, string keyword) {
		try {
			if (dfile.has_key ("Desktop Entry", keyword)) {
				return dfile.get_string ("Desktop Entry", keyword);
			} else {
				return "";
			}
		} catch (Error e) {
			message (_("Could not load desktop file values: %s").printf (e.message));
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

	public void update_with_metadata_file () {
		if (metadata_file == "")
			return;
		// Set idname if no idname was specified
		if (_idname == "") {
			idname = string_replace (Path.get_basename (metadata_file), "(.appdata.xml)", "");
			debug ("Set app-idname from AppStream metafile: %s".printf (idname));
		}

		// Get complete metadata-file path
		string fname = expand_user_dir (metadata_file);
		if (fname == "")
			return;

		string? xml;
		try {
			xml = load_file_to_string (_metadata_file);
		} catch (Error e) {
			warning (_("Could not open AppStream metadata file: %s").printf (e.message));
			return;
		}

		if (xml == null) {
			warning (_("Could not open AppStream metadata file: %s").printf (_("File does not exist.")));
			return;
		}
		xmldata = xml;

		// fetch AppStream component from data
		var mdata = new Appstream.Metadata ();
		var cpt = mdata.parse_data (xmldata);

		// set version, after retting the latest release
		Appstream.Release? release = null;
		GenericArray<Appstream.Release> releases = cpt.get_releases ();
		uint64 timestamp = 0;
		for(uint i = 0; i < releases.length; i++) {
			Appstream.Release r = releases.get (i);
			if (r.get_timestamp () > timestamp) {
				release = r;
				timestamp = r.get_timestamp ();
			}
		}
		if (release != null) {
			version = release.get_version ();
		}

		// set desktop-file
		if (cpt.id.has_suffix (".desktop")) {
			desktop_file = "%APP%/" + cpt.id;
		} else {
			warning (_("AppStream metadata for %s did not have desktop-file as ID!").printf (cpt.name));
		}

		// set new component
		metainfo = cpt;
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

	public string get_metadata_xml () {
		if (Utils.str_is_empty (xmldata)) {
			warning ("Component '%s' does not have raw XML metadata.", idname);
			return "";
		}

		return xmldata;
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
