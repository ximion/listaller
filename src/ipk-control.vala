/* ipk-control.vala - Describes data controlling the IPK setup process
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
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

// We need at least an IPK 2.0 package to process it
private static const string MINIMUM_IPK_SPEC_VERSION = "2.0";

public errordomain ControlDataError {
	NO_APPDATA,
	APPDATA_INVALID,
	DEPLIST_INVALID,
	INTERNAL,
	UNKNOWN;
}

/**
 * Generic IPK package control data
 */
public abstract class Control : Object {
	internal string appXML;
	internal MetaFile packSetting;
	protected AppItem? appItem;

	internal Control () {
		packSetting = new MetaFile ();
		appItem = null;
	}

	protected void load_appstream_data (string data) {
		appXML = data;
	}

	protected bool open_packsetting (string pksFName) {
		packSetting.clear ();
		if (!packSetting.open_file (pksFName))
			return false;
		return true;
	}

	public string get_ipk_version () {
		string s = packSetting.get_value ("Version", false);
		if (s == "")
			s = "0.4";

		return s;
	}

	public void set_ipk_version (string ipkV) {
		packSetting.add_value ("Version", ipkV);
	}

	/**
	 * Get architectures supported by this package
	 */
	public string get_architectures () {
		string s = packSetting.get_value ("Architectures", false);
		if (s == "")
			s = "all";

		return s;
	}

	/**
	 * Set architectures supported by this package
	 */
	public void set_architectures (string archs) {
		packSetting.add_value ("Architectures", archs);
	}

	/**
	 * Get replaced native components for the package.
	 *
	 * @deprecated
	 */
	public string get_replaces () {
		string s = packSetting.get_value ("Replaces", false);
		return s;
	}

	/**
	 * @deprecated
	 */
	public void set_replaces (string repList) {
		packSetting.add_value ("Replaces", repList);
	}

	/**
	 * TRUE if user should manually accept the supplied license/eula.
	 * FALSE if there is no need to do this.
	 */
	public bool user_accept_license {
		get {
			string s = packSetting.get_value ("UserAcceptLicense", false);
			if (s == "yes")
				return true;
			return false;
		}
		set {
			if (value)
				packSetting.add_value ("UserAcceptLicense", "yes");
			else {
				string s = packSetting.get_value ("UserAcceptLicense", false);
				if (s == "no")
					packSetting.add_value ("UserAcceptLicense", "yes");
			}
		}
	}

	public void set_install_modes (InstallMode modes) {
		string modesList = "";
		if (modes == InstallMode.NONE) {
			critical ("Tried to inject no install-modes in IPK control data. This should never happen and might be a bug in Listaller or your packaging. Defaulting to shared-only.");
			modes = InstallMode.SHARED;
		}

		if (modes.is_all_set (InstallMode.SHARED))
			modesList += "Shared";
		else
			modesList += "NoShared";
		modesList += ",";
		if (modes.is_all_set (InstallMode.PRIVATE))
			modesList += "Private";
		else
			modesList += "NoPrivate";
		modesList += ",";
		if (modes.is_all_set (InstallMode.TEST))
			modesList += "Test";
		else
			modesList += "NoTest";

		packSetting.add_value ("InstallModes", modesList);
	}

	public InstallMode get_install_modes () {
		// we default to shared mode only!
		InstallMode retFlags = InstallMode.SHARED;

		string modesStr = packSetting.get_value ("InstallModes", false);
		string[] modesStrV = modesStr.split (",");
		foreach (string s in modesStrV) {
			string mode_str = s.down ().strip ();

			if (mode_str == "shared")
				retFlags = retFlags.set (InstallMode.SHARED);
			else if (mode_str == "noshared")
				retFlags = retFlags.unset (InstallMode.SHARED);

			if (mode_str == "private")
				retFlags = retFlags.set (InstallMode.PRIVATE);
			else if (mode_str == "noprivate")
				retFlags = retFlags.unset (InstallMode.PRIVATE);

			if (mode_str == "test")
				retFlags = retFlags.set (InstallMode.TEST);
			else if (mode_str == "notest")
				retFlags = retFlags.unset (InstallMode.TEST);
		}

		if (retFlags == InstallMode.NONE) {
			warning ("No install mode was set or settings are invalid. Please fix your IPK package! Defaulting to shared-only.");
			retFlags = InstallMode.SHARED;
		}

		return retFlags;
	}

	public AppItem get_application () throws ControlDataError {
		Appstream.Metadata mdata;
		if (appItem != null)
			return appItem;
		AppItem? item;
		Appstream.Component cpt;

		// new AppStream metadata parser
		mdata = new Appstream.Metadata ();

		// load data separately to get correct error message
		// FIXME: Update AppItem to not load failing stuff in the constrcutor anymore
		try {
			cpt = mdata.parse_data (appXML);
		} catch (Error e) {
			throw new ControlDataError.APPDATA_INVALID (e.message);
		}
		item = new AppItem (cpt);
		// Meh...
		item.xmldata = appXML;

		if (item != null)
			appItem = item;
		else
			throw new ControlDataError.APPDATA_INVALID (_("Unable to load valid application from AppData. The XML file might be malformed or lacking data."));
		item.replaces = get_replaces ();

		return item;
	}

	public void set_dependencies (string dependencies_list, string arch) {
		string dep_field = "Requires";
		if ((!str_is_empty (arch)) && (arch != "all"))
			dep_field = "Requires[%s]".printf (arch);

		// Add the dependencies
		packSetting.update_value (dep_field, dependencies_list);
	}

	public string get_dependencies (string arch, bool arch_only = false) {
		string dep_field_arch = "";
		if ((!str_is_empty (arch)) && (arch != "all"))
			dep_field_arch = "Requires[%s]".printf (arch);
		string s = "";

		// check if we only want the arch-specific field
		if ((dep_field_arch == "") || (!arch_only))
			if (packSetting.has_field ("Requires", false))
				// fetch the arch-agnostic requires line
				s = packSetting.get_value ("Requires", false);
		if ((dep_field_arch != "") && (packSetting.has_field (dep_field_arch, false))) {
			if (!str_is_empty (s))
				s += ",";
			s = packSetting.get_value (dep_field_arch, false);
		}


		return s;
	}

	public bool set_license_text_from_file (string fname) {
		if (!FileUtils.test (fname, FileTest.EXISTS))
			return false;

		string? license_text = load_file_to_string (fname);
		if (license_text == null)
			return false;
		try {
			get_application ();
		} catch (Error e) {
			return false;
		}

		appItem.set_license_text (license_text);
		return true;
	}

	public void set_license_text (string txt) {
		assert (appItem != null);
		appItem.set_license_text (txt);
	}
}

/**
 * Control metadata of an IPK package
 */
public class PackControl : Control {
	private string ipkVersion;
	private string asData;

	public PackControl () {
		ipkVersion = "2.0";
	}

	public bool open_control (string fPackSetting, string fAppStream) {
		bool ret;
		ret = open_packsetting (fPackSetting);
		if (!ret) {
			error ("Unable to load pksetting... Temporary file was: %s".printf (fPackSetting));
			return false;
		}

		asData = load_file_to_string (fAppStream);
		if (str_is_empty (asData))
			return false;
		load_appstream_data (asData);

		ipkVersion = this.get_ipk_version ();

		return cache_appitem ();
	}

	private bool cache_appitem () {
		// Cache application entry
		this.get_application ();
		if ((appItem == null) || (this.appItem.idname == "")) {
			error ("AppItem does not have a valid id-name!");
			appItem = null;
			return false;
		}

		return true;
	}

	public bool create_new (string? newAppStreamData, string ipkV) {
		bool ret;
		if ((newAppStreamData == null) || (newAppStreamData == "")) {
			critical ("Error while processing AppStream data: Data was NULL!");
			return false;
		}

		asData = newAppStreamData;
		load_appstream_data (asData);

		ret = cache_appitem ();

		if (ret) {
			ipkVersion = ipkV;
			set_ipk_version (ipkVersion);
		}

		return ret;
	}

	private bool has_license_text () {
		assert (appItem != null);
		return (appItem.license.text != "") && (appItem.license.name != appItem.license.text);
	}

	public bool is_delta_pkg () {
		if (packSetting.get_value ("Type").down () == "delta")
			return true;

		return false;
	}

	public bool save_to_dir (string dirPath) {
		bool ret;
		assert (appItem != null);

		ret = save_string_to_file (Path.build_filename (dirPath, this.appItem.idname + ".appdata.xml", null), asData);
		if (!ret)
			return false;

		if (has_license_text ())
			ret = save_string_to_file (Path.build_filename (dirPath, "license.txt", null), appItem.license.text);

		packSetting.save_to_file (Path.build_filename (dirPath, "pksetting", null));

		return ret;
	}

	[CCode (array_length = false, array_null_terminated = true)]
	public string[] get_files () {
		string[] res;
		assert (appItem != null);

		res = { "pksetting" };
		res += this.appItem.idname + ".appdata.xml";
		// Add license text
		if (has_license_text ())
			res += "license.txt";
		res += null;

		return res;
	}

	public void update_installmode_data () {
		// Required to update the install-mode strings and to run tests if the user-data is correct
		// when building a new IPK package.
		InstallMode modes = get_install_modes ();
		set_install_modes (modes);
	}

#if 0

	public override string get_app_license () {
		string license = base.get_app_license ();
		switch (license) {
			case "GPLv3+": license = load_license ("gpl-3+");
					break;
			case "GPLv3": license = load_license ("gpl-3");
					break;
			case "GPLv2+": license = load_license ("gpl-2+");
					break;
			case "GPLv2": license = load_license ("gpl-2");
					break;
			case "LGPLv2": license = load_license ("lgpl-2");
					break;
			// To be continued...
			default: break;

		}
		return license;
	}
#endif

}

/**
 * Data stored in an IPK-source control directory.
 *
 * This class is used by tools like lipkgen to extract data about a
 * to-be-created IPK package from an IPK control directory.
 */
public class ControlDir : Control {
	private string ctrlDir;
	private string asdFile; // AppStream data file

	public ControlDir () {
		ctrlDir = "";
		asdFile = "";
	}

	private string find_appstream_data (string dir) {
		string asdFile = "";
		try {
			var directory = File.new_for_path (dir);
			var enumerator = directory.enumerate_children (FileAttribute.STANDARD_NAME, 0);

			FileInfo file_info;
			while ((file_info = enumerator.next_file ()) != null) {
				string path = Path.build_filename (dir, file_info.get_name (), null);
				if (file_info.get_is_hidden ())
					continue;

				if (path.down ().has_suffix (".appdata.xml"))
					asdFile = path;
			}

		} catch (GLib.Error e) {
			stderr.printf (_("Error: %s\n"), e.message);
			return "";
		}

		return asdFile;
	}

	public bool open_dir (string dir) throws ControlDataError {
		if (ctrlDir != "")
			return false;

		string pksFName = Path.build_filename (dir, "pkoptions", null);
		if (!FileUtils.test (pksFName, FileTest.EXISTS))
			return false;
		if (!open_packsetting (pksFName))
			return false;


		asdFile = find_appstream_data (dir);
		if (asdFile == "")
			throw new ControlDataError.NO_APPDATA (_("No valid AppStream application data found in directory %s - Can't open control files.").printf (dir));

		string asData = load_file_to_string (asdFile);
		load_appstream_data (asData);

		ctrlDir = dir;

		// ensure that we have loaded a valid application, otherwise we get in trouble later
		try {
			get_application ();
		} catch (ControlDataError e) {
			throw e;
		};

		// Open license text, if there is any...
		string licenseTxtFName = Path.build_filename (ctrlDir, "license.txt", null);
		if (FileUtils.test (licenseTxtFName, FileTest.EXISTS)) {
			this.set_license_text_from_file (licenseTxtFName);
		}

		return true;
	}

	public string get_files_rootdir () {
		string s = packSetting.get_value ("FilesRoot", false);
		if (s == "")
			s = Environment.get_current_dir ();

		return s;
	}

	public bool auto_dependency_search () {
		string s = packSetting.get_value ("AutoFindDeps", false);
		if (s.down () == "true")
			return true;
		return false;
	}

	public string? get_appstream_data () {
		if (asdFile == "")
			return null;

		string xmldata = load_file_to_string (asdFile);

		return xmldata;
	}

	public bool save_control () {
		bool ret = true;
		// FIXME
		//ret = depData.save_to_file (Path.build_filename (ctrlDir, "dependencies.list", null));
		return ret;
	}

}

} // End of namespace: Listaller.IPK
