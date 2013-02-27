/* ipk-control.vala - Describes data controlling the IPK setup process
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
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

// We need at least an IPK 1.1 package to process it
private static const string MINIMUM_IPK_SPEC_VERSION = "1.1";

public errordomain ControlDataError {
	NO_DOAP,
	DOAP_INVALID,
	DEPLIST_INVALID,
	INTERNAL,
	UNKNOWN;
}

/**
 * Generic IPK package control data
 */
public abstract class Control : Object {
	internal DoapData doap;
	internal MetaFile depData;
	internal MetaFile packSetting;
	protected AppItem? appItem;

	internal Control () {
		doap = new DoapData ();
		depData = new MetaFile ();
		packSetting = new MetaFile ();
		appItem = null;
	}

	protected bool open_doap (string data) {
		doap.add_data (data);
		return true;
	}

	protected bool open_doap_file (string fname) {
		if (doap.get_doap_url () != "")
			return false;
		doap.add_file (fname);
		return true;
	}

	protected bool open_depinfo (string depMetaFile) {
		return depData.open_file (depMetaFile);
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
			s = "1.1";

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
	 */
	public string get_replaces () {
		string s = packSetting.get_value ("Replaces", false);
		return s;
	}

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
		modesList += "\n";
		if (modes.is_all_set (InstallMode.PRIVATE))
			modesList += "Private";
		else
			modesList += "NoPrivate";
		modesList += "\n";
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
		string[] modesStrV = modesStr.split ("\n");
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

	public AppItem get_application () {
		if (appItem != null)
			return appItem;
		AppItem? item = doap.get_project ();
		if (item != null)
			appItem = item;
		item.replaces = get_replaces ();

		return item;
	}

	public void set_dependencies (ArrayList<Dependency> list) {
		// Add the dependencies
		foreach (Dependency dep in list) {
			depData.reset ();

			depData.add_value ("Name", dep.full_name);
			depData.add_value ("ID", dep.idname);
			// If we have a feed-url for this, add it
			if (dep.feed_url != "")
				depData.add_value ("Feed", dep.feed_url);
			depData.add_value ("Libraries", dep.get_components_by_type_as_str (Dep.ComponentType.SHARED_LIB));
			depData.add_value ("Binaries", dep.get_components_by_type_as_str (Dep.ComponentType.BINARY));
			depData.add_value ("Python", dep.get_components_by_type_as_str (Dep.ComponentType.PYTHON));
			depData.add_value ("Python2", dep.get_components_by_type_as_str (Dep.ComponentType.PYTHON_2));
			depData.add_value ("Files", dep.get_components_by_type_as_str (Dep.ComponentType.FILE));
		}
	}

	public ArrayList<Dependency> get_dependencies () {
		ArrayList<Dependency> depList = new ArrayList<Dependency> ();
		depData.reset ();

		while (depData.open_block_by_field ("id")) {
			Dependency dep = new Dependency (depData.get_value ("id"));
			dep.full_name = depData.get_value ("name");

			string s = depData.get_value ("feed");
			if (s.strip () != "")
				dep.feed_url = s;
			dep.add_component_list (Dep.ComponentType.SHARED_LIB, depData.get_value ("libraries"));
			dep.add_component_list (Dep.ComponentType.BINARY, depData.get_value ("binaries"));
			dep.add_component_list (Dep.ComponentType.PYTHON, depData.get_value ("python"));
			dep.add_component_list (Dep.ComponentType.PYTHON_2, depData.get_value ("python2"));
			dep.add_component_list (Dep.ComponentType.FILE, depData.get_value ("files"));
			depList.add (dep);
		}
		return depList;
	}

	public bool set_license_text_from_file (string fname) {
		if (!FileUtils.test (fname, FileTest.EXISTS))
			return false;

		string? license_text = load_file_to_string (fname);
		if (license_text == null)
			return false;
		get_application ();
		appItem.set_license_text (license_text);
		return true;
	}

	public void set_license_text (string txt) {
		appItem.set_license_text (txt);
	}
}

/**
 * Control metadata of an IPK package
 */
public class PackControl : Control {
	private string ipkVersion;
	private string doapData;

	public PackControl () {
		ipkVersion = "1.1";
	}

	public bool open_control (string fPackSetting, string fDoap, string fDeps) {
		bool ret;
		ret = open_packsetting (fPackSetting);
		if (!ret) {
			error ("Unable to load pksetting... Temporary file was: %s".printf (fPackSetting));
			return false;
		}

		doapData = load_file_to_string (fDoap);
		ret = this.open_doap (doapData);
		if (!ret)
			return false;
		ret = this.open_depinfo (fDeps);
		if (!ret)
			return false;

		ipkVersion = this.get_ipk_version ();

		return cache_appitem ();
	}

	private bool cache_appitem () {
		// Cache application entry
		this.get_application ();
		if (this.appItem.idname == "") {
			error ("Listaller AppItem did not have a valid id-name!");
			appItem = null;
			return false;
		}
		return true;
	}

	public bool create_new (string? newDoapData, string ipkV) {
		if ((newDoapData == null) || (newDoapData == "")) {
			critical ("Error while processing DOAP data: Data was NULL!");
			return false;
		}

		doapData = newDoapData;
		doap = new DoapData ();
		depData.clear ();
		bool ret = this.open_doap (doapData);
		if (ret)
			ret = cache_appitem ();
		if (ret) {
			ipkVersion = ipkV;
			set_ipk_version (ipkVersion);
		}

		return ret;
	}

	private bool has_license_text () {
		return (appItem.license.text != "") && (appItem.license.name != appItem.license.text);
	}

	public bool is_delta_pkg () {
		if (packSetting.get_value ("Type").down () == "delta")
			return true;

		return false;
	}

	public bool save_to_dir (string dirPath) {
		bool ret;
		ret = depData.save_to_file (Path.build_filename (dirPath, "dependencies.list", null));
		if (!ret)
			return false;

		ret = save_string_to_file (Path.build_filename (dirPath, this.appItem.idname + ".doap", null), doapData);
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
		res = { "dependencies.list", "pksetting" };
		res += this.appItem.idname + ".doap";
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

	public override void set_app_description (string text) {
		Xml.Node *cdata = xdoc->new_cdata_block (text, text.length);
		Xml.Node *n = get_xsubnode (app_node (), "description");
		n->add_child (cdata);
	}

	public override string get_app_description () {
		return get_node_content (get_xsubnode (app_node (), "description"));
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
	private string doapFile;

	public ControlDir () {
		ctrlDir = "";
		doapFile = "";
	}

	private string find_doap_data (string dir) {
		string doapFile = "";
		try {
			var directory = File.new_for_path (dir);
			var enumerator = directory.enumerate_children (FileAttribute.STANDARD_NAME, 0);

			FileInfo file_info;
			while ((file_info = enumerator.next_file ()) != null) {
				string path = Path.build_filename (dir, file_info.get_name (), null);
				if (file_info.get_is_hidden ())
					continue;

				if (path.down ().has_suffix (".doap"))
					doapFile = path;
			}

		} catch (GLib.Error e) {
			stderr.printf (_("Error: %s\n"), e.message);
			return "";
		}
		return doapFile;
	}

	public bool open_dir (string dir) throws ControlDataError {
		if (ctrlDir != "")
			return false;

		string pksFName = Path.build_filename (dir, "pkoptions", null);
		if (!FileUtils.test (pksFName, FileTest.EXISTS))
			return false;
		if (!open_packsetting (pksFName))
			return false;


		doapFile = find_doap_data (dir);
		if (doapFile == "")
			throw new ControlDataError.NO_DOAP (_("No valid DOAP data found in directory %s - Can't open control files.").printf (dir));

		bool ret = this.open_doap_file (doapFile);
		if (!ret)
			return false;
		ctrlDir = dir;

		string depInfoFName = Path.build_filename (ctrlDir, "dependencies.list", null);
		if (FileUtils.test (depInfoFName, FileTest.EXISTS)) {
			this.open_depinfo (depInfoFName);
		}

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

	public string? get_doap_data () {
		if (doapFile == "")
			return null;

		string doap = load_file_to_string (doapFile);
		return doap;
	}

	public bool save_control () {
		bool ret;
		ret = depData.save_to_file (Path.build_filename (ctrlDir, "dependencies.list", null));
		return ret;
	}

}

} // End of namespace: Listaller.IPK
