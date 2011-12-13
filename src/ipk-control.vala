/* ipk-control.vala - Describes data controlling the IPK setup process
 *
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@tenstral.net>
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
using Xml;
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

public errordomain ControlDataError {
	NO_DOAP,
	DOAP_INVALID,
	DEPLIST_INVALID,
	INTERNAL,
	UNKNOWN;
}

public abstract class Control : Object {
	internal DoapData doap;
	internal MetaFile depData;
	protected AppItem? appItem;

	internal Control () {
		doap = new DoapData ();
		depData = new MetaFile ();
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

	public AppItem get_application () {
		if (appItem != null)
			return appItem;
		AppItem? item = doap.get_project ();
		if (item != null)
			appItem = item;
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
			depData.add_value ("Libraries", dep.get_components_by_type_as_str (Deps.ComponentType.SHARED_LIB));
			depData.add_value ("Binaries", dep.get_components_by_type_as_str (Deps.ComponentType.BINARY));
			depData.add_value ("Python", dep.get_components_by_type_as_str (Deps.ComponentType.PYTHON));
			depData.add_value ("Python2", dep.get_components_by_type_as_str (Deps.ComponentType.PYTHON_2));
			depData.add_value ("Files", dep.get_components_by_type_as_str (Deps.ComponentType.FILE));
		}
	}

	private void add_components_to_dep (Dependency dep, Deps.ComponentType ty, string list) {
		if (list.strip () == "")
			return;
		// We don't like file dependencies
		if (ty == Deps.ComponentType.FILE)
			li_warning ("Resource %s depends on a file (%s), which is not supported at time.".printf (dep.idname, list));
		if (list.index_of ("\n") <= 0) {
			dep.add_component (list, ty);
			return;
		}

		string[] comp = list.split ("\n");
		for (int i = 0; i < comp.length; i++) {
			string s = comp[i].strip ();
			if (s != "")
				dep.add_component (s, ty);
		}

	}

	public ArrayList<Dependency> get_dependencies () {
		ArrayList<Dependency> depList = new ArrayList<Dependency> ();
		depData.reset ();

		while (depData.open_block_by_field ("id")) {
			Dependency dep = new Dependency (depData.get_value ("id"));

			string s = depData.get_value ("feed");
			if (s.strip () != "")
				dep.feed_url = s;
			add_components_to_dep (dep, Deps.ComponentType.SHARED_LIB, depData.get_value ("libraries"));
			add_components_to_dep (dep, Deps.ComponentType.BINARY, depData.get_value ("binaries"));
			add_components_to_dep (dep, Deps.ComponentType.PYTHON, depData.get_value ("python"));
			add_components_to_dep (dep, Deps.ComponentType.PYTHON_2, depData.get_value ("python2"));
			add_components_to_dep (dep, Deps.ComponentType.FILE, depData.get_value ("files"));
			depList.add (dep);
		}
		return depList;
	}
}

public class PackControl : Control {
	private string ipkVersion;
	private string doapData;

	public PackControl () {
		ipkVersion = "1.0";
	}

	public bool open_control (string fDoap, string fDeps, string fIpkV) {
		bool ret;
		doapData = load_file_to_string (fDoap);
		ret = this.open_doap (doapData);
		if (!ret)
			return false;
		ret = this.open_depinfo (fDeps);
		if (!ret)
			return false;

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

	public bool create_new (string? newDoapData, string IpkV) {
		if ((newDoapData == null) || (newDoapData == "")) {
			li_error ("Error while processing DOAP data: Data was NULL!");
			return false;
		}

		doapData = newDoapData;
		doap = new DoapData ();
		depData.clear ();
		bool ret = this.open_doap (doapData);
		if (ret)
			ret = cache_appitem ();
		return ret;
	}

	public bool save_to_dir (string dirPath) {
		bool ret;
		ret = depData.save_to_file (Path.build_filename (dirPath, "dependencies.list", null));
		if (!ret)
			return false;
		ret = save_string_to_file (Path.build_filename (dirPath, this.appItem.idname + ".doap", null), doapData);

		return ret;
	}

	[CCode (array_length = false, array_null_terminated = true)]
	public string[] get_files () {
		string[] res;
		res = { "dependencies.list" };
		res += this.appItem.idname + ".doap";
		res += null;

		return res;
	}

	public string get_license_text () {
		return "::TODO";
	}

#if 0
	public override void set_app_license (string text) {
		base.set_app_license (text);
	}

	private string load_license (string fname) {
		return "::TODO";
	}

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
			var enumerator = directory.enumerate_children (FILE_ATTRIBUTE_STANDARD_NAME, 0);

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

		doapFile = find_doap_data (dir);
		if (doapFile == "")
			throw new ControlDataError.NO_DOAP (_("No valid DOAP data found in directory %s - Can't open control files.").printf (dir));

		bool ret = this.open_doap_file (doapFile);
		if (!ret)
			return false;
		ctrlDir = dir;

		string depInfoFileName = Path.build_filename (ctrlDir, "dependencies.list", null);
		if (FileUtils.test (depInfoFileName, FileTest.EXISTS)) {
			this.open_depinfo (depInfoFileName);
		}
		return true;
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

	public bool get_autosolve_dependencies () {
		/*
		Xml.Node* n = get_xsubnode (pkg_node (), "requires");
		if (get_node_content (get_xproperty (n, "find")) == "auto")
			return true;
		return false;
		*/
		return true;
	}

}

} // End of namespace
