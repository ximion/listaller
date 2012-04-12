/* dep-info.vala - Handle information about software dependencies
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller.Dep;

namespace Listaller.Dep {

public enum ComponentType {
	SHARED_LIB,
	BINARY,
	PYTHON,
	PYTHON_2,
	FILE,
	UNKNOWN;
}

}

namespace Listaller.IPK {

/* A shared resource / external requirement */
public class Dependency: Object {
	private string _full_name;
	public string full_name {
		get {
			if (_full_name == "")
				_full_name = _idname;
			if (_full_name == "")
				return "empty";
			return _full_name;
		}
		set {
			_full_name = value;
		}
	}
	public string summary { get; set; }
	public string description { get; set; }
	public string homepage { get; set; }
	public string author { get; set; }
	public string version { get; set; }

	private bool _satisfied;
	public bool satisfied {
		get {
			return _satisfied;
		}
		set {
			if ((!__unittestmode) &&
			    (idname != "") &&
			    (!is_standardlib) &&
			    (!has_installdata ()) &&
			    (feed_url == "")) {
				warning ("Trying to set dependency %s to 'satisfied', although it is not a standardlib. (Reason: No install-data found!) - This usually is a packaging bug.", idname);
			}
			_satisfied = value;
		}
	}
	public string architecture { get; set; } // e.g. amd64
	private HashSet<string> install_data { get; set; } // The stuff stored as "real" dependency in software DB
	public bool is_standardlib { get; set; } // Whether this dependency is always satisfied (by default), set by the distributor

	public string feed_url { get; set; }
	private HashSet<string> components { get; set; } // Parts of this dependency (e.g. shlibs, python modules, files, etc.)

	public int64 install_time { get; set; }
	public string environment { get; set; }

	private string _idname;
	public string idname {
		get {
			if (_idname != "")
				return _idname;
			// Form unique dependency-id, if not already set
			_idname = "%s-%s".printf (full_name, version);
			_idname = _idname.down ().replace (" ", "_");
			return _idname;
		}
		set {
			_idname = value;
		}
	}

	private bool private { get; set; } // If this dependency is installed in user's $HOME or not

	public HashSet<string> raw_complist {
		get {
			return components;
		}
	}

	internal Dependency.blank () {
		components = new HashSet<string> ();
		install_data = new HashSet<string> ();

		_satisfied = false;
		is_standardlib = false;

		feed_url = "";
		version = "0";
		idname = "";
		install_time = -1;
		environment = "";
		author = "";
		_full_name = "";
		architecture = Utils.system_machine ();
	}

	internal Dependency (string depIdName, string depFullName = "", string depVersion = "0") {
		this.blank ();
		idname = depIdName;
		if (depFullName == "")
			full_name = idname;
		else
			full_name = depFullName;
		version = depVersion;
	}

	internal bool add_installed_comp (string sinstcomp) {
		if (sinstcomp.index_of (":") <= 0)
			warning ("Invalid install data set! This should never happen! (Data was %s)", sinstcomp);
		return install_data.add (sinstcomp);
	}

	internal void clear_installdata () {
		install_data.clear ();
	}

	private bool _add_cmpstr_instdata_save (string line) {
		string[] cmp = line.split (":", 2);
		if (cmp.length != 2) {
			critical ("Installdata string is invalid! (Error at: %s) %i", line, cmp.length);
			return false;
		}
		add_installed_comp ("%s:%s".printf (cmp[0], cmp[1]));
		return true;
	}

	private bool _add_componentstr_save (string line) {
		string[] cmp = line.split (":", 2);
		if (cmp.length != 2) {
			critical ("Component string is invalid! (Error at: %s) %i", line, cmp.length);
			return false;
		}
		components.add ("%s:%s".printf (cmp[0], cmp[1]));
		return true;
	}

	internal void set_installdata_from_string (string str) {
		if (str.index_of ("\n") < 0) {
			_add_cmpstr_instdata_save (str);
			return;
		}

		string[]? lines = str.split ("\n");
		if (lines == null)
			return;
		foreach (string s in lines) {
			if (s != "")
				if (!_add_cmpstr_instdata_save (s))
					return;
		}
	}

	internal void set_componentdata_from_string (string str) {
		if (str.index_of ("\n") < 0) {
			_add_componentstr_save (str);
			return;
		}

		string[]? lines = str.split ("\n");
		if (lines == null)
			return;
		foreach (string s in lines) {
			if (s != "")
				if (!_add_componentstr_save (s))
					return;
		}
	}

	internal string get_componentdata_as_string () {
		string res = "";
		foreach (string s in raw_complist)
			res += s + "\n";
		return res;
	}

	public HashSet<string> get_installdata () {
		return install_data;
	}

	public string get_installdata_as_string () {
		string res = "";
		foreach (string s in install_data)
			res += s + "\n";
		return res;
	}

	public bool has_installdata () {
		if (install_data == null)
			return false;
		return install_data.size > 0;
	}

	public void regenerate_depid () {
		if (_full_name == "empty") {
			warning ("Dependency full_name is empty! Something is going wrong here...");
			_full_name = "empty";
		}
		_idname = "";
		_idname = idname;
	}

	private string get_component_type_idstr (ComponentType tp) {
		string idstr = "";
		switch (tp) {
			case ComponentType.SHARED_LIB: idstr = "lib:%s";
						  break;
			case ComponentType.BINARY: idstr = "bin:%s";
						  break;
			case ComponentType.PYTHON: idstr = "python:%s";
						  break;
			default: idstr = "file:%s";
				 break;
		}
		return idstr;
	}

	public void add_component (string cname, ComponentType tp) {
		string str = get_component_type_idstr (tp).printf (cname);
		components.add (str);
	}

	public void add_component_list (Dep.ComponentType ty, string list) {
		if (list.strip () == "")
			return;
		// We don't like file dependencies
		if (ty == ComponentType.FILE)
			li_warning ("Resource %s depends on a file (%s), which is not supported at time.".printf (idname, list));

		if (list.index_of ("\n") <= 0) {
			add_component (list, ty);
			return;
		}

		string[] comp = list.split ("\n");
		for (int i = 0; i < comp.length; i++) {
			string s = comp[i].strip ();
			if (s != "")
				add_component (s, ty);
		}

	}

	public bool has_component (string cname, ComponentType tp) {
		return components.contains (get_component_type_idstr (tp).printf (cname));
	}

	public bool has_components () {
		return components.size > 0;
	}

	public bool has_feed () {
		return feed_url != "";
	}

	internal string get_components_by_type_as_str (ComponentType tp) {
		string res = "";
		foreach (string s in components) {
			if (component_get_type (s) == tp)
				res += component_get_name (s) + "\n";
		}
		return res;
	}

	public static string component_get_name (string cidname) {
		return cidname.substring (cidname.index_of (":") + 1).strip ();
	}

	public static ComponentType component_get_type (string cidname) {
		string tpid = cidname.substring (0, cidname.index_of (":")).strip ();
		ComponentType tp = ComponentType.UNKNOWN;
		switch (tpid) {
			case "lib": tp = ComponentType.SHARED_LIB;
				    break;
			case "bin": tp = ComponentType.BINARY;
				    break;
			case "python": tp = ComponentType.PYTHON;
				    break;
			case "file": tp = ComponentType.FILE;
				    break;
			default: tp = ComponentType.UNKNOWN;
				    break;
		}
		return tp;
	}

	public string get_install_dir_for_setting (Listaller.Settings conf) {
		return Path.build_filename (conf.depdata_dir (), idname, null);
	}
}

[CCode (has_target = false)]
private static uint dependency_hash_func (Dependency dep) {
	string str = dep.idname;
	return str_hash (str);
}

[CCode (has_target = false)]
private static bool dependency_equal_func (Dependency a, Dependency b) {
	if (a.idname == b.idname)
		return true;
	return false;
}

private HashSet<Dependency> dependency_hashset_new () {
	return new HashSet<Dependency> ((HashFunc) dependency_hash_func, (EqualFunc) dependency_equal_func);
}

} // End of namespace: Listaller.IPK

namespace Listaller {

private class DepInfoGenerator : Object {
	private ArrayList<IPK.Dependency> dlist;

	enum DepInfoBlock {
		UNKNOWN,
		NAME,
		FILES;
	}

	public DepInfoGenerator () {
		// Load the default components
		Listaller.Settings conf = new Listaller.Settings (true);
		string fname_default = Path.build_filename (conf.conf_dir (), "default-dependencies.list", null);
		string fname_distro = Path.build_filename (conf.conf_dir (), "dependencies.list", null);

		dlist = new ArrayList<IPK.Dependency> ();

		if (!FileUtils.test (fname_default, FileTest.EXISTS))
			return;

		var metaF = new IPK.MetaFile ();
		metaF.open_file (fname_default);
		if (FileUtils.test (fname_distro, FileTest.EXISTS))
			metaF.open_file_add_data (fname_distro);

		add_dependencies_from_metafile (metaF);
	}

	public bool add_dependencies_from_file (string fname) {
		var metaF = new IPK.MetaFile ();
		var ret = metaF.open_file (fname);

		if (ret)
			add_dependencies_from_metafile (metaF);
		return ret;
	}

	public void add_dependencies (ArrayList<IPK.Dependency> deps) {
		dlist.add_all (deps);
	}

	public IPK.Dependency? get_dep_template_for_component (string cidname) {
		foreach (IPK.Dependency dep in dlist) {
			foreach (string s in dep.raw_complist) {
				if (cidname == s)
					return dep;
				if (PatternSpec.match_simple (s, cidname))
					return dep;
			}
		}
		return null;
	}

	private void add_dependencies_from_metafile (IPK.MetaFile metaF) {
		IPK.Dependency? dep = null;

		metaF.open_block_first ();
		do {
			dep = new IPK.Dependency ("");
			dep.full_name = metaF.get_value ("Name");
			dep.idname = metaF.get_value ("ID");
			dep.feed_url = metaF.get_value ("Feed");
			if (metaF.get_value ("Standard") == "true")
				dep.is_standardlib = true;
			dep.add_component_list (ComponentType.SHARED_LIB, metaF.get_value ("Libraries"));
			dep.add_component_list (ComponentType.BINARY, metaF.get_value ("Binaries"));
			dep.add_component_list (ComponentType.PYTHON, metaF.get_value ("Python"));
			dep.add_component_list (ComponentType.PYTHON_2, metaF.get_value ("Python2"));
			dep.add_component_list (ComponentType.FILE, metaF.get_value ("Files"));
			dlist.add (dep);

		} while (metaF.block_next ());
	}

	public void update_dependency_with_system_data (ref IPK.Dependency dep, bool pedantic = false) {
		foreach (IPK.Dependency sydep in dlist) {
			if (pedantic) {
				if (sydep.idname == dep.idname) {
					dep = sydep;
					break;
				}
			} else {
				if ((sydep.full_name == dep.full_name) ||
				    (sydep.idname == dep.idname)) {
					dep = sydep;
					break;
				}
			}
		}
	}

	public HashSet<IPK.Dependency> get_dependency_list_for_components (ArrayList<string> comp) {
		var depList = IPK.dependency_hashset_new ();

		foreach (string s in comp) {
			if (IPK.Dependency.component_get_type (s) != ComponentType.UNKNOWN) {
				IPK.Dependency dtmp = get_dep_template_for_component (s);
				string dep_name = "";
				if (dtmp != null)
					dep_name = dtmp.full_name;
				else
					dep_name = Utils.string_replace (s.substring (4), "(\\.so|\\+|\\.)", "");

				if (dep_name.strip () == "") {
					debug ("dep_name would be empty for %s! (ignoring it)", s);
					continue;
				}

				IPK.Dependency dep = null;
				if (dtmp == null) {
					// If we are here, we need to create a new dependency object
					dep = new IPK.Dependency (dep_name);
					dep.add_component (IPK.Dependency.component_get_name (s.strip ()), IPK.Dependency.component_get_type (s));
				} else {
					dep = dtmp;
				}

				depList.add (dep);
			}
		}

		return depList;
	}

}

} // End of namespace: Listaller
