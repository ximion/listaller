/* depinfo.vala - (Fetch) information about software dependencies
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller.Deps;

namespace Listaller.Deps {

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

/* A shared resource */
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
			if ((idname != "") && (!is_standardlib) && (!has_installdata ()) && (feed_url == "")) {
				warning ("Trying to set dependency %s to 'satisfied', although it is not a standardlib and it does not have installdata!", idname);
			}
			_satisfied = value;
		}
	}
	public string architecture { get; set; } // e.g. linux-amd64
	private HashSet<string> install_data { get; set; } // The stuff stored as "real" dependency in software DB
	public bool is_standardlib { get; set; } // Whether this dependency is always satisfied (by default)

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

	internal bool add_install_comp (string sinstcomp) {
		if (sinstcomp.index_of (":") <= 0)
			warning ("Invalid install data set! This should never happen! (Data was %s)", sinstcomp);
		return install_data.add (sinstcomp);
	}

	internal void clear_installdata () {
		install_data.clear ();
	}

	internal void set_installdata_from_string (string str) {
		debug ("::TODO");
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
}

} // End of namespace: Listaller.IPK

namespace Listaller {

private class DepInfo : Object {
	private ArrayList<IPK.Dependency> dlist;

	enum DepInfoBlock {
		UNKNOWN,
		NAME,
		FILES;
	}

	public DepInfo () {
		Listaller.Settings conf = new Listaller.Settings (true);
		string fname = Path.build_filename (conf.conf_dir (), "dependencies.list", null);

		dlist = new ArrayList<IPK.Dependency> ();
		var file = File.new_for_path (fname);
		if (!file.query_exists ()) {
			return;
		}

		try {
			var dis = new DataInputStream (file.read ());
			string line;
			IPK.Dependency? dep = null;
			DepInfoBlock mode = DepInfoBlock.UNKNOWN;

			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				if (line.has_prefix ("#"))
					continue;

				if (line.strip () == "") {
					if ((dep != null) && (dep.full_name != ""))
						dlist.add (dep);
					dep = new IPK.Dependency ("");
					mode = DepInfoBlock.UNKNOWN;
					continue;
				}
				if (line.down ().has_prefix ("name:")) {
					dep.full_name = line.substring (line.index_of (":") + 1).strip ();
					continue;
				}
				if (line.down ().has_prefix ("id:")) {
					dep.idname = line.substring (line.index_of (":") + 1).strip ();
					continue;
				}
				if (line.down ().has_prefix ("feed:")) {
					dep.feed_url = line.substring (line.index_of (":") + 1).strip ();
					continue;
				}
				if (line.down ().has_prefix ("standard:")) {
					string s = line.substring (line.index_of (":") + 1).strip ();
					if (s.down () == "true")
						dep.is_standardlib = true;
					continue;
				}
				if (line.down ().has_prefix ("libraries:")) {
					string s = line.substring (line.index_of (":") + 1).strip ();
					if (s != "")
						dep.add_component (s, ComponentType.SHARED_LIB);
					mode = DepInfoBlock.FILES;
					continue;
				}
				if (line.substring (0, 1) == " ") {
					if (mode == DepInfoBlock.FILES)
						dep.add_component (line.strip (), ComponentType.SHARED_LIB);

				}
			}
			if (dep != null)
				dlist.add (dep);
		} catch (Error e) {
			li_error (_("Unable to fetch dependency information list: %s").printf (e.message));
			return;
		}
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

}

} // End of namespace: Listaller
