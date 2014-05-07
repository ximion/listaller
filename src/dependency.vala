/* dependency.vala -- Defines component types to resolve dependencies
 *
 * Copyright (C) 2011-2014 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller.Dep;
using Appstream;

namespace Listaller.Dep {

private errordomain ComponentError {
    VERSION_NOT_FOUND,
    DIRECTIVES_RESOLVE_FAILED,
    DIRECTIVES_INVALID
}

}

namespace Listaller {

/**
 * Definition of a dependency of an application
 *
 * Contains a #AsComponent which holds the actual information
 * about the referenced component-dependency.
 */
private class Dependency : Object {
	public Component info { get; private set; }
	public string xmldata { get; set; }

	public string architecture { get; internal set; } // e.g. amd64

	protected bool _installed;
	public string dependencies { get; private set; }

	public bool installed {
		get {
			return _installed;
		}
		set {
			if ((!Utils.__unittestmode) &&
			    (info.idname != "") &&
			    (!has_installdata ()) &&
			    (feed_url == "")) {
				warning ("Trying to set dependency %s to 'satisfied', although it is not a standardlib. (Reason: No install-data found!) - This usually is a packaging bug.", info.idname);
			}
			_installed = value;
		}
	}

	public string origin { get; internal set; }
	public int64 install_time { get; internal set; }
	public string environment { get; internal set; }

	public string feed_url { get; internal set; }

	protected string _version_raw;
	private string _version_cache;

	protected HashSet<string> item_list { get; internal set; } // Parts of this dependency (e.g. shlibs, python modules, files, etc.)

	public HashSet<string> raw_itemlist {
		get {
			return item_list;
		}
	}

	// Items which were installed in order to satisfy this dependency
	protected HashSet<string> install_data { get; internal set; }

	public Dependency.blank () {
		info = new Component ();

		item_list = new HashSet<string> ();
		install_data = new HashSet<string> ();

		environment = "";
		feed_url = "";
		origin = "unknown";
		architecture = system_machine_generic ();
	}

	public Dependency (Component cpt) {
		this.blank ();
		info = cpt;
	}

	protected bool contains_directive (string str) {
		return ((str.index_of ("shell$") >= 0) || (str.index_of ("textfile$") >= 0) || (str.index_of ("prefix$") >= 0) || (str.index_of ("envvar$") >= 0) || (str.index_of ("libversion$") >= 0));
	}

	private string get_directive_value (string directive) {
		string s = directive;
		string res;
		if (s.has_prefix ("shell$ "))
			res = s.substring (7);
		else if (s.has_prefix ("envvar$ "))
			res = s.substring (8);
		else if (s.has_prefix ("prefix$ "))
			res = s.substring (8);
		else if (s.has_prefix ("textfile$ "))
			res = s.substring (10);
		else if (s.has_prefix ("regex$ "))
			res = s.substring (7);
		else if (s.has_prefix ("libversion$ "))
			res = s.substring (12);
		else
			return s;

		// remove quotes, if expression was quoted
		if ((res.has_prefix ("\"")) && (res.has_suffix ("\"")))
			res = res.substring (1, res.length-2);

		return res;
	}

	/**
	 * @returns Value of the directives found
	 */
	protected string process_directives (string directive_str) throws ComponentError {
		// first grab all data from shell commands and env vars
		string res = "";
		string[] parts = directive_str.split ("\n");

		foreach (string str in parts) {
			string rawdata;
			string v_value = get_directive_value (str);
			if (str.has_prefix ("shell$ ")) {
				int exit_status;
				string stderr;
				string cmd = v_value;

				Process.spawn_command_line_sync (cmd, out rawdata, out stderr, out exit_status);
				if (exit_status != 0)
					throw new ComponentError.DIRECTIVES_RESOLVE_FAILED ("Unable to resolve directives for %s: Command %s returned error-code %i.", info.name, cmd, exit_status);
				res = rawdata;
				// for some stupid reason, a few tools (such as the Xserver) print their versions to stderr
				// this workaround reflects that fact - error has been catched through an exit code above
				if (str_is_empty (res))
					res = stderr;
			} else if (str.has_prefix ("envvar$ ")) {
				int exit_status;
				string varname = v_value;
				rawdata = Environment.get_variable (varname);
				res = rawdata;
			} else if (str.has_prefix ("libversion$ ")) {
				string? lib_path = find_library (v_value, new Config ());
				// ignore missing libs
				if (lib_path == null) {
					debug ("Library %s not available for version-detection. Ignoring issue.", v_value);
					continue;
				}
				lib_path = resolve_symbolic_link (lib_path);
				string absolute_libname = Path.get_basename (lib_path);
				// now extract version from library name (the stuff after the .so is the version, usually...)
				res = absolute_libname.substring (absolute_libname.index_of (".so.") + 4);
			}
		}
		foreach (string str in parts) {
			str = str.strip ();
			string v_value = get_directive_value (str);
			if (str.has_prefix ("prefix$ ")) {
				// the prefix directive fetches the data after the given prefix
				if (str_is_empty (res))
					throw new ComponentError.DIRECTIVES_INVALID ("Unable to resolve directives for %s: Get prefix-directive found, but don't have valid data to apply it.", info.name);
				string prefix = v_value;
				if (res.index_of (prefix) < 0)
					throw new ComponentError.DIRECTIVES_RESOLVE_FAILED ("Unable to get data prefixed with '%s' from raw data string '%s'.", prefix, res);

				int i = res.index_of (prefix) + prefix.length;
				if (res.index_of ("\n") < 0)
					res = res.substring (i);
				else
					res = res.substring (i, res.index_of ("\n", i)-i);
			}
		}

		if (str_is_empty (res))
			res = directive_str;

		return res;
	}

	public string get_version () throws ComponentError {
		// maybe this component is unversioned?
		if (_version_cache == "none")
			return "";
		// check if we have a cached string
		if (!str_is_empty (_version_cache))
			return _version_cache;

		if (!installed)
			throw new ComponentError.VERSION_NOT_FOUND ("Component %s is not installed, cannot retrieve version number!", info.name);

		// process version directive, if necessary
		if (!contains_directive (_version_raw))
			return _version_raw;

		string res = "";
		try {
			res = process_directives (_version_raw);
		} catch (ComponentError e) {
			throw e;
		}

		// linebreaks in version numbers are evil!
		res = res.replace ("\n", "");

		if (res == "")
			warning ("No version found for component '%s'.", info.idname);

		return res;
	}

	public void set_version (string new_version) {
		_version_cache = new_version;
	}

	public bool load_from_file (string fname, bool include_optional = false) {
		// parse canonical AppStream Component
		var mdata = new Appstream.Metadata ();
		string xmldata = null;
		Appstream.Component cpt;
		try {
			xmldata = load_file_to_string (fname);
			cpt = mdata.parse_data (xmldata);
		} catch (Error e) {
			warning (e.message);
			return false;
		}
		info = cpt;

		GenericArray<string> items = info.get_provided_items ();
		for(uint i = 0; i < items.length; i++) {
			string item = items.get (i);
			item_list.add (item);
		}

		// set version, if there is one
		GenericArray<Appstream.Release> releases = cpt.get_releases ();
		if (releases.length > 0) {
			// the fisrt item should always be the newest one
			Appstream.Release release = releases.get (0);
			set_version (release.get_version ());
		}

		// Find Listaller-specific nodes in AppStream XML

		// Parse the document from path
		Xml.Doc *xdoc = Xml.Parser.parse_doc (xmldata);
		// Get the root node
		Xml.Node* root = xdoc->get_root_element ();
		for (Xml.Node* iter = root; iter != null; iter = iter->next) {
			// discard spaces
			if (iter->type != Xml.ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == "x-version-template") {
				_version_raw = iter->get_content ();
			} else if (iter->name == "x-always-installed") {
				installed = iter->get_content () == "true";
			}
		}

		delete xdoc;

		// TODO
		/**
		// The optional stuff is usually only used by the setup process, to group
		// optional libraries to the right component, without throwing an error.
		string[] prefixes = {};
		prefixes += "";
		if (include_optional)
			prefixes += "Optional";
		**/

		return true;
	}

	/**
	 * Add item as string (performing some checks if the string is valid)
	 */
	private bool _add_itemstr_save (string line) {
		string[] item = line.split (":", 2);
		if (item.length != 2) {
			critical ("Component item string is invalid! (Error at: %s) %i", line, item.length);
			return false;
		}
		item_list.add ("%s:%s".printf (item[0], item[1]));

		return true;
	}

	/**
	 * Set items for this components, using a serialized string as source
	 */
	internal void set_items_from_string (string str) {
		if (str.index_of ("\n") < 0) {
			_add_itemstr_save (str);
			return;
		}

		string[]? lines = str.split ("\n");
		if (lines == null)
			return;
		foreach (string s in lines) {
			if (s != "")
				if (!_add_itemstr_save (s))
					return;
		}
	}

	internal string get_items_as_string () {
		string res = "";
		foreach (string s in item_list)
			res += s + "\n";
		return res;
	}

	public void add_item (ProvidesKind kind, string item_name) {
		string str = provides_item_create (kind, item_name, null);
		item_list.add (str);
	}

	public void add_item_list (ProvidesKind kind, string list) {
		if (list.strip () == "")
			return;

		if (list.index_of ("\n") < 0) {
			add_item (kind, list);
			return;
		}

		string[] comp = list.split ("\n");
		for (int i = 0; i < comp.length; i++) {
			string s = comp[i].strip ();
			if (s != "")
				add_item (kind, s);
		}

	}

	public bool has_item (ProvidesKind kind, string val) {
		string str = provides_item_create (kind, val, null);
		return item_list.contains (str);
	}

	public bool has_matching_item (ProvidesKind kind, string val) {
		string item_id = provides_item_create (kind, val, null);
		foreach (string s in item_list) {
			if (PatternSpec.match_simple (s, item_id))
				return true;
		}

		return false;
	}

	public bool has_items () {
		return item_list.size > 0;
	}

	internal string get_items_by_type_as_str (ProvidesKind kind) {
		string res = "";
		foreach (string s in item_list) {
			if (provides_item_get_kind (s) == kind)
				res += provides_item_get_value (s) + "\n";
		}

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

	internal bool add_installed_item (string sinstcomp) {
		if (sinstcomp.index_of (":") <= 0)
			warning ("Invalid install data set! This should never happen! (Data was %s)", sinstcomp);
		bool ret = install_data.add (sinstcomp);
		update_installed_status ();
		return ret;
	}

	internal void clear_installdata () {
		install_data.clear ();
		update_installed_status ();
	}

	private bool _add_itemstr_instdata_save (string line) {
		string[] cmp = line.split (":", 2);
		if (cmp.length != 2) {
			critical ("Installdata string is invalid! (Error at: %s) %i", line, cmp.length);
			return false;
		}
		add_installed_item ("%s:%s".printf (cmp[0], cmp[1]));
		return true;
	}

	internal void set_installdata_from_string (string str) {
		if (str.index_of ("\n") < 0) {
			_add_itemstr_instdata_save (str);
			return;
		}

		string[]? lines = str.split ("\n");
		if (lines == null)
			return;
		foreach (string s in lines) {
			if (s != "")
				if (!_add_itemstr_instdata_save (s))
					return;
		}
		update_installed_status ();
	}

	/**
	 * Check if the Module is installed, and update it's status if it has been
	 * installed
	 */
	protected void update_installed_status () {
		installed = install_data.size == item_list.size;
	}

	public bool has_feed () {
		return feed_url != "";
	}

	/**
	 * Get installation directory for this module, using the SettupSettings taken as argument
	 */
	public string get_install_dir_for_setting (SetupSettings setup_setting) {
		return Path.build_filename (setup_setting.depdata_dir (), info.idname, null);
	}

	/**
	* Generate a PackageKit package-id for this dependency information
	*/
	public string build_pk_package_id () {
		string package_id;
		string unique_idname = "dep:%s".printf (info.idname);

		package_id = PackageKit.Package.id_build (unique_idname,
							  get_version (),
							  architecture,
							  "local:listaller");

		return package_id;
	}
}

[CCode (has_target = false)]
private static uint dependency_hash_func (Dependency dep) {
	string str = dep.info.idname;
	return str_hash (str);
}

[CCode (has_target = false)]
private static bool dependency_equal_func (Dependency a, Dependency b) {
	if (a.info.idname == b.info.idname)
		return true;
	return false;
}

private HashSet<Dependency> dependency_hashset_new () {
	return new HashSet<Dependency> ((HashFunc) dependency_hash_func, (EqualFunc) dependency_equal_func);
}

} // End of namespace: Listaller
