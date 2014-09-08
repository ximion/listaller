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

public errordomain ComponentError {
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
public class Dependency : Object {
	internal int dbid { get; set; }

	private Component _metainfo;
	public Component metainfo {
		get {
			return _metainfo;
		}
		internal set {
			_metainfo = value;
			update_data_from_metainfo ();
		}
	}

	private string _unique_name;
	public string unique_name {
		get {
			update_unique_name ();
			return _unique_name;
		}
		internal set {
			_unique_name = value;
			if ((_metainfo.id == null) || (_metainfo.id.strip () == ""))
				_metainfo.id = _unique_name;
		}
	}

	public string architecture { get; internal set; } // e.g. amd64

	protected bool _installed;
	public bool installed {
		get {
			return _installed;
		}
		set {
			_installed = value;
		}
	}

	public string origin { get; internal set; }
	public int64 install_time { get; internal set; }
	public string environment { get; internal set; }

	public string feed_url { get; internal set; }

	protected string _version_raw;
	private string _version_cache;

	// Items which were installed in order to satisfy this dependency
	protected HashSet<string> installed_items { get; private set; }

	public Dependency () {
		dbid = -1;
		metainfo = new Component ();

		installed_items = new HashSet<string> ();

		environment = "";
		feed_url = "";
		origin = "unknown";
		architecture = system_machine_generic ();
		_version_raw = "";
		_unique_name = "";
	}

	public bool load_xml_data (string xmld) throws GLib.Error {
		var mdata = new Appstream.Metadata ();
		Appstream.Component cpt;
		try {
			cpt = mdata.parse_data (xmld);
		} catch (Error e) {
			throw e;
		}
		metainfo = cpt;

		// Find Listaller-specific nodes in AppStream XML
		Xml.Doc *xdoc = Xml.Parser.parse_doc (xmld);
		// Get the root node
		Xml.Node* root = xdoc->get_root_element ();
		for (Xml.Node* iter = root->children; iter != null; iter = iter->next) {
			// discard spaces
			if (iter->type != Xml.ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == "x-listaller_extra")
				parse_metainfo_listaller_extras (iter);
		}
		delete xdoc;

		return true;
	}

	public bool load_from_file (string fname, bool include_optional = false) {
		// parse canonical AppStream Component
		var mdata = new Appstream.Metadata ();
		Appstream.Component cpt;
		try {
			string xmld = load_file_to_string (fname);
			load_xml_data (xmld);
		} catch (Error e) {
			warning (e.message);
			return false;
		}

		return true;
	}

	private void update_data_from_metainfo () {
		// set version, if there is one
		GenericArray<Appstream.Release> releases = metainfo.get_releases ();
		if (releases.length > 0) {
			// the first item should always be the newest one
			Appstream.Release release = releases.get (0);
			set_version (release.get_version ());
		}
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
	 * @return Value of the directives found
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
					throw new ComponentError.DIRECTIVES_RESOLVE_FAILED ("Unable to resolve directives for %s: Command %s returned error-code %i.", metainfo.name, cmd, exit_status);
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
					throw new ComponentError.DIRECTIVES_INVALID ("Unable to resolve directives for %s: Get prefix-directive found, but don't have valid data to apply it.", metainfo.name);
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
			throw new ComponentError.VERSION_NOT_FOUND ("Component %s is not installed, cannot retrieve version number!", metainfo.name);

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
			warning ("No version found for component '%s'.", metainfo.id);

		return res;
	}

	public void set_version (string new_version) {
		_version_cache = new_version;
		update_unique_name (true);
	}

	private void parse_metainfo_listaller_extras (Xml.Node *node) {
		for (Xml.Node* iter = node->children; iter != null; iter = iter->next) {
			// discard spaces
			if (iter->type != Xml.ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == "version_template") {
				_version_raw = iter->get_content ();
			} else if (iter->name == "always_installed") {
				installed = iter->get_content () == "true";
			}
		}
	}

	public bool has_item (ProvidesKind kind, string val) {
		string str = provides_item_create (kind, val, null);

		GenericArray<string> items = metainfo.get_provided_items ();
		for(uint i = 0; i < items.length; i++) {
			string item = items.get (i);
			if (item == str)
				return true;
		}
		return false;
	}

	public bool has_matching_item (ProvidesKind kind, string val) {
		if (Utils.str_is_empty (val))
			return false;
		string item_id = provides_item_create (kind, val, null);

		GenericArray<string> items = metainfo.get_provided_items ();
		for(uint i = 0; i < items.length; i++) {
			string item = items.get (i);
			if (PatternSpec.match_simple (item, item_id))
				return true;
		}

		return false;
	}

	public bool has_items () {
		GenericArray<string> items = metainfo.get_provided_items ();
		return items.length > 0;
	}

	public string get_installed_items_as_string () {
		string res = "";
		foreach (string s in installed_items)
			res += s + "\n";
		return res;
	}

	public bool has_installed_items () {
		if (installed_items == null)
			return false;
		return installed_items.size > 0;
	}

	internal bool add_installed_item (string sinstcomp) {
		if (sinstcomp.index_of (";") <= 0)
			warning ("Invalid install data set! This should never happen! (Data was %s)", sinstcomp);
		bool ret = installed_items.add (sinstcomp);
		return ret;
	}

	internal void clear_installed_items () {
		installed_items.clear ();
	}

	private bool _add_itemstr_instdata_save (string line) {
		string[] cmp = line.split (";", 3);
		if (cmp.length != 3) {
			critical ("Installdata string is invalid! (Error at: %s) %i", line, cmp.length);
			return false;
		}
		add_installed_item ("%s;%s;%s".printf (cmp[0], cmp[1], cmp[2]));
		return true;
	}

	internal void set_installdata_from_string (string str) {
		if (str.index_of ("\n") < 0) {
			if (str != "")
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
	}

	public bool has_feed () {
		return !Utils.str_is_empty (feed_url);
	}

	/**
	 * Get installation directory for this module, using the SettupSettings taken as argument
	 */
	public string get_install_dir_for_setting (SetupSettings setup_setting) {
		return Path.build_filename (setup_setting.depdata_dir (), unique_name, null);
	}

	public string get_metadata_xml () {
		return metainfo.to_xml ();
	}

	/**
	 * Update the unique name of this dependency. The unique-name is composed
	 * as in applications, but dependencies also include the version-number.
	 */
	private void update_unique_name (bool force = false) {
		if ((_unique_name.strip () != "") && (!force))
			return;
		string dep_baseid = _metainfo.id;
		if (Utils.str_is_empty (dep_baseid))
			dep_baseid = _metainfo.name.down ();
		if (Utils.str_is_empty (dep_baseid))
			error ("Unable to generate unique identifier for application!");
		string? version = null;
		try {
			version = get_version ();
		} catch (Error e) {}
		if (!Utils.str_is_empty (version))
			dep_baseid = "%s-%s".printf (dep_baseid, version);
		_unique_name = string_replace (dep_baseid, "( )", "_");
	}

	/**
	* Generate a PackageKit package-id for this dependency information
	*/
	public string build_pk_package_id () {
		string package_id;
		string uname = "dep:%s".printf (metainfo.id);

		package_id = PackageKit.Package.id_build (uname,
							  get_version (),
							  architecture,
							  "local:listaller");

		return package_id;
	}

	/**
	 * Returns %TRUE if the dependency is valid (= contains sane data)
	 */
	public bool is_valid () {
		if (Utils.str_is_empty (unique_name) || Utils.str_is_empty (get_version ()))
			return false;
		if (origin == "unknown")
			return false;
		return true;
	}

}

[CCode (has_target = false)]
private static uint dependency_hash_func (Dependency dep) {
	string str = dep.unique_name;
	return str_hash (str);
}

[CCode (has_target = false)]
private static bool dependency_equal_func (Dependency a, Dependency b) {
	if (a.unique_name == b.unique_name)
		return true;
	return false;
}

private HashSet<Dependency> dependency_hashset_new () {
	return new HashSet<Dependency> (dependency_hash_func, dependency_equal_func);
}

} // End of namespace: Listaller
