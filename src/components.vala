/* components.vala -- Defines component types to resolve dependencies
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Dep {

private errordomain ComponentError {
    VERSION_NOT_FOUND,
    DIRECTIVES_RESOLVE_FAILED,
    DIRECTIVES_INVALID
}

public enum ItemType {
	SHARED_LIB,
	BINARY,
	PYTHON,
	PYTHON_2,
	FILE,
	UNKNOWN;
}

/**
 * Definition of common properties of a component
 *
 * Ancestor class for Framework and Module
 */
private abstract class Component : Object {
	private string _full_name;
	public string full_name {
		get {
			if (_full_name == "")
				_full_name = _idname;
			if (_full_name == "")
				return "empty";
			return _full_name;
		}
		internal set {
			_full_name = value;
		}
	}

	private string _idname;
	public string idname {
		get {
			if (_idname != "")
				return _idname;

			// Form unique dependency-id, if not already set
			_idname = full_name;
			_idname = _idname.down ().replace (" ", "_");

			return _idname;
		}
		set {
			_idname = value;
		}
	}

	public string summary { get; internal set; }
	public string description { get; internal set; }
	public string homepage { get; internal set; }
	public string author { get; internal set; }

	public string architecture { get; internal set; } // e.g. amd64

	protected bool _installed;
	public bool installed { get { return _installed; } internal set { _installed = value; } }

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

	public Component (string id_name) {
		idname = id_name;
		_full_name = "";

		item_list = new HashSet<string> ();
		install_data = new HashSet<string> ();
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
					throw new ComponentError.DIRECTIVES_RESOLVE_FAILED ("Unable to resolve directives for %s: Command %s returned error-code %i.", full_name, cmd, exit_status);
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
					throw new ComponentError.DIRECTIVES_INVALID ("Unable to resolve directives for %s: Get prefix-directive found, but don't have valid data to apply it.", full_name);
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
			throw new ComponentError.VERSION_NOT_FOUND ("Component %s is not installed, cannot retrieve version number!", full_name);

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
			warning ("No version found for component '%s'.", idname);

		return res;
	}

	public void set_version (string new_version) {
		_version_cache = new_version;
	}

	protected virtual IPK.MetaFile? load_from_file_internal (string fname, bool include_optional = false) {
		var data = new IPK.MetaFile ();
		var ret = data.open_file (fname);
		if (!ret)
			return null;
		data.open_block_first ();

		full_name = data.get_value ("Name");
		idname = data.get_value ("ID");
		_version_raw = data.get_value ("Version");
		if (!data.has_field ("Version"))
			// if there is no version field, we can never determine the version, so this component is not versioned
			_version_cache = "none";

		if (data.get_value ("AlwaysInstalled") == "true")
			installed = true;

		// The optional stuff is usually only used by the setup process, to group
		// optional libraries to the right component, without throwing an error.
		string[] prefixes = {};
		prefixes += "";
		if (include_optional)
			prefixes += "Optional";

		// add item data for this component
		foreach (string prefix in prefixes) {
			add_item_list (ItemType.SHARED_LIB, data.get_value (prefix + "Libraries"));
			add_item_list (ItemType.BINARY, data.get_value (prefix + "Binaries"));
			add_item_list (ItemType.PYTHON, data.get_value (prefix + "Python"));
			add_item_list (ItemType.PYTHON_2, data.get_value (prefix + "Python2"));
			add_item_list (ItemType.FILE, data.get_value (prefix + "Files"));
		}

		return data;
	}

	public bool load_from_file (string fname, bool include_optional = false) {
		IPK.MetaFile? data = load_from_file_internal (fname, include_optional);
		if (data == null)
			return false;

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

	private static string get_item_type_idstr (ItemType tp) {
		string idstr = "";
		switch (tp) {
			case ItemType.SHARED_LIB: idstr = "lib:%s";
						  break;
			case ItemType.BINARY: idstr = "bin:%s";
						  break;
			case ItemType.PYTHON: idstr = "python:%s";
						  break;
			case ItemType.PYTHON_2: idstr = "python2:%s";
						  break;
			default: idstr = "file:%s";
				 break;
		}

		return idstr;
	}

	public void add_item (ItemType tp, string item_name) {
		string str = get_item_type_idstr (tp).printf (item_name);
		item_list.add (str);
	}

	public void add_item_list (ItemType ty, string list) {
		if (list.strip () == "")
			return;

		// We don't like file requirements
		if (ty == ItemType.FILE)
			warning ("Component %s depends on a file (%s), which is not supported at time.".printf (idname, list));

		if (list.index_of ("\n") < 0) {
			add_item (ty, list);
			return;
		}

		string[] comp = list.split ("\n");
		for (int i = 0; i < comp.length; i++) {
			string s = comp[i].strip ();
			if (s != "")
				add_item (ty, s);
		}

	}

	public bool has_item (ItemType tp, string cname) {
		return item_list.contains (get_item_type_idstr (tp).printf (cname));
	}

	public bool has_matching_item (ItemType tp, string cname) {
		string item_id = get_item_type_idstr (tp).printf (cname);
		foreach (string s in item_list) {
			if (PatternSpec.match_simple (s, item_id))
				return true;
		}

		return false;
	}

	public bool has_items () {
		return item_list.size > 0;
	}

	internal string get_items_by_type_as_str (ItemType tp) {
		string res = "";
		foreach (string s in item_list) {
			if (item_get_type (s) == tp)
				res += item_get_name (s) + "\n";
		}

		return res;
	}

	public static string item_get_name (string item_idstr) {
		return item_idstr.substring (item_idstr.index_of (":") + 1).strip ();
	}

	public static ItemType item_get_type (string item_idstr) {
		string tpid = item_idstr.substring (0, item_idstr.index_of (":")).strip ();
		ItemType tp = ItemType.UNKNOWN;
		switch (tpid) {
			case "lib": tp = ItemType.SHARED_LIB;
				    break;
			case "bin": tp = ItemType.BINARY;
				    break;
			case "python": tp = ItemType.PYTHON;
				    break;
			case "python2": tp = ItemType.PYTHON_2;
				    break;
			case "file": tp = ItemType.FILE;
				    break;
			default: tp = ItemType.UNKNOWN;
				    break;
		}

		return tp;
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
	private void update_installed_status () {
		installed = install_data.size == item_list.size;
	}
}

/**
 * Definition of a framework dependency
 *
 * A framework is a (usually large) system component, such as the KDELibs
 * or the GNOME platform, or stuff like PolicyKit. It usually requires tight
 * system integration.
 * The only way to satisfy this dependency is via the distribution's native package
 * manager.
 */
private class Framework : Component {

	public Framework (string idname) {
		base (idname);
	}

	public Framework.blank () {
		base ("");
	}
}

/**
 * Definition of a module dependency
 *
 * A module is a dependency which can be installed with Listaller and satisfied using 3rd-party
 * sources.
 * It often simply is a small library, iconset, etc.
 */
private class Module : Component {
	public string dependencies { get; private set; }

	public bool installed {
		get {
			return _installed;
		}
		set {
			if ((!Utils.__unittestmode) &&
			    (idname != "") &&
			    (!has_installdata ()) &&
			    (feed_url == "")) {
				warning ("Trying to set dependency %s to 'satisfied', although it is not a standardlib. (Reason: No install-data found!) - This usually is a packaging bug.", idname);
			}
			_installed = value;
		}
	}

	public string origin { get; internal set; }
	public int64 install_time { get; internal set; }
	public string environment { get; internal set; }

	public string feed_url { get; internal set; }

	public Module (string idname) {
		base (idname);
		environment = "";
		feed_url = "";
		origin = "unknown";
	}

	public Module.blank () {
		base ("");
		environment = "";
		feed_url = "";
		origin = "unknown";
	}

	protected override IPK.MetaFile? load_from_file_internal (string fname, bool include_optional = false) {
		IPK.MetaFile? data = base.load_from_file_internal (fname, include_optional);
		if (data == null)
			return null;

		feed_url = data.get_value ("Feed");

		return data;
	}

	public bool has_feed () {
		return feed_url != "";
	}

	/**
	 * Get installation directory for this module, using the SettupSettings taken as argument
	 */
	public string get_install_dir_for_setting (SetupSettings setup_setting) {
		return Path.build_filename (setup_setting.depdata_dir (), idname, null);
	}

	/**
	* Generate a PackageKit package-id for this dependency information
	*/
	public string build_pk_package_id () {
		string package_id;
		string unique_idname = "dep:%s".printf (idname);

		package_id = PackageKit.Package.id_build (unique_idname,
							  get_version (),
							  architecture,
							  "local:listaller");

		return package_id;
	}
}

[CCode (has_target = false)]
private static uint module_hash_func (Module dep_mod) {
	string str = dep_mod.idname;
	return str_hash (str);
}

[CCode (has_target = false)]
private static bool module_equal_func (Module a, Module b) {
	if (a.idname == b.idname)
		return true;
	return false;
}

private HashSet<Module> module_hashset_new () {
	return new HashSet<Module> ((HashFunc) module_hash_func, (EqualFunc) module_equal_func);
}

} // End of namespace: Listaller.Dep
