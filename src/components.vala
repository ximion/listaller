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
	public string homepage { get; internal set; }

	public string environment { get; internal set; }

	public bool installed { get; internal set; }

	protected string _version_raw;
	private string _version_cache;

	protected HashSet<string> item_list { get; set; } // Parts of this dependency (e.g. shlibs, python modules, files, etc.)

	public Component () {
		idname = "";
		environment = "";
		_full_name = "";

		item_list = new HashSet<string> ();
	}

	protected bool contains_directive (string str) {
		return ((str.index_of ("shell$") > 0) || (str.index_of ("prefix$") > 0) || (str.index_of ("envvar$") > 0));
	}

	private string get_directive_value (string directive) {
		string s = directive;
		if (s.has_prefix ("shell$ "))
			return s.substring (7);
		else if (s.has_prefix ("envvar$ "))
			return s.substring (8);
		else if (s.has_prefix ("prefix$ "))
			return s.substring (8);
		else if (s.has_prefix ("textfile$ "))
			return s.substring (10);
		else if (s.has_prefix ("regex$ "))
			return s.substring (7);
		else
			return s;
	}

	protected string process_directives (string directive_str) throws ComponentError {
		// first grab all data from shell commands and env vars
		string rawdata = "";
		string[] parts = directive_str.split ("\n");
		foreach (string str in parts) {
			string s = str.strip ();
			if (s.has_prefix ("shell$ ")) {
				int exit_status;
				string cmd = get_directive_value (s);
				Process.spawn_command_line_sync (cmd, out rawdata, null, out exit_status);
				if (exit_status != 0)
					throw new ComponentError.DIRECTIVES_RESOLVE_FAILED ("Unable to resolve directives for %s: Command %s returned error-code %i.", full_name, cmd, exit_status);
			} else if (s.has_prefix ("envvar$ ")) {
				int exit_status;
				string varname = get_directive_value (s);
				rawdata = Environment.get_variable (varname);
			}
		}
		foreach (string str in parts) {
			if (str.has_prefix ("prefix$")) {
				// the prefix directive fetches the data after the given prefix
				if (str_empty (rawdata))
					throw new ComponentError.DIRECTIVES_INVALID ("Unable to resolve directives for %s: Get prefix-directive, but don't have valid data to apply it.", full_name);
				string prefix = get_directive_value (str);
				int i = rawdata.index_of (prefix) + prefix.length;
				var res = rawdata.substring (i, rawdata.index_of ("\n", i));

				return res;
			}
		}

		return directive_str;
	}

	public string get_version () throws ComponentError {
		if (!installed)
			throw new ComponentError.VERSION_NOT_FOUND ("Component %s is not installed, cannot retrieve version number!", full_name);
		// check if we have a cached string
		if (!str_empty (_version_cache))
			return _version_cache;

		// process version directive, if necessary
		if (!contains_directive (_version_raw))
			return _version_raw;

		string res = "";
		try {
			res = process_directives (_version_raw);
		} catch (ComponentError e) {
			throw e;
		}

		return res;
	}

	public virtual bool load_from_file (string fname) {
		var data = new IPK.MetaFile ();
		var ret = data.open_file (fname);
		if (!ret)
			return ret;

		data.open_block_first ();
		full_name = data.get_value ("Name");
		idname = data.get_value ("ID");
		_version_raw = data.get_value ("Version");

		// add item data for this component
		add_item_list (ItemType.SHARED_LIB, data.get_value ("Libraries"));
		add_item_list (ItemType.BINARY, data.get_value ("Binaries"));
		add_item_list (ItemType.PYTHON, data.get_value ("Python"));
		add_item_list (ItemType.PYTHON_2, data.get_value ("Python2"));
		add_item_list (ItemType.FILE, data.get_value ("Files"));

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

		if (list.index_of ("\n") <= 0) {
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

	public bool has_item (string cname, ItemType tp) {
		return item_list.contains (get_item_type_idstr (tp).printf (cname));
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
	public Framework () {
		base ();
	}

	public override bool load_from_file (string fname) {
		return base.load_from_file (fname);
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

	public Module () {
		base ();
	}

	public override bool load_from_file (string fname) {
		return base.load_from_file (fname);
	}
}

} // End of namespace: Listaller.Dep
