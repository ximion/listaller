/* varsolver.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
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

namespace Listaller {

private class Variable {
	private string _var;
	private string _su_subst;
	private string _subst;
	private string _id_subst;
	private bool _sysvar;

	public string var {
		get { return _var; }
		set { _var = value; }
	}

	public string id_subst {
		get { return _id_subst; }
		set { _id_subst = value; }
	}

	public string subst {
		get { return _subst; }
		set { _subst = value; }
	}

	public string su_subst {
		get { return _su_subst; }
		set { _su_subst = value; }
	}

	public bool system_var {
		get { return _sysvar; }
		set { _sysvar = value; }
	}

}

private class VarSolver : Object {
	private Settings conf;
	private HashMap<string, Variable> pathMap;
	private delegate string LiConfGetType ();
	public string appName {get; set; }
	public bool contained_sysvars {get; set;}

	public VarSolver (string appIdName = "") {
		contained_sysvars = false;
		conf = new Settings ();

		appName = appIdName;
		if (appName == "")
			warning ("Using VarSolver without valid application-id!");

		// Build map of Listaller path variables
		pathMap = new HashMap<string, Variable> ();

		/* Define all Listaller pkg variables */
		// Application installation directory
		add_var_from_conf ("$INST", Path.build_filename ("appdata", appName, null), conf.appdata_dir, appName);
		// Desktop-file directory
		add_var_from_conf ("$APP", "desktop", conf.desktop_dir);
		// Generic dependency directory (for published dependencies, like shared libs)
		add_var_from_conf ("$DEP", "depend", conf.depdata_dir);
		// Private dependency directory: Used only for private libraries
		add_var_from_conf ("$LIB_PRIVATE", Path.build_filename ("appdata", appName, "_libs", null),
				   conf.appdata_dir, Path.build_filename (appName, "_libs", null));

		/*
		 * System variables
		 *
		 * THESE VARS SHOULD NOT BE USED IN INSTALLATIONS!
		 */
		Variable v;
		bool x = conf.sumode;
		conf.sumode = false;

		v = add_var ("$SYS_LIB", "_sys_lib", Path.build_filename (conf.depdata_dir (), "..", "_syslibs", null), conf.sys_libdir);
		v.system_var = true;
		v = add_var ("$SYS_BIN", "_sys_bin", Path.build_filename (conf.depdata_dir (), "..", "_sysbin", null), conf.sys_bindir);
		v.system_var = true;
		v = add_var ("$SYS_SHARE", "_sys_lib", Path.build_filename (conf.depdata_dir (), "..", "_sysshare", null), conf.sys_sharedir);
		v.system_var = true;
		v = add_var ("$SYS_ETC", "_sys_lib", Path.build_filename (conf.depdata_dir (), "..", "_sysetc", null), conf.sys_etcdir);
		v.system_var = true;

		conf.sumode = x;

		// Add icon sizes
		add_icon_var (0);
		add_icon_var (16);
		add_icon_var (24);
		add_icon_var (32);
		add_icon_var (48);
		add_icon_var (64);
		add_icon_var (128);
		add_icon_var (265);
	}

	private void add_icon_var (int size) {
		conf.sumode = false;
		string s = conf.icon_size_dir (size);
		conf.sumode = true;
		if (size == 0) {
			add_var ("$PIX", "icon/common", s, conf.icon_size_dir (size));
		} else {
			add_var ("$ICON-" + size.to_string (), "icon/" + size.to_string (), s, conf.icon_size_dir (size));
		}
	}

	private Variable add_var (string key, string idsubst, string subst, string susubst) {
		Variable v = new Variable ();
		v.var = key;
		v.id_subst = idsubst;
		v.subst = subst;
		v.su_subst = susubst;
		pathMap.set (v.var, v);
		return v;
	}

	private Variable add_var_from_conf (string key, string idsubst, LiConfGetType get, string appendPath = "") {
		bool x = conf.sumode;
		conf.sumode = false;
		string s = Path.build_filename (get (), appendPath, null);
		conf.sumode = true;
		Variable v = add_var (key, idsubst, s, Path.build_filename (get (), appendPath, null));
		conf.sumode = x;
		return v;
	}

	public string substitute_vars_su (string s) {
		contained_sysvars = false;
		string res = s;
		foreach (var entry in pathMap.entries) {
			res = string_replace (res, "(\\" + entry.key + ")", entry.value.su_subst);
			if (entry.value.system_var)
				contained_sysvars = true;
		}
		return res;
	}

	public string substitute_vars_home (string s) {
		string res = s;
		foreach (var entry in pathMap.entries) {
			res = string_replace (res, "(\\" + entry.key + ")", entry.value.subst);
			if (entry.value.system_var)
				contained_sysvars = true;
		}
		return res;
	}

	public string substitute_vars_id (string s) {
		string res = s;
		foreach (var entry in pathMap.entries) {
			res = string_replace (res, "(\\" + entry.key + ")", entry.value.id_subst);
			if (entry.value.system_var)
				contained_sysvars = true;
		}
		return res;
	}

	public string substitute_vars_auto (string s, Listaller.Settings conf) {
		string res = s;
		// Substitute vars by config option
		if (conf.sumode) {
			res = substitute_vars_su (s);
		} else {
			res = substitute_vars_home (s);
		}
		// Check for testmode
		if (conf.testmode) {
			res = Path.build_filename (conf.get_unique_install_tmp_dir (), substitute_vars_id (s), null);
		}
		return res;
	}

	public string substitute_path_with_vars (string path) {
		string res = path;
		foreach (var entry in pathMap.entries) {
			res = string_replace (res, "(\\" + entry.value.su_subst + ")", entry.key);
			res = string_replace (res, "(\\" + entry.value.subst + ")", entry.key);
			if (entry.value.system_var)
				contained_sysvars = true;
		}
		return res;
	}
}

class VersionNumber : Object {
	private int major;    // x.0.0
	private int minor;    // 0.x.0
	private int revision; // 0.0.x
	private string originalString;

	public VersionNumber (string version)
	{
		originalString = version;
		try
		{
			var regex = new Regex("([[:digit:]]*)\\.([[:digit:]]*)\\.*([[:digit:]]*)");
			var split = regex.split (version);
			assert (split.length > 1); // TODO: Don't use assert, print a nice error message instead
			major = int.parse (split[1]);
			if (split.length > 2)
			{
				minor = int.parse (split[2]);
			}
			else
			{
				minor = 0;
			}
			if (split.length > 3)
			{
				revision = int.parse (split[3]);
			}
			else
			{
				revision = 0;
			}
		}
		catch (GLib.RegexError e)
		{
			warning ("Error compiling regular expression!");
		}
	}

	public bool newerThan (VersionNumber other)
	{
		if (major > other.major)
		{
			return true;
		}
		else if (major == other.major)
		{
			if (minor > other.minor)
			{
				return true;
			}
			else if (minor == other.minor)
			{
				if (revision > other.revision)
				{
					return true;
				}
			}
		}
		return false;
	}

	public string to_string ()
	{
		return originalString;
	}
}

} // End of namespace
