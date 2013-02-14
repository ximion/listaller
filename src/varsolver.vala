/* varsolver.vala
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

/**
 * A List of all allowed Listaller global variables (except for SYS_*)
 * is stored in docs/pages/directory-substvars.page
 * Please keep this document up-to-date!
 */

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
	private SetupSettings tmp_ssettings;
	private Config conf;
	private HashMap<string, Variable> pathMap;
	private delegate string LiConfGetType ();

	public string swName {get; set; }
	public bool contained_sysvars {get; set;}

	public VarSolver (string softwareIdName = "") {
		contained_sysvars = false;
		tmp_ssettings = new SetupSettings ();
		conf = new Config ();

		swName = softwareIdName;
		if (swName == "")
			warning ("Using VarSolver without valid application-id or resource-id!");

		// Build map of Listaller path variables
		pathMap = new HashMap<string, Variable> ();

		/* Define all Listaller pkg variables */
		// Application installation directory
		add_var_from_conf ("INST", Path.build_filename ("appdata", swName, null), tmp_ssettings.appdata_dir, swName);
		// Desktop-file directory
		add_var_from_conf ("APP", "desktop", tmp_ssettings.applications_dir);
		// Generic dependency directory (for published dependencies, like shared libs)
		add_var_from_conf ("DEP", Path.build_filename ("depend", swName, null), tmp_ssettings.depdata_dir, swName);
		// Private dependency directory: Used only for private libraries
		add_var_from_conf ("LIB_PRIVATE", Path.build_filename ("appdata", swName, "_libs", null),
				   tmp_ssettings.appdata_dir, Path.build_filename (swName, "_libs", null));

		/*
		 * System variables
		 *
		 * THESE VARS SHOULD NOT BE USED IN INSTALLATIONS!
		 */
		Variable v;
		IPK.InstallMode x = tmp_ssettings.current_mode;
		tmp_ssettings.current_mode = IPK.InstallMode.PRIVATE;

		v = add_var ("SYS_LIB", "_sys_lib", Utils.build_filename (tmp_ssettings.depdata_dir (), "..", "_syslibs", null), conf.sys_libdir);
		v.system_var = true;
		v = add_var ("SYS_BIN", "_sys_bin", Utils.build_filename (tmp_ssettings.depdata_dir (), "..", "_sysbin", null), conf.sys_bindir);
		v.system_var = true;
		v = add_var ("SYS_SHARE", "_sys_lib", Utils.build_filename (tmp_ssettings.depdata_dir (), "..", "_sysshare", null), conf.sys_sharedir);
		v.system_var = true;
		v = add_var ("SYS_ETC", "_sys_lib", Utils.build_filename (tmp_ssettings.depdata_dir (), "..", "_sysetc", null), conf.sys_etcdir);
		v.system_var = true;

		tmp_ssettings.current_mode = x;

		// Add icon sizes
		add_icon_var (0);
		add_icon_var (16);
		add_icon_var (24);
		add_icon_var (32);
		add_icon_var (48);
		add_icon_var (64);
		add_icon_var (128);
		add_icon_var (256);
	}

	private void add_icon_var (int size) {
		tmp_ssettings.current_mode = IPK.InstallMode.PRIVATE;
		string i_privdir = tmp_ssettings.icon_size_dir (size);
		tmp_ssettings.current_mode = IPK.InstallMode.SHARED;
		var i_sudir = tmp_ssettings.icon_size_dir (size);

		if (size == 0) {
			add_var ("PIX", "icon/common", i_privdir, i_sudir);
		} else {
			add_var ("ICON-" + size.to_string (), "icon/" + size.to_string (), i_privdir, i_sudir);
		}
	}

	private Variable add_var (string key, string idsubst, string subst, string susubst) {
		// make the keyname a keystring
		string key_str = "%" + key + "%";
		Variable v = new Variable ();
		v.var = key_str;
		v.id_subst = idsubst;
		v.subst = subst;
		v.su_subst = susubst;
		pathMap.set (v.var, v);
		return v;
	}

	private Variable add_var_from_conf (string key, string idsubst, LiConfGetType get, string appendPath = "") {
		IPK.InstallMode x = tmp_ssettings.current_mode;
		tmp_ssettings.current_mode = IPK.InstallMode.PRIVATE;
		string s = Path.build_filename (get (), appendPath, null);
		tmp_ssettings.current_mode = IPK.InstallMode.SHARED;
		Variable v = add_var (key, idsubst, s, Path.build_filename (get (), appendPath, null));
		tmp_ssettings.current_mode = x;
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
		if (res.has_prefix ("%")) {
			// We have a unknown variable - emit a warning and send file to the appdata directory
			Report.log_warning (_("Package uses custom variables - usually this is not intentional, please contact the package author!"));
			res = Path.build_filename (pathMap.get ("%INST%").su_subst, res.substring (1));
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
		if (res.has_prefix ("%")) {
			// We have a unknown variable - emit a warning and send file to the appdata directory
			Report.log_warning (_("Package uses custom variables - usually this is not intentional, please contact the package author!"));
			res = Path.build_filename (pathMap.get ("%INST%").subst, res.substring (1));
		}
		return res;
	}

	public string substitute_vars_id (string s) {
		string res = s;
		foreach (var entry in pathMap.entries) {
			res = res.replace (entry.key, entry.value.id_subst);
			if (entry.value.system_var)
				contained_sysvars = true;
		}
		if (res.has_prefix ("%")) {
			// We have a unknown var... this should not happen.
			Report.log_warning (_("Package uses custom variables - usually this is not intentional, please contact the package author!"));
			res = res.substring (1);
		}

		return res;
	}

	public string substitute_vars_auto (string s, SetupSettings setup_settings) {
		string res = s;
		// Substitute vars by config option
		if (setup_settings.shared_mode) {
			res = substitute_vars_su (s);
		} else {
			res = substitute_vars_home (s);
		}
		// Check for testmode
		if (setup_settings.test_mode) {
			if (!s.has_prefix ("/"))
				res = Path.build_filename (setup_settings.get_unique_install_tmp_dir (), substitute_vars_id (s), null);
			else
				res = substitute_vars_id (s);
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

	private string find_icon_imagefile (string fname) {
		string img = fname;
		if (FileUtils.test (img, FileTest.EXISTS))
			return img;
		img = fname + ".png";
		if (FileUtils.test (img, FileTest.EXISTS))
			return img;
		img = fname + ".xpm";
		if (FileUtils.test (img, FileTest.EXISTS))
			return img;
		img = fname + ".svg";
		if (FileUtils.test (img, FileTest.EXISTS))
			return img;
		return "";
	}

	private string find_liappicon (string icon_name, int size, SetupSettings setup_settings) {
		string v;
		if (size == 0)
			v = "%PIX%";
		else
			v = "%ICON-%i".printf (size);
		string fname = Path.build_filename (substitute_vars_auto (v, setup_settings), icon_name, null);
		fname = find_icon_imagefile (fname);
		if (fname != "") {
			return fname;
		}
		return "";
	}

	public string find_icon_in_ivarpaths (string icon_name, SetupSettings? setup_settings = null) {
		SetupSettings? ssettings = setup_settings;
		if (ssettings == null)
			ssettings = new SetupSettings (IPK.InstallMode.PRIVATE);

		string fname;
		fname = find_liappicon (icon_name, 0, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 64, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 48, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 128, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 256, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 32, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 24, ssettings);
		if (fname != "")
			return fname;
		fname = find_liappicon (icon_name, 16, ssettings);
		if (fname != "")
			return fname;
		return icon_name;
	}

	public string find_exe_in_varpath (string exe_name, SetupSettings? setup_settings = null) {
		SetupSettings? ssettings = setup_settings;
		if (ssettings == null)
			ssettings = new SetupSettings (IPK.InstallMode.PRIVATE);

		string fname = Path.build_filename (substitute_vars_auto ("%INST%", ssettings), exe_name, null);
		if (FileUtils.test (fname, FileTest.EXISTS)) {
			return fname;
		}
		return exe_name;
	}

	public static string autosubst_instvars (string varstr, string swName, IPK.InstallMode inst_mode = IPK.InstallMode.PRIVATE) {
		VarSolver vs = new VarSolver (swName);
		SetupSettings? ssettings = new SetupSettings (inst_mode);

		return vs.substitute_vars_auto (varstr, ssettings);
	}
}


} // End of namespace
