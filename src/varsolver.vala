/* varsolver.vala
 *
 * Copyright (C) 2011  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using GLib;
using Gee;

// Workaround for Vala bug #618931
private const string _PKG_VERSION11 = Config.VERSION;

private class LiVariable {
	private string _var;
	private string _su_subst;
	private string _subst;
	private string _id_subst;

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

}
private class VarSolver : Object {
	private HashMap<string, LiVariable> pathMap;
	delegate string LiConfGetType ();

	public VarSolver () {
		LiSettings conf = new LiSettings ();
		conf.sumode = true;
		string su_appdir = conf.appdata_dir ();
		conf.sumode = false;
		string appdir = conf.appdata_dir ();

		// Build map of Listaller path variables
		pathMap = new HashMap<string, LiVariable> ();
		add_var ("$INST", "inst", appdir, su_appdir);
		add_var_from_conf ("$APP", "desktop", conf, conf.desktop_dir);
	}

	private void add_var (string key, string idsubst, string subst, string susubst) {
		LiVariable v = new LiVariable ();
		v.var = key;
		v.id_subst = idsubst;
		v.subst = subst;
		v.su_subst = susubst;
		pathMap.set (v.var, v);
	}

	private void add_var_from_conf (string key, string idsubst, LiSettings conf, LiConfGetType get) {
		bool x = conf.sumode;
		conf.sumode = false;
		var s = get ();
		conf.sumode = true;
		add_var (key, idsubst, s, get ());
		conf.sumode = x;
	}

	public string substitute_vars_su (string s) {
		string res = s;
		foreach (var entry in pathMap.entries) {
			res = string_replace (res, "(\\" + entry.key + ")", entry.value.su_subst);
		}
		return res;
	}
}
