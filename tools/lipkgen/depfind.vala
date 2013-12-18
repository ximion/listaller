/* depfind.vala - Search & group dependencies using the "depscan" tool
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU General Public License Version 3
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
 */

using GLib;
using Gee;
using Listaller;

namespace Listaller {

private class DepFind : Object {
	private string input_dir;
	private string module_dir;

	public DepFind (string indir, string mod_dir) {
		input_dir = indir;
		module_dir = mod_dir;
	}

	public ArrayList<string>? get_dependency_list () {
		string output;
		string[] cmd = { "depscan", "-rc", "--include-modules=%s".printf (module_dir), input_dir };

		try {
			Process.spawn_sync (input_dir, cmd, null,
					SpawnFlags.SEARCH_PATH, null, out output, null, null);
		} catch (Error e) {
			error ("Unable to scan dependencies: %s", e.message);
			return null;
		}
		output = output.escape ("\n").replace ("\\r", "");
		string[] deps = output.split ("\n");
		ArrayList<string> deplist = new ArrayList<string> ();
		for (uint i = 0; i < deps.length; i++)
			if ((deps[i] != null) && (deps[i] != "")) {
				deplist.add (deps[i]);
			}
		return deplist;
	}

	public string get_auto_dependencies (string? defined_deps = null) {
		pkbuild_action ("Scanning for dependencies...");
		string deplist = "";

		ArrayList<string> dependency_data = get_dependency_list ();
		string unknown_deps = "";
		bool have_unknown_deps = false;
		foreach (string s in dependency_data) {
			if (s.has_prefix ("----")) {
				have_unknown_deps = true;
				continue;
			}
			if (have_unknown_deps) {
				unknown_deps = "%s%s\n".printf (unknown_deps, s);
				continue;
			}
			// we don't add dependencies which were already defined by the user
			if (defined_deps.index_of (s) >= 0)
				continue;

			if (deplist == "")
				deplist = s;
			else
				deplist = "%s, %s".printf (deplist, s);

		}

		if (unknown_deps != "") {
			warning ("The packaged application has unknown dependencies: %s", unknown_deps);
		}

		return deplist;
	}
}

} // End of namespace
