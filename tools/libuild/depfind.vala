/* depfind.vala
 *
 * Copyright (C) 2011 Matthias Klumpp
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

	public DepFind (string indir) {
		input_dir = indir;
	}

	public ArrayList<string>? get_dependency_list () {
		string output;

		string[] cmd = { "depscan", "-r", "--simpletext", "." };
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

	public ArrayList<IPK.Dependency> get_dependencies () {
		ArrayList<IPK.Dependency> deplist = new ArrayList<IPK.Dependency> ();

		pkbuild_action ("Scanning for dependencies...");
		DepInfo dinfo = new DepInfo ();

		// TODO: There are way too much foreach () loops here (and in DepInfo) - maybe there is a smarter
		//	and faster way to do this...

		ArrayList<string> files = get_dependency_list ();
		foreach (string s in files) {
			IPK.Dependency dtmp = dinfo.get_dep_template_for_file (s);
			string dep_name = "";
			if (dtmp != null)
				dep_name = dtmp.full_name;
			else
				dep_name = Utils.string_replace (s, "(\\.so|\\+|\\.)", "");

			if (dep_name.strip () == "") {
				debug ("dep_name would be empty for %s!", s);
				continue;
			}

			bool done = false;
			foreach (IPK.Dependency d in deplist) {
				if (d.full_name == dep_name) {
					d.files.add (s.strip ());
					done = true;
					break;
				}
			}
			if (done)
				continue;

			// If we are here, we need to create a new dependency object
			IPK.Dependency dep = new IPK.Dependency (dep_name);
			if (dtmp != null) {
				dep.idname = dtmp.idname;
				dep.version = dtmp.version;
				dep.feed_url = dtmp.feed_url;
				dep.is_standardlib = dtmp.is_standardlib;
			}
			dep.files.add (s);

			deplist.add (dep);
		}

		// Remove default dependencies
		uint i = 0;
		while (i < deplist.size) {
			IPK.Dependency dep = deplist.get ((int) i);
			if (dep.is_standardlib) {
				deplist.remove (dep);
				continue;
			}
			if (PatternSpec.match_simple ("libnvidia-*", dep.full_name)) {
				deplist.remove (dep);
				continue;
			}

			i++;
		}

		return deplist;
	}
	}

} // End of namespace
