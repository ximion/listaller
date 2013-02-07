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

	public HashSet<IPK.Dependency> get_dependencies (ArrayList<IPK.Dependency>? deps = null) {
		pkbuild_action ("Scanning for dependencies...");
		var dinfo = new DepInfoGenerator ();

		// Add more template dependencies to genrator
		if (deps != null)
			dinfo.add_dependencies (deps);

		ArrayList<string> files = get_dependency_list ();
		var deplist = dinfo.get_dependency_list_for_components (files);


		// Remove default dependencies
		//! Disabled at time, we want complete IPK package deps
		/*
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
		*/

		return deplist;
	}
	}

} // End of namespace
