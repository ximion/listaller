/* depinfo.vala
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

private class DepInfo : Object {
	private ArrayList<IPK.Dependency> dlist;

	public DepInfo () {
		Listaller.Settings conf = new Listaller.Settings (true);
		string fname = Path.build_filename (conf.conf_dir (), "dependencies.list", null);

		dlist = new ArrayList<IPK.Dependency> ();
		var file = File.new_for_path (fname);
		if (!file.query_exists ()) {
			return;
		}

		try {
			var dis = new DataInputStream (file.read ());
			string line;
			IPK.Dependency? dep = null;
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				if ((line == "") || (line.has_prefix ("#")))
					continue;
				if (line.substring (0, 1) != " ") {
					if (dep != null)
						dlist.add (dep);
					dep = new IPK.Dependency (line.substring (0, line.index_of (":")).strip ());
					string s = line.substring (line.index_of (":") + 1).strip ();
					if (s != "")
						dep.files.add (s);
					continue;
				}
				dep.files.add (line.strip ());
			}
			if (dep != null)
				dlist.add (dep);
		} catch (Error e) {
			li_error (_("Unable to fetch dependency information list: %s").printf (e.message));
			return;
		}
	}

	public IPK.Dependency? get_dep_template_for_file (string fname) {
		foreach (IPK.Dependency dep in dlist) {
			foreach (string s in dep.files) {
				if (fname == s)
					return dep;
				if (PatternSpec.match_simple (s, fname))
					return dep;
			}
		}
		return null;
	}

}

} // End of namespace
