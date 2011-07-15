/* depinfo.vala
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

namespace Listaller.IPK {

public class Dependency : Object {
	public string name { get; set; }
	public string summary { get; set; }
	public string description { get; set; }
	public string homepage { get; set; }
	public string version { get; set; }

	public bool satisfied { get; set; }
	public string architecture { get; set; } // e.g. linux-amd64
	public HashSet<string> meta_info { get; set; }

	public string feed_url { get; set; }
	public ArrayList<string> files { get; set; }
	public string data { get; set; }

	public bool is_standardlib { get; set; }

	internal Dependency (string dep_name) {
		name = dep_name;
		satisfied = false;
		is_standardlib = false;

		files = new ArrayList<string> ();
		meta_info = new HashSet<string> ();
		data = "";
		feed_url = "";
		version = "0";
	}

	public string get_id () {
		// Form unique dependency-id
		string id = "%s-%s".printf (name, version);
		id = id.down ().replace (" ", "_");
		return id;
	}
}

} // Endof namespace: Listaller.IPK

namespace Listaller {

private class DepInfo : Object {
	private ArrayList<IPK.Dependency> dlist;

	enum DepInfoBlock {
		UNKNOWN,
		NAME,
		FILES;
	}

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
			DepInfoBlock mode = DepInfoBlock.UNKNOWN;

			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				if (line.has_prefix ("#"))
					continue;

				if (line.strip () == "") {
					if ((dep != null) && (dep.name != ""))
						dlist.add (dep);
					dep = new IPK.Dependency ("");
					mode = DepInfoBlock.UNKNOWN;
					continue;
				}
				if (line.down ().has_prefix ("name:")) {
					dep.name = line.substring (line.index_of (":") + 1).strip ();
					continue;
				}
				if (line.down ().has_prefix ("feed:")) {
					dep.feed_url = line.substring (line.index_of (":") + 1).strip ();
					continue;
				}
				if (line.down ().has_prefix ("standard:")) {
					string s = line.substring (line.index_of (":") + 1).strip ();
					if (s.down () == "true")
						dep.is_standardlib = true;
					continue;
				}
				if (line.down ().has_prefix ("files:")) {
					string s = line.substring (line.index_of (":") + 1).strip ();
					if (s != "")
						dep.files.add (s);
					mode = DepInfoBlock.FILES;
					continue;
				}
				if (line.substring (0, 1) == " ") {
					if (mode == DepInfoBlock.FILES)
						dep.files.add (line.strip ());

				}
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
