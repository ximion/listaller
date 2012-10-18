/* repo-list.vala - Access Listaller repository lists
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Repo {

private class ListFile : Object {
	private LinkedList<string>? content;

	public ListFile () {
		content = new LinkedList<string> ();
	}

	private bool add_data_from_file (string fname) {
		var file = File.new_for_path (fname);
		if (!file.query_exists ()) {
			return false;
		}

		try {
			string line;
			var dis = new DataInputStream (file.read ());
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				content.add (line);
			}

		} catch (Error e) {
			warning (_("Unable to get meta-information list: %s").printf (e.message));
			content = null;
			return false;
		}
		return true;
	}

	public bool open_file (string fname) {
		content.clear ();

		return add_data_from_file (fname);
	}

	public bool open_file_add_data (string fname) {
		content.add ("");
		var ret = add_data_from_file (fname);
		if (!ret)
			content.remove_at (content.size);

		return ret;
	}

	public string get_data () {
		string out_str = "";

		foreach (string line in content) {
			out_str = "%s%s\n".printf (out_str, line);
		}

		return out_str;
	}

	public bool save_to_file (string fname, bool overrideExisting = false) {
		if (content.size > 0) {
			if (content.last ().strip () == "")
				content.remove_at (content.size - 1);
		} else {
			/* Create an empty file to prevent other modules from crashing
			 * if "save_to_file" does not result in a file existing */
			content.add ("");
		}

		var file = File.new_for_path (fname);
		if (file.query_exists ())
			if (!overrideExisting)
				return false;
			else
				file.delete ();

		try {
			var file_stream = file.create (FileCreateFlags.NONE);
			var data_stream = new DataOutputStream (file_stream);

			// Write contents of this metalist to file
			foreach (string line in content) {
					data_stream.put_string (line + "\n");
			}

		} catch (Error e) {
			warning (_("Unable to write meta information! Message: %s").printf (e.message));
			return false;
		}

		return true;
	}

	private bool is_empty (string line) {
		string s = line.down ().strip ();
		if ((s == "") || (s.has_prefix ("#")))
			return true;

		return false;
	}

	public void clear () {
		content.clear ();
	}

}

} // End of namespace: Listaller.Repo
