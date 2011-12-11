/* ipk-metafile.vala - Work with IPK meta-info files
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@tenstral.net>
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

private class MetaFile : Object {
	private ArrayList<string>? content;
	private int current_block_id;

	public MetaFile () {
		content = null;
	}

	public bool open_file (string fname, bool strip_comments = true) {
		if (content != null)
			return false;

		var file = File.new_for_path (fname);
		if (!file.query_exists ()) {
			return false;
		}

		content = new ArrayList<string> ();

		try {
			string line;
			var dis = new DataInputStream (file.read ());
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				if (strip_comments)
					if (line.has_prefix ("#"))
						continue;
				content.add (line);
			}

		} catch (Error e) {
			li_error (_("Unable to get meta-information list: %s").printf (e.message));
			content = null;
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

	public bool open_block_by_value (string field, string value) {
		reset ();
		var iter = content.list_iterator ();
		iter.first ();

		while (iter.has_next ()) {
			string line = iter.get ();
			if ((line.down ().has_prefix (field.down () + ":")) &&
			(line.substring (line.index_of (":") + 1).strip ().down () == value.down ())) {
				current_block_id = iter.index ();
				while (iter.has_previous ()) {
					line = iter.get ();
					if (is_empty (line)) {
						current_block_id = iter.index () + 1;
						return true;
					}
					iter.previous ();
				}
				break;
			}
			iter.next ();
		}
		return false;
	}

	public string get_value (string field) {
		string res = "";
		bool addToBlock = false;

		var iter = content.list_iterator ();
		iter.first ();

		while (iter.has_next ()) {
			if (iter.index () < current_block_id) {
				iter.next ();
				continue;
			}

			string line = iter.get ();
			if (is_empty (line))
				break;

			if ((addToBlock) && (line.substring (0, 1) == " ")) {
				res += "\n" + line.strip ();
			}

			if (line.down ().has_prefix (field.down () + ":")) {
				res = line.substring (line.index_of (":") + 1).strip ();
				addToBlock = true;
			}
			iter.next ();
		}
		return res;
	}

	public bool add_value (string field, string value) {
		if (field == "")
			return false;

		// Prepare the value
		string[] newValue = value.split ("\n");

		if (current_block_id == 0) {
			// Create a new field
			if (newValue.length == 1) {
				content.add ("%s: %s".printf (field, newValue[0]));
				content.add ("");
			} else {
				content.add ("%s: %s".printf (field, newValue[0]));
				for (int i = 1; i < newValue.length; i++) {
					content.add (" " + newValue[i]);
				}
				content.add ("");
			}
		} else {
			// Insert into existing field
			var iter = content.list_iterator ();
			iter.first ();
			int location = -1;

			while (iter.has_next ()) {
				if (iter.index () < current_block_id) {
					iter.next ();
					continue;
				}
				if (is_empty (iter.get ())) {
					location = iter.index ();
					break;
				}
				iter.next ();
			}
			if (location < 0)
				location = content.size;

			if (newValue.length == 1) {
				content.insert (location, "%s: %s".printf (field, newValue[0]));

			} else {
				content.insert (location, "%s: %s".printf (field, newValue[0]));
				location++;
				for (int i = 1; i < newValue.length; i++) {
					content.insert (location, " " + newValue[i]);
					location++;
				}
			}
		}
		return true;
	}

	public void _test_print () {
		foreach (string s in content) {
			stdout.printf (s + "\n");
		}
	}

	public void reset () {
		current_block_id = 0;
	}

}

} // End of namespace: Listaller.IPK
