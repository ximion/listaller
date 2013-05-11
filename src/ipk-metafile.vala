/* ipk-metafile.vala - Work with IPK meta-info files
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

namespace Listaller.IPK {

private class MetaFile : Object {
	private LinkedList<string>? content;
	private int currentBlockId;

	public MetaFile () {
		content = new LinkedList<string> ();
	}

	private bool add_data_from_file (string fname, bool strip_comments = true) {
		var file = File.new_for_path (fname);
		if (!file.query_exists ()) {
			return false;
		}

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
			warning (_("Unable to get meta-information list: %s").printf (e.message));
			content = null;
			return false;
		}
		return true;
	}

	public bool open_file (string fname, bool strip_comments = true) {
		content.clear ();

		return add_data_from_file (fname, strip_comments);
	}

	public bool open_file_add_data (string fname, bool strip_comments = true) {
		content.add ("");
		var ret = add_data_from_file (fname, strip_comments);
		if (!ret)
			content.remove_at (content.size);

		return ret;
	}

	public bool add_data (string data, bool strip_comments = true) {
		if (data == null)
			return false;

		if (content.size > 0)
			content.add ("");

		string[] lines = data.split ("\n");

		foreach (string line in lines) {
				if (strip_comments)
					if (line.has_prefix ("#"))
						continue;

				content.add (line);
		}
		// kill empty line at the end
		if (content.last () == "")
			content.poll_tail ();

		return true;
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

	public bool open_block_by_value (string field, string value, bool reset_index = true) {
		if (reset_index)
			reset ();
		var iter = content.list_iterator ();
		iter.first ();

		if (content.size == 0) {
			currentBlockId = -1;
			return false;
		}

		do {
			string line = iter.get ();
			if ((line.down ().has_prefix (field.down () + ":")) &&
			(line.substring (line.index_of (":") + 1).strip ().down () == value.down ())) {
				currentBlockId = iter.index ();

				while (iter.has_previous ()) {
					line = iter.get ();
					if (is_empty (line)) {
						currentBlockId = iter.index () + 1;
						return true;
					}
					iter.previous ();
				}
				return true;
			}
		} while (iter.next ());
		currentBlockId = -1;

		return false;
	}

	public bool open_block_by_field (string field, bool reset_index = false) {
		if (reset_index)
			reset ();
		var iter = content.list_iterator ();

		if (content.size == 0) {
			currentBlockId = -1;
			return false;
		}

		bool start = false;
		if (currentBlockId < 0)
			start = true;

		if (!iter.first ())
			return false;
		do {
			if (iter.index () < currentBlockId)
				continue;

			string line = iter.get ();
			if (is_empty (line))
				start = true;
			if (!start)
				continue;

			if (line.down ().has_prefix (field.down () + ":")) {
				currentBlockId = iter.index () - 1;

				while (iter.has_previous ()) {
					line = iter.get ();
					if (is_empty (line)) {
						currentBlockId = iter.index () + 1;
						return true;
					}
					iter.previous ();
				}
				return true;
			}
		} while (iter.next ());
		currentBlockId = -1;

		return false;
	}

	/**
	 * Open the first block found in our MetaFile
	 */
	public bool open_block_first () {
		reset ();
		var iter = content.list_iterator ();

		bool start = false;

		if (!iter.first ())
			return false;
		do {
			string line = iter.get ();
			if (is_empty (line))
				start = true;
			if (!start)
				continue;

			while (iter.has_next ()) {
				line = iter.get ();
				if (!is_empty (line)) {
					currentBlockId = iter.index ();
					return true;
				}
				iter.next ();
			}

		} while (iter.next ());

		return false;
	}

	public bool block_next () {
		var iter = content.list_iterator ();

		if (!iter.first ())
			return false;
		do {
			if (iter.index () < currentBlockId)
				continue;

			string line = iter.get ();

			if (is_empty (line)) {
				currentBlockId = iter.index () -1;
				while (iter.has_next ()) {
					line = iter.get ();
					if (is_empty (line)) {
						currentBlockId = iter.index () + 1;
						if (iter.index () == content.size)
							return false;
						return true;
					}
					iter.next ();
				}
			}

		} while (iter.next ());

		return false;
	}

	public string get_value (string field, bool respectOpenedBlock = true) {
		string res = "";
		bool addToBlock = false;
		var iter = content.list_iterator ();

		if (!iter.first ())
			return "";
		do {
			if (iter.index () < currentBlockId)
				continue;

			string line = iter.get ();

			if (is_empty (line))
				if (respectOpenedBlock)
					break;
				else
					continue;


			if (addToBlock) {
				if (line.substring (0, 1) == " ") {
					res += "\n" + line.strip ();
				} else {
					break;
				}
			}

			if (line.down ().has_prefix (field.down () + ":")) {
				res = line.substring (line.index_of (":") + 1).strip ();
				addToBlock = true;
			}

		} while (iter.next ());

		return res;
	}

	public bool has_field (string field, bool respectOpenedBlock = true) {
		bool ret = false;
		var iter = content.list_iterator ();

		if (!iter.first ())
			return false;
		do {
			if (iter.index () < currentBlockId)
				continue;

			string line = iter.get ();

			if (is_empty (line))
				if (respectOpenedBlock)
					break;
				else
					continue;

			if (line.down ().has_prefix (field.down () + ":")) {
				ret = true;
				break;
			}

		} while (iter.next ());

		return ret;
	}


	/**
	 * Add new value to index
	 *
	 * @param field Field name to add the value to
	 * @param value The string value to be set
	 * @param open_new_block TRUE if new block should be created, starting with this field/value pair. (Default: FALSE)
	 *
	 * @return TRUE on success.
	 */
	public bool add_value (string field, string value, bool open_new_block = false) {
		if (field == "")
			return false;
		if (value == "")
			return true;

		// Prepare the value
		string[] newValue = value.split ("\n");

		if ((open_new_block) || (content.size <= 0))
			currentBlockId = -1;

		if (currentBlockId < 0) {
			// Create a new field
			if (content.size > 0)
				content.add ("");

			if (newValue.length == 1) {
				content.add ("%s: %s".printf (field, newValue[0]));
			} else {
				content.add ("%s: %s".printf (field, newValue[0]));
				for (int i = 1; i < newValue.length; i++) {
					if ((newValue[i] != null) && (newValue[i].strip () != ""))
						content.add (" " + newValue[i]);
				}
			}
			currentBlockId = content.size;
		} else {
			// Insert into existing field
			var iter = content.list_iterator ();
			iter.first ();
			int location = -1;

			while (iter.has_next ()) {
				if (iter.index () < currentBlockId) {
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
					if ((newValue[i] != null) && (newValue[i].strip () != "")) {
						content.insert (location, " " + newValue[i]);
						location++;
					}
				}
			}
		}

		return true;
	}

	/**
	 * Update a field value in the current opened block.
	 * If the field does not yet exist, it will be created.
	 *
	 * @param field Field name
	 * @param value Updated value for field
	 *
	 * @return TRUE on success.
	 */
	public bool update_value (string field, string value) {
		if (content.size == 0) {
			currentBlockId = -1;
			return false;
		}

		var iter = content.list_iterator ();

		if (!iter.first ())
			return false;

		bool remove_mlfield = false;
		do {
			if (iter.index () < currentBlockId)
				continue;

			string line = iter.get ();
			if (remove_mlfield) {
				// when in remove mode and we have a multiline field, remove all extra lines
				// break if new field starts (removal completed)
				if (line.has_prefix (" "))
					iter.remove ();
				else
					break;
			}

			// if the line is empty, we are in front of a new block - stop processing there.
			if (is_empty (line))
				break;

			if (line.down ().has_prefix (field.down () + ":")) {
				iter.remove ();
				remove_mlfield = true;
			}
		} while (iter.next ());

		bool ret = add_value (field, value, false);

		return ret;
	}

	internal void _test_print () {
		foreach (string s in content) {
			stdout.printf (s + "\n");
		}
	}

	public void clear () {
		reset ();
		content.clear ();
	}

	public void reset () {
		currentBlockId = -1;
	}

}

} // End of namespace: Listaller.IPK
