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
	private uint current_block_id;

	public MetaFile () {
		content = null;
	}

	public bool open_file (string fname) {
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

	public bool open_block_by_value (string field, string value) {
		// TODO
		return false;
	}

	public string get_value (string field, string value) {
		// TODO
		return "";
	}

	public void reset () {
		current_block_id = 0;
	}

}

} // End of namespace: Listaller.IPK
