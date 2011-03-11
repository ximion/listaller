/* ipkfilelist.vala
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
private const string _PKG_VERSION7 = Config.VERSION;

private class IPKFileEntry : Object {
	private string _destination;
	private string _fname;
	private string _hash;

	public string fname {
		get { return _fname; }
		set { _fname = value; }
	}

	public string destination {
		get { return _destination; }
		set { _destination = value; }
	}

	public string hash {
		get { return _hash; }
		set { _hash = value; }
	}

	public IPKFileEntry () {
		destination = "";
		fname = "";
		hash = "";
	}

	public string get_full_filename () {
		string s = Path.build_filename (destination, fname, null);
		return s;
	}

	public string to_string () {
		return "[ (" + fname + ") to (" + destination + ") hash: (" + hash + ") ]";
	}

}

private class IPKFileList : Object {
	private LinkedList<string> text;

	public IPKFileList () {
		text = new LinkedList<string> ();
		text.add ("# IPK file list");
		text.add ("");
	}

	public bool open (string fname) {

		var file = File.new_for_path (fname);

		if (!file.query_exists ()) {
			warning (_("File '%s' doesn't exist."), file.get_path ());
			return false;
		}

		text.clear ();
		try {
			// Read the IPK file list into our text list
			var dis = new DataInputStream (file.read ());
			string line;
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				text.add (line);
			}
		} catch (Error e) {
			error ("%s", e.message);
		}

		return true;
	}

	public bool save (string fname)  {
		if (FileUtils.test (fname, FileTest.EXISTS)) {
			warning (_("Could not create file '%s'! File already exists."), fname);
			return false;
		}
		var file = File.new_for_path (fname);

		{
		try {
			var file_stream = file.create (FileCreateFlags.NONE);

			if (!file.query_exists ()) {
				warning (_("Unable to create file '%s'!"), file.get_path ());
				return false;
			}
			// Write IPK text list to output stream
			var dos = new DataOutputStream (file_stream);
			string line;
			foreach (string s in text) {
				dos.put_string (s + "\n");
			}
		} catch (Error e) {
			error ("%s", e.message);
		}
		}

		return true;
	}

	private int get_folder_index (string folder, bool create = false) {
		// Build folder string
		string s = "> " + folder;
		int i = text.index_of (s);
		// Add folder to filelist, if it does not exist
		if ((create) && (i < 0)) {
			text.add (s);
			i = text.index_of (s);
		}
		return i;
	}

	public bool add_file (string fname, string fdest) {
		// Get index of destination dir
		int findex = get_folder_index (fdest, true);
		findex++;

		string checksum = compute_checksum_for_file (fname);
		if (checksum == "")
			return false;
		text.insert (findex, checksum);
		text.insert (findex, Path.get_basename (fname));

		return true;
	}

	public ArrayList<IPKFileEntry> get_files () {
		string current_dir = "";
		Iterator<string> it = text.iterator ();
		ArrayList<IPKFileEntry> flist = new ArrayList<IPKFileEntry> ();

		it.first ();
		while (it.has_next ()) {
			// Go to the next entry (entry at position 0 is always the header string)
			it.next ();
			string s = it.get ();
			if (s.length <= 2)
				continue;
			if (s.substring (0, 2) == "> ") {
				current_dir = s.substring (2, s.length - 2);
				continue;
			}
			if (current_dir == "")
				continue;
			IPKFileEntry e = new IPKFileEntry ();
			e.fname = s;
			if (!it.next ())
				break;
			e.hash = it.get ();
			e.destination = current_dir;
			flist.add (e);
		}
		return flist;
	}

	public string to_string () {
		string res = "";
		foreach (string s in text) {
			res += s + "\n";
		}
		return res;
	}
}
