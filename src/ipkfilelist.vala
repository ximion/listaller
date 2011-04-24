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
using Listaller;

namespace Listaller.IPK {

private class FileEntry : Object {
	private string _destination;
	private string _fname;
	private string _hash;
	private string _fname_installed;
	private bool _installed;

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

	public bool installed {
		get { return _installed; }
		set { _installed = value; }
	}

	public string fname_installed {
		get { return _fname_installed; }
		set { _fname_installed = value; }
	}

	public FileEntry () {
		destination = "";
		fname = "";
		hash = "";
		installed = false;
	}

	public string get_full_filename () {
		string s = Path.build_filename (destination, fname, null);
		return s;
	}

	public string to_string () {
		return "[ (" + fname + ") to (" + destination + ") hash: (" + hash + ") ]";
	}

}

private class FileList : Object {
	private LinkedList<string> text;
	private bool has_hashes;
	public string rootdir { set; get; }

	public FileList (bool with_hashes = true) {
		has_hashes = with_hashes;
		text = new LinkedList<string> ();
		text.add ("# IPK file list");
		text.add ("");
		rootdir = Environment.get_current_dir ();
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

		foreach (string line in text) {
			// Search for header information
			if (line.has_prefix ("::")) {
				// Check for rootdir info
				int index = line.index_of ("rootdir=");
				if (index < 1)
					continue;
				string[] s = line.split ("=");
				if (s[1] == null) {
					warning ("Syntax error in fileinfo file!");
					continue;
				}
				rootdir = s[1].strip ();
			}
			// If the fileinfo part starts, we can't define header information
			if (line.has_prefix (">"))
				break;
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

	private bool add_file_internal (string fname, string fdest, string checksum) {
		// Get index of destination dir
		int findex = get_folder_index (fdest, true);
		findex++;

		if (checksum == "")
			return false;
		text.insert (findex, checksum);
		text.insert (findex, Path.get_basename (fname));

		return true;
	}

	public bool add_file (string fname, string fdest) {
		string checksum = compute_checksum_for_file (fname);
		return add_file_internal (fname, fdest, checksum);
	}

	public ArrayList<FileEntry> get_files_list () {
		string current_dir = "";
		Iterator<string> it = text.iterator ();
		ArrayList<FileEntry> flist = new ArrayList<FileEntry> ();

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
			FileEntry e = new FileEntry ();
			e.fname = s;
			// Only add hash value if list has hashes
			if (has_hashes) {
				if (!it.next ())
					break;
				e.hash = it.get ();
			}
			e.destination = current_dir;
			flist.add (e);
		}
		return flist;
	}

	public bool set_files_list (ArrayList<FileEntry> flist) {
		text.clear ();
		text.add ("# IPK File List");
		text.add ("");

		foreach (FileEntry fe in flist) {
			if (!add_file_internal (fe.fname, fe.destination, fe.hash)) {
				// warning ("Tried to add invalid IPK FileEntry to file list!");
				return false;
			}
		}
		return true;
	}

	public string to_string () {
		string res = "";
		foreach (string s in text) {
			res += s + "\n";
		}
		return res;
	}
}

} // End of namespace
