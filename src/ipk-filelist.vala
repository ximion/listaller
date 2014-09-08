/* ipk-filelist.vala - The IPK-style list of files
 *
 * Copyright (C) 2011-2014 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller.Utils;

namespace Listaller.IPK {

private enum FileEntryType {
	UNKNOWN,
	FILE,
	DIRECTORY;
}

private class FileEntry : Object {
	private string _destination;
	private string _fname;
	private string _hash;
	private string _fname_installed;
	private FileEntryType _type;

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

	public string fname_installed {
		get { return _fname_installed; }
		set { _fname_installed = value; }
	}

	public FileEntryType entry_type {
		get { return _type; }
		set { _type = value; }
	}

	public FileEntry () {
		entry_type = FileEntryType.FILE;
		destination = "";
		fname = "";
		hash = "";
	}

	public string get_full_filename () {
		string s = Path.build_filename (destination, fname, null);
		return s;
	}

	public bool is_installed () {
		if ((fname_installed == null) || (fname_installed == ""))
			return false;
		return true;
	}

	public string to_string () {
		return "[ (" + fname + ") to (" + destination + ") hash: (" + hash + ") ]";
	}

}

[CCode (has_target = false)]
private static uint fileentry_hash_func (FileEntry fe) {
	string str = fe.fname + fe.destination;
	return str_hash (str);
}

[CCode (has_target = false)]
private static bool fileentry_equal_func (FileEntry a, FileEntry b) {
	if ((a.fname == b.fname) && (a.destination == b.destination))
		return true;
	return false;
}

private HashSet<FileEntry> fileentry_hashset_new () {
	return new HashSet<FileEntry> (fileentry_hash_func, fileentry_equal_func);
}

/**
 * Represents a list of files
 *
 * List of files as stored in IPK files.
 */
private class FileList : Object {
	private HashSet<FileEntry> list;
	private bool has_hashes;
	public string rootdir { set; get; }

	/** Simple comment to identify the file-list */
	public string comment { set; get; }

	public FileList (bool with_hashes = true) {
		has_hashes = with_hashes;
		comment = "IPK file list";
		list = fileentry_hashset_new ();

		rootdir = Environment.get_current_dir ();
	}

	public bool open (string fname) {
		bool ret;

		// clear the internal list
		list.clear ();
		ret = data_append_listfile (fname);

		return ret;
	}

	public bool data_append_listfile (string fname) {
		var file = File.new_for_path (fname);

		if (!file.query_exists ()) {
			warning (_("File '%s' doesn't exist."), file.get_path ());
			return false;
		}

		var text = new ArrayList<string> ();
		try {
			// Read the IPK file list into our text list
			var dis = new DataInputStream (file.read ());
			string line;
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				text.add (line);
			}
		} catch (Error e) {
			error ("%s".printf (e.message));
		}

		string current_dir = "";
		BidirListIterator<string> it = text.bidir_list_iterator ();

		it.first ();
		while (it.has_next ()) {
			// Go to the next entry (entry at position 0 is always the header string)
			it.next ();
			string s = it.get ();
			if (s.length <= 2)
				continue;
			if (s.has_prefix ("#"))
				continue;
			if (s.substring (0, 3) == ":: ") {
				current_dir = s.substring (3, s.length - 3);
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
			list.add (e);
		}

		return true;
	}

	public bool save (string fname)  {
		if (FileUtils.test (fname, FileTest.EXISTS)) {
			warning (_("Could not create file '%s'! File already exists."), fname);
			return false;
		}
		var file = File.new_for_path (fname);

		var text = to_stringlist ();
		if (text == null)
			return false;

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
				error ("%s".printf (e.message));
			}
		}

		return true;
	}

	private int textlist_get_folder_index (ArrayList<string> text, string folder, bool create = false) {
		// Build folder string
		string s = ":: " + folder;
		int i = text.index_of (s);
		// Add folder to filelist, if it does not exist
		if ((create) && (i < 0)) {
			text.add (s);
			i = text.index_of (s);
		}
		return i;
	}

	private bool textlist_add_file (ArrayList<string> text, string fname, string fdest, string checksum) {
		// Get index of destination dir
		int findex = textlist_get_folder_index (text, fdest, true);
		findex++;

		if (checksum == "")
			return false;
		text.insert (findex, checksum);
		text.insert (findex, Path.get_basename (fname));

		return true;
	}

	private ArrayList<string>? to_stringlist () {
		var text = new ArrayList<string> ();
		text.add ("# %s".printf (comment));
		text.add ("");

		foreach (FileEntry fe in list) {
			if (!textlist_add_file (text, fe.fname, fe.destination, fe.hash))
				return null;
		}
		return text;
	}

	public bool add_file (string fname, string fdest) {
		string checksum = compute_checksum_for_file (fname);
		if (checksum == "")
			return false;

		var fe = new FileEntry ();
		fe.fname = fname;
		fe.destination = fdest;
		fe.hash = checksum;
		list.add (fe);
		return true;
	}

	public HashSet<FileEntry> get_files_list () {
		return list;
	}

	private void refine_fileentry_add_to_list (ArrayList<FileEntry> resList, FileEntry fe, string root_dir, string fname) {
		FileEntry e = new FileEntry ();

		// Create secure filenames, so Listaller doesn't try to rename files with spaces in their name
		var fs = fname;
		if (fs.index_of (" ") > 0)
			fs = "'%s'".printf (fs);

		e.fname = fs;
		e.destination = Path.build_filename (fe.destination, Path.get_dirname (fname).replace (root_dir, ""), null);
		resList.add (e);
	}

	public ArrayList<FileEntry> get_files_list_expanded () {
		var resList = new ArrayList<FileEntry> ();
		ArrayList<FileEntry> wcEntries = new ArrayList<FileEntry> ();

		resList.add_all (list);
		foreach (FileEntry fe in resList) {
			if ((fe.fname.index_of ("*") > -1) ||
				(fe.fname.index_of ("?") > -1)) {
					var s = fe.fname;
					if ((s.index_of (" ") > -1) && (s.last_index_of ("'") != s.length))
						warning ("Malformed file listing line - you cannot use wildcards in file-renaming lines! (Line was: %s)", s);
					else
						wcEntries.add (fe);
			}
		}

		foreach (FileEntry fe in wcEntries) {
			string dir = Path.build_filename (rootdir, Path.get_dirname (fe.fname), null);
			// rootdir might contain relative paths...
			dir = real_path (dir);

			// find files, not recursively
			HashSet<string> files = find_files (dir, false);

			resList.remove (fe);
			if (files == null)
				continue;

			foreach (string s in files) {
				string ematch = Path.build_filename ("*", fe.fname, null);
				if (PatternSpec.match_simple (ematch, s)) {
					// in case the wildcard matched a directory, we add the whole thing
					if (FileUtils.test (s, FileTest.IS_DIR)) {
						HashSet<string> subfiles = find_files (s, true);
						foreach (string sf in subfiles) {
							refine_fileentry_add_to_list (resList, fe, dir, sf);
						}
					} else {
						// else just add the matched file
						refine_fileentry_add_to_list (resList, fe, dir, s);
					}
				}
			}
		}

		return resList;
	}

	public void set_files_list (ArrayList<FileEntry> flist) {
		list.clear ();
		foreach (FileEntry fe in flist)
			list.add (fe);
	}

	public string to_string () {
		string res = "";
		var strlist = to_stringlist ();
		if (strlist == null)
			return "<empty>";

		foreach (string s in strlist) {
			res += s + "\n";
		}
		return res;
	}
}

} // End of namespace
