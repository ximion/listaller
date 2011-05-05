/* util.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
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

namespace Listaller {

public ulong timeval_to_ms (TimeVal time_val) {
	return (((ulong) time_val.tv_sec) * 1000) + (((ulong) time_val.tv_usec) / 1000);
}

public ulong now_ms () {
	return timeval_to_ms (TimeVal());
}

public ulong now_sec () {
	TimeVal time_val = TimeVal ();

	return time_val.tv_sec;
}

private string string_replace (string str, string regex_str, string replace_str) {
	string res = str;
	try {
		var regex = new Regex (regex_str);
		res = regex.replace (str, -1, 0, replace_str);
	} catch (RegexError e) {
		warning ("%s", e.message);
	}
	return res;
}

/*
 * Count the appearance of string b in a
 */
private int count_str (string a, string b) {
	if (!(b in a))
		return 0;

	int count = -1;
	int last_index = 0;

	while (last_index >= 0) {
		count++;
		last_index = a.index_of (b, last_index + 1);
	}

	return count;
}

private bool is_root () {
	if (Posix.getuid () == 0) {
		return true;
	} else {
		return false;
	}
}

/*
 * Calculate checksum for file
 */
private string compute_checksum_for_file (string fname, ChecksumType cstype = ChecksumType.SHA1) {
	Checksum cs;
	uchar data [1024];
	size_t size = 0;

	cs = new Checksum (cstype);
	Posix.FILE input = Posix.FILE.open (fname, "rb" );

	// Return empty string if we were unable to open the file
	if (input == null) {
		return "";
	}

	// Build the checksum
	do {
		size = Posix.read (input.fileno (), (void*) data, 1024);
		cs.update (data, size);
	} while (size == 1024);
	Posix.close (input.fileno ());

	string sum = cs.get_string ();
	return sum;
}

/*
 * Remove folder like rm -r does
 */
private bool delete_dir_recursive (string dirname) {
	try {
		if (!FileUtils.test (dirname, FileTest.IS_DIR))
			return true;
		File dir = File.new_for_path (dirname);
		FileEnumerator enr = dir.enumerate_children ("standard::name", FileQueryInfoFlags.NOFOLLOW_SYMLINKS);
		if (enr != null) {
			FileInfo info = enr.next_file ();
			while (info != null) {
				string path = Path.build_filename (dirname, info.get_name ());
				if (FileUtils.test (path, FileTest.IS_DIR)) {
					delete_dir_recursive (path);
				} else {
					FileUtils.remove (path);
				}
				info = enr.next_file ();
			}
			if (FileUtils.test (dirname, FileTest.EXISTS))
				DirUtils.remove (dirname);
		}
	} catch (Error e) {
		critical ("Could not remove directory: %s", e.message);
		return false;
	}
	return true;
}

/*
 * Fetch current system architecture
 */
private string system_architecture () {
	Posix.utsname uts = Posix.utsname ();
	return uts.machine;
}

/*
 * Create directory structure
 */
private bool create_dir_parents (string dirname) {
	File d = File.new_for_path (dirname);
	try {
		if (!d.query_exists ()) {
			d.make_directory_with_parents ();
		}
	} catch (Error e) {
		warning ("Could not create directory: %s", e.message);
		return false;
	}
	return true;
}

private ArrayList<string>? find_files (string dir, bool recursive = false) {
	ArrayList<string> list = new ArrayList<string> ();
	try {
		var directory = File.new_for_path (dir);

		var enumerator = directory.enumerate_children (FILE_ATTRIBUTE_STANDARD_NAME, 0);

		FileInfo file_info;
		while ((file_info = enumerator.next_file ()) != null) {
			string path = Path.build_filename (dir, file_info.get_name (), null);
			if (file_info.get_is_hidden ())
				continue;
			if ((!FileUtils.test (path, FileTest.IS_REGULAR)) && (recursive)) {
				ArrayList<string> subdir_list = find_files (path, recursive);
				// There was an error, exit
				if (subdir_list == null)
					return null;
				list.add_all (subdir_list);
			} else {
				list.add (path);
			}
		}

	} catch (Error e) {
		stderr.printf (_("Error: %s\n"), e.message);
		return null;
	}
	return list;
}

} // End of namespace
