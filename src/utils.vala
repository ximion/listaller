/* utils.vala
 *
 * Copyright (C) 2010-2012 Matthias Klumpp <matthias@tenstral.net>
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

internal static bool __unittestmode = false;

namespace Listaller.Utils {

private ulong timeval_to_ms (TimeVal time_val) {
	return (((ulong) time_val.tv_sec) * 1000) + (((ulong) time_val.tv_usec) / 1000);
}

private ulong now_ms () {
	return timeval_to_ms (TimeVal());
}

private ulong now_sec () {
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

private bool str_empty (string? str) {
	if ((str == "") || (str == null))
		return true;
	return false;
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
private string compute_checksum_for_file (string fname, ChecksumType cstype = GLib.ChecksumType.SHA1) {
	Checksum cs;
	uchar data [4096];
	size_t size = 0;

	cs = new Checksum (cstype);
	Posix.FILE input = Posix.FILE.open (fname, "rb" );

	// Return empty string if we were unable to open the file
	if (input == null) {
		return "";
	}

	// Build the checksum
	do {
		size = Posix.read (input.fileno (), (void*) data, 4096);
		cs.update (data, size);
	} while (size == 4096);
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
private string system_osname_arch () {
	Posix.utsname uts = Posix.utsname ();
	return "%s-%s".printf (uts.sysname.down (), uts.machine);
}

private string system_os () {
	Posix.utsname uts = Posix.utsname ();
	return uts.sysname.down ();
}

private string system_os_full () {
	Posix.utsname uts = Posix.utsname ();
	return uts.sysname;
}

private string system_machine () {
	Posix.utsname uts = Posix.utsname ();
	return uts.machine;
}

private string system_osname_arch_generic () {
	return "%s-%s".printf (system_os (), arch_generic (system_machine ()));
}

private string arch_generic (string arch) {
	string res = arch;

	if (PatternSpec.match_simple ("i?86", arch))
		res = "ix86";
	if (arch == "x86_64")
		res = "amd64";

	return res;
}

private string system_machine_generic () {
	return arch_generic (system_machine ());
}

/**
 * Create directory structure
 */
private bool create_dir_structure (string dirname) {
	File d = File.new_for_path (dirname);
	try {
		if (!d.query_exists ()) {
			d.make_directory_with_parents ();
		}
	} catch (Error e) {
		warning ("Unable to create directories! Error: %s".printf (e.message));
		return false;
	}

	return true;
}

private HashSet<string>? find_files_matching (string dir, string pattern, bool recursive = false) {
	var list = new HashSet<string> ();
	try {
		var directory = File.new_for_path (real_path (dir));

		var enumerator = directory.enumerate_children (FileAttribute.STANDARD_NAME, 0);

		FileInfo file_info;
		while ((file_info = enumerator.next_file ()) != null) {
			string path = Path.build_filename (dir, file_info.get_name (), null);

			if (file_info.get_is_hidden ())
				continue;
			if ((!FileUtils.test (path, FileTest.IS_REGULAR)) && (recursive)) {
				HashSet<string> subdir_list = find_files_matching (path, pattern, recursive);
				// There was an error, exit
				if (subdir_list == null)
					return null;
				list.add_all (subdir_list);
			} else {
				if (pattern != "") {
					string fname = file_info.get_name ();
					if (!PatternSpec.match_simple (pattern, fname))
						continue;
				}
				list.add (real_path (path));
			}
		}

	} catch (Error e) {
		stderr.printf (_("Error while finding files in directory %s: %s") + "\n", dir, e.message);
		return null;
	}
	return list;
}

private HashSet<string>? find_files (string dir, bool recursive = false) {
	return find_files_matching (dir, "", recursive);
}

private string? find_dir_containing_file (string dir, string pattern, bool recursive = false) {
	try {
		var directory = File.new_for_path (real_path (dir));

		var enumerator = directory.enumerate_children (FileAttribute.STANDARD_NAME, 0);

		FileInfo file_info;
		while ((file_info = enumerator.next_file ()) != null) {
			string path = Path.build_filename (dir, file_info.get_name (), null);
			if (file_info.get_is_hidden ())
				continue;
			if ((!FileUtils.test (path, FileTest.IS_REGULAR)) && (recursive)) {
				string? resDir = find_dir_containing_file (path, pattern, recursive);
				// A directory was found, exit
				if (resDir != null)
					return resDir;
			} else {
				if (PatternSpec.match_simple (pattern, file_info.get_name ()))
					return dir;
			}
		}

	} catch (Error e) {
		stderr.printf (_("Error while finding files in directory %s: %s") + "\n", dir, e.message);
		return null;
	}
	return null;
}

/* convert ArrayList to zero-terminated string array */
[CCode (array_length = false, array_null_terminated = true)]
private string[]? array_list_to_strv (ArrayList<string> list) {
	// if the list is empty, return null
	if (list.size == 0)
		return null;
	string[] strv = {};
	foreach (string s in list)
		strv += s;
	strv += null;
	return strv;
}

private ArrayList<string>? strv_to_array_list ([CCode (array_length = false, array_null_terminated = true)]
						string[]? strv) {
	if (strv == null)
		return null;
	ArrayList<string> list = new ArrayList<string> ();

	for (uint i = 0; strv[i] != null; i++) {
		list.add (strv[i]);
	}
	return list;
}

private string strv_to_string ([CCode (array_length = false, array_null_terminated = true)] string[]? strv) {
	if (strv == null)
		return "";

	string res = "";
	for (uint i = 0; strv[i] != null; i++) {
		res += strv[i] + "\n";
	}

	return res;
}

private bool move_file (string source, string destination) throws Error {
	try {
		var file = File.new_for_path (source);

		if (!file.query_exists ()) {
			return false;
		}

		// Make a copy
		var dest = File.new_for_path (destination);
		if (dest.query_exists ()) {
			//!
		}
		file.copy (dest, FileCopyFlags.NONE);

		// Delete original
		file.delete ();
	} catch (Error e) {
		throw e;
	}

	return true;
}

private bool copy_file (string source, string destination) throws Error {
	try {
		var file = File.new_for_path (source);

		if (!file.query_exists ()) {
			return false;
		}

		// Make a copy
		var dest = File.new_for_path (destination);
		if (dest.query_exists ()) {
			//!
		}
		file.copy (dest, FileCopyFlags.NONE);

	} catch (Error e) {
		throw e;
	}

	return true;
}

private bool dir_is_empty (string dirname) {
	int n = 0;
	Posix.DirEnt *d;
	Posix.Dir dir = Posix.opendir (dirname);

	if (dir == null)
		return false;
	while ((d = Posix.readdir (dir)) != null)
		n++;

	return n == 0;
}

private string fold_user_dir (string? path) {
	if ((path == null) || (path == ""))
		return "";
	string udir = Environment.get_home_dir ();
	if (!path.has_prefix (udir))
		return path;

	string folded_path = path.replace (udir, "~");
	return folded_path;
}

private string expand_user_dir (string path) {
	if (!path.has_prefix ("~"))
		return path;

	string full_path = path.substring (1);
	full_path = Path.build_filename (Environment.get_home_dir (), full_path, null);
	return full_path;
}

private bool string_is_valid (string str) {
	return (str != null) && (str != "");
}

private static bool ends_with_dir_separator (string s) {
		return Path.is_dir_separator (s.get_char (s.length - 1));
}

/* ported from glibc */
private static string __realpath (string name) {
	string rpath;

	// start of path component
	weak string start;
	// end of path component
	weak string end;

	if (!Path.is_absolute (name)) {
		// relative path
		rpath = Environment.get_current_dir ();

		start = end = name;
	} else {
		// set start after root
		start = end = Path.skip_root (name);

		// extract root
		rpath = name.substring (0, (int) ((char*) start - (char*) name));
	}

	long root_len = (long) ((char*) Path.skip_root (rpath) - (char*) rpath);

	for (; start.get_char () != 0; start = end) {
		// skip sequence of multiple path-separators
		while (Path.is_dir_separator (start.get_char ())) {
			start = start.next_char ();
		}

		// find end of path component
		long len = 0;
		for (end = start; end.get_char () != 0 && !Path.is_dir_separator (end.get_char ()); end = end.next_char ()) {
			len++;
		}

		if (len == 0) {
			break;
		} else if (len == 1 && start.get_char () == '.') {
			// do nothing
		} else if (len == 2 && start.has_prefix ("..")) {
			// back up to previous component, ignore if at root already
			if (rpath.length > root_len) {
				do {
					rpath = rpath.substring (0, rpath.length - 1);
				} while (!ends_with_dir_separator (rpath));
			}
		} else {
			if (!ends_with_dir_separator (rpath)) {
				rpath += Path.DIR_SEPARATOR_S;
			}

			rpath += start.substring (0, len);
		}
	}

	if (rpath.length > root_len && ends_with_dir_separator (rpath)) {
		rpath = rpath.substring (0, rpath.length - 1);
	}

	if (Path.DIR_SEPARATOR != '/') {
		// don't use backslashes internally,
		// to avoid problems in #include directives
		string[] components = rpath.split ("\\");
		rpath = string.joinv ("/", components);
	}

	return rpath;
}

/**
 * real_path:
 *
 * Resolves paths like ../../Desktop/foobar.ipk to /home/matthias/Desktop/foobar.ipk
 * TODO: We should use canonicalize_filename() in gio/glocalfile.c as realpath()
 * is crap.
 **/
private string? real_path (string path)
{
	string temp;

	// don't trust realpath one little bit
	if (!string_is_valid (path))
		return null;

	temp = __realpath (path);
	if (temp != null)
		return temp;
	return path;
}

public static string li_build_filename (string first_element, ...) {
	string path = first_element;

	var l = va_list ();
	while (true) {
		string? val = l.arg ();
		if (val == null) {
			break;  // end of the list
		}
		path = Path.build_filename (path, val, null);
	}

	path = real_path (path);

	return path;
}

internal string? load_file_to_string (string fname) throws IOError {
	var file = File.new_for_path (fname);
	if (!file.query_exists ()) {
		return null;
	}

	string res = "";
	try {
		string line;
		var dis = new DataInputStream (file.read ());
		// Read lines until end of file (null) is reached
		while ((line = dis.read_line (null)) != null) {
			res += line + "\n";
		}

	} catch (IOError e) {
		throw e;
	}
	return res;
}

private bool save_string_to_file (string fname, string data, bool overrideExisting = false) throws IOError {
	var file = File.new_for_path (fname);
	if ( (!overrideExisting) && (file.query_exists ()))
		return false;

	try {
		var file_stream = file.create (FileCreateFlags.NONE);
		var data_stream = new DataOutputStream (file_stream);

		data_stream.put_string (data);

	} catch (IOError e) {
		throw e;
	}
	return true;
}

private string concat_binfiles (string afname, string bfname) {
	const int BUFFER_SIZE = 512;
	//TODO: This can be done better, but it's easier to debug

	string ofname = real_path (Path.build_filename (afname, "..", "..", "combined.tmp", null));
	File f = File.new_for_path (ofname);
	FileOutputStream fo_stream = null;

	try {
		if (f.query_exists (null))
			f.delete (null);
		fo_stream = f.create(FileCreateFlags.REPLACE_DESTINATION, null);
	}
	catch(Error e) {
		warning ("Cannot create file. %s\n".printf (e.message));
		return "";
	}

	var afile = File.new_for_path (afname);
	var bfile = File.new_for_path (bfname);

	var file_stream = afile.read ();
	var data_stream = new DataInputStream (file_stream);
	data_stream.set_byte_order (DataStreamByteOrder.LITTLE_ENDIAN);


	// Seek and read the image data chunk
	uint8[] buffer = new uint8[BUFFER_SIZE];
	file_stream.seek (0, SeekType.CUR);
	while (data_stream.read (buffer) > 0)
		fo_stream.write (buffer);

	file_stream = bfile.read ();
	data_stream = new DataInputStream (file_stream);

	file_stream.seek (0, SeekType.CUR);
	while (data_stream.read (buffer) > 0)
		fo_stream.write (buffer);

	return f.get_path ();
}

} // End of namespace: Listaller.Utils
