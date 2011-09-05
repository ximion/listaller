/* utils.vala
 *
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller {

private static bool __debug_errors_fatal = false;
private static bool __unittestmode = false;

private static void li_info (string msg) {
	stdout.printf (" I:" + " " + msg + "\n");
}

private static void li_warning (string msg) {
	if (__debug_errors_fatal) {
		warning (msg);
	} else {
		stdout.printf (" W:" + " " + msg + "\n");
	}
}

private static void li_error (string msg) {
	if (__debug_errors_fatal) {
		error (msg);
	} else {
		stderr.printf ("[error]" + " " + msg + "\n");
	}
}

/* This stuff is defined to be able to use the PackageKit PkBackend directly in Listaller.
 * It allows us to call the native backend instead of invoking a new PackageKit native transaction,
 * which can never work if there already is a PackageKit Listaller transaction. (it would come to
 * a deadlock then)
 * The PkBackend reference is set by Listaller's PK plugin.and should _only_ be set by it. It can only
 * be used if the user is root (because PK is a root-daemon, if PkBackend is set and we're a unprivileged
 * user, something is going wrong or someone did a foolish thing.
 */
private class PkBackend { }
private PkBackend pkit_native_backend = null;

private void set_pkit_backend (void* backend) {
	if (!Utils.is_root ()) {
		error ("Tried to set a PackageKit native backend, but application does not run as root (and therefore can not " +
			"have been called from packagekitd) This should NEVER happen, maybe someone is using the API wrong.");
		return;
	}
	pkit_native_backend = backend as PkBackend;
}

private PkBackend? get_pk_backend () {
	if (!Utils.is_root ())
		return null;
	return pkit_native_backend;
}

}

namespace Listaller.Utils {

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

private bool touch_dir (string dirname) {
		File d = File.new_for_path (dirname);
		try {
			if (!d.query_exists ()) {
				d.make_directory_with_parents ();
			}
		} catch (Error e) {
			li_error ("Unable to create directories! Error: %s".printf (e.message));
			return false;
		}
		return true;
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
private string system_architecture () {
	Posix.utsname uts = Posix.utsname ();
	return "%s-%s".printf (uts.sysname.down (), uts.machine);
}

private string system_os () {
	Posix.utsname uts = Posix.utsname ();
	return uts.sysname.down ();
}

private string system_machine () {
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
		return false;
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

public static string fold_user_dir (string path) {
	if ((path == null) || (path == ""))
		return "";
	string udir = Environment.get_home_dir ();
	if (!path.has_prefix (udir))
		return path;

	string folded_path = path.replace (udir, "~");
	return folded_path;
}

public static string expand_user_dir (string path) {
	if (!path.has_prefix ("~"))
		return path;

	string full_path = path.substring (1);
	full_path = Path.build_filename (Environment.get_home_dir (), full_path, null);
	return full_path;
}

private string concat_binfiles (string afname, string bfname) {
	const int BUFFER_SIZE = 512;
	//TODO: This can be done better, but it's easier to debug

	string ofname = Path.build_filename (afname, "..", "..", "combined.tmp", null);
	File f = File.new_for_path (ofname);
	FileOutputStream fo_stream = null;

	try {
		if (f.query_exists (null))
			f.delete (null);
		fo_stream = f.create(FileCreateFlags.REPLACE_DESTINATION, null);
	}
	catch(Error e) {
		li_error ("Cannot create file. %s\n".printf (e.message));
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

} // End of namespace
