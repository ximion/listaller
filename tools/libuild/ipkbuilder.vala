/* ipkbuilder.vala
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
using Archive;
using Listaller;

// Workaround for Vala bug #618931
private const string _PKG_VERSION14 = Config.VERSION;

namespace Listaller.IPK {

private class Builder : Object {
	private string tmpdir;
	private string srcdir;
	private IPK.Script ipks;
	private IPK.FileList ipkf;

	public signal void error_message (string details);
	public signal void message (MessageItem message);

	public Builder (string input_dir) {
		srcdir = input_dir;
		Listaller.Settings conf = new Listaller.Settings ();
		tmpdir = conf.get_unique_tmp_dir ("ipkbuild");
		ipks = new IPK.Script ();
		ipkf = new IPK.FileList ();
	}

	~Builder () {
		// Remove workspace
		delete_dir_recursive (tmpdir);
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		message (item);
		warning (msg);
	}

	private void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
	}

	private void emit_error (string details) {
		error_message (details);
	}

	private bool validate_srcdir (string dir) {
		// Check if IPK sources are present
		string tmp = dir;
		if (FileUtils.test (tmp, FileTest.IS_DIR)) {
			if (FileUtils.test (Path.build_filename (tmp, "control.xml", null), FileTest.EXISTS) &&
			    FileUtils.test (Path.build_filename (tmp, "files-current.list", null), FileTest.EXISTS)) {
				// Set current source dir and exit
				srcdir = tmp;
				return true;
			}
		}
		return false;
	}

	private bool write_ipk_data (ArrayList<IPK.FileEntry> src, string arch = "") {
		const int buffsize = 8192;
		char buff[8192];
		bool ret = true;
		if ((arch == null) || (arch == "")) {
			arch = "all";
		}

		Write a = new Write ();
		// Define archive format
		a.set_compression_xz ();
		a.set_format_pax_restricted ();
		// Open output
		create_dir_parents (Path.build_filename (tmpdir, "data", null));
		string apath = Path.build_filename (tmpdir, "data", "data-" + arch + ".tar.xz", null);
		a.open_filename (apath);
		VarSolver vs = new VarSolver ();

		Entry entry = new Entry ();
		foreach (IPK.FileEntry fe in src) {
			// Prepare
			entry.clear ();
			// Grab filepath
			string fname;
			if (!Path.is_absolute (fe.fname)) {
				fname = Path.build_filename (srcdir, "..", fe.fname, null);
			} else {
				fname = fe.fname;
			}
			fe.fname = Path.get_basename (fe.fname);
			// Fetch file details
			Posix.Stat st;
			Posix.stat (fname, out st);
			if (st.st_size <= 0) {
				debug ("File %s not found.", fname);
				ret = false;
				break;
			}
			entry.set_pathname (vs.substitute_vars_id (fe.get_full_filename ()));
			entry.set_size (st.st_size);
			entry.set_filetype (0100000); // AE_IFREG
			entry.set_perm (0644);
			a.write_header (entry);
			int fd = Posix.open (fname, Posix.O_RDONLY);
			ssize_t len = Posix.read (fd, buff, buffsize);
			while (len > 0) {
				a.write_data (buff, len);
				len = Posix.read (fd, buff, buffsize);
			}
			Posix.close (fd);
		}
		a.close ();
		return ret;
	}

	public bool initialize () {
		// Check for valid installer source dirs
		if (!validate_srcdir (Path.build_filename (srcdir, "ipkinstall", null)))
			if (!validate_srcdir (Path.build_filename (srcdir, "install", null)))
				if (!validate_srcdir (Path.build_filename (srcdir, "data", "install", null))) {
					//: IPk buolder could not find IPK source scripts
					emit_error (_("Could not find IPK source files!"));
					return false;
				}
		return true;
	}

	public bool build_ipk () {
		bool ret = false;

		// Load definitions
		ipks.load_from_file (Path.build_filename (srcdir, "control.xml", null));
		IPK.FileList flist = new IPK.FileList (false);
		flist.open (Path.build_filename (srcdir, "files-current.list", null));

		//TODO: Build IPK here!
		ret = write_ipk_data (flist.get_files ());
		return ret;
	}

}

} // End of namespace
