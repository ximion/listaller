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

namespace Listaller.IPK {

private class Builder : Object {
	private string tmpdir;
	private string srcdir;
	private string outname;
	private string outdir;
	private bool failed = false;
	private IPK.Script ipks;
	private ArrayList<string> ctrlfiles;
	private ArrayList<string> datapkgs;
	private AppItem appInfo;

	public signal void error_message (string details);
	public signal void message (MessageItem message);

	public string output_dir {
		get { return outdir; }
		set { outdir = value; }
	}

	public Builder (string input_dir) {
		srcdir = input_dir;
		Listaller.Settings conf = new Listaller.Settings ();
		tmpdir = conf.get_unique_tmp_dir ("ipkbuild");
		ipks = new IPK.Script ();
		outdir = "";
		outname = "";
		ctrlfiles = new ArrayList<string> ();
		datapkgs = new ArrayList<string> ();
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
		failed = true;
	}

	private void pkinfo_hint (string msg) {
		stdout.printf (" H: " + msg);
	}

	private void pkinfo_info (string msg) {
		stdout.printf (" I: " + msg);
	}

	private void pkinfo_warning (string msg) {
		stdout.printf (" W: " + msg);
	}

	private void pkinfo_error (string msg) {
		stdout.printf (" E: " + msg);
	}

	private bool write_ipk_file_data (ref ArrayList<IPK.FileEntry> src, string rdir, string arch = "") {
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
		VarSolver vs = new VarSolver (appInfo.appid);

		Entry entry = new Entry ();
		foreach (IPK.FileEntry fe in src) {
			// Prepare
			entry.clear ();
			// Grab filepath
			string fname;
			if (!Path.is_absolute (fe.fname)) {
				fname = Path.build_filename (rdir, fe.fname, null);
			} else {
				fname = fe.fname;
			}
			fe.fname = Path.get_basename (fe.fname);
			fe.hash = compute_checksum_for_file (fname);
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
		// Add this payload package to data pkg list
		datapkgs.add (apath);
		return ret;
	}

	private bool build_ipk_files_structure (IPK.FileList flist, string arch = "") {
		bool ret = false;
		if ((arch == null) || (arch == "")) {
			arch = "all";
		}

		// Set the correct install dir
		if (flist.rootdir == "%INSTDIR%") {
			flist.rootdir = Path.build_filename (srcdir, "installtarget", null);
		} else
			if (!Path.is_absolute (flist.rootdir))
				flist.rootdir = Path.build_filename (srcdir, flist.rootdir, null);

		string rdir = flist.rootdir;

		ArrayList<IPK.FileEntry> fileslst = flist.get_files_list_expanded ();
		ret = write_ipk_file_data (ref fileslst, rdir, arch);
		if (!ret) {
			error_message ("Unable to write IPK payload - do all files exist?");
			return false;
		}
		ret = flist.set_files_list (fileslst);
		if (!ret) {
			error_message ("Could not build IPK file list!");
			return false;
		}
		string tmp = Path.build_filename (tmpdir, "control", "files-" + arch + ".list", null);
		ret = flist.save (tmp);
		// Add to control file list, if file was created successfully
		if (ret)
			ctrlfiles.add (tmp);
		return ret;
	}

	private bool write_ipk_control_data () {
		const int buffsize = 8192;
		char buff[8192];
		bool ret = true;

		Write a = new Write ();
		// Define archive format
		a.set_compression_xz ();
		a.set_format_pax_restricted ();
		// Open output
		string apath = Path.build_filename (tmpdir, "control", "control.tar.xz", null);
		a.open_filename (apath);

		Entry entry = new Entry ();
		foreach (string fname in ctrlfiles) {
			// Prepare
			entry.clear ();
			// Fetch file details
			Posix.Stat st;
			Posix.stat (fname, out st);
			if (st.st_size <= 0) {
				debug ("File %s not found.", fname);
				ret = false;
				break;
			}
			entry.set_pathname (Path.get_basename (fname));
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

	private bool finalize_ipk () {
		const int buffsize = 8192;
		char buff[8192];
		bool ret = true;

		// Set output file name
		if (outname == "") {
			string ipkname;
			ipkname = appInfo.idname.down () + "-" + appInfo.version.down () + "_install.ipk";
			if (outdir == "")
				outdir = Path.build_filename (srcdir, "..", null);
			outname = Path.build_filename (outdir, ipkname, null);
		}
		// Remove spaces in filename
		outname = string_replace (outname, "( )", "-");

		if (FileUtils.test (outname, FileTest.EXISTS)) {
			error_message ("Cannot override %s! Delete this package or run libuild with '-o' parameter!".printf (outname));
			return false;
		}

		Write a = new Write ();
		// Define archive format
		a.set_compression_none ();
		a.set_format_pax_restricted ();
		// Open output
		a.open_filename (outname);

		Entry entry = new Entry ();
		// Add all data tarballs
		foreach (string fname in datapkgs) {
			// Prepare
			entry.clear ();
			// Fetch file details
			Posix.Stat st;
			Posix.stat (fname, out st);
			if (st.st_size <= 0) {
				warning ("File %s not found.", fname);
				error_message ("Internal error: Unable to find IPK payload archive!");
				ret = false;
				break;
			}
			entry.set_pathname (Path.get_basename (fname));
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
		// Add control info
		string ctrlfile = Path.build_filename (tmpdir, "control", "control.tar.xz", null);
		entry.clear ();
		// Fetch file details
		Posix.Stat st;
		Posix.stat (ctrlfile, out st);
		if (st.st_size <= 0) {
			warning ("File %s not found.", ctrlfile);
			ret = false;
			error_message ("Internal error: IPK control info tarball was not found!");
			return false;
		}
 		entry.set_pathname (Path.get_basename (ctrlfile));
		entry.set_size (st.st_size);
		entry.set_filetype (0100000); // AE_IFREG
		entry.set_perm (0644);
		a.write_header (entry);
		int fd = Posix.open (ctrlfile, Posix.O_RDONLY);
		ssize_t len = Posix.read (fd, buff, buffsize);
		while (len > 0) {
			a.write_data (buff, len);
			len = Posix.read (fd, buff, buffsize);
		}
		Posix.close (fd);

		// TODO: Sign package here

		a.close ();
		return ret;
	}

	public void set_output_ipk (string fname) {
		outname = fname;
	}

	public bool initialize () {
		// Check for valid installer source dirs
		srcdir = find_ipk_source_dir (srcdir);
		if (srcdir == null) {
			//: IPk builder was unable to find IPK source scripts
			emit_error (_("Could not find IPK source files!"));
			return false;
		}
		return true;
	}

	private string load_text_from_element (string in) {
		if (in.has_prefix ("file:")) {
			var file = File.new_for_path (Path.build_filename (srcdir, in.substring (5), null));

			if (!file.query_exists ()) {
				error_message ("File '%s' doesn't exist.".printf (file.get_path ()));
				return "";
			}
			string text = "";
			try {
				var dis = new DataInputStream (file.read ());
				string line;
				// Read file and build description
				while ((line = dis.read_line (null)) != null) {
					text += "%s\n".printf (line);
				}
			} catch (Error e) {
				error_message ("%s".printf (e.message));
				return "";
			}
			return text;
		} else {
			return in;
		}
	}

	public bool build_ipk () {
		bool ret = false;

		IPK.Control ictrl = new IPK.Control ();

		// Load definitions
		ipks.load_from_file (Path.build_filename (srcdir, "control.xml", null));
		IPK.FileList flist = new IPK.FileList (false);
		flist.open (Path.build_filename (srcdir, "files-current.list", null));

		create_dir_parents (Path.build_filename (tmpdir, "control", null));
		create_dir_parents (Path.build_filename (tmpdir, "data", null));

		// Get application-id from IPK source control XML file
		appInfo = ipks.get_application ();

		// Build IPK control file
		ictrl.create_new ();
		ictrl.set_application (appInfo);
		ictrl.set_app_license (ipks.get_app_license ());
		ictrl.set_app_description (load_text_from_element (ipks.get_app_description ()));

		ictrl.set_pkg_dependencies (ipks.get_pkg_dependencies ());

		if (failed)
			return false;

		string tmp = Path.build_filename (tmpdir, "control", "control.xml", null);
		ictrl.save_to_file (tmp);
		ctrlfiles.add (tmp);

		ret = build_ipk_files_structure (flist);
		if (!ret)
			return false;
		ret = write_ipk_control_data ();
		if (!ret)
			return false;
		ret = finalize_ipk ();

		return ret;
	}

}

} // End of namespace
