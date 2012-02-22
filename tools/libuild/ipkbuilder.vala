/* ipkbuilder.vala
 *
 * Copyright (C) 2011-2012 Matthias Klumpp
 *
 * Licensed under the GNU General Public License Version 3
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
 */

using GLib;
using Gee;
using Archive;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

private errordomain BuildError {
	INTERNAL_ERROR,
	FILE_NOT_FOUND;
}

private class Builder : Object {
	private string tmpdir;
	private string srcdir;
	private string outname;
	private string outdir;
	private string ipkVersion;
	private bool failed = false;
	private IPK.ControlDir ipkCDir;
	private ArrayList<string> ctrlfiles;
	private ArrayList<string> datapkgs;
	private AppItem appInfo;

	public signal void error_message (string details);
	public signal void message (MessageItem message);

	public string output_dir {
		get { return outdir; }
		set { outdir = value; }
	}

	public bool sign_package { get; set; }

	public Builder (string input_dir) {
		srcdir = input_dir;
		Listaller.Settings conf = new Listaller.Settings ();
		tmpdir = conf.get_unique_tmp_dir ("ipkbuild");
		ipkCDir = new IPK.ControlDir ();
		outdir = "";
		outname = "";
		ctrlfiles = new ArrayList<string> ();
		datapkgs = new ArrayList<string> ();
		ipkVersion = "1.0";
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
		VarSolver vs = new VarSolver (appInfo.idname);

		Entry entry = new Entry ();
		foreach (IPK.FileEntry fe in src) {
			// Prepare
			entry.clear ();
			// Grab filepath
			string fname_orig;
			string fname_dest;

			// Check if we need to change the name of this file
			if (fe.fname.index_of (" ") > 0) {
				int i = 0;
				int j = 0;
				var s = fe.fname;
				if (s.has_prefix ("'")) {
					i = 1;
					j = s.last_index_of ("'") - 1;
				} else {
					j = s.last_index_of (" ");
				}
				fname_orig = s.substring (i, j);
				if ((j + 2) != s.length)
					fname_dest = s.substring (j + i + 1);
				else
					fname_dest = Path.get_basename (fname_orig);

			} else {
				fname_orig = fe.fname;
				fname_dest = Path.get_basename (fname_orig);
			}

			if (fname_dest.index_of ("/") > 0) {
				emit_error (_("Malformed line in file listing: %s").printf (fe.fname));
				return false;
			}

			if (!Path.is_absolute (fname_orig)) {
				fname_orig = Path.build_filename (rdir, fname_orig, null);
			}

			if (fname_dest == "")
				error ("Destination filename was empty for file '%s'! This should never happen and might be a bug in Listaller!", fname_orig);

			fe.fname = Path.get_basename (fname_dest);
			fe.hash = compute_checksum_for_file (fname_orig);

			// Fetch file details
			Posix.Stat st;
			Posix.stat (fname_orig, out st);
			if (st.st_size <= 0) {
				debug ("File %s not found.", fname_orig);
				ret = false;
				break;
			}

			entry.set_pathname (vs.substitute_vars_id (fe.get_full_filename ()));
			entry.copy_stat (st);
			a.write_header (entry);
			int fd = Posix.open (fname_orig, Posix.O_RDONLY);
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
		string rootdir = ipkCDir.get_files_rootdir ();

		if (rootdir == "%INSTDIR%") {
			flist.rootdir = Path.build_filename (srcdir, "inst_target", null);
		} else
			if (!Path.is_absolute (rootdir))
				flist.rootdir = real_path (Path.build_filename (srcdir, rootdir, null));
		/* Just a shortcut to the root dir. It is important that our FileList
		 * knows the right file-root path to resolve filenames. */
		rootdir = flist.rootdir;

		ArrayList<IPK.FileEntry> fileslst = flist.get_files_list_expanded ();
		ret = write_ipk_file_data (ref fileslst, rootdir, arch);
		if (!ret) {
			error_message ("Unable to write IPK payload - do all files exist?");
			return false;
		}
		flist.set_files_list (fileslst);
		string tmp = Path.build_filename (tmpdir, "control", "files-" + arch + ".list", null);
		ret = flist.save (tmp);
		if (!ret) {
			error_message ("Could not compile IPK file list!");
			return false;
		}
		// Add to control file list, if file was created successfully
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
			entry.copy_stat (st);
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

	private string delete_chars (string str, string[] invalid_chars) {
		var res = str;
		foreach (string s in invalid_chars)
			res = res.replace (s, "");
		return res;
	}

	private bool finalize_ipk () {
		const int buffsize = 8192;
		char buff[8192];
		bool ret = true;

		// Set output file name
		if (outname == "") {
			string ipkname;
			// Delete invalid chars from filename
			string s = delete_chars (appInfo.full_name, {"(", ")", "[", "]", "#", " "});
			ipkname = s + "-" + appInfo.version.down () + "_install.ipk";
			if (outdir == "")
				outdir = real_path (Path.build_filename (srcdir, "..", "..", null));
			outname = Path.build_filename (outdir, ipkname, null);
		}

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
			entry.copy_stat (st);
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
		entry.copy_stat (st);
		a.write_header (entry);
		int fd = Posix.open (ctrlfile, Posix.O_RDONLY);
		ssize_t len = Posix.read (fd, buff, buffsize);
		while (len > 0) {
			a.write_data (buff, len);
			len = Posix.read (fd, buff, buffsize);
		}
		Posix.close (fd);

		// Sign the IPK package
		if (sign_package) {
			GPGSign gsig = new GPGSign ();
			string signature;
			// TODO: Make this work for more data packages
			gsig.sign_package (ctrlfile, datapkgs.get (0), out signature);
			string sigfile = Path.build_filename (tmpdir, "_signature", null);
			try {
				var file = File.new_for_path (sigfile);
				{
					var file_stream = file.create (FileCreateFlags.NONE);

					var data_stream = new DataOutputStream (file_stream);
					data_stream.put_string (signature);
				}
			} catch (Error e) {
				li_error (_("Unable to write signature file! Message: %s").printf (e.message));
				return false;
			}

			// Now add signature to IPK tarball
			entry.clear ();
			// Fetch file details
			Posix.stat (sigfile, out st);
			if (st.st_size <= 0) {
				ret = false;
				error_message ("Internal error: IPK signature was not found!");
				return false;
			}
			entry.set_pathname (Path.get_basename (sigfile));
			entry.copy_stat (st);
			a.write_header (entry);
			fd = Posix.open (sigfile, Posix.O_RDONLY);
			len = Posix.read (fd, buff, buffsize);
			while (len > 0) {
				a.write_data (buff, len);
				len = Posix.read (fd, buff, buffsize);
			}
			Posix.close (fd);
		}

		a.close ();
		return ret;
	}

	public void set_output_ipk (string fname) {
		outname = fname;
	}

	public bool initialize () {
		pkbuild_action ("Initializing...");
		// Check for valid installer source dirs
		srcdir = find_ipk_source_dir (srcdir);
		if (srcdir == null) {
			// IPK builder was unable to find IPK source scripts
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

		pkbuild_action ("Building IPK control file.");

		IPK.PackControl ictrl = new IPK.PackControl ();

		// Load definitions
		try {
			ret = ipkCDir.open_dir (srcdir);
		} catch (Error e) {
			li_error (_("Unable to build package: %s").printf (e.message));
			return false;
		}
		if (!ret)
			return false;

		// Set IPK package version
		ipkVersion = ipkCDir.get_ipk_version ();

		IPK.FileList flist = new IPK.FileList (false);
		flist.open (Path.build_filename (srcdir, "files-current.list", null));

		create_dir_parents (Path.build_filename (tmpdir, "control", null));
		create_dir_parents (Path.build_filename (tmpdir, "data", null));

		// Get application-id from IPK source control XML file
		appInfo = ipkCDir.get_application ();

		// Build IPK control directory
		ictrl.create_new (ipkCDir.get_doap_data (), ipkVersion);

		ArrayList<IPK.Dependency> deps = ipkCDir.get_dependencies ();
		if (ipkCDir.auto_dependency_search ()) {
			DepFind df = new DepFind (real_path (Path.build_filename (srcdir, "..", null)));
			var list = df.get_dependencies (deps);
			foreach (IPK.Dependency d1 in list) {
				foreach (IPK.Dependency d2 in deps) {
					if (d1.full_name == d2.full_name) {
						list.remove (d1);
						break;
					}
				}
			}
			deps.add_all (list);
		}
		ictrl.set_dependencies (deps);

		if (failed)
			return false;

		// Set package setting information
		ictrl.set_architectures (ipkCDir.get_architectures ());

		// Set license...
		ictrl.set_license_text (ipkCDir.get_application ().license.text);

		string tmp = Path.build_filename (tmpdir, "control", null);
		ictrl.save_to_dir (tmp);

		// Get the files this control data consists of
		string[] files = ictrl.get_files ();
		for (int i = 0; files[i] != null; i++) {
			ctrlfiles.add (Path.build_filename (tmp, files[i], null));
		}

		pkbuild_action ("Generating package...");

		ret = build_ipk_files_structure (flist);
		if (!ret)
			return false;
		ret = write_ipk_control_data ();
		if (!ret)
			return false;
		ret = finalize_ipk ();

		pkbuild_action ("Done.");

		return ret;
	}

}

} // End of namespace
