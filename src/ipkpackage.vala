/* ipkpackage.vala
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

// Workaround for Vala bug #618931
private const string _PKG_VERSION6 = Config.VERSION;

private class IPKPackage : Object {
	private LiSettings conf;
	private string fname;
	private string wdir;
	private bool ipk_valid;
	private string data_archive;
	private IPKControl ipkc;
	private IPKFileList ipkf;

	public signal void error_code (LiErrorItem error);
	public signal void message (LiMessageItem message);

	public IPKControl control {
		get { return ipkc; }
	}

	public IPKPackage (string filename, LiSettings? settings) {
		fname = filename;

		conf = settings;
		if (conf == null)
			conf = new LiSettings ();
		wdir = conf.get_unique_tmp_dir ();

		ipk_valid = false;
		ipkc = new IPKControl ();
		ipkf = new IPKFileList ();
	}

	~IPKPackage () {
		// Remove workspace
		// TODO: Make this recursive
		DirUtils.remove (wdir);
		Posix.rmdir (wdir);
	}

	private void emit_warning (string msg) {
		// Construct warning message
		LiMessageItem item = new LiMessageItem(LiMessageType.WARNING);
		item.details = msg;
		message (item);
		warning (msg);
	}

	private void emit_message (string msg) {
		// Construct info message
		LiMessageItem item = new LiMessageItem(LiMessageType.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void emit_error (LiError id, string details) {
		// Construct error
		LiErrorItem item = new LiErrorItem(id);
		item.details = details;
		error_code (item);
		critical (details);
	}

	public bool is_valid () {
		return ipk_valid;
	}

	public ArrayList<IPKFileEntry> get_filelist () {
		return ipkf.get_files ();
	}

	private bool process_control_archive (string arname) {
		bool ret = false;
		weak Entry e;

		// Now read all control stuff
		Read ar = new Read ();
		// Control archives are always XZ compressed tarballs
		ar.support_format_tar ();
		// FIXME: Make compression_xz work
		ar.support_compression_all ();
		if (ar.open_filename (arname, 4096) != Result.OK)
			error (_("Could not read IPK control information! Error: %s"), ar.error_string ());

		while (ar.next_header (out e) == Result.OK) {
			switch (e.pathname ()) {
				case "control.xml":
					ret = extract_entry_to (ar, e, wdir);
					break;
				case "files.list":
					ret = extract_entry_to (ar, e, wdir);
					break;
			}
		}
		// Close & remove tmp archive
		ar.close ();
		FileUtils.remove (arname);

		if (!ret)
			return false;

		// Check if all metadata is available
		string tmpf = Path.build_filename (wdir, "control.xml", null);
		ret = false;
		if (FileUtils.test (tmpf, FileTest.EXISTS)) {
			ret = ipkc.open (tmpf);
		}
		tmpf = Path.build_filename (wdir, "files.list", null);
		if ((ret) && (FileUtils.test (tmpf, FileTest.EXISTS))) {
			ret = ipkf.open (tmpf);
		}

		// If everything was successful, the IPK file is valid
		ipk_valid = ret;

		return ret;
	}

	private bool extract_entry_to (Read ar, Entry e, string dest) {
		bool ret = false;
		assert (ar != null);

		// Create new writer
		WriteDisk writer = new WriteDisk ();
		writer.set_options (ExtractFlags.SECURE_NODOTDOT | ExtractFlags.NO_OVERWRITE);

		// Change dir to extract to the right path
		string old_cdir = Environment.get_current_dir ();
		Environment.set_current_dir (dest);

		Result header_response = writer.write_header (e);
		if (header_response == Result.OK) {
			ret = archive_copy_data (ar, writer);
		} else {
			emit_warning (_("Could not extract file! Error: %s").printf (writer.error_string ()));
		}

		// Restore working dir
		Environment.set_current_dir (old_cdir);

		return ret;
	}

	private bool archive_copy_data (Read source, Write dest)
	{
		const int size = 10240;
		char buff[10240];
		ssize_t readBytes;

		readBytes = source.read_data (buff, size);
		while (readBytes > 0) {
			dest.write_data(buff, readBytes);
			if (dest.errno () != Result.OK) {
				emit_warning ("Error while extracting..." + dest.error_string () + "(error nb =" + dest.errno ().to_string () + ")");
				return false;
			}

			readBytes = source.read_data (buff, size);
		}
		return true;
	}

	private Read open_base_ipk () {
		// Create a new archive object for reading
		Read ar = new Read ();

		weak Entry e;

		// Disable compression, as IPK main is not compressed
		ar.support_compression_none ();
		// IPK packages are GNU-Tar archives
		ar.support_format_tar ();

		// Open the file, if it fails exit
		if (ar.open_filename (fname, 4096) != Result.OK)
			emit_error (LiError.IPK_LOADING_FAILED,
				    _("Could not open IPK file! Error: %s").printf (ar.error_string ()));
		return ar;
	}

	public bool initialize () {
		bool ret = false;

		Read ar = open_base_ipk ();

		weak Entry e;
		while (ar.next_header (out e) == Result.OK) {
			// Extract control files
			if (e.pathname () == "control.tar.xz") {
				ret = extract_entry_to (ar, e, wdir);
				if (ret) {
					// Now read the control file
					ret = process_control_archive (Path.build_filename (wdir, "control.tar.xz", null));
				} else {
					warning (_("Unable to extract IPK metadata!"));
				}
				break;
			}
		}
		ar.close ();

		ipk_valid = ret;

		return ret;
	}

	private bool prepare_extracting () {
		if (FileUtils.test (data_archive, FileTest.EXISTS))
			return true;
		bool ret = false;

		Read ar = open_base_ipk ();
		weak Entry e;
		while (ar.next_header (out e) == Result.OK) {
			// Extract payload
			if (e.pathname () == "data.tar.xz") {
				ret = extract_entry_to (ar, e, wdir);
				if (!ret) {
					warning (_("Unable to extract IPK data!"));
					emit_error (LiError.IPK_INCOMPLETE,
						    _("Could not extract IPK payload! Package might be damaged. Error: %s").printf (ar.error_string ()));
				}
				break;
			}
		}
		ar.close ();

		data_archive = Path.build_filename (wdir, "data.tar.xz", null);

		return ret;
	}

	private Read? open_payload_archive () {
		if (!FileUtils.test (data_archive, FileTest.EXISTS))
			return null;

		// Open the payload archive
		Read plar = new Read ();
		// Data archives are usually XZ compressed tarballs
		plar.support_format_tar ();
		plar.support_compression_all ();
		if (plar.open_filename (data_archive, 4096) != Result.OK) {
			emit_error (LiError.IPK_DAMAGED,
				    _("Could not read IPK payload container! Package might be damaged. Error: %s").printf (plar.error_string ()));
			return null;
		}
		return plar;
	}

	private bool touch_dir (string dirname) {
		File d = File.new_for_path (dirname);
		try {
			if (!d.query_exists ()) {
				d.make_directory_with_parents ();
			}
		} catch (Error e) {
			emit_error (LiError.FILE_INSTALL_FAILED,
				    _("Could not create destination directory. Error: %s").printf (e.message));
			return false;
		}
		return true;
	}

	public bool install_file (IPKFileEntry fe) {
		bool ret = true;
		// This extracts a file and verifies it's checksum
		if (!is_valid ()) {
			warning (_("Tried to perform action on invalid IPK package."));
			return false;
		}
		assert (fe.hash != "");

		// This ensures our IPK package is ready to extract files
		ret = prepare_extracting ();
		if (!ret)
			return ret;

		VarSolver vs = new VarSolver ();
		string int_path = vs.substitute_vars_id (fe.get_full_filename ());

		Read plar = open_payload_archive ();
		if (plar == null)
			return false;

		ret = false;
		weak Entry e;
		while (plar.next_header (out e) == Result.OK) {
			if (e.pathname () == int_path) {
				ret = true;
				break;
			}
		}
		if (ret) {
			string dest;
			// Set right destination
			if (conf.sumode) {
				dest = vs.substitute_vars_su (fe.destination);
			} else {
				dest = vs.substitute_vars_home (fe.destination);
			}
			// Check for testmode
			if (conf.testmode) {
				dest = Path.build_filename (conf.get_unique_install_tmp_dir (), vs.substitute_vars_id (fe.destination), null);
			}
			string fname = Path.build_filename (dest, fe.fname, null);

			// Check if file already exists
			if (FileUtils.test (fname, FileTest.EXISTS)) {
				// Throw error and exit
				emit_error (LiError.FILE_EXISTS,
					    _(@"Could not override file $fname, this file already exists!"));
				return false;
			}
			// Now extract it!
			touch_dir (dest);
			ret = extract_entry_to (plar, e, wdir);
			if (!ret)
				return ret;
			// The temporary location where the file has been extracted to
			string tmp = Path.build_filename (wdir, int_path);

			// Validate new file
			string new_hash = compute_checksum_for_file (tmp);
			if (new_hash != fe.hash) {
				// Very bad, we have a checksum missmatch -> throw error, delete file and exit
				emit_error (LiError.HASH_MISSMATCH,
					    _(@"Could not validate file $fname. This IPK file might have been modified after creation!\nPlease obtain a new copy and try again."));
				// Now remove the corrupt file
				FileUtils.remove (fname);

				return false;
			}
			ret = FileUtils.rename (tmp, fname) == 0;
			DirUtils.remove (tmp);
			if (!ret)
				return ret;

			plar.close ();
			// If we are here, everything went fine. Mark the file as installed
			fe.installed = true;
		}

		return ret;
	}
}

class VersionNumber : Object {
	private int major;    // x.0.0
	private int minor;    // 0.x.0
	private int revision; // 0.0.x
	private string originalString;

	public VersionNumber (string version)
	{
		originalString = version;
		try
		{
			var regex = new Regex("([[:digit:]]*)\\.([[:digit:]]*)\\.*([[:digit:]]*)");
			var split = regex.split (version);
			assert (split.length > 1); // TODO: Don't use assert, print a nice error message instead
			major = split[1].to_int ();
			if (split.length > 2)
			{
				minor = split[2].to_int ();
			}
			else
			{
				minor = 0;
			}
			if (split.length > 3)
			{
				revision = split[3].to_int ();
			}
			else
			{
				revision = 0;
			}
		}
		catch (GLib.RegexError e)
		{
			warning ("Error compiling regular expression!");
		}
	}

	public bool newerThan (VersionNumber other)
	{
		if (major > other.major)
		{
			return true;
		}
		else if (major == other.major)
		{
			if (minor > other.minor)
			{
				return true;
			}
			else if (minor == other.minor)
			{
				if (revision > other.revision)
				{
					return true;
				}
			}
		}
		return false;
	}

	public string to_string ()
	{
		return originalString;
	}
}
