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
using Archive;

// Workaround for Vala bug #618931
private const string _PKG_VERSION6 = Config.VERSION;

private class IPKPackage : Object {
	private string fname;
	private string wdir;
	private bool ipk_valid;
	private IPKControl _control;
	private IPKFileList _filelist;

	public signal void error_code (LiErrorItem error);
	public signal void message (LiMessageItem message);

	public IPKControl control {
		get { return _control; }
	}

	public IPKFileList filelist {
		get { return _filelist; }
	}

	public IPKPackage (string filename, LiSettings? settings) {
		fname = filename;

		LiSettings conf = settings;
		if (conf == null)
			conf = new LiSettings ();
		wdir = conf.get_unique_tmp_dir ();

		ipk_valid = false;
		_control = new IPKControl ();
		_filelist = new IPKFileList ();
	}

	~IPKPackage () {
		// Remove workspace
		// TODO: Make this recursive
		DirUtils.remove (wdir);
	}

	public bool is_valid () {
		return ipk_valid;
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
			debug (e.pathname ());
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
			ret = control.open (tmpf);
		}
		tmpf = Path.build_filename (wdir, "files.list", null);
		if ((ret) && (FileUtils.test (tmpf, FileTest.EXISTS))) {
			ret = filelist.open (tmpf);
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

	public bool initialize () {
		bool ret = false;

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
}
