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
using Listaller;

namespace Listaller.IPK {

private class Package : Object {
	private Settings conf;
	private string fname;
	private string wdir;
	private bool ipk_valid;
	private string data_archive;
	private IPK.Control ipkc;
	private IPK.FileList ipkf;
	private string appID;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public IPK.Control control {
		get { return ipkc; }
	}

	public Package (string filename, Settings? settings) {
		fname = filename;

		conf = settings;
		if (conf == null)
			conf = new Settings ();
		wdir = conf.get_unique_tmp_dir ();

		ipk_valid = false;
		ipkc = new IPK.Control ();
		ipkf = new IPK.FileList ();
	}

	~Package () {
		// Remove workspace
		delete_dir_recursive (wdir);
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
		GLib.message (msg);
	}

	private void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		critical (details);
	}

	public bool is_valid () {
		return ipk_valid;
	}

	public ArrayList<FileEntry> get_filelist () {
		return ipkf.get_files_list ();
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
				// FIXME: Fix this for mutiple data archives
				case "files-all.list":
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
			ret = ipkc.open_file (tmpf);
		}
		tmpf = Path.build_filename (wdir, "files-all.list", null);
		if ((ret) && (FileUtils.test (tmpf, FileTest.EXISTS))) {
			ret = ipkf.open (tmpf);
		}

		// Generate appID
		// TODO: Implement the application-id system!
		appID = ipkc.get_app_name ().down () + "-" + ipkc.get_app_version ().down ();

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

	private Read? open_base_ipk () {
		// Create a new archive object for reading
		Read ar = new Read ();

		weak Entry e;

		// Disable compression, as IPK main is not compressed
		ar.support_compression_none ();
		// IPK packages are GNU-Tar archives
		ar.support_format_tar ();

		// Open the file, if it fails exit
		if (ar.open_filename (fname, 4096) != Result.OK) {
			emit_error (ErrorEnum.IPK_LOADING_FAILED,
				_("Could not open IPK file! Error: %s").printf (ar.error_string ()));
			return null;
		}
		return ar;
	}

	public bool initialize () {
		bool ret = false;

		if (!FileUtils.test (fname, FileTest.IS_REGULAR)) {
			emit_error (ErrorEnum.IPK_LOADING_FAILED,
				    _("IPK package could not be found!"));
			return false;
		}

		Read ar = open_base_ipk ();
		if (ar == null) {
			return false;
		}

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
			// FIXME: Handle multiple data archives
			if (e.pathname () == "data-all.tar.xz") {
				ret = extract_entry_to (ar, e, wdir);
				if (!ret) {
					warning (_("Unable to extract IPK data!"));
					emit_error (ErrorEnum.IPK_INCOMPLETE,
						_("Could not extract IPK payload! Package might be damaged. Error: %s").printf (ar.error_string ()));
				}
				break;
			}
		}
		ar.close ();

		// FIXME: Handle multiple data archives
		data_archive = Path.build_filename (wdir, "data-all.tar.xz", null);

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
			emit_error (ErrorEnum.IPK_DAMAGED,
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
			emit_error (ErrorEnum.FILE_INSTALL_FAILED,
				_("Could not create destination directory. Error: %s").printf (e.message));
			return false;
		}
		return true;
	}

	private bool extract_file_copy_dest (IPK.FileEntry fe, Read plar, Entry e) {
		bool ret = true;

		// Varsolver to solve LI variables
		VarSolver vs = new VarSolver (appID);
		string int_path = vs.substitute_vars_id (fe.get_full_filename ());

		string dest = vs.substitute_vars_auto (fe.destination, conf);
		string fname = Path.build_filename (dest, fe.fname, null);

		// Check if file already exists
		if (FileUtils.test (fname, FileTest.EXISTS)) {
			// Throw error and exit
			emit_error (ErrorEnum.FILE_EXISTS,
				_("Could not override file %s, this file already exists!").printf (fname));
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
			emit_error (ErrorEnum.HASH_MISSMATCH,
				_("Could not validate file %s. This IPK file might have been modified after creation!\nPlease obtain a new copy and try again.").printf (fname));
			// Now remove the corrupt file
			FileUtils.remove (fname);

			return false;
		}
		ret = FileUtils.rename (tmp, fname) == 0;
		delete_dir_recursive (tmp);
		if (!ret) {
			// If we are here, everything went fine. Mark the file as installed
			fe.installed = true;
			fe.fname_installed = fname;
		}
		return ret;
	}

	public bool install_file (IPK.FileEntry fe) {
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

		VarSolver vs = new VarSolver (appID);
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
			ret = extract_file_copy_dest (fe, plar, e);
		}
		plar.close ();

		return ret;
	}

	// Search for IPKFileEntry with the given IPK internal path
	private IPK.FileEntry? get_fe_by_int_path (ArrayList<IPK.FileEntry> list, string int_path) {
		IPK.FileEntry re = null;
		VarSolver vs = new VarSolver (appID);
		foreach (IPK.FileEntry e in list) {
			if (vs.substitute_vars_id (e.get_full_filename ()) == int_path) {
				re = e;
				break;
			}
		}
		return re;
	}

	public bool install_all_files () {
		bool ret = true;
		// This extracts all files in this package to their destination
		if (!is_valid ()) {
			warning (_("Tried to perform action on invalid IPK package."));
			return false;
		}

		// This ensures our IPK package is ready to extract files
		ret = prepare_extracting ();
		if (!ret)
			return ret;

		Read plar = open_payload_archive ();
		if (plar == null)
			return false;

		// Cache file list
		ArrayList<IPK.FileEntry> flist = ipkf.get_files_list ();

		double one = 100d / flist.size;
		int prog = 0;
		ret = false;
		weak Entry e;
		IPK.FileEntry fe = null;
		// Now extract & validate all stuff
		while (plar.next_header (out e) == Result.OK) {
			fe = get_fe_by_int_path (flist, e.pathname ());
			if (fe != null) {
				// File was found, so install it now
				ret = extract_file_copy_dest (fe, plar, e);
				prog++;
				progress_changed ((int) Math.round (one * prog));
				// Stop on error
				if (!ret)
					break;
			}
		}
		plar.close ();

		return ret;
	}
}

} // End of namespace
