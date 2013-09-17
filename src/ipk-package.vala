/* ipk-package.vala - Definition of an IPK setup package
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
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
using Archive;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

private class Package : MessageObject {
	private string fname;
	private string wdir;
	private bool ipk_valid;
	private string[] data_archives;
	private IPK.PackControl ipkc;
	private IPK.FileList ipkf;
	private HashMap<string, IPK.FileEntry>? fcache;
	private AppItem appInfo;
	private const int DEFAULT_BLOCK_SIZE = 65536;
	private string ipk_selected_arch_;
	private GPGSignature? gpg_signature;

	public IPK.PackControl control {
		get { return ipkc; }
	}

	public SetupSettings setup_settings { get; private set; }

	public Package (string filename) {
		fname = filename;
		ipk_selected_arch_ = "";

		setup_settings = new SetupSettings ();
		var conf = new Config ();
		string[] tmp_id_parts = Path.get_basename (filename).split (".");
		string tmp_id = filename;
		if (tmp_id.length >= 1)
			tmp_id = tmp_id_parts[0];
		wdir = conf.get_unique_tmp_dir (tmp_id);

		ipk_valid = false;
		ipkc = new IPK.PackControl ();
		ipkf = new IPK.FileList ();
		fcache = null;
	}

	~Package () {
		// Remove workspace
		delete_dir_recursive (wdir);
	}

	public Collection<IPK.FileEntry>? get_file_entries () {
		if (fcache == null)
			return null;
		return fcache.values;
	}

	public bool is_valid () {
		return ipk_valid;
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
		if (ar.open_filename (arname, 4096) != Result.OK) {
			emit_error (ErrorEnum.IPK_DAMAGED,
				    _("Could not read IPK control information! Error: %s").printf (ar.error_string ()));
			return false;
		}

		string asdFileName = "";
		ipk_selected_arch_ = "";
		string archFListName = "";
		string currentArch = arch_generic (system_machine ());

		// set up directory for extra dependency files
		string wdir_depdir = Path.build_filename (wdir, "dependencies", null);
		create_dir_structure (wdir_depdir);

		while (ar.next_header (out e) == Result.OK) {
			string pName = e.pathname ();
			if (pName.has_suffix (".appdata.xml")) {
					ret = extract_entry_to (ar, e, wdir);
					if (ret)
						asdFileName = pName;
			} else {
				switch (pName) {
					case "pksetting":
						ret = extract_entry_to (ar, e, wdir);
						break;
					case "files-all.list":
						ret = extract_entry_to (ar, e, wdir);
						break;
					case "license.txt":
						ret = extract_entry_to (ar, e, wdir);
						break;
					default:
						if (pName.has_prefix ("files-")) {
							// extract filelists
							string arch = pName.substring (6, pName.last_index_of (".") - 6);

							if (arch == currentArch) {
								ipk_selected_arch_ = arch;
								ret = extract_entry_to (ar, e, wdir);
								archFListName = pName;
								break;
							}
						} else if (pName.has_prefix ("dependencies/")) {
							// extract additional module dependencies
							ret = extract_entry_to (ar, e, wdir_depdir);
							break;
						}
						ar.read_data_skip ();
						break;
				}
			}
		}
		// Close & remove tmp archive
		ar.close ();

		if (!ret)
			return false;
		if (asdFileName == "")
			return false;

		string tmpf = Path.build_filename (wdir, asdFileName, null);
		ret = false;
		if (FileUtils.test (tmpf, FileTest.EXISTS)) {
			ret = ipkc.open_control (Path.build_filename (wdir, "pksetting", null),
						 tmpf);
		}

		// Set license text, if we have any. (This fubction returns false if file doesn't exists)
		ipkc.set_license_text_from_file (Path.build_filename (wdir, "license.txt", null));

		// Fetch application-information as an app-id
		appInfo = ipkc.get_application ();

		// Load arch-independent file-list
		tmpf = Path.build_filename (wdir, "files-all.list", null);
		if ((ret) && (FileUtils.test (tmpf, FileTest.EXISTS))) {
			ret = ipkf.data_append_listfile (tmpf);
		}
		if (archFListName != "") {
			tmpf = Path.build_filename (wdir, archFListName, null);
			if (FileUtils.test (tmpf, FileTest.EXISTS))
				ret = ipkf.data_append_listfile (tmpf);
		}

		// Cache list of files
		var list = ipkf.get_files_list ();
		fcache = new HashMap<string, IPK.FileEntry> ();
		VarSolver vs = new VarSolver (appInfo.idname);
		foreach (IPK.FileEntry fe in list) {
			fcache.set (vs.substitute_vars_id (fe.get_full_filename ()), fe);
		}

		// If everything was successful, the IPK file is valid
		ipk_valid = ret;

		return ret;
	}

	private bool install_entry_and_validate (IPK.FileEntry fe, Read plar, Entry e, VarSolver vs, ChecksumType cstype = ChecksumType.SHA1) {
		bool ret = true;
		assert (plar != null);

		// Some preparation

		string dest = vs.substitute_vars_auto (fe.destination, setup_settings);
		string fname = Path.build_filename (dest, fe.fname, null);
		string fname_tmp = fname + ".listaller-new";

		// Check if file already exists
		if (FileUtils.test (fname, FileTest.EXISTS)) {
			rollback_installation ();
			// Throw error and exit
			emit_error (ErrorEnum.FILE_EXISTS,
				_("Could not override file %s! Maybe the software database is damaged or this package is broken!").printf (fname));
			return false;
		}

		ret = create_dir_structure (dest);
		if (!ret) {
			// Undo changes & emit error
			rollback_installation ();
			emit_error (ErrorEnum.FILE_INSTALL_FAILED,
				_("Unable to create destination directory."));
			return false;
		}

		// Now extract everything

		void *buff;
		size_t size, bytes_to_write;
		ssize_t bytes_written, total_written;
		Posix.off_t offset;
		Posix.off_t output_offset;
		Result r;

		total_written = 0;
		output_offset = 0;

		var cs = new Checksum (cstype);

		// File descriptor for tmp file
		int fd = Posix.open (fname_tmp, Posix.O_CREAT | Posix.O_WRONLY | Posix.O_TRUNC,
	                         Posix.S_IRUSR | Posix.S_IWUSR | Posix.S_IRGRP | Posix.S_IROTH);
		if (fd < 0) {
			emit_error (ErrorEnum.FILE_INSTALL_FAILED,
				_("Unable to extract file. %s").printf (strerror (errno)));
			return false;
		}

		while ((r = plar.read_data_block(out buff, out size, out offset)) == Result.OK) {
			char *p = buff;
			if (offset > output_offset) {
				Posix.lseek(fd, offset - output_offset, Posix.SEEK_CUR);
				output_offset = offset;
			}
			while (size > 0) {
				bytes_to_write = size;
				if (bytes_to_write > DEFAULT_BLOCK_SIZE)
					bytes_to_write = DEFAULT_BLOCK_SIZE;

				bytes_written = Posix.write (fd, p, bytes_to_write);
				cs.update ((uchar[]) p, bytes_to_write);

				if (bytes_written < 0) {
					emit_warning (_("Could not extract file %s! Error: %s").printf (fname, plar.error_string ()));
					FileUtils.remove (fname_tmp);
					rollback_installation ();
					emit_error (ErrorEnum.IPK_DAMAGED,
						_("Unable to extract data file %s. This IPK package might be damaged, please obtain a new copy.").printf (Path.get_basename (e.pathname ())));
					return false;
				}
				output_offset += bytes_written;
				total_written += bytes_written;
				p += bytes_written;
				size -= bytes_written;
			}
		}
		if (Posix.close (fd) != 0)
			warning ("Closing of file desriptor failed. %s".printf (strerror (errno)));
		// Set permissions
		Posix.chmod (fname_tmp, e.mode ());

		if ((r != Result.OK) && (r != Result.EOF)) {
			FileUtils.remove (fname_tmp);
			emit_error (ErrorEnum.IPK_DAMAGED,
						_("Unable to extract data file %s. Message: %s").printf (Path.get_basename (e.pathname ()), strerror (errno)));
			return false;
		}

		// Check the checksums
		string sum = cs.get_string ();
		if (sum != fe.hash) {
			debug ("Checksum conflict: %s [vs] %s", sum, fe.hash);
			rollback_installation ();
			// Now remove the corrupt file
			FileUtils.remove (fname_tmp);
			// Very bad, we have a checksum missmatch -> throw error, delete file and exit
			emit_error (ErrorEnum.HASH_MISSMATCH,
				_("Validation of file %s failed! This IPK file might have been modified after creation.\nPlease obtain a new copy and try again.").printf (fname));
			return false;
		}

		string einfo = "";
		try {
			ret = move_file (fname_tmp, fname);
		} catch (Error e) {
			einfo = e.message;
		}

		if (ret) {
			// If we are here, everything went fine. Mark the file as installed
			fe.fname_installed = fname;
		} else {
			rollback_installation ();
			emit_error (ErrorEnum.COPY_ERROR,
				    _("Could not copy file %s to its destination. Do you have the necessary rights to perform this action?\nError message was \"%s\".").printf (fname, einfo));
		}

		// Store a/the .desktop file, if one is found
		if ((fe.destination == "%APP%") && (fe.fname.has_suffix (".desktop")))
			appInfo.desktop_file = fe.get_full_filename ();

		return ret;
	}

	private bool extract_entry_to (Read ar, Entry e, string dest) {
		bool ret = false;
		assert (ar != null);

		// Build target filename
		string fname = Path.build_filename (dest, Path.get_basename (e.pathname ()));

		if (FileUtils.test (fname, FileTest.EXISTS)) {
			emit_error (ErrorEnum.FILE_EXISTS,
				_("Could not override file %s, this file already exists!").printf (fname));
			return false;
		}

		void *buff;
		size_t size, bytes_to_write;
		ssize_t bytes_written, total_written;
		Posix.off_t offset;
		Posix.off_t output_offset;
		Result r;

		total_written = 0;
		output_offset = 0;

		int fd = Posix.open (fname, Posix.O_CREAT | Posix.O_WRONLY | Posix.O_TRUNC,
	                         Posix.S_IRUSR | Posix.S_IWUSR | Posix.S_IRGRP | Posix.S_IROTH);
		if (fd < 0) {
			emit_error (ErrorEnum.FILE_INSTALL_FAILED,
				_("Unable to extract file. %s").printf (strerror (errno)));
			return false;
		}

		ret = true;
		while ((r = ar.read_data_block(out buff, out size, out offset)) == Result.OK) {
			char *p = buff;
			if (offset > output_offset) {
				Posix.lseek(fd, offset - output_offset, Posix.SEEK_CUR);
				output_offset = offset;
			}
			while (size > 0) {
				bytes_to_write = size;
				if (bytes_to_write > DEFAULT_BLOCK_SIZE)
					bytes_to_write = DEFAULT_BLOCK_SIZE;
				bytes_written = Posix.write (fd, p, bytes_to_write);
				if (bytes_written < 0) {
					emit_error (ErrorEnum.FILE_INSTALL_FAILED,
						_("Unable to extract file. %s").printf (strerror (errno)));
					ret = false;
					break;
				}
				output_offset += bytes_written;
				total_written += bytes_written;
				p += bytes_written;
				size -= bytes_written;
			}
		}
		if (Posix.close (fd) != 0)
			warning ("Closing of file desriptor failed. %s".printf (strerror (errno)));
		Posix.chmod (fname, e.mode ());

		return ret;
	}


	private Read? open_base_ipk () {
		// Create a new archive object for reading
		Read ar = new Read ();

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
			} else {
				ar.read_data_skip ();
			}
		}
		ar.close ();

		// set sane default setup-settings
		IPK.InstallMode modes = control.get_install_modes ();
		if (modes.is_all_set (IPK.InstallMode.SHARED))
			setup_settings.current_mode = IPK.InstallMode.SHARED;
		else if (modes.is_all_set (IPK.InstallMode.PRIVATE))
			setup_settings.current_mode = IPK.InstallMode.PRIVATE;
		else {
			warning ("Setup %s does not support shared and private installations! This is usually a bug. Defaulting to test-mode now.", Path.get_basename (fname));
			setup_settings.current_mode = IPK.InstallMode.TEST;
		}

		ipk_valid = ret;

		return ret;
	}

	public IPK.InstallMode get_supported_install_modes () {
		return control.get_install_modes ();
	}

	public bool set_install_mode (IPK.InstallMode mode) {
		IPK.InstallMode modes = get_supported_install_modes ();
		if (modes.is_all_set (mode)) {
			setup_settings.current_mode = mode;
			return true;
		} else {
			warning ("Tried to set an install mode which is unsupported by this package!");
			return false;
		}
	}

	private GPGSignature? get_signature () {
		bool ret = false;
		// return cached signature, if we have one
		if (gpg_signature != null)
			return gpg_signature;

		prepare_extracting ();
		Read ar = open_base_ipk ();
		if (ar == null) {
			return null;
		}

		var file = File.new_for_path (Path.build_filename (wdir, "_signature", null));

		if (!file.query_exists ()) {
			weak Entry e;
			while (ar.next_header (out e) == Result.OK) {
				// Extract control files
				if (e.pathname () == "_signature") {
					ret = extract_entry_to (ar, e, wdir);
					if (!ret) {
						warning (_("Unable to extract signature! Maybe package is not signed."));
					}
					break;
				} else {
					ar.read_data_skip ();
				}
			}
			ar.close ();
			if (!ret)
				return null;
		}

		// if the signature is still not extracted, we simply don't have one
		if (!file.query_exists ()) {
			return null;
		}

		string sig_text = "";
		try {
			var dis = new DataInputStream (file.read ());
			string line;
			// Read lines until end of file (null) is reached
			while ((line = dis.read_line (null)) != null) {
				sig_text += line + "\n";
			}
		} catch (Error e) {
			warning (_("Unable to read package signature! Message: %s").printf (e.message));
			return null;
		}

		GPGSignature sig = new GPGSignature (sig_text);
		sig.verify_package (Path.build_filename (wdir, "control.tar.xz", null));

		gpg_signature = sig;

		return gpg_signature;
	}

	public SecurityInfo get_security_info () {
		GPGSignature? sig = get_signature ();

		SecurityInfo sec = new SecurityInfo ();

		if (sig == null)
			return sec;
		sec.signature_status = sig.sigstatus;
		sec.signature_trustlevel = sig.trust_level;

		sec.user_names = sig.user_names;
		sec.key_fpr = sig.key_fpr;
		sec.trust_reason = sig.trust_reason;

		return sec;
	}

	private bool prepare_extracting () {
		bool ret = false;

		foreach (string fname in data_archives) {
			if (FileUtils.test (fname, FileTest.EXISTS))
				return true;
		}
		ret = false;

		Read ar = open_base_ipk ();
		string[] data_archives_tmp = {};

		// Only extract data-archives for arch-indep and the current architecture
		string archDataFName = "";
		debug ("Selected architecture: %s", ipk_selected_arch_);
		if (ipk_selected_arch_ != "")
			archDataFName = "data-%s.tar.xz".printf (ipk_selected_arch_);

		weak Entry e;
		while (ar.next_header (out e) == Result.OK) {
			// Determine name of arch-dependent part
			// Extract payload
			string s = e.pathname ();
			if ((s == "data-all.tar.xz") || (s == archDataFName)) {
				data_archives_tmp += e.pathname ();
				ret = extract_entry_to (ar, e, wdir);
				if (!ret) {
					warning (_("Unable to extract IPK data!"));
					emit_error (ErrorEnum.IPK_INCOMPLETE,
						_("Could not extract IPK payload! Package might be damaged. Error: %s").printf (ar.error_string ()));
				}
				continue;
			} else {
				ar.read_data_skip ();
			}
		}
		ar.close ();

		data_archives = {};
		foreach (string s in data_archives_tmp) {
			debug ("Processing archive: %s", s);
			if ((s == "data-all.tar.xz") || (s == archDataFName))
				data_archives += Path.build_filename (wdir, s, null);
		}

		return ret;
	}

	private Read? open_payload_archive (string ar_fname) {
		if (!FileUtils.test (ar_fname, FileTest.EXISTS))
			return null;

		// Open the payload archive
		Read plar = new Read ();
		// Data archives are usually XZ compressed tarballs
		plar.support_format_tar ();
		plar.support_compression_all ();
		if (plar.open_filename (ar_fname, 4096) != Result.OK) {
			emit_error (ErrorEnum.IPK_DAMAGED,
				_("Could not read IPK payload container! Package might be damaged. Error: %s").printf (plar.error_string ()));
			return null;
		}
		return plar;
	}

	/** Internal helper method for install_file() */
	private bool install_file_arc_internal (Read plar, FileEntry fe) {
		VarSolver vs = new VarSolver (appInfo.idname);
		string int_path = vs.substitute_vars_id (fe.get_full_filename ());

		if (plar == null)
			return false;

		bool ret = false;
		weak Entry e;
		while (plar.next_header (out e) == Result.OK) {
			if (e.pathname () == int_path) {
				ret = true;
				break;
			}
		}
		if (ret) {
			ret = install_entry_and_validate (fe, plar, e, vs);
		}

		plar.close ();

		return ret;
	}

	/**
	 * Install selected file of this package to it's' destination
	 *
	 * @param fe The FileEntry to extract
	 *
	 * @return TRUE if no error was sent
	 */
	public bool install_file (IPK.FileEntry fe) {
		bool ret = true;
		// This extracts a file and verifies it's checksum
		if (!is_valid ()) {
			warning (_("Tried to perform action on invalid IPK package."));
			return false;
		}
		assert (fe.hash != "");

		// Make sure that our IPK package is ready to extract files
		ret = prepare_extracting ();
		if (!ret)
			return ret;

		// Install all enabled data-archives
		if (data_archives.length <= 0) {
			// No data archives, we can't install anything! :(
			emit_error (ErrorEnum.INTERNAL,
				    _("No payload data archive has been registered! This might be a bug in this application, please report the issue!"));
			return false;
		}

		// Go through all data archives and seek the to-be-installed file
		foreach (string ar_fname in data_archives) {
			Read plar = open_payload_archive (ar_fname);
			ret = install_file_arc_internal (plar, fe);
			if (!ret)
				return false;
		}

		// VarSetter to set LI path vars in files
		VarSetter vset = new VarSetter (setup_settings, appInfo.idname);
		if (ret)
			vset.execute (fe.fname_installed);

		return ret;
	}

	/** Internal helper method for install_all_files() */
	private bool install_all_files_arc_internal (Read plar, double one, ref int prog) {
		if (plar == null)
			return false;

		bool ret = false;
		weak Entry e;

		IPK.FileEntry fe = null;
		VarSolver vs = new VarSolver (appInfo.idname);
		// Now extract & validate all stuff
		while (plar.next_header (out e) == Result.OK) {
			fe = fcache.get (e.pathname ());

			if ((fe != null) && (!fe.is_installed ())) {
				// File was found, so install it now
				ret = install_entry_and_validate (fe, plar, e, vs);
				// Stop on error
				if (!ret)
					break;

				prog++;
				change_progress ((int) Math.round (one * prog));
			}
		}

		plar.close ();

		return ret;
	}

	/**
	 * Install all files of this package to their destination
	 *
	 * @return TRUE if no error was sent
	 */
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

		// Install all enabled data-archives
		if (data_archives.length <= 0) {
			// No data archives, we can't install anything! :(
			emit_error (ErrorEnum.INTERNAL,
				    _("No payload data archive has been registered! This might be a bug in this application, please report the issue!"));
			return false;
		}

		double one = 100d / fcache.size;
		int prog = 0;
		foreach (string ar_fname in data_archives) {
			debug ("Installing from archive: %s", ar_fname);
			Read plar = open_payload_archive (ar_fname);
			ret = install_all_files_arc_internal (plar, one, ref prog);
			if (!ret)
				return false;
		}

		if ((prog != fcache.size) && (ret == true)) {
			// Check which files might be missing...
			string missingFiles = "";
			foreach (FileEntry dFe in get_file_entries ())
				if (!dFe.is_installed ())
					missingFiles = "%s | %s\n".printf (dFe.get_full_filename (), dFe.fname_installed);
			debug ("Some files weren't found in payload! List of missing files: %s", missingFiles);

			// Now roll back the installation.
			rollback_installation ();

			emit_error (ErrorEnum.IPK_INCOMPLETE,
				    _("Some files of this package could not be installed, because they were not found in payload data.\nThis IPK package might be damaged, please obtain a new copy!"));

			ret = false;
		}

		// Create new varsetter, so we can set path variables directly in files
		VarSetter vset = new VarSetter (setup_settings, appInfo.idname);
		// Set variables in external files
		if (ret)
			foreach (IPK.FileEntry f in get_file_entries ()) {
				vset.execute (f.fname_installed);
			}

		return ret;
	}

	public bool rollback_installation () {
		// Remove all installed files
		foreach (IPK.FileEntry fe in get_file_entries ()) {
			if (!fe.is_installed ())
				continue;
			if (FileUtils.remove (fe.fname_installed) == 0) {
				fe.fname_installed = "";
				//string dir = Path.get_basedir (fe.fname_installed);
			}
		}
		return true;
	}

	public Dep.ComponentFactory get_component_factory () {
		string wdir_depdir = Path.build_filename (wdir, "dependencies", null);

		var cfactory = new Dep.ComponentFactory (setup_settings);
		cfactory.initialize ();
		cfactory.load_extra_modules (wdir_depdir);

		return cfactory;
	}
}

} // End of namespace: Listaller.IPK
