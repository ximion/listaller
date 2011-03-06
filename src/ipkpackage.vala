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
	private bool initialized;
	private string wdir;
	private bool ipk_valid;

	public IPKPackage (string filename, LiSettings? settings) {
		fname = filename;
		initialized = false;
		LiSettings conf = settings;
		if (conf == null)
			conf = new LiSettings ();
		wdir = conf.get_unique_tmp_dir ();
		ipk_valid = false;
	}

	public bool is_valid () {
		return ipk_valid;
	}

	private bool read_control_archive (Read ar) {
		weak Entry e;

		while (ar.next_header (out e) == Result.OK) {
			message (e.pathname ());
		}
		ar.close ();
		return true;
	}

	private bool archive_copy_data(Read source, Write dest)
	{
		char buff[10240];
		ssize_t readBytes;

		readBytes = source.read_data(buff, 10240);
		while (readBytes > 0) {
			dest.write_data(buff, readBytes);
			if (dest.errno () != Result.OK) {
				warning ("Error while extracting..." + dest.error_string () + "(error nb =" + dest.errno ().to_string () + ")");
				return false;
			}

			readBytes = source.read_data(buff, 10240);
		}
		return true;
	}

	public bool initialize () {
		bool ret = false;

		// Change dir to extract to the right path
		string old_cdir = Environment.get_current_dir ();
		Posix.chdir (wdir);

		// Create a new archive object for reading
		Read ar = new Read ();

		weak Entry e;

		// Disable compression, as IPK main is not compressed
		ar.support_compression_none ();
		// IPK packages are GNU-Tar archives
		ar.support_format_tar ();

		// Create new writer
		WriteDisk writer = new WriteDisk ();

		// Open the file, if it fails exit
		if (ar.open_filename (fname, 4096) != Result.OK)
			error (_("Could not open IPK file! Error: %s"), ar.error_string ());


		while (ar.next_header (out e) == Result.OK) {
			// Extract control files
			if (e.pathname () == "control.tar.xz") {
				Result header_response = writer.write_header (e);
				if (header_response == Result.OK) {
					ret = archive_copy_data(ar, writer);
				} else {
					warning (_("Could not read IPK control information! Error: %s"), writer.error_string ());
				}
				if (ret) {
					// Now read all control stuff
					Read ctrlar = new Read ();
					// Control archives are always XZ compressed TARballs
					ctrlar.support_compression_xz ();
					ctrlar.support_format_tar ();
					if (ctrlar.open_filename (Path.build_filename (wdir, "control.tar.xz", null), 4096) != Result.OK)
						error (_("Could not read IPK control information! Error: %s"), ctrlar.error_string ());

					// Now read the control file
					read_control_archive (ctrlar);
				}
				break;
			}
		}
		ar.close ();

		ipk_valid = ret;

		// Restore working dir
		Posix.chdir (old_cdir);

		return ret;
	}
}
