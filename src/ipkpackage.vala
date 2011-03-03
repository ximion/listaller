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
private const string _PKG_VERSION6 = PkgConfig.VERSION;

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

		message ("Here I am!");
		while (ar.next_header (out e) == Result.OK) {
			message (e.pathname ());
		}
		ar.close ();
		return true;
	}

	private Result archive_copy_data (Read ar, Write aw)
	{
		Result res;
		void *buff;
		size_t size;
		Posix.off_t offset;

		for (;;) {
			res = ar.read_data_block (out buff, out size, out offset);
			if (res == Result.EOF) {
				return Result.OK;
			}
			if (res != Result.OK)
				return res;
			if (aw.write_data_block (buff, size, offset) != 0) {
				error (ar.error_string ());
				return res;
			}
		}
	}

	public bool initialize () {
		bool ret = false;

		// Create a new archive object for reading
		Read ar = new Read ();
		// A buffer which will hold read data
		char buff[4096];

		weak Entry e;

		// Disable compression, as IPK main is not compressed
		ar.support_compression_none ();
		// IPK packages are GNU-Tar archives
		ar.support_format_tar ();

		// Create new writer
		WriteDisk ext = new WriteDisk ();
		ext.set_options (0);

		// Open the file, if it fails exit
		if (ar.open_filename (fname, 4096) != Result.OK)
			error (_("Could not open IPK file: %s"), ar.error_string ());


		while (ar.next_header (out e) == Result.OK) {
			// Extract control files
			if (e.pathname () == "control.tar.xz") {
				ext.write_header (e);
				archive_copy_data (ar, ext);

				// We found & read the control files, so we can exit the loop now
				break;
			}
		}
		ar.close ();

		ipk_valid = ret;
		return ret;
	}
}
