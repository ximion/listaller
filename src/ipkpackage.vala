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

	public IPKPackage (string filename) {
		fname = filename;
		initialized = false;
		LiSettings conf = new LiSettings ();
		wdir = conf.get_unique_tmp_dir ();
		ipk_valid = false;
	}

	public bool is_valid () {
		return ipk_valid;
	}

	public bool initialize () {
		bool ret = false;

		// Create a new archive object for reading
		Read archive = new Read ();
		// A buffer which will hold read data
		int buf[4096];

		weak Entry e;

		// Disable compression, as IPK main is not compressed
		archive.support_compression_none ();
		// IPK packages are GNU-Tar archives
		archive.support_format_tar ();

		WriteDisk ext = new WriteDisk ();
		// Create archive reader for control files
		Read ctrlar = new Read ();
		// IPK control files are always XZ compressed
		ctrlar.support_compression_lzma ();
		ctrlar.support_format_tar ();

		// Open the file, if it fails exit
		if (archive.open_filename (fname, 4096) != Result.OK)
			error (_("Could not open IPK file: %s"), archive.error_string ());


		while (archive.next_header (out e) == Result.OK) {
			// Extract control files
			message (e.pathname ());
			if (e.pathname () == "control.tar.xz") {
				Result r = ext.write_header (e);
				if (r != Result.OK)
					critical (_("Error while extracting files: %s"));
				else {
					copy_data (archive, ext);
					r = ext.finish_entry ();
					if (r != Result.OK)
						critical (_("Error while extracting files: %s"));
				}
			}
		}
		archive.close ();

		ipk_valid = ret;
		return ret;
	}
}
