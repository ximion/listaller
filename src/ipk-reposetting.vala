/* ipk-reposetting.vala - Classes defining special files used in Listaller's IPK repositories
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller;
using Listaller.IPK;
using Listaller.Utils;

namespace Listaller.Repo {

/**
 * Settings of an IPK package repository
 */
internal class Settings : Object {
	private MetaFile data;

	public Settings () {
		data = new MetaFile ();

		data.add_value ("Format", "1.0");
	}

	public bool open (string fname) {
		return data.open_file (fname);
	}

	public bool save (string fname) {
		return data.save_to_file (fname, true);
	}

	public void set_repo_name (string name) {
		data.add_value ("Name", name);
	}

	public string get_repo_name () {
		return data.get_value ("Name");
	}
}

/**
 * Access an IPK-repo content-index
 */
internal class ContentIndex : Object {
	private MetaFile data;

	public ContentIndex () {
		data = new MetaFile ();
	}

	public bool open (string fname) {
		bool ret = false;
		weak Archive.Entry e;

		size_t size;
		char buff[4096];
		string cont_str = "";


		// Now read all control stuff
		var ar = new Archive.Read ();
		ar.support_format_raw ();
		// FIXME: Make compression_xz work
		ar.support_compression_all ();

		if (ar.open_filename (fname, 4096) != Archive.Result.OK) {
			critical ("Unable to open compressed repository content index!");
			return false;
		}

		while (ar.next_header (out e) == Archive.Result.OK)
			if (e.pathname () == "data") {
				for (;;) {
					size = ar.read_data (buff, 4096);
					if (size < 0) {
						// ERROR
					}
					if (size == 0)
						break;

					cont_str = "%s%s".printf (cont_str, (string) buff);
				}
			}
		if (cont_str != "")
			ret = data.open_data (cont_str);

		// Close archive
		ar.close ();

		return ret;
	}

	public bool save (string fname, bool override_existing = true) {
		string tmp_fname = "%s~%i".printf (fname.substring (0, fname.length - 3), Random.int_range (100, 999));

		var file = File.new_for_path (fname);
		if (file.query_exists ())
			if (!override_existing)
				return false;
			else
				file.delete ();

		data.save_to_file (tmp_fname, true);

		// LibArchive is unable to do XZ compression without also creating a tarball.
		// therefore we have to use the XZ tool.
		//! FIXME

		string[] argv = { "/usr/bin/xz", tmp_fname, null};

		try {
			Process.spawn_sync (null, argv, null, 0, null);
		} catch (Error e) {
			error ("Unable to compress content-index. Error: %s", e.message);
			return false;
		}

		FileUtils.rename ("%s.xz".printf (tmp_fname), fname);

		return true;
	}

	public void add_application (AppItem app) {

	}
}

} // End of namespace: Listaller.Repo
