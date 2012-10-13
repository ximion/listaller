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

	private bool load_data_from_archive (Archive.Read ar, Archive.Entry e) {
		bool ret = false;
		assert (ar != null);

		size_t entry_size;
		char *contents;

		entry_size = (size_t) e.size ();
		contents = (char*)malloc (entry_size);
		ar.read_data(contents, entry_size);

		ret = data.open_data ((string) contents);

		free (contents);

		return ret;
	}

	public bool open (string fname) {
		bool ret = false;
		weak Archive.Entry e;

		// Now read all control stuff
		var ar = new Archive.Read ();
		// XZ compressed tarballs
		ar.support_format_tar ();
		// FIXME: Make compression_xz work
		ar.support_compression_all ();
		if (ar.open_filename (fname, 4096) != Archive.Result.OK) {
			critical ("Unable to open compressed repository content index!");
			return false;
		}

		while (ar.next_header (out e) == Archive.Result.OK)
			if (e.pathname () == "contents") {
				ret = load_data_from_archive (ar, e);
			}

		// Close archive
		ar.close ();

		return ret;
	}
}

} // End of namespace: Listaller.Repo
