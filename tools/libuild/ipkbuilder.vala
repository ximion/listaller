/* ipkbuilder.vala
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

// Workaround for Vala bug #618931
private const string _PKG_VERSION14 = Config.VERSION;

namespace Listaller.IPK {

private class Builder : Object {
	private string tmpdir;
	private string srcdir;
	private IPK.Script ipks;
	private IPK.FileList ipkf;

	public signal void error_message (string details);
	public signal void message (MessageItem message);

	public Builder (string input_dir) {
		srcdir = input_dir;
		Listaller.Settings conf = new Listaller.Settings ();
		tmpdir = conf.get_unique_tmp_dir ("ipkbuild");
		ipks = new IPK.Script ();
		ipkf = new IPK.FileList ();
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
	}

	private bool validate_srcdir (string dir) {
		// Check if IPK sources are present
		string tmp = dir;
		if (FileUtils.test (tmp, FileTest.IS_DIR)) {
			if (FileUtils.test (Path.build_filename (tmp, "control.xml", null), FileTest.EXISTS) &&
			    FileUtils.test (Path.build_filename (tmp, "files-current.list", null), FileTest.EXISTS)) {
				// Set current source dir and exit
				srcdir = tmp;
				return true;
			}
		}
		return false;
	}

	public bool initialize () {
		// Check for valid installer source dirs
		if (!validate_srcdir (Path.build_filename (srcdir, "ipkinstall", null)))
			if (!validate_srcdir (Path.build_filename (srcdir, "install", null)))
				if (!validate_srcdir (Path.build_filename (srcdir, "data", "install", null))) {
					//: IPk buolder could not find IPK source scripts
					emit_error (_("Could not find IPK source files!"));
					return false;
				}
		bool ret = false;

		// Load definitions
		ipks.load_from_file (Path.build_filename (srcdir, "control.xml", null));
		IPK.FileList flist = new IPK.FileList ();
		flist.open (Path.build_filename (srcdir, "files-current.list", null));

		//TODO: Build IPK here!
		return ret;
	}

}

} // End of namespace
