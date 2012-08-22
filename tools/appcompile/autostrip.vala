/* autostrip.vala
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU General Public License Version 3
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
 */

using GLib;
using Gee;
using Listaller.Utils;

namespace Listaller.Extra {

	private class AutoStrip : Object {
		private string srcdir;
		private string targetdir;

		public AutoStrip (string source_dir, string target_dir = "") {
			srcdir = source_dir;
			targetdir = target_dir;
		}

		public void strip_file (string fname) {
			Process.spawn_command_line_sync ("strip --strip-all %s".printf (fname));
		}

		public int strip_binaries () {
			int ret = -1;
			string lastdir = Environment.get_current_dir ();
			Environment.set_current_dir (srcdir);

			targetdir = verify_install_target (targetdir, srcdir);
			if (targetdir == "") {
				error ("Unable to proceed: IPK source dir not found!");
				return 1;
			}

			var files = find_files (targetdir, true);
			foreach (string fname in files) {
				if (FileUtils.test (fname, FileTest.IS_EXECUTABLE)) {
					// TODO: Check if file is binary or text
					strip_file (fname);
				}
			}

			Environment.set_current_dir (lastdir);
			return ret;
		}

	}

} // End of namespace
