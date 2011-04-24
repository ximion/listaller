/* appbuilder.vala
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

namespace Listaller.Extra {

private class AppBuilder : Object {
	private string srcdir;

	public AppBuilder (string source_dir) {
 		srcdir = source_dir;
	}

	~AppBuilder () {

	}

	private int compile_automake () {
		string conff = Path.build_filename (srcdir, "configure", null);
		bool runconfigure = false;
		if (FileUtils.test (conff, FileTest.EXISTS)) {
			runconfigure = true;
		}
		if (!runconfigure) {
			string agenf = Path.build_filename (srcdir, "autogen.sh", null);
			if (FileUtils.test (agenf, FileTest.EXISTS)) {
				// Create AM configure script
				int exit_status;
				Process.spawn_command_line_sync	(agenf, null, null, out exit_status);
				if (exit_status != 0)
					return exit_status;
				runconfigure = true;
			}
		}
		// Now run the configure script
		int exit_status;
		Process.spawn_command_line_sync	(conff, null, null, out exit_status);
		if (exit_status != 0)
			return exit_status;
		// Make it!
		Process.spawn_command_line_sync	("make", null, null, out exit_status);
		if (exit_status != 0)
			return exit_status;

		// Done.
		return 0;
	}

	public int compile_software () {
		int ret = -1;
		string lastdir = Environment.get_current_dir ();
		Environment.set_current_dir (srcdir);

		ret = compile_automake ();

		Environment.set_current_dir (lastdir);
		return ret;
	}

}

} // End of namespace
