/* autocompiler.vala
 *
 * Copyright (C) 2011 Matthias Klumpp
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

namespace Listaller.Extra {

private class AutoCompiler : Object {
	private string srcdir;
	private string targetdir;

	public AutoCompiler (string source_dir, string target_dir = "") {
 		srcdir = source_dir;
		targetdir = target_dir;
		if (!Path.is_absolute (targetdir))
			targetdir = Path.build_filename (Environment.get_current_dir (), targetdir, null);
	}

	~AutoCompiler () {

	}

	private int compile_makefile (string dir = "") {
		// Check for Makefile
		if ((!FileUtils.test (Path.build_filename (Environment.get_current_dir (), "Makefile", null), FileTest.EXISTS)) &&
		    (!FileUtils.test (Path.build_filename (dir, "Makefile", null), FileTest.EXISTS)))
			return -1;

		int exit_status = 0;
		string cmd = "make";
		if (dir != "")
			cmd = "make -C %s".printf (dir);
		// Make it!
		Process.spawn_command_line_sync	(cmd + " all", null, null, out exit_status);
		if (exit_status != 0)
			return 1;
		// Install it, if possible
		Process.spawn_command_line_sync	(cmd + " install DESTDIR=\"" + targetdir + "\"",
			null, null, out exit_status);
		if (exit_status != 0)
			return 1;
		return exit_status;
	}

	private int compile_automake (string args) {
		string conff = Path.build_filename (srcdir, "configure", null);
		bool runconfigure = false;
		if (FileUtils.test (conff, FileTest.EXISTS)) {
			runconfigure = true;
		}
		if (!runconfigure) {
			string cmd = "autogen.sh";
			if (args != "")
				cmd = "%s %s".printf (cmd, args);

			string agenf = Path.build_filename (srcdir, cmd, null);
			if (FileUtils.test (agenf, FileTest.EXISTS)) {
				// Create AM configure script
				int exit_status;
				Process.spawn_command_line_sync	(agenf, null, null, out exit_status);
				if (exit_status != 0)
					return exit_status;
				runconfigure = true;
			}
		}
		if (!runconfigure)
			return -1;

		// Now run the configure script
		int exit_status;
		Process.spawn_command_line_sync	(conff, null, null, out exit_status);
		if (exit_status != 0)
			return exit_status;

		exit_status = compile_makefile ();

		// Done.
		return exit_status;
	}

	private int compile_cmake (string args) {
		// Check if we can use cmake...
		if (!FileUtils.test (Path.build_filename (srcdir, "CMakeLists.txt", null), FileTest.EXISTS)) {
			return -1;
		}
		string builddir = Path.build_filename (srcdir, "build_%s".printf (Utils.system_architecture ()), null);
		Utils.touch_dir (builddir);
		string lastdir = Environment.get_current_dir ();
		Environment.set_current_dir (builddir);

		int exit_status = 0;
		string cmd;
		string a = args;
		if (a != "")
			a += " ";

		a += "-DCMAKE_VERBOSE_MAKEFILE=ON";
		cmd = "cmake %s ..".printf (a);

		Process.spawn_command_line_sync	(cmd, null, null, out exit_status);
		if (exit_status != 0)
			return 1;
		Environment.set_current_dir (lastdir);

		exit_status = compile_makefile (builddir);
		if (exit_status < 0)
			exit_status = 1;


		// Done.
		return exit_status;
	}

	public int compile_software (string args = "") {
		int ret = -1;
		string lastdir = Environment.get_current_dir ();
		Environment.set_current_dir (srcdir);

		targetdir = verify_install_target (targetdir, srcdir);
		if (targetdir == "") {
			prerror ("Unable to proceed: IPK source dir not found!");
			return 1;
		}

		ret = compile_makefile ();
		if (ret < 0)
			ret = compile_automake (args);
		if (ret < 0)
			ret = compile_cmake (args);

		if (ret < 0)
			ret = 6;

		Environment.set_current_dir (lastdir);
		return ret;
	}

}

} // End of namespace
