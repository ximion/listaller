/* depscan.vala
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

// Workaround for Vala bug #618931
private const string _PKG_VERSION2 = Config.VERSION;

private class DependencyScanner : Object {
	private DepscanLDD ldd;
	private string targetdir;
	private ArrayList<string> requires;

	public ArrayList<string> required_files {
		get { return requires; }
	}

	public DependencyScanner (string target_dir) {
		targetdir = target_dir;
		requires = new ArrayList<string> ();
		ldd = new DepscanLDD ();
	}

	private ArrayList<string>? get_file_list (string dir) {
		ArrayList<string> list = new ArrayList<string> ();
		try {
			var directory = File.new_for_path (dir);

			var enumerator = directory.enumerate_children (FILE_ATTRIBUTE_STANDARD_NAME, 0);

			FileInfo file_info;
			while ((file_info = enumerator.next_file ()) != null) {
				string path = Path.build_filename (dir, file_info.get_name (), null);
				if ((FileUtils.test (path, FileTest.IS_SYMLINK)) || (file_info.get_is_hidden ()))
					continue;
				if (!FileUtils.test (path, FileTest.IS_REGULAR)) {
					ArrayList<string> subdir_list = get_file_list (path);
					// There was an error, exit
					if (subdir_list == null)
						return null;
					list.add_all (subdir_list);
				} else {
					// Presort files here
					if (FileUtils.test (path, FileTest.IS_EXECUTABLE))
						list.add (path);
				}
			}

		} catch (Error e) {
			stderr.printf (_("Error: %s\n"), e.message);
			return null;
		}
		return list;
	}

	public bool compile_required_files_list () {
		requires.clear ();
		stdout.printf ("Please wait...");
		ArrayList<string> files = get_file_list (targetdir);
		if (files == null)
			return false;

		// Temporary list of dependencies
		LinkedList<string> tmp_requires = new LinkedList<string> ();

		foreach (string s in files) {
			if (ldd.fetch_required_files (s)) {
				tmp_requires.add_all (ldd.required_files);
			}
		}

		stdout.printf ("\r");
		// TODO: Finish this...
		foreach (string s in tmp_requires) {
			stdout.printf (s + "\n");
		}
		return true;
	}
}
