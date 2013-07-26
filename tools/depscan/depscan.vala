/* depscan.vala - Automatically detect dependencies
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller;

public interface IDepScanEngine {
	public abstract ArrayList<string> required_files ();
	public abstract bool can_be_used (string fname);
	public abstract bool fetch_required_files (string fname);
}

enum ScannerOutput {
	STANDARD,
	SIMPLE_TEXT,
	COMPONENTS,
	LAST;
}

private class DependencyScanner : Object {
	private string targetdir;
	private ScannerOutput output_format;
	public bool recursive { get; set; }
	public string extra_modules_dir { get; set; }

	public DependencyScanner (string target_dir, ScannerOutput oformat = ScannerOutput.STANDARD) {
		targetdir = target_dir;
		output_format = oformat;
	}

	private ArrayList<string>? get_file_list (string dir) {
		ArrayList<string> list = new ArrayList<string> ();
		try {
			var directory = File.new_for_path (dir);

			var enumerator = directory.enumerate_children (FileAttribute.STANDARD_NAME, 0);

			FileInfo file_info;
			while ((file_info = enumerator.next_file ()) != null) {
				string path = Path.build_filename (dir, file_info.get_name (), null);
				if (file_info.get_is_hidden ())
					continue;
				if ((!FileUtils.test (path, FileTest.IS_REGULAR)) && (recursive)) {
					ArrayList<string> subdir_list = get_file_list (path);
					// There was an error, exit
					if (subdir_list == null)
						return null;
					list.add_all (subdir_list);
				} else {
					// Presort files here
					bool uncertain = false;
					string ctype = ContentType.guess (path, null, out uncertain);
					if ((ContentType.can_be_executable (ctype)) ||
						FileUtils.test (path, FileTest.IS_EXECUTABLE))
						list.add (path);
				}
			}

		} catch (Error e) {
			stderr.printf (_("Error: %s\n"), e.message);
			return null;
		}
		return list;
	}

	private void scan_engine_process (ArrayList<string> files, IDepScanEngine eng, ref HashSet<string> required_items) {
		foreach (string s in files) {
			// skip all symlinks here, we don't want to check them (but we need them in the list, e.g. for lib symlinks)
			if (FileUtils.test (s, FileTest.IS_SYMLINK))
				continue;

			if (eng.can_be_used (s))
				if (eng.fetch_required_files (s)) {
					required_items.add_all (eng.required_files ());
				}
		}
	}

	private ArrayList<string> get_scanner_output_default (HashSet<string> required_items) {
		var res = new ArrayList<string> ();
		foreach (string s in required_items) {
				res.add (s);
		}

		return res;
	}

	private ArrayList<string> get_scanner_output_components (HashSet<string> required_items) {
		var cfactory = new Dep.ComponentFactory ();
		var comp_list = new HashMap<string, Dep.Component> ();
		var res = new ArrayList<string> ();

		// intialize factory, loading optional component-items for better dependency-resolving
		cfactory.initialize (true);
		if (!Utils.str_is_empty (extra_modules_dir))
			cfactory.load_extra_modules (extra_modules_dir);
		foreach (Dep.Framework frmw in cfactory.registered_frameworks.values) {
			var iter = required_items.iterator ();
			if (!iter.first ())
				continue;
			do {
				string s = iter.get ();

				Dep.ItemType itype = Dep.Component.item_get_type (s);
				string iname = Dep.Component.item_get_name (s);

				if (frmw.has_matching_item (itype, iname)) {
					comp_list.set (frmw.idname, frmw);
					iter.remove ();
				}

			} while (iter.next ());
		}

		foreach (Dep.Module cmod in cfactory.registered_modules.values) {
			var iter = required_items.iterator ();
			if (!iter.first ())
				continue;
			do {
				string s = iter.get ();
				Dep.ItemType itype = Dep.Component.item_get_type (s);
				string iname = Dep.Component.item_get_name (s);

				if (cmod.has_matching_item (itype, iname)) {
					comp_list.set (cmod.idname, cmod);
					iter.remove ();
				}

			} while (iter.next ());
		}

		foreach (Dep.Component comp in comp_list.values) {
			// pretend to be installed
			// FIXME: don't use this hack and *really* determine if dependency was installed
			comp.installed = true;

			string version = "";
			try {
				version = comp.get_version ();
			} catch (Error e) {
				debug ("Unable to retrieve version for %s: %s", comp.idname, e.message);
			}
			if (version == "")
				res.add (comp.idname);
			else
				res.add ("%s (>= %s)".printf (comp.idname, version));
		}

		if (required_items.size != 0) {
			// Add stuff which we were unable to detect
			res.add ("----");
			res.add ("Dependencies which are not matching a (installed) framework or module:");
			res.add_all (required_items);
		}

		return res;
	}

	public bool compile_required_files_list () {
		var required_items = new HashSet<string> ();
		if (output_format == ScannerOutput.STANDARD)
			stdout.printf ("%s\r", "Please wait...");
		ArrayList<string> files;
		if (FileUtils.test (targetdir, FileTest.IS_REGULAR)) {
			files = new ArrayList<string> ();
			files.add (targetdir);
		} else {
			files = get_file_list (targetdir);
		}
		if (files == null)
			return false;

		// Process binaries
		scan_engine_process (files, new DepscanLDD (files), ref required_items);

		// do we have results?
		if (required_items.size == 0) {
			stdout.printf ("%s\n", "No dependencies found!");
			return false;
		}

		// do transformations of the scanner output and receive it
		ArrayList<string> scan_result;

		if (output_format == ScannerOutput.COMPONENTS)
			scan_result = get_scanner_output_components (required_items);
		else
			scan_result = get_scanner_output_default (required_items);

		// print result
		foreach (string s in scan_result) {
			stdout.printf (s + "\n");
		}

		return true;
	}
}
