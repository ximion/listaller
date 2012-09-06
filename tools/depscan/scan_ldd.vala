/* scan_ldd.vala - Detect ELF binary dependencies using LDD
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

private class DepscanLDD : Object, IDepScanEngine {
	private ArrayList<string> filedeps;
	private ArrayList<string> project_files;

	public DepscanLDD (ArrayList<string> available_files) {
		filedeps = new ArrayList<string> ();
		project_files = new ArrayList<string> ();

		foreach (string s in available_files) {
			project_files.add (Path.get_basename (s));
		}

	}

	public bool fetch_required_files (string binaryname) {
		filedeps.clear ();

		string cmd = "/usr/bin/ldd " + binaryname;
		string output, stderror = null;
		int exit_status = 0;
		try {
			Process.spawn_command_line_sync (cmd, out output, out stderror, out exit_status);
		} catch (SpawnError e) {
			debug (e.message);
			return false;
		}
		if ((exit_status != 0) || (output == null))
			return false;

		string[] tmp = output.split ("\n");

		for (int i = 0; i < tmp.length; i++) {
			string h = tmp[i];
			string[] dep = h.split ("=>");
			if (dep.length <= 0)
				continue;

			h = dep[0].strip ();
			if (!h.contains ("(")) {
				if (project_files.index_of (h) <= 0)
					filedeps.add ("%s%s".printf ("lib:", h));
			}
		}

		return true;
	}

	public bool can_be_used (string fname) {
		return FileUtils.test (fname, FileTest.IS_EXECUTABLE);
	}

	public ArrayList<string> required_files () {
		return filedeps;
	}
}
