/* runapp.vala
 *
 * Copyright (C) 2010-2011 Matthias Klumpp
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

public class RunApp : Object {
	private string appName;
	private Listaller.Manager limgr;

	public RunApp (string aname) {
		appName = aname;
		limgr = new Listaller.Manager (null);
	}

	public void run_application (string commandLine) {
		try {
			Process.spawn_command_line_sync (commandLine);
		} catch (Error e) {
			stderr.printf ("Could not run: %s\n", e.message);
		}
	}

	public void run () {
		Listaller.AppItem? app = limgr.get_appitem_by_idname (appName);
		if (app != null) {
			run_application (app.get_raw_cmd (true));
			return;
		}
		run_application (appName);
	}

	static int main (string[] args) {
		if (args[1] == null) {
			stderr.printf ("No application specified!\n");
			return 5;
		}

		var main = new RunApp (args[1]);
		main.run ();
		return 0;
	}

}
