/* main.vala
 *
 * Copyright (C) 2011  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
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

// Workaround for Vala bug #618931
private const string _PKG_VERSION1 = Config.VERSION;

public class DepScanCmd : Object {
	// Cmd options
	private static bool _show_version = false;
	private static bool _run_recursive = false;
	private static string _input_path = null;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref _show_version,
			N_("Show the application's version"), null },
		{ "recursive", 'r', 0, OptionArg.NONE, ref _run_recursive,
			N_("Use recursive mode"), null },
		{ null }
	};

	public DepScanCmd (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- scan software dependencies.");
		opt_context.set_help_enabled (true);
		opt_context.add_main_entries (options, null);
		try {
			opt_context.parse (ref args);
		} catch (Error e) {
			stdout.printf (e.message + "\n");
			stdout.printf (_("Run '%s --help' to see a full list of available command line options.\n"), args[0]);
			exit_code = 1;
			return;
		}

		for (int i = 1; i < args.length; i++) {
			string arg = args[i];
			if (_input_path == null) {
				_input_path = arg;
			}
		}
	}

	private void on_error (string details) {
		stderr.printf (_("ERROR: %s"), details + "\n");
		exit_code = 6;
	}

	public void run () {
		bool done = false;
		if (_show_version) {
			stdout.printf ("Listaller bundle version: %s\n", Config.VERSION);
			return;
		}
		if ((_input_path == null) || (_input_path == "")) {
			stdout.printf (_("No path given!") + "\n");
			exit_code = 2;
			return;
		}

		DependencyScanner scan = new DependencyScanner (_input_path);
		scan.recursive = _run_recursive;
		scan.compile_required_files_list ();
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
		Intl.bind_textdomain_codeset(Config.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(Config.GETTEXT_PACKAGE);
		// Run the application
		var main = new DepScanCmd (args);
		main.run ();
		int code = main.exit_code;
		return code;
	}

}
