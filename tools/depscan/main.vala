/* main.vala
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

public class DepScanCmd : Object {
	// Cmd options
	private static bool o_show_version = false;
	private static bool o_run_recursive = false;
	private static bool o_simpletext = false;
	private static bool o_verbose_mode = false;
	private static string o_input_path = null;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
			N_("Show the application's version"), null },
		{ "recursive", 'r', 0, OptionArg.NONE, ref o_run_recursive,
			N_("Use recursive mode"), null },
		{ "simpletext", 0, 0, OptionArg.NONE, ref o_simpletext,
			N_("Print machine-readable simple text"), null },
		{ "verbose", 'r', 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
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

		for (uint i = 1; i < args.length; i++) {
			string arg = args[i];
			if (o_input_path == null) {
				o_input_path = arg;
			}
		}
	}

	private void on_error (string details) {
		stderr.printf (_("ERROR: %s"), details + "\n");
		exit_code = 6;
	}

	public void run () {
		bool done = false;
		if (o_show_version) {
			stdout.printf ("Listaller bundle version: %s\n", Config.VERSION);
			return;
		}
		if ((o_input_path == null) || (o_input_path == "")) {
			stdout.printf (_("No path given!") + "\n");
			exit_code = 2;
			return;
		}

		DependencyScanner scan = new DependencyScanner (o_input_path, o_simpletext);
		scan.recursive = o_run_recursive;
		scan.compile_required_files_list ();
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
		Intl.bind_textdomain_codeset(Config.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(Config.GETTEXT_PACKAGE);

		var main = new DepScanCmd (args);
		Listaller.set_console_mode (true);
		Listaller.set_verbose_mode (o_verbose_mode);
		Listaller.add_log_domain ("DepScan");

		// Run the application
		main.run ();
		int code = main.exit_code;
		return code;
	}

}
