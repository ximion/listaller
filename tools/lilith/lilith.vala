/* lilith.vala -- Listaller package validator and quality checker
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller;

public class LilithTool : Object {
	// Command-line options
	private static bool o_show_version = false;
	private static bool o_verbose_mode;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Enable verbose mode"), null },
		{ null }
	};

	public LilithTool (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- software package quality checker.");
		opt_context.set_help_enabled (true);
		opt_context.add_main_entries (options, null);

		if (args.length <= 1) {
			stderr.printf ("No arguments given!\n");
			exit_code = 1;
			return;
		}

		try {
			opt_context.parse (ref args);
		} catch (Error e) {
			stdout.printf (e.message + "\n");
			stdout.printf (_("Run '%s --help' to see a full list of available command line options.\n"), args[0]);
			exit_code = 1;
			return;
		}
	}

	public void run () {
		bool done = false;
		if (o_show_version) {
			stdout.printf ("Lilith, version: %s\n", PkgConfig.VERSION);
			return;
		}

		return;
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		// Set everything up...
		var main = new LilithTool (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("Lilith");

		// Now run the application!
		main.run ();
		int code = main.exit_code;
		return code;
	}
}
