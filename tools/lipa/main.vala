/* main.vala -- Listaller command-line tool (main unit)
 *
 * Copyright (C) 2010-2012 Matthias Klumpp
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
using Config;
using Listaller;

public class CmdApp : Object {
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;

	private static bool o_mode_install = false;

	public int exit_code { set; get; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
		{ "install", 'i', 0, OptionArg.NONE, ref o_mode_install,
		N_("Install an IPK package"), null },
		{ null }
	};

	public CmdApp (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- Listaller command-line tool.");
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

		if (args.length <= 1) {
			stdout.printf (_("Listaller command-line tool: No command specified.\nRun '%s --help' to see a list of available commands.").printf (args[0]) + "\n");
			exit_code = 0;
			return;
		}
	}

	public void run () {
		if (exit_code != 0)
			return;

		if (o_show_version) {
			stdout.printf ("Part of Listaller version: %s\n", Config.VERSION);
			return;
		}

		var lipa = new Lipa ();
		exit_code = lipa.execute ();
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
		Intl.bind_textdomain_codeset(Config.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(Config.GETTEXT_PACKAGE);

		var main = new CmdApp (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("Lipa");

		// Run the application
		main.run ();
		int code = main.exit_code;
		return code;
	}
}
