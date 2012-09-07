/* lirepo.vala -- Main file for Listaller repository tool
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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

public class Lirepo : Object {
	// Cmd options
	private static string o_repo_dir = "";
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
		{ null }
	};

	public Lirepo (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- maintain IPK package repositories.");
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
	}

	public void run () {
		if (exit_code > 0)
			return;

		bool done = false;
		if (o_show_version) {
			stdout.printf ("lirepo tool, part of Listaller version: %s\n", Listaller.get_full_version_info_str ());
			return;
		}
		// Take directory from options, otherwise use current dir
		string repodir = o_repo_dir;
		if (repodir == "")
			repodir = Environment.get_current_dir ();

		//! TODO
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		var main = new Lirepo (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("lirepo");

		// Run the application
		main.run ();

		// Display final report
		Report report = Report.get_instance ();
		if (!report.is_empty ())
			stdout.printf ("\nRepository Tool Report:\n%s\n", report.to_string ());

		int code = main.exit_code;
		return code;
	}

}
