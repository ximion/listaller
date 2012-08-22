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
using Listaller;

public class AppCompile : Object {
	// Cmd options
	private static string o_src_dir = "";
	private static string o_target_dir = "";
	private static bool o_show_version = false;
	private static bool o_strip_files = false;
	private static bool o_verbose_mode;
	private string cmp_arguments = "";

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "sourcedir", 's', 0, OptionArg.FILENAME, ref o_src_dir,
			N_("Path to the application's source code"), N_("DIRECTORY") },
		{ "target_dir", 'o', 0, OptionArg.FILENAME, ref o_target_dir,
			N_("Software install prefix"), N_("DIRECTORY") },
		{ "strip", 0, 0, OptionArg.NONE, ref o_strip_files,
			N_("Strip debug infos from files in install-target"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Enable verbose mode"), null },
		{ null }
	};

	public AppCompile (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- compile software automatically.");
		opt_context.set_help_enabled (true);
		opt_context.add_main_entries (options, null);

		bool b = false;
		for (uint i = 0; i < args.length; i++) {
			string arg = args[i];
			if (b)
				cmp_arguments += " " + arg;
			if (arg == "--")
				b = true;
		}
		cmp_arguments.chug ();

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
			stdout.printf ("appcompile helper, version: %s\n", Config.VERSION);
			return;
		}
		// Take directory from options, otherwise use current dir
		string srcdir = o_src_dir;
		string targetdir = o_target_dir;

		if (srcdir == "")
			srcdir = Environment.get_current_dir ();

		if (o_strip_files) {
			Extra.AutoStrip strip = new Extra.AutoStrip (srcdir, targetdir);
			exit_code = strip.strip_binaries ();
			Report.log_info ("Stripped debug information from binaries.");
			return;
		}
		Extra.AutoCompiler acomp = new Extra.AutoCompiler (srcdir, targetdir);
		exit_code = acomp.compile_software (cmp_arguments);

		Report report = Report.get_instance ();
		if (!report.is_empty ())
			stdout.printf ("AppCompile Report:\n%s\n", report.to_string ());

		return;
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
		Intl.bind_textdomain_codeset(Config.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(Config.GETTEXT_PACKAGE);

		// Set everything up...
		var main = new AppCompile (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("AppCompile");

		// Now run the application!
		main.run ();
		int code = main.exit_code;
		return code;
	}

}
