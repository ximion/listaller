/* main.vala
 *
 * Copyright (C) 2011 Matthias Klumpp
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
	private static string _src_dir = "";
	private static string _target_dir = "";
	private static bool _show_version = false;
	private static bool _strip_files = false;
	private string cmp_arguments = "";

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref _show_version,
		N_("Show the application's version"), null },
		{ "sourcedir", 's', 0, OptionArg.FILENAME, ref _src_dir,
			N_("Path to the application's source code"), N_("DIRECTORY") },
		{ "target_dir", 'o', 0, OptionArg.FILENAME, ref _target_dir,
			N_("Software install prefix"), N_("DIRECTORY") },
		{ "strip", 0, 0, OptionArg.NONE, ref _strip_files,
			N_("Strip debug infos from files in install-target"), null },
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
		if (_show_version) {
			stdout.printf ("Listaller bundle version: %s\n", Config.VERSION);
			return;
		}
		// Take directory from options, otherwise use current dir
		string srcdir = _src_dir;
		string targetdir = _target_dir;

		if (srcdir == "")
			srcdir = Environment.get_current_dir ();

		if (_strip_files) {
			Extra.AutoStrip strip = new Extra.AutoStrip (srcdir, targetdir);
			exit_code = strip.strip_binaries ();
			Extra.prinfo ("Stripped debug information from binaries.");
			return;
		}
		Extra.AutoCompiler acomp = new Extra.AutoCompiler (srcdir, targetdir);
		exit_code = acomp.compile_software (cmp_arguments);
		return;
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(Config.GETTEXT_PACKAGE, Config.LOCALEDIR);
		Intl.bind_textdomain_codeset(Config.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(Config.GETTEXT_PACKAGE);
		// Run the application
		var main = new AppCompile (args);
		main.run ();
		int code = main.exit_code;
		return code;
	}

}
