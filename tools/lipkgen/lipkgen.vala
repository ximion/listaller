/* lipkgen.vala -- Main file for lipkgen
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
using Listaller.Extra;

public class LipkgenTool : Object {
	// Cmd options
	private static string o_src_dir = "";
	private static string o_output_dir = "";
	private static bool o_show_version = false;
	private static bool o_build_mode = false;
	private static bool o_do_autocompile = false;
	private static bool o_sign_pkg = false;
	private static bool o_verbose_mode = false;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "build", 'b', 0, OptionArg.NONE, ref o_build_mode,
		N_("Build IPK package"), null },
		{ "sourcedir", 's', 0, OptionArg.FILENAME, ref o_src_dir,
			N_("Path to Listaller package source directory"), N_("DIRECTORY") },
		{ "outdir", 'o', 0, OptionArg.FILENAME, ref o_output_dir,
			N_("IPK package output directory"), N_("DIRECTORY") },
		{ "sign", 0, 0, OptionArg.NONE, ref o_sign_pkg,
			N_("GPG-Sign the resulting package"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
		{ null }
	};

	public LipkgenTool (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- build IPK setup packages.");
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

	private void on_error (string details) {
		stderr.printf (_("error: %s"), details + "\n");
		exit_code = 6;
	}

	private void on_message (MessageItem mitem) {
		string prefix = "?";
		switch (mitem.mtype) {
			case MessageEnum.INFO: prefix = "I";
				break;
			case MessageEnum.WARNING: prefix = "W";
				break;
			case MessageEnum.CRITICAL: prefix = "C";
				break;
			default: prefix = "!?";
				break;
		}
		stdout.printf (" " + prefix + ": %s", mitem.details + "\n");
	}

	public void run () {
		if (exit_code > 0)
			return;

		bool done = false;
		if (o_show_version) {
			stdout.printf ("lipkgen tool, part of Listaller version: %s\n", Listaller.get_full_version_info_str ());
			return;
		}
		// Take directory from options, otherwise use current dir
		string srcdir = o_src_dir;
		if (srcdir == "")
			srcdir = Environment.get_current_dir ();

		if (o_build_mode) {
			// Prepare the application
			AppPrepare prep = new AppPrepare (srcdir);
			prep.error_message.connect (on_error);
			if (!prep.initialize ()) {
				exit_code = 3;
				return;
			}
			pkbuild_action ("Compiling/preparing sources", true);
			exit_code = prep.run_compile ();
			if (exit_code != 0)
				return;

			// Build the IPK package
			pkbuild_action ("Building package", true);
			IPK.Builder builder = new IPK.Builder (srcdir);
			builder.error_message.connect (on_error);
			builder.message.connect (on_message);
			builder.output_dir = o_output_dir;
			builder.sign_package = o_sign_pkg;

			if (!builder.initialize ()) {
				exit_code = 3;
				return;
			}
			if (!builder.build_ipk ()) {
				exit_code = 4;
				return;
			}
		}
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		var main = new LipkgenTool (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("lipkgen");

		// Run the application
		main.run ();

		// Display final report
		Report report = Report.get_instance ();
		if (!report.is_empty ())
			stdout.printf ("\nLipkgen Final Report:\n%s\n", report.to_string ());

		int code = main.exit_code;
		return code;
	}

}
