/* runapp.vala - Small wrapper to run Listaller-installed apps
 *
 * Copyright (C) 2010-2013 Matthias Klumpp
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

public class RunApp : Object {
	private Manager limgr;

	public RunApp () {
		limgr = new Manager (true);
	}

	public int run_application (string commandLine, string ld_env) {
		int exit_status;
		try {
			debug ("LD_PATH env is: %s", ld_env);
			debug ("Command line: %s", commandLine);
			Process.spawn_command_line_sync (commandLine, null, null, out exit_status);
		} catch (Error e) {
			stderr.printf ("Could not run: %s\n", e.message);
		}
		return exit_status;
	}

	public int execute (string appName) {
		if (appName == "")
			return 4;

		Listaller.AppItem? app = limgr.get_application_by_idname (appName);

		if (app != null) {
			string ld_env = limgr.get_app_ld_environment (app);
			return run_application (app.get_raw_cmd (true), ld_env);
		}

		return run_application (appName, "");
	}

}

public class CmdApp : Object {
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;
	private string o_appname;

	public int exit_code { set; get; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
		{ null }
	};

	public CmdApp (string[] args) {
		exit_code = 0;
		o_appname = null;
		var opt_context = new OptionContext ("- run applications.");
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
			if (o_appname == null) {
				o_appname = arg;
			}
		}
	}

	public void run () {
		if (exit_code != 0)
			return;

		if (o_show_version) {
			stdout.printf ("runapp tool, part of Listaller version: %s\n", Listaller.get_full_version_info_str ());
			return;
		}

		if (o_appname == null) {
			stderr.printf (_("No application specified!") + "\n");
			exit_code = 4;
			return;
		}

		var runApp = new RunApp ();
		exit_code = runApp.execute (o_appname);
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		var main = new CmdApp (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("RunApp");

		// Run the application
		main.run ();
		int code = main.exit_code;
		return code;
	}
}