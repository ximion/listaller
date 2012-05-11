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

private static const int CONSOLE_RESET   = 0;
private static const int CONSOLE_BLACK   = 30;
private static const int CONSOLE_RED     = 31;
private static const int CONSOLE_GREEN   = 32;
private static const int CONSOLE_YELLOW  = 33;
private static const int CONSOLE_BLUE    = 34;
private static const int CONSOLE_MAGENTA = 35;
private static const int CONSOLE_CYAN    = 36;
private static const int CONSOLE_WHITE   = 37;

public class CmdApp : Object {
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;

	private static bool o_mode_install = false;
	private static bool o_mode_remove = false;

	private string[] args;

	private static LipaModule lipa;

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

	private void print_nocommand_msg () {
		stdout.printf (_("Listaller command-line tool: No command specified.\nRun '%s --help' to see a list of available commands.").printf (args[0]) + "\n");
	}

	public CmdApp (string[] arguments) {
		exit_code = 0;
		var opt_context = new OptionContext ("- Listaller command-line tool.");
		opt_context.set_help_enabled (true);
		opt_context.add_main_entries (options, null);

		try {
			opt_context.parse (ref arguments);
		} catch (Error e) {
			stdout.printf (e.message + "\n");
			stdout.printf (_("Run '%s --help' to see a full list of available command line options.\n"), arguments[0]);
			exit_code = 1;
			return;
		}
		args = arguments;

		if (args.length <= 1) {
			print_nocommand_msg ();
			exit_code = 0;
			return;
		}

		lipa = null;
	}

	[CCode (has_target = false)]
	public static void handle_SIGINT(int sig_num ){
		//stdout.write(STDOUT_FILENO, buffer, strlen(buffer));
		stdout.printf ("Terminating...\n");
		if (lipa != null)
			lipa.terminate_action ();
		Posix.exit(0);
	}

	public void run () {
		if (exit_code != 0)
			return;

		if (o_show_version) {
			stdout.printf ("Part of Listaller version: %s\n", Config.VERSION);
			return;
		}

		string? value = null;
		if (args.length > 1)
			value = args[1];

		// Set up signal handler for termination
		Posix.sigaction_t handler = Posix.sigaction_t ();
		handler.sa_handler = handle_SIGINT;
		handler.sa_flags = 0;
		Posix.sigemptyset(handler.sa_mask);
		Posix.sigaction(Posix.SIGINT, handler, null);

		if (o_mode_install) {
			if (value == null) {
				stderr.printf ("Missing parameter for 'install' action: No IPK package specified.");
				exit_code = 4;
				return;
			}

			var lipaInstall = new LipaInstaller ();
			lipa = lipaInstall;

			lipaInstall.install_package (value);

		} else if (o_mode_remove) {

		} else {
			print_nocommand_msg ();
			exit_code = 0;
			return;
		}

		exit_code = lipa.error_code;
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
