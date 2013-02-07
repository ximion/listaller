/* lipa.vala -- Listaller command-line tool (main unit)
 *
 * Copyright (C) 2010-2013 Matthias Klumpp <matthias@tenstral.net>
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

private static const int CONSOLE_RESET   = 0;
private static const int CONSOLE_BLACK   = 30;
private static const int CONSOLE_RED     = 31;
private static const int CONSOLE_GREEN   = 32;
private static const int CONSOLE_YELLOW  = 33;
private static const int CONSOLE_BLUE    = 34;
private static const int CONSOLE_MAGENTA = 35;
private static const int CONSOLE_CYAN    = 36;
private static const int CONSOLE_WHITE   = 37;

public class LipaTool : Object {
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;

	private static bool o_mode_install = false;
	private static bool o_mode_install_remote = false;
	private static bool o_mode_remove = false;

	private static bool o_mode_list = false;
	private static bool o_mode_list__all = false;

	private static bool o_mode_refresh = false;

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
		{ "install-remote", 0, 0, OptionArg.NONE, ref o_mode_install_remote,
		N_("Install a remote IPK package"), null },
		{ "remove", 'r', 0, OptionArg.NONE, ref o_mode_remove,
		N_("Remove an application installed using Listaller"), null },
		{ "list-apps", 'l', 0, OptionArg.NONE, ref o_mode_list,
		N_("List installed Listaller applications"), null },
		{ "all", 0, 0, OptionArg.NONE, ref o_mode_list__all,
		N_("List all installed applications"), null },
		{ "refresh-cache", 0, 0, OptionArg.NONE, ref o_mode_refresh,
		N_("Refresh repository application cache"), null },
		{ null }
	};

	private void print_nocommand_msg () {
		stdout.printf (_("Listaller command-line tool: No command specified.\nRun '%s --help' to see a list of available commands.").printf (args[0]) + "\n");
	}

	public LipaTool (string[] arguments) {
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

		lipa = null;
	}

	[CCode (has_target = false)]
	public static void handle_SIGINT (int sig_num ){
		//stdout.write(STDOUT_FILENO, buffer, strlen(buffer));
		// Clear line
		stdout.printf ("                                              \r");
		stdout.printf ("Terminating...\n");
		if (lipa != null)
			lipa.terminate_action ();

		Posix.termios old = {0};
		Posix.tcgetattr (0, out old);
		console_termios_restore (old);

		Posix.exit(13);
	}

	public void run () {
		if (exit_code != 0)
			return;

		if (o_show_version) {
			stdout.printf ("lipa tool, part of Listaller version: %s\n", Listaller.get_full_version_info_str ());
			return;
		}

		string? value = null;
		if (args.length > 1)
			value = args[1];

		// Set up signal handler for termination
		Posix.sigaction_t handler = Posix.sigaction_t ();
		handler.sa_handler = handle_SIGINT;
		handler.sa_flags = 0;
		Posix.sigemptyset (handler.sa_mask);
		Posix.sigaction (Posix.SIGINT, handler, null);

		if (o_mode_install) {
			if (value == null) {
				stderr.printf (_("Missing parameter for 'install' action: No IPK package specified.\n"));
				exit_code = 4;
				return;
			}

			var lipaInstall = new LipaInstaller ();
			lipa = lipaInstall;

			lipaInstall.install_package (value);

		} else if (o_mode_install_remote) {
			if (value == null) {
				stderr.printf (_("Missing parameter for 'install-remote' action: No IPK package-id specified.\n"));
				exit_code = 4;
				return;
			}

			var lipaManager = new LipaManager ();
			lipa = lipaManager;

			lipaManager._test_install_remote_app (value);

		} else if (o_mode_remove) {
			if (value == null) {
				stderr.printf (_("Missing parameter for 'remove' action: No application-id or name specified.\n"));
				exit_code = 4;
				return;
			}

			var lipaManager = new LipaManager ();
			lipa = lipaManager;

			lipaManager.remove_application (value);

		} else if (o_mode_list) {
			var lipaManager = new LipaManager ();
			lipa = lipaManager;

			lipaManager.list_applications (o_mode_list__all);

		} else if (o_mode_refresh) {
			var lipaManager = new LipaManager ();
			lipa = lipaManager;

			lipaManager.refresh_cache ();

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
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		var main = new LipaTool (args);
		set_console_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("Lipa");

		// Run the application
		main.run ();
		int code = main.exit_code;
		return code;
	}
}
