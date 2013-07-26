/* likey.vala -- Manage Listaller's key database
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

public class LikeyTool : Object {
	// Cmd options
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;

	private static string o_lookup_key = null;
	private static string o_import_key_fpr = null;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
		{ "lookup", 'l', 0, OptionArg.STRING, ref o_lookup_key,
			N_("Lookup key which matches PATTERN"), N_("PATTERN") },
		{ "import", 'i', 0, OptionArg.STRING, ref o_import_key_fpr,
			N_("Import key with fingerprint FPR"), N_("FPR") },
		{ null }
	};

	public LikeyTool (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- manage Listaller's GPG trusted key database.");
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
		if (exit_code > 0)
			return;

		bool done = false;
		if (o_show_version) {
			stdout.printf ("likey tool, part of Listaller version: %s\n", Listaller.get_full_version_info_str ());
			return;
		}

		/** First handle all the read actions which don't require root privileges */

		KeyManager keymgr = new KeyManager ();

		if (o_lookup_key != null) {
			string key_info;
			key_info = keymgr.get_key_info (o_lookup_key);
			stdout.printf ("%s\n", key_info);
			return;
		}

		if (!Utils.is_root ()) {
			stderr.printf ("%s\n", _("You need to be root to change Listaller's key database!"));
			exit_code = 6;
			return;
		}

		if (o_import_key_fpr != null) {
			bool ret;
			ret = keymgr.import_key (o_import_key_fpr);
			if (!ret) {
				exit_code = 7;
				stderr.printf ("%s\n", _("Failed to import key with fingerprint '%s'.").printf (o_import_key_fpr));
				return;
			}

			stdout.printf ("%s\n", _("Key '%s' imported successfully!").printf (o_import_key_fpr));

			return;
		}
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		var main = new LikeyTool (args);
		set_clitool_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("lirepo");

		// Run the application
		main.run ();

		int code = main.exit_code;
		return code;
	}

}
