/* lirepo.vala -- Main file for Listaller repository tool
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

public class LirepoTool : Object {
	// Cmd options
	private static string o_repo_dir = "";
	private static bool o_show_version = false;
	private static bool o_verbose_mode = false;
	private static bool o_init = false;
	private static string? o_add_pkg = null;

	public int exit_code { get; set; }

	private const OptionEntry[] options = {
		{ "init", 0, 0, OptionArg.NONE, ref o_init,
		N_("Initialize an empty repository"), null },
		{ "add", 'a', 0, OptionArg.FILENAME, ref o_add_pkg,
		N_("Add a package to the repository"), null },
		{ "version", 'v', 0, OptionArg.NONE, ref o_show_version,
		N_("Show the application's version"), null },
		{ "verbose", 0, 0, OptionArg.NONE, ref o_verbose_mode,
			N_("Activate verbose mode"), null },
		{ null }
	};

	public LirepoTool (string[] args) {
		exit_code = 0;
		var opt_context = new OptionContext ("- maintain local IPK package repositories.");
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

		if (o_show_version) {
			stdout.printf ("lirepo tool, part of Listaller version: %s\n", Listaller.get_full_version_info_str ());
			return;
		}
		// Take directory from options, otherwise use current dir
		string repodir = o_repo_dir;
		if (repodir == "")
			repodir = Environment.get_current_dir ();

		if (o_init) {
			if (FileUtils.test (Path.build_filename (repodir, "reposetting", null), FileTest.EXISTS)) {
				stderr.printf ("%s\n", _("Could not initialize empty repository: Repo already exists!"));
				exit_code = 1;
				return;
			}

			var repo = new IPK.RepoLocal (repodir);
			stdout.printf ("%s\n", _("Initialized empty IPK repository."));
		} else if (o_add_pkg != null) {
			bool ret = false;
			var repo = new IPK.RepoLocal (repodir);

			ret = repo.add_package (o_add_pkg);
			if (!ret)
				exit_code = 4;
		}

		// show the report
		var report = Report.get_instance ();
		if (!report.is_empty ()) {
			stdout.printf ("%s\n%s\n", _("Repository issue report:"), report.to_string ());
		}
	}

	static int main (string[] args) {
		// Bind Listaller locale
		Intl.setlocale(LocaleCategory.ALL,"");
		Intl.bindtextdomain(PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
		Intl.bind_textdomain_codeset(PkgConfig.GETTEXT_PACKAGE, "UTF-8");
		Intl.textdomain(PkgConfig.GETTEXT_PACKAGE);

		var main = new LirepoTool (args);
		set_clitool_mode (true);
		set_verbose_mode (o_verbose_mode);
		add_log_domain ("lirepo");

		// Run the application
		main.run ();

		// delete report
		Report.get_instance ().delete ();

		int code = main.exit_code;
		return code;
	}

}
