/* lipa-manager.vala -- Handle application management tasks in lipa
 *
 * Copyright (C) 2012 Matthias Klumpp
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

public class LipaManager : LipaModule {
	private Manager mgr;
	private bool show_progress;

	public LipaManager () {
		base ();
		show_progress = true;

		mgr = new Manager (liconf);
		mgr.message.connect (manager_message);
		mgr.error_code.connect (manager_error_code);
		//! mgr.status_changed.connect (manager_status_changed);
		mgr.progress_changed.connect (manager_progress_changed);
		mgr.application.connect (manager_new_application);
	}

	private void manager_error_code (ErrorItem error) {
		stderr.printf (error.details);
		error_code = (int) error.error;
	}

	private void manager_message (MessageItem message) {
		stdout.printf ("%s\n", message.details);
	}

	private void manager_progress_changed (int progress) {
		if (show_progress)
			progress_bar.set_percentage (progress);
	}

	private string app_ownership_str (AppItem app) {
		string str_ownership;
		if (!app.shared)
			str_ownership = _("personal");
		else
			str_ownership = _("shared");

		return str_ownership;
	}

	private void print_appitem (AppItem app) {
		stdout.printf ("[%s|%s] %s -- %s\n", _("INSTALLED"), app_ownership_str (app), app.full_name, app.summary);
	}

	private void manager_new_application (AppItem app) {
		print_appitem (app);
	}

	public void remove_application (string app_identifier) {
		bool ret;
		AppItem? app = null;

		// Try to find an application which matches the name the user throws at us
		app = mgr.get_application_by_idname (app_identifier);
		if (app == null) {
			var appList = mgr.get_applications_by_fullname (app_identifier);
			if ((appList == null) || (appList.size == 0)) {
				stderr.printf (_("Could not find application which matches '%s'!\n"), app_identifier);
				error_code = 8;
				return;
			}
			// TODO: This has to be handled better!
			stdout.printf ("Selected application:\n");
			app = appList[0];
			print_appitem (app);
		}

		ret = console_get_prompt (_("Do you want to remove %s (%s) now?").printf (app.full_name, app_ownership_str (app)), true);
		// If user doesn't want to remove the application, exit
		if (!ret)
			return;

		progress_bar.start (_("Removing"));
		// Go!
		ret = mgr.remove_application (app);
		progress_bar.set_percentage (100);
		progress_bar.end ();

		if (ret) {
			print (_("Removal of %s completed successfully!\n"), app.full_name);
		} else {
			print (_("Removal of %s failed!"), app.full_name);
			error_code = 3;
		}
	}

	public void list_applications (bool all = false) {
		AppSource filter = AppSource.EXTERN;
		if (all)
			filter = AppSource.ALL;

		show_progress = false;
		mgr.find_applications (filter);
		show_progress = true;
	}

	public override void terminate_action () {
		if (mgr != null) {
			critical ("Please don't kill the application, it could damage installed applications and produce unexpected behavior!");
		}
	}

}
