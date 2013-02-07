/* lipa-manager.vala -- Handle application management tasks in lipa
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

public class LipaManager : LipaModule {
	private Listaller.Task mgr_task;
	private bool show_progress;

	public LipaManager () {
		base ();
		show_progress = true;

		mgr_task = new Listaller.Task (use_shared_mode);
		mgr_task.message.connect (manager_message_cb);
		mgr_task.error_code.connect (manager_error_code_cb);
		//! mgr_task.status_changed.connect (manager_status_changed);
		mgr_task.progress.connect (manager_progress_cb);
		mgr_task.application.connect (manager_new_application);

		// we don't want duplicate messages
		Report.set_print_fatal_msg (false);
	}

	private void manager_error_code_cb (ErrorItem error) {
		// End progress-bar, if it is shown
		show_progress = false;
		if (!get_verbose_mode ())
			progress_bar.end ();

		stderr.printf ("%s\n", error.details);
		error_code = (int) error.error;
	}

	private void manager_message_cb (MessageItem message) {
		stdout.printf ("%s\n", message.details);
	}

	private void manager_progress_cb (ProgressItem item) {
		int value = item.value;
		if (value < 0)
			return;
		if (item.prog_type != ProgressEnum.MAIN_PROGRESS)
			return;

		if (!get_verbose_mode ())
			if (show_progress)
				progress_bar.set_percentage (value);
	}

	private string app_ownership_str (AppItem app) {
		string str_ownership;
		if (app.state != AppState.INSTALLED_SHARED)
			str_ownership = _("personal");
		else
			str_ownership = _("shared");

		return str_ownership;
	}

	private void print_appitem (AppItem app) {
		string app_state_str = "";

		if (app.state == AppState.AVAILABLE)
			app_state_str = "[%s]".printf (_("AVAILABLE"));
		else
			app_state_str = "[%s|%s]".printf (_("INSTALLED"), app_ownership_str (app));

		stdout.printf ("%s <%s> %s %s -- %s\n", app_state_str, app.idname, app.full_name, app.version, app.summary);
	}

	private void manager_new_application (AppItem app) {
		print_appitem (app);
	}

	public void remove_application (string app_identifier) {
		bool ret;
		AppItem? app = null;

		// Try to find an application which matches the name the user throws at us
		app = mgr_task.get_application_by_idname (app_identifier);
		if (app == null) {
			var appList = mgr_task.get_applications_by_fullname (app_identifier);
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

		if (!get_verbose_mode ())
			progress_bar.start (_("Removing"));

		// Go!
		ret = mgr_task.remove_application (app);
		// On success, set everything to done
		if (ret) {
			if (!get_verbose_mode ())
				progress_bar.set_percentage (100);
		}
		if (!get_verbose_mode ())
			progress_bar.end ();

		if (ret) {
			stdout.printf ("%s\n", _("Removal of %s completed successfully!").printf (app.full_name));
		} else {
			stdout.printf ("%s\n", _("Removal of %s failed!").printf (app.full_name));
			error_code = 3;
		}
	}

	/**
	 * NOTE: This is experimental code at time.
	 * As soon as it is sane, this method will get a better name.
	 */
	public void _test_install_remote_app (string app_idname) {
		stdout.printf ("Downloading...\n");
		Setup? inst = mgr_task.prepare_setup_for_app (app_idname);
		if (inst == null) {
			stdout.printf (_("Could not find application!"));
		} else {
			message (inst.fname);
		}

		var lipa_inst = new LipaInstaller ();
		lipa_inst.run_setup (inst);
		error_code = lipa_inst.error_code;
	}

	public void list_applications (bool all = false) {
		string filter = "?";
		if (all)
			filter = "*";

		show_progress = false;
		mgr_task.find_applications (filter);
		show_progress = true;
	}

	public void refresh_cache () {
		bool ret;

		if (!get_verbose_mode ())
			progress_bar.start (_("Updating package cache"));
		ret = mgr_task.refresh_repository_cache ();

		if (!get_verbose_mode ()) {
			progress_bar.set_percentage (100);
			progress_bar.end ();
		}
	}

	public override void terminate_action () {
		if (mgr_task != null) {
			critical ("Please don't kill the application, it could damage installed applications and produce unexpected behavior!");
		}
	}

}
