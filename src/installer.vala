/* installer.vala
 *
 * Copyright (C) 2010-2012 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU Lesser General Public License Version 3
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

using GLib;
using Listaller;
using Listaller.Utils;

[CCode (cheader_filename = "listaller-glib/installer.h")]

namespace Listaller {

public class Setup : MsgObject {
	private Settings conf;
	private string fname;
	private IPK.Package ipkp;
	private bool initialized;
	private int inst_progress;
	private int full_progress;

	public signal void status_changed (StatusItem status);

	public Settings settings {
		get { return conf; }
	}

	public IPK.Control? control {
		get { if (initialized) {
				return ipkp.control;
			} else {
				return null;
			}
		}
	}

	public Setup (string ipkfilename, Settings? settings) {
		base ();
		conf = settings;
		if (conf == null)
			conf = new Settings (false);
		fname = ipkfilename;
		// Set up IPK package instance and connect it with this setup
		ipkp = new IPK.Package (fname, settings);
		connect_with_object_all (ipkp);

		initialized = false;
	}

	internal override void change_progress (int progress, int sub_progress) {
		if (progress >= 0)
			full_progress = (int) Math.round ((inst_progress + progress) / 2);

		progress_changed (full_progress, sub_progress);
	}

	private void emit_status (StatusEnum status, string info) {
		StatusItem item = new StatusItem (status);
		item.info = info;
		status_changed (item);
	}

	public bool initialize () {
		bool ret = false;
		ret = ipkp.initialize ();
		if (ret)
			initialized = true;
		inst_progress = 0;
		full_progress = 0;
		return ret;
	}

	public AppItem? get_current_application () {
		if (control == null)
			return null;
		return control.get_application ();
	}

	private bool install_application () {
		bool ret = true;

		conf.lock ();
		// Create software DB link and connect status handlers
		SoftwareDB db = new SoftwareDB (conf, is_root ());
		connect_with_object_all (db);

		// Open & lock database (we need write access here!)
		if (!db.open_write ()) {
			emit_error (ErrorEnum.DB_OPEN_FAILED, _("Could not open the software database, maybe it is locked at time.\nPlease close all other running installations to continue!"));
			return false;
		}

		// Construct LiAppItem
		AppItem app = ipkp.control.get_application ();
		app.origin = AppOrigin.IPK;
		app.dependencies = "?";
		app.fast_check ();
		// This is required to only check the shared db for this application
		if (is_root ())
			app.shared = true;

		// We start now!
		emit_status (StatusEnum.ACTION_STARTED,
			     _("Running installation of %s").printf (app.full_name));

		inst_progress = 25;
		change_progress (0, -1);

		// Check if this application is already installed
		AppItem? dbapp = db.get_application_by_id (app);
		if (dbapp != null) {
			string text = "";
			int i = app.compare_version_with (dbapp.version);
			if (i < 0)
				// TODO: If we have a lower version, offer upgrade!
				text = _("A lower version '%s' of this application has already been installed.").printf (dbapp.version);
			else if (i > 0)
				text = _("A higher version '%s' of this application has already been installed.").printf (dbapp.version);
			else
				text = _("This application has already been installed.");
			emit_error (ErrorEnum.ALREADY_INSTALLED, "%s %s".printf (text, _("Please remove the existing version to continue!")));
			return false;
		}

		// Emit status message
		emit_status (StatusEnum.RESOLVING_DEPENDENCIES,
			     _("Resolving dependencies of '%s'.").printf (app.full_name));

		Gee.ArrayList<IPK.Dependency> pkgDeps = ipkp.control.get_dependencies ();

		// Construct new dependency manager
		DepInstaller depinst = new DepInstaller (db);
		// Forward DependencyManager events to setup. Send DepMan progress as subprogress
		connect_with_object (depinst, ObjConnectFlags.IGNORE_PROGRESS);

		// Install possibly missing dependencies
		ret = depinst.install_dependencies (ref pkgDeps);
		if (!ret) {
			// If dependency installation failed, exit
			return false;
		}

		inst_progress = 50;

		// Emit status message
		emit_status (StatusEnum.INSTALLING_FILES,
			     _("Copying files to their destination"));

		// Install all files to their destination
		ret = ipkp.install_all_files ();

		if (!ret) {
			return false;
		}

		inst_progress = 75;
		change_progress (100, -1);

		// Emit status message
		emit_status (StatusEnum.REGISTERING_APPLICATION,
			     _("Making '%s' known to your system.").printf (app.full_name));

		// We don't trust the IPKpackage: App might have changed, so set again if this
		// is a shared app.
		if (is_root ())
			app.shared = true;

		// Now register the item
		ret = db.add_application (app);
		if (!ret) {
			ipkp.rollback_installation ();
			return false;
		}

		// Link the dependency idNames to this application
		ret = db.set_application_dependencies (app.idname, pkgDeps);
		if (!ret) {
			return false;
		}

		ret = db.add_application_filelist (app, ipkp.get_file_entries ());
		conf.unlock ();

		inst_progress = 100;
		change_progress (100, -1);

		// Emit status message (setup finished)
		StatusItem sitem = new StatusItem (StatusEnum.INSTALLATION_FINISHED);
		if (ret) {
			sitem.info = _("Installation completed!");
		} else {
			// Undo all changes
			ipkp.rollback_installation ();
			sitem.info = _("Installation failed!");
		}
		status_changed (sitem);

		return ret;
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		if ((type == PackageKit.ProgressType.PERCENTAGE) ||
			(type == PackageKit.ProgressType.SUBPERCENTAGE)) {
				change_progress (progress.percentage, progress.subpercentage);
			}
	}

	private bool install_superuser () {
		bool ret = true;
		PackageKit.Client client = new PackageKit.Client ();

		/* PackageKit will handle all Listaller superuser installations.
		 * Therefore, PackageKit needs to be compiled with Listaller support enabled.
		 */

		PackageKit.Results? pkres;
		client.background = false;

		try {
			pkres = client.install_files (true, { fname, null }, null, pk_progress_cb);
		} catch (Error e) {
			emit_error (ErrorEnum.INSTALLATION_FAILED, e.message);
			return false;
		}

		if (pkres.get_exit_code () != PackageKit.Exit.SUCCESS) {
			PackageKit.Error error = pkres.get_error_code ();
			emit_error (ErrorEnum.INSTALLATION_FAILED, error.get_details ());
			return false;
		}

		inst_progress = 100;
		change_progress (100, -1);

		// Emit status message (setup finished)
		StatusItem sitem = new StatusItem (StatusEnum.INSTALLATION_FINISHED);
		if (ret) {
			sitem.info = _("Installation completed!");
		} else {
			// Undo all changes
			ipkp.rollback_installation ();
			sitem.info = _("Installation failed!");
		}
		status_changed (sitem);

		return ret;
	}

	public bool run_installation () {
		bool ret = false;

		// Check if setup was initialized
		if (!initialized) {
			emit_error (ErrorEnum.SETUP_NOT_INITIALIZED, _("Setup has not been initialized!"));
			return false;
		}

		// Initialize console message system
		init_limessage ();

		if ((!is_root ()) && (conf.sumode == true)) {
			ret = install_superuser ();
			finish_limessage ();
			return ret;
		}

		ret = install_application ();
		finish_limessage ();

		return ret;
	}

	public IPK.PackSecurity get_security_info () {
		return ipkp.get_security_info ();
	}
}

} // End of namespace
