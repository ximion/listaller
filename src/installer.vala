/* installer.vala
 *
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@nlinux.org>
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

public class Setup : Object {
	private Settings conf;
	private string fname;
	private IPK.Package ipkp;
	private bool initialized;
	private int inst_progress;
	private int full_progress;
	public bool unittestmode {get; set;}

	public signal void error_code (ErrorItem error);
	public signal void progress_changed (int progress, int subprogress);
	public signal void status_changed (StatusItem status);
	public signal void message (MessageItem message);

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
		unittestmode = false;
		conf = settings;
		if (conf == null)
			conf = new Settings (false);
		fname = ipkfilename;
		// Set up IPK package instance
		ipkp = new IPK.Package (fname, settings);
		ipkp.error_code.connect ((e) => { this.error_code (e); });
		ipkp.message.connect ((m) => { this.message (m); });
		ipkp.progress_changed.connect ((i) => { change_progress (i, -1); });
		initialized = false;
	}

	private void change_progress (int progress, int subprogress) {
		if (progress >= 0)
			full_progress = (int) Math.round ((inst_progress + progress) / 2);
		progress_changed (full_progress, subprogress);
	}

	private void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		message (item);
		li_warning (msg);
	}

	private void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		li_error (details);
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
		SoftwareDB db = new SoftwareDB (conf);
		db.error_code.connect ((error) => {
			this.error_code (error);
		});
		db.message.connect ((message) => {
			this.message (message);
		});
		// Open & lock database
		if (!db.open ()) {
			emit_error (ErrorEnum.DB_OPEN_FAILED, _("Could not open the software database, maybe it is locked at time.\nPlease close all other running installations to continue!"));
			return false;
		}

		// Construct LiAppItem
		AppItem app = ipkp.control.get_application ();
		app.origin = AppOrigin.IPK;
		app.dependencies = "?";
		app.fast_check ();

		// We start now!
		emit_status (StatusEnum.ACTION_STARTED,
			     _("Running installation of %s").printf (app.full_name));

		inst_progress = 25;
		change_progress (0, -1);

		// Check if this application is already installed
		AppItem? dbapp = db.get_application_by_id (app);
		if (dbapp != null) {
			db.close ();
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

		// We don't solve dependencies when unit tests are running
		if (!unittestmode) {
			Deps.Solver solver = new Deps.Solver (db, ipkp.control.get_pkg_dependencies ());
			solver.error_code.connect ((error) => {
				this.error_code (error);
			});
			solver.message.connect ((msg) => {
				this.message (msg);
			});
			solver.progress_changed.connect ((p) => {
				change_progress (-1, p);
			});

			ret = solver.execute ();
			if (!ret) {
				db.close ();
				return false;
			}
		}

		inst_progress = 50;

		// Emit status message
		emit_status (StatusEnum.INSTALLING_FILES,
			     _("Copying files to their destination"));

		// Install all files to their destination
		ret = ipkp.install_all_files ();

		if (!ret) {
			db.close ();
			return false;
		}

		inst_progress = 75;
		change_progress (100, -1);

		// Emit status message
		emit_status (StatusEnum.REGISTERING_APPLICATION,
			     _("Making '%s' known to your system.").printf (app.full_name));

		app.description = control.get_app_description ();
		// Now register the item
		ret = db.add_application (app);
		if (!ret) {
			db.close ();
			return false;
		}
		ret = db.add_application_filelist (app, ipkp.file_list);
		db.close ();

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

		return true;
	}

	public bool run_installation () {
		bool ret = false;

		// Check if setup was initialized
		if (!initialized) {
			emit_error (ErrorEnum.SETUP_NOT_INITIALIZED, _("Setup has not been initialized!"));
			return false;
		}

		if ((!is_root ()) && (conf.sumode == true)) {
			ret = install_superuser ();
			return ret;
		}

		ret = install_application ();
		return ret;
	}

	public IPK.PackSecurity get_security_info () {
		return ipkp.get_security_info ();
	}
}

} // End of namespace
