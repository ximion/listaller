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

/**
 * Performs the installation of an IPK package
 *
 * This class handles all stuff required to install an
 * application.
 * All methods are syncronous right now.
 */
public class Setup : MessageObject {
	private Settings conf;
	private string fname;
	private IPK.Package ipkp;
	private bool initialized;
	private int inst_progress;
	private int full_progress;

	// Holds information about native packages this installation could replace
	private string? pkgReplaces;

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
		pkgReplaces = null;
		conf = settings;
		if (conf == null)
			conf = new Settings (false);

		fname = ipkfilename;
		// Set up IPK package instance and connect it with this setup
		ipkp = new IPK.Package (fname, settings);
		connect_with_object_all (ipkp);

		initialized = false;
	}

	internal override void change_progress (int progress) {
		if (progress >= 0)
			full_progress = (int) Math.round ((inst_progress + progress) / 2);

		progress_changed (full_progress);
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

		// Check for high-enough IPK version
		string ipkSpecVersion = ipkp.control.get_ipk_version ();
		if (ipkSpecVersion != "") {
			// Check if we have a IPK spec which is incompatible with our Listaller (broken backwards compatibility)
			string[] ipk_version_parts = ipkSpecVersion.split (".");
			string[] supported_version_parts = IPK.MINIMUM_IPK_SPEC_VERSION.split (".");
			if (compare_versions (ipk_version_parts[0], supported_version_parts[0]) > 0) {
				// We need a newer Listaller to process this...
				emit_error (ErrorEnum.IPK_NOT_SUPPORTED,
				    _("This package was built using a more recent version of Listaller.\n" +
					"Please update your copy of Listaller to be able to install it."));
				return false;
			}
		}
		if (compare_versions (ipkSpecVersion, IPK.MINIMUM_IPK_SPEC_VERSION) > 0) {
			// Package is too old... we need a newver version of this...
			emit_error (ErrorEnum.IPK_NOT_SUPPORTED,
				    _("This package was built using spec version '%s', but we need at least '%s' or higher to proceed.\n" +
					"Please ask the package author to rebuild the package using a newer Listaller version.").printf (ipkSpecVersion, IPK.MINIMUM_IPK_SPEC_VERSION));
			return false;
		}

		return ret;
	}

	public AppItem? get_current_application () {
		if (control == null)
			return null;
		return control.get_application ();
	}

	private bool install_app_normal () {
		bool ret = true;

		conf.lock ();
		// Create software DB link and connect status handlers
		SoftwareDB db = new SoftwareDB (conf, is_root ());
		connect_with_object_all (db);

		// Open & lock database (we need write access here!)
		if (!db.open_write ()) {
			emit_error (ErrorEnum.DB_OPEN_FAILED,
				    _("Could not open the software database, maybe it is locked at time.\nPlease close all other running installations to continue!"));
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
		change_progress (0);

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

		if (pkgReplaces == null) {
			get_replaced_native_packs ();
		}
		app.replaces = pkgReplaces;

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
		change_progress (100);

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
		change_progress (100);

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

	public void kill_installation_process () {
		if (ipkp != null) {
			// FIXME: We don't support this properly at time!'
			critical ("Aborting an installation! You should not do that!");
			ipkp.rollback_installation ();
		}
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		if (type == PackageKit.ProgressType.PERCENTAGE)
			change_progress (progress.percentage);
		if (type == PackageKit.ProgressType.ITEM_PROGRESS)
			change_item_progress (progress.item_progress.package_id, progress.item_progress.percentage);
	}

	private bool install_app_shared () {
		bool ret = true;
		PackageKit.Task pktask = new PackageKit.Task ();

		/* PackageKit will handle all Listaller superuser installations.
		 * Therefore, PackageKit needs to be compiled with Listaller support enabled.
		 */

		PackageKit.Results? pkres;
		pktask.background = false;

		try {
			pkres = pktask.install_files (PackageKit.TransactionFlag.NONE, { fname, null }, null, pk_progress_cb);
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
		change_progress (100);

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

	/**
	 * Check if there are native packages providing the same functionality
	 *
	 * Software packages can declare a "replaces" element, showing which native componants
	 * become obsolete if their 3rd-party app gets installed. E.g. Firefox could declare
	 * a replacement of "/usr/bin/firefox", so the user gets hinted to maybe remove the native
	 * package. This feature is optional, and Listaller won't perform any removal actions!
	 * This function will fail if the user did not request a shared installation.
	 *
	 * @return List of replaces package-ids, separated by newlines or NULL if no
	 * replacement was declared.
	 */
	public string? get_replaced_native_packs () {
		// No superuser-mode -> No need to replace system packages
		if (!conf.sumode)
			return null;
		AppItem? app = get_current_application ();

		if ((app == null) || (app.replaces == ""))
			return null;

		string[] list = app.replaces.split ("\n");

		var pkslv = new Dep.PkResolver (conf);
		string res = "";
		foreach (string id in list) {
			string? pkid = pkslv.package_name_for_file (id);
			if (pkid == null)
				continue;
			res = "%s%s\n".printf (res, pkid);
		}

		if (res == "")
			return null;

		pkgReplaces = res;

		return res;
	}

	/**
	 * Execute software installation
	 *
	 * @return Success of the operation (if FALSE, an error was emitted)
	 */
	public bool run_installation () {
		bool ret = false;

		// Check if setup was initialized
		if (!initialized) {
			emit_error (ErrorEnum.SETUP_NOT_INITIALIZED, _("Setup has not been initialized!"));
			return false;
		}

		// We only support Linux right now, most apps won't work when installed
		// on other *nix systems (FreeBSD). If someone wants to implement support
		// for other OSes properly, I'm happy to accept patches.
		if (system_os () != "linux") {
			emit_error (ErrorEnum.WRONG_ARCHITECTURE,
				_("The installer will only work with the %s operating system, but you use '%s'.\nSetup can not continue, sorry.").printf ("Linux", system_os_full ()));
			return false;
		}

		// Initialize console message system
		init_limessage ();

		if ((!is_root ()) && (conf.sumode == true)) {
			ret = install_app_shared ();
			finish_limessage ();
			return ret;
		}

		ret = install_app_normal ();
		finish_limessage ();

		return ret;
	}

	public IPK.PackSecurity get_security_info () {
		return ipkp.get_security_info ();
	}
}

} // End of namespace
