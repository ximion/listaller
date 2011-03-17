/* installer.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
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
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using GLib;

// Workaround for Vala bug #618931
private const string _PKG_VERSION8 = Config.VERSION;

public class LiSetup : Object {
	private LiSettings conf;
	private string fname;
	private IPKPackage ipkp;
	private bool initialized;

	public signal void error_code (LiErrorItem error);
	public signal void progress_changed (int progress, int subprogress);
	public signal void status_changed (LiStatusItem status);
	public signal void message (LiMessageItem message);

	public LiSettings settings {
		get { return conf; }
	}

	public IPKControl control {
		get { return ipkp.control; }
	}

	public LiSetup (string ipkfilename, LiSettings? settings) {
		conf = settings;
		if (conf == null)
			conf = new LiSettings (false);
		fname = ipkfilename;
		// Set up IPK package instance
		ipkp = new IPKPackage (fname, settings);
		ipkp.error_code.connect ((e) => { this.error_code (e); });
		ipkp.message.connect ((m) => { this.message (m); });
		initialized = false;
	}

	private void emit_warning (string msg) {
		// Construct warning message
		LiMessageItem item = new LiMessageItem(LiMessageType.WARNING);
		item.details = msg;
		message (item);
		warning (msg);
	}

	private void emit_message (string msg) {
		// Construct info message
		LiMessageItem item = new LiMessageItem(LiMessageType.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void emit_error (LiError id, string details) {
		// Construct error
		LiErrorItem item = new LiErrorItem(id);
		item.details = details;
		error_code (item);
		critical (details);
	}

	public bool initialize () {
		bool ret = false;
		ret = ipkp.initialize ();
		if (ret)
			initialized = true;
		return ret;
	}

	public bool run_installation () {
		// Check if setup was initialized
		if (!initialized) {
			emit_error (LiError.SETUP_NOT_INITIALIZED, _("Setup has not been initialized!"));
			return false;
		}
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
		db.open ();

		// Construct LiAppItem
		LiAppItem aitem = new LiAppItem (ipkp.control.get_app_name (), ipkp.control.get_app_version ());
		aitem.origin = AppOrigin.IPK;
		aitem.summary = ipkp.control.get_app_summary ();
		//aitem.author = ipkp.control.get_app_author ();
		//aitem.pkgmaintainer = ipkp.control.get_pkg_maintainer ();
		//aitem.categories = ipkp.control.get_app_categories ();
		//aitem.desktop_file = ipkp.control.get_app_desktopfile ();
		aitem.dependencies = "?";
		aitem.fast_check ();

		// Emit status message
		LiStatusItem status1 = new LiStatusItem (LiStatus.RESOLVING_DEPENDENCIES);
		status1.info = _("Resolving dependencies of '%s'.").printf (ipkp.control.get_app_name ());
		status_changed (status1);

		// TODO: Resolve dependencies here!

		// Emit status message
		LiStatusItem status2 = new LiStatusItem (LiStatus.INSTALLING_FILES);
		status2.info = _("Copying files to their destination");
		status_changed (status2);

		// Install all files to their destination
		ret = ipkp.install_all_files ();

		if (!ret)
			return false;

		// Emit status message
		LiStatusItem status3 = new LiStatusItem (LiStatus.REGISTERING_APPLICATION);
		status3.info = _("Making '%s' known to your system.").printf (ipkp.control.get_app_name ());
		status_changed (status3);

		// Set install timestamp
		DateTime dt = new DateTime.now_local ();
		aitem.install_time = dt.to_unix ();
		// Now register the item
		ret = db.add_application (aitem);
		conf.unlock ();

		return ret;
	}
}
