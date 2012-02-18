/* manager.vala
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
using Gee;
using Listaller;
using Listaller.Utils;

[CCode (cheader_filename = "listaller-glib/manager.h")]

namespace Listaller {

public class Manager : MsgObject {
	private Settings conf;

	public signal void status_changed (StatusItem status);
	public signal void application (AppItem appid);

	public Settings settings {
		get { return conf; }
		set { conf = value; }
	}

	/*
	 * @param: settings A valid LiSettings instance, describing basic settings (or null)
	 */
	public Manager (Settings? settings) {
		base ();
		conf = settings;
		if (conf == null)
			conf = new Settings (false);
	}

	private void emit_status (StatusEnum status, string info) {
		StatusItem item = new StatusItem (status);
		item.info = info;
		status_changed (item);
	}

	private bool init_db (out SoftwareDB sdb, bool writeable = true) {
		SoftwareDB db = new SoftwareDB (conf, true);
		// Connect the database events with this application manager
		connect_with_object (db, ObjConnectFlags.NONE);
		db.application.connect ( (a) => { this.application (a); } );

		sdb = db;
		if (writeable) {
			if (!db.open_write ()) {
				emit_error (ErrorEnum.DB_OPEN_FAILED, _("Unable to open software database for reading & writing!"));
				return false;
			}
		} else {
			if (!db.open_read ()) {
				emit_error (ErrorEnum.DB_OPEN_FAILED, _("Unable to open software database for reading only!"));
				return false;
			}
		}
		return true;
	}

	public bool find_applications (AppSource filter, out ArrayList<AppItem> appList = null) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return false;

		return db.find_all_applications (filter, out appList);
	}

	/* find_applications_by_values: Find applications which match the strings in values
	 *
	 * @values:
	 */
	public bool find_applications_by_values (AppSource filter,
						 [CCode (array_null_terminated = true, array_length = false)] string[] values,
						 out ArrayList<AppItem> appList = null) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return false;

		ArrayList<AppItem> res = db.find_applications (values);
		// Emit signals for found applications
		foreach (AppItem app in res)
			application (app);
		return true;
	}

	public bool remove_application (AppItem app) {
		app.fast_check ();

		if (app.shared != conf.sumode) {
			if (app.shared)
				warning (_("Trying to remove shared application, but AppManager is not in superuser mode!\nSetting AppManager to superuse mode now."));
			else
				warning (_("Trying to remove local application, but AppManager is in superuser mode!\nSetting AppManager to local mode now."));
			conf.sumode = app.shared;
		}

		// Emit that we're starting
		emit_status (StatusEnum.ACTION_STARTED,
			     _("Removal of %s started.").printf (app.full_name));

		SoftwareDB db;
		if (!init_db (out db))
			return false;

		// Check if this application exists, if not exit
		if (db.get_application_by_id (app) == null) {
			emit_error (ErrorEnum.REMOVAL_FAILED, _("Could not uninstall application %s. It is not installed.").printf (app.full_name));
			return false;
		}
		// Remove all files which belong to this application
		ArrayList<string>? files = db.get_application_filelist (app);
		if (files == null) {
			emit_error (ErrorEnum.REMOVAL_FAILED, _("'%s' has no file-list registered. The software database might be broken.").printf (app.full_name));
			return false;
		}

		foreach (string fname in files) {
			if (FileUtils.test (fname, FileTest.EXISTS)) {
				int ret = FileUtils.remove (fname);
				if (ret != 0) {
					emit_error (ErrorEnum.REMOVAL_FAILED, _("Could not remove file %s!").printf (fname));
					return false;
				}
				string dirn = Path.get_dirname (fname);
				// Remove directory if it is empty
				if (dir_is_empty (dirn)) {
					DirUtils.remove (dirn);
				}
			}
		}
		bool ret = db.remove_application (app);

		emit_status (StatusEnum.REMOVAL_FINISHED,
			     _("Removal of %s finished.").printf (app.full_name));

		return ret;
	}

	public AppItem? get_appitem_by_idname (string idname) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return null;

		AppItem? app = db.get_application_by_idname (idname);
		return app;
	}

	public AppItem? get_appitem_by_fullname (string full_name) {
		SoftwareDB db;
		if (!init_db (out db, false))
			return null;

		AppItem? app = db.get_application_by_fullname (full_name);
		return app;
	}

	public bool scan_applications () {
		//TODO: Scan for 3rd-party installed apps
		return true;
	}

}

} // End of namespace
