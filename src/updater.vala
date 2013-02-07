/* updater.vala -- Update applications from IPK update repositories
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller {

/**
 * All necessary methods to keep installed applications up-to-date
 *
 * This class allows installing updates for installed applications.
 */
public class Updater : MessageObject {
	private SetupSettings ssettings;
	private Repo.Manager repo_mgr;

	public SetupSettings settings {
		get { return ssettings; }
		set { ssettings = value; }
	}

	public signal void update (UpdateItem update);

	/**
	 * Create a new Listaller update manager
	 *
	 * @param shared_mode Whether we are in shared mode or not.
	 */
	public Updater (bool shared_mode = true) {
		base ();

		ssettings = new SetupSettings ();
		if (shared_mode)
			ssettings.current_mode = IPK.InstallMode.SHARED;
		else
			ssettings.current_mode = IPK.InstallMode.PRIVATE;
	}

	private bool is_shared_mode () {
		return ssettings.current_mode == IPK.InstallMode.SHARED;
	}

	private bool init_db (out SoftwareDB sdb, bool writeable = true) {
		SoftwareDB db = new SoftwareDB (ssettings, true);
		// Connect the database events with this application manager
		connect_with_object (db, ObjConnectFlags.NONE);

		sdb = db;
		if (writeable) {
			if (!db.open_write ()) {
				emit_error (ErrorEnum.DATABASE_OPEN_FAILED,
					    _("Unable to open software database for reading & writing!"));
				return false;
			}
		} else {
			if (!db.open_read ()) {
				emit_error (ErrorEnum.DATABASE_OPEN_FAILED,
					    _("Unable to open software database for reading only!"));
				return false;
			}
		}

		return true;
	}

	private void emit_update (AppItem old_app, AppItem new_app, IPK.Changelog changes) {
		var item = new UpdateItem ();
		item.old_app = old_app;
		item.new_app = new_app;
		item.changelog = changes;

		update (item);
	}

	public bool find_updates () {
		//! TODO
		return false;
	}

	public bool apply_updates (ArrayList<UpdateItem> update_list) {
		//! TODO
		return false;
	}

}

} // End of namespace
