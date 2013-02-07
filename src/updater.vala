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
	private Manager app_mgr;

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

		repo_mgr = new Repo.Manager ();
		connect_with_object (repo_mgr);

		app_mgr = new Manager (shared_mode);
		connect_with_object (app_mgr, ObjConnectFlags.IGNORE_PROGRESS);
	}

	private bool is_shared_mode () {
		return ssettings.current_mode == IPK.InstallMode.SHARED;
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

		// fetch all installed applications
		ArrayList<AppItem> apps_installed;
		AppState filter = AppState.INSTALLED_SHARED | AppState.INSTALLED_PRIVATE;
		app_mgr.filter_applications (filter, out apps_installed);

		return false;
	}

	public bool apply_updates (ArrayList<UpdateItem> update_list) {
		//! TODO
		return false;
	}

}

} // End of namespace
