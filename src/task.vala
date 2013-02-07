/* task.vala -- Perform more complex app-management tasks
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
 * Allows performing advanced application management tasks
 *
 * This class allows performing more complex software management tasks,
 * like installing multiple packages or fetching packages from remote sources.
 * This class combines functionality from all three Listaller departments,
 * updater, installer and manager.
 */
public class Task : Manager {

	/**
	 * Create a new Listaller task
	 *
	 * @param shared_mode Whether we are in shared mode or not.
	 */
	public Task (bool shared_mode = true) {
		base (shared_mode);
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		if (type == PackageKit.ProgressType.PERCENTAGE)
			change_progress (progress.percentage);
		if (type == PackageKit.ProgressType.ITEM_PROGRESS)
			change_item_progress (progress.item_progress.package_id, progress.item_progress.percentage);
	}


	public Setup? prepare_setup_for_app (string app_idname) {
		var repoMgr = new Repo.Manager ();
		return repoMgr.get_setup_for_remote_app (app_idname);
	}
}

} // End of namespace: Listaller
