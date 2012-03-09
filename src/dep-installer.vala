/* dep-installer.vala - Perform everything required for dependency solving & installing
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller.Dep;

namespace Listaller {

private class DepInstaller : MsgObject {
	private SoftwareDB db;
	private DepManager depman;
	private Listaller.Settings conf;

	public DepInstaller (SoftwareDB lidb) {
		base ();
		db = lidb;
		conf = lidb.get_liconf ();

		// This should never happen!
		if (conf == null) {
			error ("Listaller config was NULL in DepManager constructor!");
			conf = new Listaller.Settings ();
		}
		if (!db.database_locked ()) {
			critical ("Dependency installer received a read-only database! This won't work if write actions have to be performed!");
		}

		// Create a new dependency manager to fetch information about installed dependencies
		depman = new DepManager (db);
		depman.connect_with_object_all (this);
	}

}

} // End of namespace Listaller
