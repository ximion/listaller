/* dep-manager.vala - Perform tasks related to software dependency management
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
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

private class DepManager : MsgObject {
	private SoftwareDB db;
	private Listaller.Settings conf;

	public DepManager (SoftwareDB lidb) {
		base ();
		db = lidb;
		conf = lidb.get_liconf ();
		// This should never happen!
		if (conf == null) {
			error ("Listaller config was NULL in DepManager constructor!");
			conf = new Listaller.Settings ();
		}
	}

	public SoftwareDB get_sdb () {
		return db;
	}

	public bool dependency_is_installed (ref IPK.Dependency dep) {
		IPK.Dependency? dbDep = db.get_dependency_by_id (dep.idname);
		if (dbDep != null) {
			debug ("Dependency with id [%s] is already installed :)", dep.idname);
			dep = dbDep;
			return true;
		}
		return false;
	}

	public string get_absolute_library_paths (IPK.Dependency dep) {
		return "";
	}

}

} // End of namespace
