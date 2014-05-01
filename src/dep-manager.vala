/* dep-manager.vala - Perform tasks related to software dependency management
 *
 * Copyright (C) 2011-2014 Matthias Klumpp <matthias@tenstral.net>
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

private class DepManager : MessageObject {
	private SoftwareDB db;
	private SetupSettings ssettings;
	private ComponentFactory cfactory;

	public DepManager (SoftwareDB lidb) {
		base ();
		db = lidb;
		ssettings = db.setup_settings;

		// This should never happen!
		if (ssettings == null) {
			error ("Listaller config was NULL in DepManager constructor!");
			ssettings = new SetupSettings ();
		}

		cfactory = new ComponentFactory (ssettings);
		cfactory.initialize ();
	}

	public SoftwareDB get_sdb () {
		return db;
	}

	public bool module_is_installed (ref Dependency dep) {
		Dependency? dbDep = db.get_dependency_by_id (dep.idname);
		if (dbDep != null) {
			debug ("Dependency with id [%s] is already installed :)", dep.idname);
			dep = dbDep;
			return true;
		}

		return false;
	}

	public string get_absolute_library_path (Dependency dep) {
		// No components => no libraries
		if (!dep.has_items ())
			return "";

		if (dep.environment != "") {
			string str = dep.environment;
			if (str.index_of ("LD_LIBRARY_PATH=\"") >= 0) {
				string res;
				int i = str.index_of ("_PATH=\"") + 7;
				int j = str.index_of ("\"", i + 1);
				res = str.slice (i, j);
				if (res.strip () != "")
					return res;
			}
		}

		bool contains_libs = false;
		foreach (string comp in dep.raw_itemlist) {
			if (Dependency.item_get_type (comp) == Dep.ItemType.SHARED_LIB) {
				contains_libs = true;
				break;
			}
		}
		// No shared libs => no library paths
		if (!contains_libs)
			return "";

		string depInstallDir = Path.build_filename (ssettings.depdata_dir (), dep.idname, null);
		// If directory is non-existent, we don't need to continue here. Maybe we have a system library/package installed.'
		if (!FileUtils.test (depInstallDir, FileTest.EXISTS))
			return "";

		string? resDir = find_dir_containing_file (depInstallDir, "*.so", true);
		if (resDir == null) {
			GLib.message ("Could not find shared libraries for dependency '%s'. this might be an error.", dep.idname);
			return "";
		}

		db.set_dependency_environment (dep.idname, "LD_LIBRARY_PATH=\"%s\"".printf (resDir));

		return resDir;
	}

	public Dependency? dependency_from_idname (string depIdName) {
		Dependency? dep = db.get_dependency_by_id (depIdName);
		if (dep == null)
			debug ("Dependency not found in database: %s", depIdName);

		return dep;
	}

	public HashSet<Dependency> dependencies_from_idlist (string[] dep_ids) {
		HashSet<Dependency> resList = dependency_hashset_new ();
		Dependency? dep = null;
		foreach (string s in dep_ids) {
			dep = db.get_dependency_by_id (s);
			if (dep == null) {
				debug ("Dependency not found in database: %s", s);
				continue;
			}
			resList.add (dep);
		}

		return resList;
	}
}

} // End of namespace: Listaller
