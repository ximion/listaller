/* depsolver.vala - Solve IPK dependencies to a set of internal dependenciesmatthia
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
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

namespace Listaller.Deps {

private class Solver : Object {
	private ArrayList<IPK.Dependency> ipkDeplist;
	private ArrayList<IPK.Dependency> intDeplist;
	private Listaller.Settings conf;
	private DepManager depman;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public Solver (DepManager dman, ArrayList<IPK.Dependency> dependency_list, bool connect_to_depmanager = false) {
		ipkDeplist = dependency_list;
		intDeplist = new ArrayList<IPK.Dependency> ();

		depman = dman;
		conf = depman.get_sdb ().get_liconf ();

		if (connect_to_depmanager) {
			// Connect solver signals to dependency manager signals
			this.error_code.connect ( (error) => { depman.error_code (error); });
			this.message.connect ( (msg) => { depman.message (msg); });
			this.progress_changed.connect ( (prog) => { depman.progress_changed (prog); });
		}
	}

	public bool execute () {
		bool ret = false;
		ErrorItem? error = null;
		var di = new DepInfo ();
		var pksolv = new PkResolver (conf);
		intDeplist.clear ();

		foreach (IPK.Dependency idep in ipkDeplist) {
			/* Update package dependencies with system data (which might add some additional information here, provided
			 * by the distributor */
			di.update_dependency_with_system_data (ref idep);

			if (idep.is_standardlib) {
				// If we have a system standard-lib, always consider it as installed
				idep.satisfied = true;
				continue;
			}

			// If dependency is already installed, skip it & update it from db
			if (depman.dependency_is_installed (ref idep))
				continue;

			// Try to find native distribution packages for this dependency
			ret = pksolv.search_dep_packages (ref idep);
			if (!ret)
				error = pksolv.last_error;

			// TODO: Build the correct list
			intDeplist.add (idep);
		}

		if (error != null)
			error_code (error);

		return ret;
	}

	public ArrayList<IPK.Dependency> get_package_dependencies () {
		return ipkDeplist;
	}

	/* Get a list of exact (matching this system) dependencies, which are direct dependencies of the application.
	 * (And not dependencies of other dependencies, we don't support this at time */
	public ArrayList<IPK.Dependency> get_exact_direct_dependencies () {
		return intDeplist;
	}

}

} // End of namespace
