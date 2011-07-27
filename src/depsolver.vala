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
	private SoftwareDB db;
	private Listaller.Settings conf;
	private DepManager depman;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public Solver (SoftwareDB lidb, ArrayList<IPK.Dependency> dependency_list) {
		db = lidb;
		ipkDeplist = dependency_list;
		conf = db.get_liconf ();

		depman = new DepManager (db);
		depman.error_code.connect ( (error) => { this.error_code (error); });
		depman.message.connect ( (msg) => { this.message (msg); });
		depman.progress_changed.connect ( (prog) => { this.progress_changed (prog); });
	}

	public bool execute () {
		ErrorItem? error = null;
		var di = new DepInfo ();
		var pksolv = new PkResolver (conf);
		foreach (IPK.Dependency idep in ipkDeplist) {
			/* Update package dependencies with system data (which might add some additional information here, provided
			 * by the distributor */
			di.update_dependency_with_system_data (ref idep);

			// Try to find native distribution packages for this dependency
			bool ret = pksolv.search_dep_packages (ref idep);
			if (!ret)
				error = pksolv.last_error;
		}
		bool ret = depman.install_dependencies (ipkDeplist);
		return ret;
	}

	public ArrayList<IPK.Dependency> get_package_dependencies () {
		return ipkDeplist;
	}


}

} // End of namespace
