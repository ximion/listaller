/* depsolver.vala
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
	private ArrayList<IPK.Dependency> deplist;
	private SoftwareDB db;
	private Listaller.Settings conf;
	private DepManager depman;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public Solver (SoftwareDB lidb, ArrayList<IPK.Dependency> dependency_list) {
		db = lidb;
		deplist = dependency_list;
		conf = db.get_liconf ();

		depman = new DepManager (db);
		depman.error_code.connect ( (error) => { this.error_code (error); });
		depman.message.connect ( (msg) => { this.message (msg); });
		depman.progress_changed.connect ( (prog) => { this.progress_changed (prog); });
	}

	public bool execute () {
		// We don't do any solving here at time... We just install the dependencies
		bool ret = depman.install_dependencies (deplist);
		return ret;
	}

	public ArrayList<IPK.Dependency> get_direct_dependencies () {
		return deplist;
	}


}

} // End of namespace
