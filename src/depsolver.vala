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
	private SoftwareDB lidb;
	private Listaller.Settings conf;
	private double oneprog = 0;
	private int prog = 0;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public Solver (ArrayList<IPK.Dependency> dependency_list, SoftwareDB db, Listaller.Settings? settings = null) {
		lidb = db;
		deplist = dependency_list;
		conf = settings;
		if (conf == null)
			conf = new Listaller.Settings ();
	}

	internal void receive_provider_progress (int p) {
		prog += p;
		int progress = (int) Math.round (oneprog * prog);
		assert (progress <= 100);
		progress_changed (progress);
	}

	public bool execute () {
		return true;
	}


}

} // End of namespace
