/* depsolver.vala
 *
 * Copyright (C) 2011  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using GLib;
using Gee;
using Listaller;

namespace Listaller.Deps {

public enum SolverType {
	UNKNOWN,
	FILES,
	FEEDS;
}

private abstract class Provider : Object {
	protected IPK.Dependency dep;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public Provider (IPK.Dependency dep_data) {
		dep = dep_data;
	}

	public void connect_solver (Solver s) {
		error_code.connect ((error) => {
			s.error_code (error);
		});
		message.connect ((msg) => {
			s.message (msg);
		});
		progress_changed.connect ((p) => {
			s.receive_provider_progress (p);
		});
	}

	protected void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		message (item);
		warning (msg);
	}

	protected void emit_info (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	public virtual bool execute () {
		return false;
	}
}

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

	private bool run_provider (Provider p) {
		p.connect_solver (this);
		return p.execute ();
	}

	public bool execute () {
		bool ret = false;
		if (deplist.size == 0)
			return true;
		oneprog = 100 / deplist.size * 100;
		// Resolve dependencies!
		foreach (IPK.Dependency dep in deplist) {
			ret = run_provider (new PkitProvider (dep));
			if (!ret)
				break;
		}

		return ret;
	}

}

} // End of namespace
