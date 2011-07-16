/* depman.vala
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

private class DepManager : Object {
	private SoftwareDB db;
	private Listaller.Settings conf;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public DepManager (SoftwareDB lidb, Listaller.Settings? liconf = null) {
		db = lidb;
		conf = liconf;
		if (conf == null)
			conf = new Listaller.Settings ();
	}

	private void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		li_error (details);
	}

	private void emit_depmissing_error (ErrorItem? inst_error, IPK.Dependency dep) {
		string text = "";
		if (inst_error != null)
			text = "\n\n%s".printf (inst_error.details);
		emit_error (ErrorEnum.DEPENDENCY_MISSING, "%s%s".printf (
					_("Unable to find valid candidate to satisfy dependency '%s'!").printf (dep.name),
					text));
	}

	private bool install_dependency_internal (PkInstaller pkinst, FeedInstaller finst,
						ref IPK.Dependency dep, bool force_feedinstall = false) {
		if ((force_feedinstall) && (dep.feed_url == ""))
			return false;

		bool ret = false;
		ErrorItem? error = null;

		// TODO: Use util-linux "whereis" utility to find shared stuff faster

		// Try to find native distribution packages for this dependency
		ret = pkinst.search_dep_packages (ref dep);
		if (!ret)
			error = pkinst.last_error;

		// Finish if the dependency is already satisfied
		if (dep.satisfied)
			return true;

		/* Now try the PackageKit dependency provider, if feedinstall is not forced
		 * and the previous package dependency searching was successful */
		if ((!force_feedinstall) && (ret)) {
			ret = pkinst.install_dependency (ref dep);
			if (!ret)
				error = pkinst.last_error;
		}

		// Finish if the dependency is satisfied
		if (dep.satisfied)
			return true;

		// Emit error if this dependency has no feed assigned
		if (dep.feed_url == "") {
			emit_depmissing_error (error, dep);
			return false;
		}

		// Now try to install from dependency-feed
		ret = finst.install_dependency (ref dep);
		if (!ret) {
			error = finst.last_error;
			emit_depmissing_error (error, dep);
			return false;
		}

		if (ret)
			db.add_dependency (dep);

		return ret;
	}

	public bool install_dependency (ref IPK.Dependency dep, bool force_feedinstall = false) {
		PkInstaller pkinst = new PkInstaller (conf);
		pkinst.message.connect ( (m) => { this.message (m); } );

		FeedInstaller finst = new FeedInstaller (conf);
		finst.message.connect ( (m) => { this.message (m); } );

		// If we have a system standard-lib (a minimal distribution dependency), consider it as installed
		if (dep.is_standardlib)
			return true;

		bool ret = install_dependency_internal (pkinst, finst, ref dep, force_feedinstall);
		return ret;
	}

	public bool install_dependencies (ArrayList<IPK.Dependency> depList, bool force_feedinstall = false) {
		PkInstaller pkinst = new PkInstaller (conf);
		pkinst.message.connect ( (m) => { this.message (m); } );

		FeedInstaller finst = new FeedInstaller (conf);
		finst.message.connect ( (m) => { this.message (m); } );

		bool ret = true;
		foreach (IPK.Dependency dep in depList) {
			// If this is a default lib, just continue
			if (dep.is_standardlib)
				continue;
			ret = install_dependency_internal (pkinst, finst, ref dep, force_feedinstall);
			if (!ret)
				break;
		}
		return ret;
	}

}

} // End of namespace
