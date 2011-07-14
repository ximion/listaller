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

	public bool install_dependency (ref IPK.Dependency dep, bool force_feedinstall = false) {
		if ((force_feedinstall) && (dep.feed_url == ""))
			return false;

		bool ret = false;
		ErrorItem? error = null;

		// First try the PackageKit dependency provider, if feedinstall is not forced
		if (!force_feedinstall) {
			PkInstaller pkinst = new PkInstaller ();
			pkinst.message.connect ( (m) => { this.message (m); } );
			ret = pkinst.install_dependency (ref dep);

			if (!ret)
				error = pkinst.last_error;
		}
		// Finish if the dependency is already satisfied
		if (dep.satisfied)
			return true;

		// Emit error if this dependency has no feed assigned
		if (dep.feed_url == "") {
			emit_depmissing_error (error, dep);
			return false;
		}

		// Now try to install from dependency-feed
		FeedInstaller finst = new FeedInstaller ();
		finst.message.connect ( (m) => { this.message (m); } );
		ret = finst.install_dependency (ref dep);
		if (!ret) {
			error = finst.last_error;
			emit_depmissing_error (error, dep);
			return false;
		}

		return ret;
	}

}

} // End of namespace
