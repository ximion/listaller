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

private class Manager : Object {
	private SoftwareDB db;
	private Listaller.Settings conf;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public Manager (SoftwareDB lidb, Listaller.Settings? liconf = null) {
		db = lidb;
		conf = liconf;
		if (conf == null)
			conf = new Listaller.Settings ();
	}

	public bool install_dependency (IPK.Dependency dep, bool force_feedinstall = false) {
		if ((force_feedinstall) && (dep.feed_url == ""))
			return false;

		return true;
	}

}

} // End of namespace
