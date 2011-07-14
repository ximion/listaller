/* feed-installer.vala
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

private class FeedInstaller : Object {

	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public ErrorItem? last_error { get; set; }

	public FeedInstaller () {
		last_error = null;
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem (MessageEnum.WARNING);
		item.details = msg;
		message (item);
		li_warning (msg);
	}

	private void emit_info (string msg) {
		// Construct info message
		MessageItem item = new MessageItem (MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void set_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem (id);
		item.details = details;
		last_error = item;
		debug ("FeedInstaller: %s", details);
	}

	public bool install_dependency (ref IPK.Dependency dep) {
		if (dep.feed_url == "")
			return false;

		return false;
	}

}

} // End of namespace
