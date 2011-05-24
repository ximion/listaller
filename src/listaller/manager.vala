/* manager.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
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

[CCode (cheader_filename = "listaller-glib/manager.h")]

namespace Listaller {

public enum Filter {
	NO_FILTER,
	INSTALLED,
	AVAILABLE;
}

public class Manager : Object {
	private Settings conf;
	private SoftwareDB db;

	public signal void error_code (ErrorItem error);
	public signal void progress_changed (int progress);
	public signal void status_changed (StatusItem status);
	public signal void message (MessageItem message);
	public signal void application (AppItem appid);

	public Settings settings {
		get { return conf; }
		set { conf = value; }
	}

	/*
	 * @param: settings A valid LiSettings instance, describing basic settings (or null)
	 */
	public Manager (Settings? settings) {
		conf = settings;
		if (conf == null)
			conf = new Settings (false);

		db = new SoftwareDB (conf);
		db.error_code.connect ((error) => {
			this.error_code (error);
		});
		db.message.connect ((message) => {
			this.message (message);
		});

		debug ("This is Listaller " + Config.VERSION);
	}

	private void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		message (item);
		warning (msg);
	}

	private void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		critical (details);
	}

	public ArrayList<AppItem> find_applications (Filter filter) {
		db.open ();
		ArrayList<AppItem> alist = new ArrayList<AppItem> ();
		int i = 1;
		AppItem capp = null;
		for (capp = db.get_application_by_dbid (i); capp != null; i++) {
			application (capp);
			alist.add (capp);
		}
		db.close ();
		return alist;
	}

	public bool remove_application (AppItem app) {
		return true;
	}
}

} // End of namespace
