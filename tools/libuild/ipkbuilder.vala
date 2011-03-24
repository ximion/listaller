/* ipkbuilder.vala
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
using Archive;
using Listaller;

// Workaround for Vala bug #618931
private const string _PKG_VERSION14 = Config.VERSION;

namespace Listaller.IPK {

private class Builder : Object {
	private string fname;
	private string wdir;
	private string data_archive;
	private IPK.Control ipkc;
	private IPK.FileList ipkf;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);

	public IPK.Control control {
		get { return ipkc; }
	}

	public Builder () {
		ipkc = new IPK.Control ();
		ipkf = new IPK.FileList ();
	}

	~Builder () {
		// Remove workspace
		// TODO: Make this recursive
		DirUtils.remove (wdir);
		Posix.rmdir (wdir);
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		message (item);
		warning (msg);
	}

	private void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		critical (details);
	}

}

} // End of namespace
