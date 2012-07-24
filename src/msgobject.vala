/* msgobject.vala
 *
 * Copyright (C) 2010-2012 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller;

namespace Listaller {

protected enum ObjConnectFlags {
	NONE,
	PROGRESS_TO_SUBPROGRESS,
	IGNORE_PROGRESS,
	IGNORE_ERROR_CODE;
}

/**
 * Providing basic message handling
 *
 * Base class for all Listaller classes which
 * want to send messages between each other.
 * (Used e.g. for GUI stuff and in the public API)
 */
public abstract class MsgObject : Object {
	private int prog;
	private string error_hint_str;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);
	public signal void item_progress_changed (string id, int progress);

	public MsgObject () {
		prog = -1;
		prog_sub = -1;
		error_hint_str = "";
	}

	internal virtual void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		debug ("Info: %s", msg);
		message (item);
	}

	internal virtual void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		warning (msg);
		li_warning (msg);
	}

	internal virtual void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		if (error_hint_str == "")
			li_error (details);
		else
			li_error ("[%s]:%s".printf (error_hint_str, details));
	}

	internal virtual void change_progress (int progress) {
		if ((progress == 0) || (progress >= prog))
			prog = progress;
		else
			li_warning ("Progress cannot go down!");

		//! debug ("Progress changed: %i", progress);
		progress_changed (prog);
	}

	internal virtual void change_item_progress (string id, int progress) {
		item_progress_changed (id, progress);
	}

	protected void set_error_hint_str (string str) {
		error_hint_str = str;
	}

	protected void connect_with_object (MsgObject other_obj, ObjConnectFlags flags) {
		if (!(ObjConnectFlags.IGNORE_ERROR_CODE in flags)) {
			other_obj.error_code.connect ((error) => {
				this.error_code (error);
			});
		}

		other_obj.message.connect ((msg) => {
			this.message (msg);
		});

		if (!(ObjConnectFlags.IGNORE_PROGRESS in flags)) {
				other_obj.progress_changed.connect ((pA) => {
					change_progress (pA);
				});
		}
	}

	protected void connect_with_object_all (MsgObject other_obj) {
		connect_with_object (other_obj, ObjConnectFlags.NONE);
	}

}

} // End of namespace: Listaller
