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

/* Base class for all Listaller types which
 * want to send messages between each other.
 * (Used e.g. for GUI stuff and in the public API)
 */
public abstract class MsgObject : Object {
	private int prog;
	private int prog_sub;
	private string error_hint_str;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress, int sub_progress);

	public MsgObject () {
		prog = -1;
		prog_sub = -1;
		error_hint_str = "";
	}

	internal void emit_message (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		debug ("Info: %s", msg);
		message (item);
	}

	internal void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		warning (msg);
		li_warning (msg);
	}

	internal void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		if (error_hint_str == "")
			li_error (details);
		else
			li_error ("[%s]:%s".printf (error_hint_str, details));
	}

	internal void change_progress (int progress, int sub_progress) {
		prog = progress;
		prog_sub = sub_progress;
		progress_changed (progress, sub_progress);
	}

	internal void change_main_progress (int progress) {
		change_progress (progress, prog_sub);
	}

	internal void change_sub_progress (int sub_progress) {
		change_progress (prog, sub_progress);
	}

	protected void set_error_hint_str (string str) {
		error_hint_str = str;
	}

	protected void connect_with_object (MsgObject other, ObjConnectFlags flags) {
		if (ObjConnectFlags.IGNORE_ERROR_CODE in flags) {
			other.error_code.connect ((error) => {
				this.error_code (error);
			});
		}

		other.message.connect ((msg) => {
			this.message (msg);
		});

		if (!(ObjConnectFlags.IGNORE_PROGRESS in flags)) {
			if (ObjConnectFlags.PROGRESS_TO_SUBPROGRESS in flags) {
				other.progress_changed.connect ((pA, pB) => {
					change_progress (-1, pB);
				});
			} else {
				other.progress_changed.connect ((pA, pB) => {
					change_progress (pA, pB);
				});
			}
		}
	}


}

} // End of namespace: Listaller
