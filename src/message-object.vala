/* message-object.vala
 *
 * Copyright (C) 2010-2013 Matthias Klumpp <matthias@tenstral.net>
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
 * Class providing basic message handling
 *
 * Base class for all Listaller classes which
 * want to send messages to each other.
 * (Used e.g. for GUI stuff and in the public API)
 */
public abstract class MessageObject : Object {
	private ProgressItem prog_item;
	private string error_hint_str;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress (ProgressItem prog);

	public MessageObject () {
		error_hint_str = "";
		prog_item = null;
	}

	private void init_progress () {
		prog_item = new ProgressItem ();
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
	}

	internal virtual void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;

		// emit
		error_code (item);

		if (error_hint_str == "")
			GLib.message ("ERROR: %s", details);
		else
			GLib.message ("ERROR: [%s]:%s", error_hint_str, details);
	}

	internal virtual void change_progress (int prog_value) {
		init_progress ();

		prog_item.prog_type = ProgressEnum.MAIN_PROGRESS;
		prog_item.item_id = "";
		if (prog_value == 0) {
			if ((prog_item.prog_type == ProgressEnum.MAIN_PROGRESS) && (prog_value <= prog_item.value))
				warning ("Progress cannot go down!");
			else
				prog_item.value = prog_value;
		}

		//! debug ("Progress changed: %i", progress);
		// emit
		progress (prog_item);
	}

	internal virtual void change_item_progress (string id, uint item_progress) {
		init_progress ();

		prog_item.prog_type = ProgressEnum.ITEM_PROGRESS;
		prog_item.item_id = id;
		prog_item.value = (int) item_progress;

		// emit
		progress (prog_item);
	}

	protected void set_error_hint_str (string str) {
		error_hint_str = str;
	}

	protected void connect_with_object (MessageObject other_obj, ObjConnectFlags flags = ObjConnectFlags.NONE) {
		if (!(ObjConnectFlags.IGNORE_ERROR_CODE in flags)) {
			other_obj.error_code.connect ((error) => {
				this.error_code (error);
			});
		}

		other_obj.message.connect ((msg) => {
			this.message (msg);
		});

		if (!(ObjConnectFlags.IGNORE_PROGRESS in flags)) {
				other_obj.progress.connect ((pA) => {
					this.progress (pA);
				});
		}
	}

	protected void connect_with_object_all (MessageObject other_obj) {
		connect_with_object (other_obj, ObjConnectFlags.NONE);
	}

}

} // End of namespace: Listaller
