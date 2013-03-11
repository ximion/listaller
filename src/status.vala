/* status.vala
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
using Listaller.Utils;

namespace Listaller {

public enum ErrorEnum {
	INVALID,
	INTERNAL,
	SETUP_NOT_INITIALIZED,
	IPK_LOADING_FAILED,
	IPK_INCOMPLETE,
	IPK_DAMAGED,
	IPK_NOT_SUPPORTED,
	OPERATION_NOT_ALLOWED,
	UNPACKING_FAILED,
	FILE_EXISTS,
	HASH_MISSMATCH,
	FILE_INSTALL_FAILED,
	COPY_ERROR,
	INSTALLATION_FAILED,
	DATABASE_LOCKED,
	DATABASE_FAILURE,
	DATABASE_OPEN_FAILED,
	ALREADY_INSTALLED,
	REMOVAL_FAILED,
	WRONG_ARCHITECTURE,
	DEPENDENCY_MISSING,
	DEPENDENCY_INSTALL_FAILED,
	NATIVE_TRANSACTION_FAILED,
	NETWORK_ERROR,
	REFRESH_FAILED,
	UPDATE_FAILED;

	public string to_string () {
		return ((int) this).to_string ();
	}
}

public enum StatusEnum {
	UNKNOWN,
	ACTION_STARTED,
	RESOLVING_DEPENDENCIES,
	INSTALLING_FILES,
	REGISTERING_APPLICATION,
	INSTALLATION_FINISHED,
	REMOVAL_FINISHED;
}

public enum MessageEnum {
	UNKNOWN,
	INFO,
	WARNING,
	CRITICAL;

	public string to_string() {
		switch (this) {
			case UNKNOWN:
				return ("msg:UNKNOWN");

			case INFO:
				return ("msg:INFO");

			case WARNING:
				return ("msg:WARNING");

			case CRITICAL:
				return ("msg:CRITICAL");

			default:
				return ("msg:<?>");
		}
	}
}

public enum ProgressEnum {
	MAIN_PROGRESS,
	ITEM_PROGRESS;
}

/**
 * Return type describing an error
 */
public class ErrorItem : Object {
	private ErrorEnum _etype;
	private string _details;

	public ErrorEnum error {
		get { return _etype; }
		internal set { _etype = value; }
	}

	public string details {
		get { return _details; }
		internal set { _details = value; }
	}

	public ErrorItem (ErrorEnum type) {
		error = type;
		details = "";
	}

	public string to_string () {
		string str;
		str = "{ [" + error.to_string () + "]=> " + details + " }";
		return str;
	}
}

/**
 * Return type describing a message
 */
public class MessageItem : Object {
	private MessageEnum _mtype;
	private string _details;

	public MessageEnum mtype {
		get { return _mtype; }
		internal set { _mtype = value; }
	}

	public string details {
		get { return _details; }
		internal set { _details = value; }
	}

	public MessageItem (MessageEnum type) {
		mtype = type;
		details = "";
	}

	public string to_string () {
		string str;
		str = "{ [" + mtype.to_string () + "]=> " + details + " }";
		return str;
	}
}

/**
 * Return type describing a status change
 */
public class StatusItem : Object {
	private StatusEnum _stype;
	private string _info;

	public StatusEnum status {
		get { return _stype; }
		internal set { _stype = value; }
	}

	public string info {
		get { return _info; }
		internal set { _info = value; }
	}

	public StatusItem (StatusEnum type) {
		status = type;
		info = "";
	}
}

/**
 * Return type describing progress changes
 */
public class ProgressItem : Object {
	private ProgressEnum _ptype;
	private string _item_id;
	private int _progress;

	public ProgressEnum prog_type {
		get { return _ptype; }
		internal set { _ptype = value; }
	}

	public string item_id {
		get { return _item_id; }
		internal set { _item_id = value; }
	}

	public int value {
		get { return _progress; }
		internal set { _progress = value; }
	}

	public ProgressItem () {
		prog_type = ProgressEnum.MAIN_PROGRESS;
		_progress = -1;
	}
}

/**
 * Return type describing a software update
 */
public class UpdateItem : Object {
	public Type sw_type { get; internal set; }

	/* these objects can be of type Dependency or AppItem (type indicated in sw_type) */
	public Object sw_old { get; internal set; }
	public Object sw_new { get; internal set; }

	public string architecture { get; internal set; }
	public IPK.Changelog changelog { get; internal set; }

	public bool completed { get; internal set; }

	public UpdateItem () {
		completed = false;
	}
}

} // End of namespace: Listaller
