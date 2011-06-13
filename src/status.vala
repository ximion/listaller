/* status.vala
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
using Listaller;

namespace Listaller {

public enum ErrorEnum {
	UNKNOWN,
	IPK_LOADING_FAILED,
	UNPACKING_FAILED,
	SETUP_NOT_INITIALIZED,
	IPK_INCOMPLETE,
	IPK_DAMAGED,
	FILE_EXISTS,
	HASH_MISSMATCH,
	FILE_INSTALL_FAILED,
	COPY_ERROR,
	DATABASE_FAILURE,
	DB_OPEN_FAILED,
	ALREADY_INSTALLED,
	REMOVAL_FAILED,
	INSTALLATION_FAILED,
	INVALID;

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
	INSTALLATION_FINISHED;
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

public class ErrorItem : Object {
	private ErrorEnum _etype;
	private string _details;

	public ErrorEnum error {
		get { return _etype; }
		set { _etype = value; }
	}

	public string details {
		get { return _details; }
		set { _details = value; }
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

public class MessageItem : Object {
	private MessageEnum _mtype;
	private string _details;

	public MessageEnum mtype {
		get { return _mtype; }
		set { _mtype = value; }
	}

	public string details {
		get { return _details; }
		set { _details = value; }
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

public class StatusItem : Object {
	private StatusEnum _stype;
	private string _info;

	public StatusEnum status {
		get { return _stype; }
		set { _stype = value; }
	}

	public string info {
		get { return _info; }
		set { _info = value; }
	}

	public StatusItem (StatusEnum type) {
		status = type;
		info = "";
	}
}

} // End of namespace
