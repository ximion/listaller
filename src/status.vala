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

public enum LiError {
	UNKNOWN,
	IPK_LOADING_FAILED,
	UNPACKING_FAILED;

	public string to_string () {
		return ((int) this).to_string ();
	}
}

public enum LiStatus {
	UNKNOWN,
	FINISHED,
	FAILED;
}

public enum LiMessageType {
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

public class LiErrorItem : Object {
	private LiError _etype;
	private string _details;

	public LiError error {
		get { return _etype; }
		set { _etype = value; }
	}

	public string details {
		get { return _details; }
		set { _details = value; }
	}

	public LiErrorItem (LiError type) {
		error = type;
		details = "";
	}

	public string to_string () {
		string str;
		str = "{ [" + error.to_string () + "]=> " + details + " }";
		return str;
	}
}

public class LiMessageItem : Object {
	private LiMessageType _mtype;
	private string _details;

	public LiMessageType mtype {
		get { return _mtype; }
		set { _mtype = value; }
	}

	public string details {
		get { return _details; }
		set { _details = value; }
	}

	public LiMessageItem (LiMessageType type) {
		mtype = type;
		details = "";
	}

	public string to_string () {
		string str;
		str = "{ [" + mtype.to_string () + "]=> " + details + " }";
		return str;
	}
}

public class LiStatusItem : Object {
	private LiStatus _stype;
	private string _details;

	public LiStatus status {
		get { return _stype; }
		set { _stype = value; }
	}

	public string details {
		get { return _details; }
		set { _details = value; }
	}

	public LiStatusItem (LiStatus type) {
		status = type;
		details = "";
	}
}
