/* components.vala -- Defines component types to resolve dependencies
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Dep {

/**
 * Definition of common properties of a component
 *
 * Ancestor class for Framework and Module
 */
private abstract class Component : Object {
	private string _full_name;
	public string full_name {
		get {
			if (_full_name == "")
				_full_name = _idname;
			if (_full_name == "")
				return "empty";
			return _full_name;
		}
		internal set {
			_full_name = value;
		}
	}

	private string _idname;
	public string idname {
		get {
			if (_idname != "")
				return _idname;

			// Form unique dependency-id, if not already set
			_idname = "%s-%s".printf (full_name, version);
			_idname = _idname.down ().replace (" ", "_");

			return _idname;
		}
		set {
			_idname = value;
		}
	}

	public string summary { get; internal set; }
	public string homepage { get; internal set; }
	public string version { get; internal set; }

	public string environment { get; internal set; }

	public Component () {
		version = "0";
		idname = "";
		environment = "";
		_full_name = "";
	}
}

/**
 * Definition of a framework dependency
 *
 * A framework is a (usually large) system component, such as the KDELibs
 * or the GNOME platform, or stuff like PolicyKit. It usually requires tight
 * system integration.
 * The only way to satisfy this dependency is via the distribution's native package
 * manager.
 */
private class Framework : Component {
	public Framework () {
		base ();
	}
}

/**
 * Definition of a module dependency
 *
 * A module is a dependency which can be installed with Listaller and satisfied using 3rd-party
 * sources.
 * It often simply is a small library, iconset, etc.
 */
private class Module : Component {
	public Module () {
		base ();
	}
}

} // End of namespace: Listaller.Dep
