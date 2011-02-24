/* items.vala
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

// Workaround for Vala bug #618931
private const string _PKG_VERSION4 = PkgConfig.VERSION;

public class LiAppItem : Object {
	private string _name;
	private string _version;
	private string _summary;
	private string _author;
	private string _maintainer;
	private string _categories;
	private ulong  _install_time;
	private string _dependencies;
	private int _id;

	public string name {
		get { return _name; }
		set { _name = value; }
	}

	public string version {
		get { return _version; }
		set { _version = value; }
	}

	public string summary {
		get { return _summary; }
		set { _summary = value; }
	}

	public string author {
		get { return _author; }
		set { _author = value; }
	}

	public string maintainer {
		get { return _maintainer; }
		set { _maintainer = value; }
	}

	public string categories {
		get { return _categories; }
		set { _categories = value; }
	}

	public ulong install_time {
		get { return _install_time; }
		set { _install_time = value; }
	}

	public string dependencies {
		get { return _id; }
		set { _id = value; }
	}

	public int id {
		get { return _dependencies; }
		set { _dependencies = value; }
	}

	public LiAppItem (string aname, string aversion) {
		name = aname;
		version = aversion;
		install_time = 0;
		categories = "all;";
		id = -1;
	}

}

public class LiDependencyItem : Object {
	private string _name;
	private string _version;

	public string name {
		get { return _name; }
		set { _name = value; }
	}

	public string version {
		get { return _version; }
		set { _version = value; }
	}

	public LiDependencyItem (string dname, string dversion) {
		name = dname;
		version = dversion;
	}

	public string get_idstring () {
		return name + version;
	}

}
