/* depman.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
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
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

public class Dependency : Object {
	public string name { get; set; }
	public bool satisfied { get; set; }
	public ArrayList<string> files { get; set; }
	public string data { get; set; }
	public HashSet<string> meta_info { get; set; }

	internal Dependency (string dep_name) {
		name = dep_name;
		satisfied = false;

		files = new ArrayList<string> ();
		meta_info = new HashSet<string> ();
		data = "";
	}
}

} // Endof namespace: Listaller.IPK

namespace Listaller {

private class DependencyManager : Object {
	private SoftwareDB db;
	private Listaller.Settings conf;

	public DependencyManager (SoftwareDB lidb, Listaller.Settings? liconf = null) {
		db = lidb;
		conf = liconf;
		if (conf == null)
			conf = new Listaller.Settings ();
	}
}

} // End of namespace
