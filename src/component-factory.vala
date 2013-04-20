/* component-factory.vala -- Read and process component information
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
 * Read and process component information
 *
 * The ComponentFactory is responsible for compiling a list of available components
 * and to check if their requirements are met.
 * It can also generate new components on the fly.
 */
private class ComponentFactory : Object {
	private string system_components_dir;

	public ArrayList<Dep.Framework> system_frameworks { get; private set; }
	public ArrayList<Dep.Module> system_modules { get; private set; }

	public ComponentFactory () {
		system_components_dir = PkgConfig.DATADIR + "/listaller/components";
	}

	public void initialize () {
		// TODO: Find system components/frameworks
	}

	public bool is_satisfied (Dep.Component dep, out string reason = null) {
		// TODO: Check if component is satisfied
	}

	public Dep.Framework? get_framework (string name) {
		return null;
	}

	public Dep.Module? get_module (string name) {
		return null;
	}
}

} // End of namespace: Listaller.Dep
