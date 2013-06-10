/* basic-solver.vala -- Dummy solver to solve basic dependencies (libraries and binaries)
 *
 * Copyright (C) 2013 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Dep {

private class BasicSolver : AbstractSolver {

	public BasicSolver (SetupSettings setup_settings) {
		base (setup_settings);
		id = "Basic";
	}

	private bool check_component_library_items_installed (Dep.Component comp, out string? explanation = null) {
		/* Search files using "find_library" before calling PackageKit to do this
		 * (this is a huge speed improvement)
		 * This only works for library dependencies!
		 */
		bool ret = true;
		Config conf = new Config ();
		foreach (string sitem in comp.raw_itemlist) {
			if (Dep.Component.item_get_type (sitem) == Dep.ItemType.SHARED_LIB) {
				string s = Component.item_get_name (sitem);
				ret = find_library (s, conf);
				if (!ret) {
					debug ("Library not found: %s", s);
					explanation = _("Library %s was not found").printf (s);
					break;
				}
			}
		}

		return ret;
	}

	private bool check_component_binary_items_installed (Dep.Component comp, out string? explanation = null) {
		string[] bin_dirs = {"/usr/bin", "/usr/sbin", "/sbin", "/bin"}; // constant! (no const stmt, since some C compilers have issues with the resulting C code)

		// Check if binaries are present
		bool ret = true;
		foreach (string sitem in comp.raw_itemlist) {
			if (Dep.Component.item_get_type (sitem) == Dep.ItemType.BINARY) {
				string s = Component.item_get_name (sitem);
				if (s.has_prefix ("/")) {
					ret = FileUtils.test (s, FileTest.EXISTS);
				} else {
					foreach (string prefix in bin_dirs) {
						ret = FileUtils.test (Path.build_filename (prefix, s, null), FileTest.EXISTS);
						if (ret)
							break;
					}
				}
				if (!ret) {
					debug ("Binary not found: %s", s);
					explanation = _("Binary %s was not found").printf (s);
					break;
				}
			}
		}

		return ret;
	}

	public override bool check_framework_items_installed (Framework cfrmw, out string? reason = null) {
		bool ret;
		string explanation;
		ret = check_component_library_items_installed (cfrmw, out explanation);
		if (!ret)
			return false;
		ret = check_component_binary_items_installed (cfrmw, out explanation);

		return ret;
	}

	public override bool check_module_items_installed (Module cmod, out string? reason = null) {
		bool ret;
		ret = check_component_library_items_installed (cmod, out reason);
		if (!ret)
			return false;
		ret = check_component_binary_items_installed (cmod, out reason);

		// If all libraries/binaries were found, add them to installdata and exit
		if (ret) {
			foreach (string s in cmod.raw_itemlist) {
				switch (cmod.item_get_type (s)) {
					case Dep.ItemType.SHARED_LIB:
					case Dep.ItemType.BINARY:
						cmod.add_installed_item (s);
						break;
					default:
						// we have an unknown dependency, so this Module is not satisfied!
						reason = "Dependency %s was not found!".printf (s);
						cmod.installed = false;
						return false;
				}
			}

			return true;
		}

		return ret;
	}

	public override bool install_module (Module cmod) {
		// libraries and binaries are installed through PackageKit or other solvers
		return false;
	}
}

} // End of namespace: Listaller.Dep
