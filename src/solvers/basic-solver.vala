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
using Appstream;
using Listaller;
using Listaller.Utils;

namespace Listaller.Dep {

private class BasicSolver : AbstractSolver {

	public BasicSolver (SetupSettings setup_settings) {
		base (setup_settings);
		id = "Basic";
	}

	private bool check_dependency_library_items_installed (Dependency dep, out string? explanation = null) {
		/* Search files using "find_library" before calling PackageKit to do this
		 * (this is a huge speed improvement)
		 * This only works for library dependencies!
		 */
		bool ret = true;
		Config conf = new Config ();
		foreach (string sitem in dep.raw_itemlist) {
			if (provides_item_get_kind (sitem) == ProvidesKind.LIBRARY) {
				string s = provides_item_get_value (sitem);
				ret = library_exists (s, conf);
				if (!ret) {
					debug ("Library not found: %s", s);
					explanation = _("Library %s was not found").printf (s);
					break;
				}
			}
		}

		return ret;
	}

	private bool check_dependency_binary_items_installed (Dependency dep, out string? explanation = null) {
		string[] bin_dirs = {"/usr/bin", "/usr/sbin", "/sbin", "/bin"}; // constant! (no const stmt, since some C compilers have issues with the resulting C code)

		// Check if binaries are present
		bool ret = true;
		foreach (string sitem in dep.raw_itemlist) {
			if (provides_item_get_kind (sitem) == ProvidesKind.BINARY) {
				string s = provides_item_get_value (sitem);
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

	private bool check_dependency_items_installed (Dependency dep, out string? reason = null) {
		bool ret;
		ret = check_dependency_library_items_installed (dep, out reason);
		if (!ret)
			return false;
		ret = check_dependency_binary_items_installed (dep, out reason);

		// If all libraries/binaries were found, add them to installdata and exit
		if (ret) {
			foreach (string s in dep.raw_itemlist) {
				switch (Appstream.provides_item_get_kind (s)) {
					case Appstream.ProvidesKind.LIBRARY:
					case Appstream.ProvidesKind.BINARY:
						dep.add_installed_item (s);
						break;
					default:
						// we have an unknown dependency, so this Module is not satisfied!
						reason = "Dependency %s was not found!".printf (s);
						dep.installed = false;
						return false;
				}
			}

			return true;
		}

		return ret;
	}

	public override bool check_dependency_installed (Dependency dep, out string? reason = null) {
		return check_dependency_items_installed (dep, out reason);
	}

	public override bool install_dependency (Dependency dep) {
		// libraries and binaries are installed through PackageKit or other solvers
		return false;
	}
}

} // End of namespace: Listaller.Dep
