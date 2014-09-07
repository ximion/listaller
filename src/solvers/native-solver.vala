/* native-solver.vala -- Solver using Listaller's built-in PackageKit bridge to satisfy dependencies
 *
 * Copyright (C) 2013-2014 Matthias Klumpp <matthias@tenstral.net>
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
using Appstream;
using Listaller;
using Listaller.Utils;

namespace Listaller.Dep {

private class NativeSolver : AbstractSolver {
	private PkResolver pksolv;
	private PkInstaller pkinst;

	// NOTE: This solver only cares about modules, nothing else is handled
	public NativeSolver (SetupSettings setup_settings) {
		base (setup_settings);
		id = "Native";

		pkinst = new PkInstaller (ssettings);
		pkinst.message.connect ( (m) => { message ("message from PackageKit Installer: %s", m.details); } );
		pksolv = new PkResolver (ssettings);
		pksolv.message.connect ( (m) => { message ("message from PackageKit Installer: %s", m.details); } );
	}

	private string? get_full_library_path (string lib_name) {
		/* Search files using "find_library" before calling PackageKit,
		 * since some PK backends want absolute paths and we can rule out
		 * false-positives.
		 */
		string? path = lib_name;
		Config conf = new Config ();
		path = find_library (lib_name, conf);

		return path;
	}

	private string? get_full_binary_path (string bin_name) {
		string[] bin_dirs = {"/usr/bin", "/usr/sbin", "/sbin", "/bin"}; // constant (no const statement, since some C compilers have issues with the resulting C code)

		string? path = null;
		// Check if binaries are present
		if (bin_name.has_prefix ("/")) {
			path = bin_name;
		} else {
			foreach (string prefix in bin_dirs) {
				var tmp = Path.build_filename (prefix, bin_name, null);
				if (FileUtils.test (tmp, FileTest.EXISTS)) {
					path = tmp;
					break;
				}
			}
		}

		return path;
	}

	public override bool check_dependency_installed (Dependency dep, out string? reason = null) {
		bool ret = true;

		if (dep.metainfo.pkgnames == null) {
			/**
			* If we don't have a package name for this component, try to guess it from the
			* component provides.
			*/
			HashTable<string, string> pkg_table = new HashTable<string, string> (str_hash, str_equal);

			GenericArray<string> items = dep.metainfo.get_provided_items ();
			for(uint i = 0; i < items.length; i++) {
				string sitem = items.get (i);
				string? path = null;
				ProvidesKind kind = provides_item_get_kind (sitem);
				string? val = provides_item_get_value (sitem);

				if (kind == ProvidesKind.BINARY) {
					path = get_full_binary_path (val);
				} else if (kind == ProvidesKind.LIBRARY) {
					path = get_full_library_path (val);
				}

				if (path == null)
					break;

				string pkg;
				try {
					pkg = pksolv.find_package_name_for_file (path);
				} catch (Error e) {
					reason = e.message;
					ret = false;
					break;
				}
				pkg_table.insert (pkg, null);
			}

			var keys = pkg_table.get_keys ();
			if (keys.length () > 0) {
				// assume all packages are installed and dependency is satisfied
				// TODO/FIXME: Are there PMs which by default search for files of not-installed pkgs?
				// if so, add smarter logic here.
				dep.installed = true;

				string[] pkgs = {};
				foreach (string key in keys) {
					string[] id_parts = key.split (";");
					if (id_parts.length <= 2) {
						error ("Invalid package-id: %s", key);
						reason = "Invalid package-id: %s".printf (key);
						return false;
					}

					pkgs += id_parts[0];
				}
				// null-terminate array
				pkgs += null;

				dep.metainfo.pkgnames = pkgs;
			} else {
				reason = "No package found for dependency '%s'.".printf (dep.unique_name);
				ret = false;
			}
		} else {
			foreach (string pkg in dep.metainfo.pkgnames) {
				string? pkg_resolved;
				pkg_resolved = pksolv.resolve (pkg);
				if (pkg_resolved == null) {
					reason = "Could not resolve package name '%s' for dependency '%s'.".printf (pkg, dep.unique_name);
					ret = false;
					break;
				}
			}
			if (ret)
				dep.installed = true;
		}

		return ret;
	}

	public override bool install_dependency (Dependency dep) throws SolverError {
		bool ret;

		ret = pkinst.install_dependency (dep);
		if (!ret) {
			if (pkinst.last_error == null)
				ret = true;
			else
				throw new SolverError.INSTALLATION_FAILED (pkinst.last_error.details);
		}

		return ret;
	}
}

} // End of namespace: Listaller.Dep
