/* dep-installer.vala - Perform everything required for dependency solving & installing
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
using Gee;
using Listaller;
using Listaller.Utils;
using Listaller.Dep;

namespace Listaller {

private class DepInstaller : MessageObject {
	private SoftwareDB db;
	private DepManager depman;
	private Config conf;

	public DepInstaller (SoftwareDB software_db) {
		base ();
		db = software_db;

		if (!db.database_writeable ()) {
			critical ("Dependency installer received a read-only database! This won't work if write actions have to be performed!");
		}

		// Create a new dependency manager to fetch information about installed dependencies
		depman = new DepManager (db);
		depman.connect_with_object_all (this);

		conf = new Config ();
	}

	private void emit_depmissing_error (ErrorItem? inst_error, Dep.Module dep) {
		string text = "";
		if (inst_error != null)
			text = "\n\n%s".printf (inst_error.details);
		emit_error (ErrorEnum.DEPENDENCY_MISSING, "%s%s".printf (
			_("Unable to find valid candidate to satisfy dependency '%s'!").printf (dep.full_name),
			  text));
	}

	private bool install_module_dep_internal (ComponentFactory cfactory, Dep.Module cmod) {
		// If dependency is already satisfied, we don't need to run an installation
		// (usually, this function shouldn't be called then, but we check it again for sanity)
		if (cmod.installed)
			return true;

		// all the solver we have - now for installing stuff
		ArrayList<AbstractSolver> solver_pool = cfactory.get_solverpool ();

		string error_msg = null;
		bool ret = false;

		foreach (AbstractSolver solver in solver_pool) {
			if (!solver.usable (cmod))
				continue;
			if ((solver.id != "Native") && (conf.installer_get_bool ("Install3rdPartyModules") == false)) {
				error_msg = _("System policy forbids installation of 3rd-party modules.");
				break;
			}

			try {
				ret = solver.install_module (cmod);
				debug ("Solver '%s', result: %i", solver.id, (int) ret);
			} catch (SolverError e) {
				string? msg = e.message;
				if (msg == null)
					msg = "Solver did not return an error.";
				debug ("Solver install error: %s", msg);
				error_msg = _("Unable to install module '%s' which is required for this installation.\nMessage: %s").printf (cmod.full_name, msg);
			}
			if (ret)
				return true;
		}

		// emit the last error message found
		if (error_msg != null) {
			emit_error (ErrorEnum.DEPENDENCY_INSTALL_FAILED, error_msg);
			return false;
		}

		// all resolvers failed, but we don't have an explicit error -> no resolver was able to handle the dependency
		if (!ret) {
			emit_error (ErrorEnum.DEPENDENCY_INSTALL_FAILED,
						_("Unable to install module '%s' which is required for this installation.\nUnable to find a method to satisfy the dependency.").printf (cmod.full_name));
		}

		return ret;
	}

	/**
	 * Install dependencies from a dependency-string of an IPK package.
	 *
	 * @param dependencies_str Dependency-information as string (comma-separated)
	 * @param cfactory The ComponentFactory instance which belongs to the given package, or an empty, initialized ComponentFactory instance
	 * The function emits an error signal on the DepInstaller instance on failure.
	 *
	 * @returns TRUE if there were no errors.
	 */
	public bool install_dependencies (string dependencies_str, ComponentFactory cfactory) {
		ArrayList<Dep.Module> req_mods;
		string fail_reason;

		bool ret = cfactory.can_be_installed (dependencies_str, out req_mods, out fail_reason);
		if (!ret) {
			emit_error (ErrorEnum.DEPENDENCY_MISSING, fail_reason);
			return false;
		}

		foreach (Dep.Module dep_mod in req_mods) {
			debug ("Prepared module dependency %s, satisfied: %i", dep_mod.idname, (int) dep_mod.installed);

			ret = true;
			if (!depman.module_is_installed (ref dep_mod)) {
				ret = install_module_dep_internal (cfactory, dep_mod);
				if ((ret) && (dep_mod.installed))
					db.add_dependency (dep_mod);
			}
			if (!ret)
				break;
		}

		return ret;
	}

	/**
	 * This method is useful for testing, and should only be used for that
	 * purpose internally.
	 */
	internal bool install_existing_module_dependency (ComponentFactory cfactory, ref Dep.Module dep_mod) {
		bool ret = true;
		if (!depman.module_is_installed (ref dep_mod)) {
			ret = install_module_dep_internal (cfactory, dep_mod);
			if ((ret) && (dep_mod.installed))
				db.add_dependency (dep_mod);
		}

		return ret;
	}

}

} // End of namespace: Listaller
