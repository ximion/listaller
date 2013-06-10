/* abstract-solver.vala -- Base class for dependency solvers
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

private errordomain SolverError {
	RESOLVING_FAILED,
	INSTALLATION_FAILED,
	INTERNAL;
}

private abstract class AbstractSolver : Object {
	public string id { get; protected set; }

	protected SetupSettings ssettings;
	protected Config conf;

	public AbstractSolver (SetupSettings setup_settings) {
		id = "AbstractSolver";

		ssettings = setup_settings;
		conf = new Config ();
	}

	/**
	 * Returns if the current solver can be used on the given component.
	 * If it the function returns FALSE, the solver is skipped.
	 */
	public virtual bool usable (Component cmp) {
		bool ret;
		ret = (id == "Basic") || conf.installer_get_string_in_list ("UseResolvers", id);
		if (!ret) {
			debug ("Dependency resolver '%s' has been disabled in system configuration.", id);
			return false;
		}

		return !cmp.installed;
	}

	/**
	 * Function to check if items of a framework are already installed.
	 * This function should complete relatively fast.
	 *
	 * @param cfrmw Framework which should be checked.
	 * @param reason Reference to a string in which the reason why the dependency can not be satisfied is returned.
	 *
	 * @return TRUE if this solver was successful, FALSE otherwise.
	 */
	public virtual bool check_framework_items_installed (Framework cfrmw, out string? reason = null) {
		return true;
	}

	/**
	 * Function to check if items of a module are already installed.
	 * This function should complete relatively fast, since it is run even before
	 * a request to PackageKit is sent.
	 * Installed items are added to the list of installed items.
	 *
	 * @param cmod Module which should be checked.
	 * @param reason Reference to a string in which the reason why the dependency can not be satisfied is returned.
	 *
	 * @return TRUE if this solver was successful, FALSE otherwise.
	 */
	public abstract bool check_module_items_installed (Module cmod, out string? reason = null);

	public abstract bool install_module (Module cmod) throws SolverError;


}

} // End of namespace: Listaller.Dep
