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

	private bool enabled;

	public AbstractSolver (SetupSettings setup_settings) {
		id = "AbstractSolver";

		ssettings = setup_settings;
		conf = new Config ();
		enabled = true;
	}

	/**
	 * Returns if the current solver can be used on the given component.
	 * If it the function returns FALSE, the solver is skipped.
	 */
	public virtual bool usable (Dependency dep) {
		if (!enabled)
			return false;

		enabled = (id == "Basic") || conf.installer_get_string_in_list ("UseResolvers", id);
		// we enable all solvers when running unit-tests
		if (__unittestmode)
			enabled = true;
		if (!enabled) {
			debug ("Dependency resolver '%s' has been disabled in system configuration.", id);
			return false;
		}

		return !dep.installed;
	}

	/**
	 * Function to check if items of a module are already installed.
	 * This function should complete relatively fast, since it is run even before
	 * a request to PackageKit is sent.
	 * Installed items are added to the list of installed items.
	 *
	 * @param dep Dependency which should be checked.
	 * @param reason Reference to a string in which the reason why the dependency can not be satisfied is returned.
	 *
	 * @return TRUE if this solver was successful, FALSE otherwise.
	 */
	public abstract bool check_dependency_installed (Dependency dep, out string? reason = null);

	/**
	 * Trigger installation of a module.
	 *
	 * The function should throw an error if installation fails.
	 *
	 * @param dep Dependency which should be installed
	 *
	 * @return TRUE if there was no error (even if the solver is unable to perform an installation). FALSE if installation failed.
	 */
	public virtual bool install_dependency (Dependency dep) throws SolverError {
		return false;
	}


}

} // End of namespace: Listaller.Dep
