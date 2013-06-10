/* pkit-solver.vala -- Solver using Listaller's built-in PackageKit bridge to satisfy dependencies
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
using Listaller;
using Listaller.Utils;

namespace Listaller.Dep {

private class PkitSolver : AbstractSolver {
	private PkResolver pksolv;
	private PkInstaller pkinst;

	// NOTE: This solver only cares about modules, nothing else is handled
	public PkitSolver (SetupSettings setup_settings) {
		base (setup_settings);
		id = "Native";

		pkinst = new PkInstaller (ssettings);
		pkinst.message.connect ( (m) => { message ("message from PackageKit Installer: %s", m.details); } );
		pksolv = new PkResolver (ssettings);
	}

	public override bool check_module_items_installed (Module cmod, out string? reason = null) {
		bool ret;

		// Try if we can find native packages providing the dependency
		ret = pksolv.search_dep_packages (cmod);
		if (!ret)
			reason = "No native package found to satisfy dependency.";

		return ret;
	}

	public override bool install_module (Module cmod) throws SolverError {
		bool ret;

		ret = pkinst.install_dependency (cmod);
		if (!ret)
			throw new SolverError.INSTALLATION_FAILED(pkinst.last_error.details);

		return ret;
	}
}

} // End of namespace: Listaller.Dep
