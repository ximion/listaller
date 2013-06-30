/* zfeed-solver.vala -- Solve dependencies using ZeroInstall feeds
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

private class ZFeedSolver : AbstractSolver {
	public ZFeedSolver (SetupSettings setup_settings) {
		base (setup_settings);
		id = "ZeroInstall";
	}

	public override bool usable (Component cmp) {
		if (!base.usable (cmp))
			return false;

		// we can only handle modules
		if (cmp is Framework)
			return false;
		if (cmp is Module) {
			Module cmod = cmp as Module;
			if (!cmod.has_feed ())
				return false;
			return true;
		}

		// we should never get here....
		warning ("Some invalid component type was received in ZFeedSolver.usable() method!");
		return false;
	}

	public override bool check_module_items_installed (Module cmod, out string? reason = null) {
		// we do not handle install-checks
		return true;
	}

	public override bool install_module (Module cmod) throws SolverError {
		Dep.FeedInstaller finst = new Dep.FeedInstaller (ssettings);
		bool ret;
		ret = finst.install_dependency (cmod);
		if (finst.last_error != null) {
			throw new SolverError.INSTALLATION_FAILED (finst.last_error.details);
		}

		return ret;
	}
}

} // End of namespace: Listaller.Dep
