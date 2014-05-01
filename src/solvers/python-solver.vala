/* python-solver.vala -- Solver for dependencies on Python modules
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

private class PythonSolver : AbstractSolver {
	private bool check_python2;
	private bool check_python3;

	private HashMap<string, string> python2_modules;
	private HashMap<string, string> python3_modules;

	public PythonSolver (SetupSettings setup_settings) {
		base (setup_settings);
		id = "Python";

		python2_modules = new HashMap<string, string> ();
		python3_modules = new HashMap<string, string> ();
	}

	public override bool usable (Dependency dep) {
		if (!base.usable (dep))
			return false;

		// check if we have the pip-binary
		check_python2 = FileUtils.test ("/usr/bin/pip", FileTest.EXISTS);
		if (!check_python2)
			debug ("Cannot resolve Python2 modules: pip binary is missing.");
		check_python3 = FileUtils.test ("/usr/bin/pip3", FileTest.EXISTS);
		if (!check_python3)
			debug ("Cannot resolve Python3 modules: pip3 binary is missing.");

		return check_python2 || check_python3;
	}

	private void fill_python_modulemaps () {
		// TODO
	}


	public override bool check_dependency_installed (Dependency dep, out string? reason = null) {
		// TODO
		return true;
	}

	public override bool install_dependency (Dependency dep) throws SolverError {
		// TODO
		return false;
	}
}

} // End of namespace: Listaller.Dep
