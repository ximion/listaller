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
using Gee;
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
internal class ComponentFactory : Object {
	private string system_components_dir;
	private ArrayList<AbstractSolver> solver_pool;
	private SetupSettings ssettings;

	public HashMap<string, Dep.Framework> registered_frameworks { get; private set; }
	public HashMap<string, Dep.Module> registered_modules { get; private set; }


	public ComponentFactory (SetupSettings? setup_settings = null) {
		system_components_dir = PkgConfig.DATADIR + "/listaller/components";

		registered_frameworks = new HashMap<string, Dep.Framework> ();
		registered_modules = new HashMap<string, Dep.Module> ();
		solver_pool = new ArrayList<AbstractSolver> ();

		ssettings = setup_settings;
		if (ssettings == null)
			ssettings = new SetupSettings (IPK.InstallMode.SHARED);
	}

	public void initialize () {
		HashSet<string>? framework_info_files = find_files_matching (Path.build_filename (system_components_dir, "frameworks", null), "*.framework");
		HashSet<string>? module_info_files = find_files_matching (Path.build_filename (system_components_dir, "modules", null), "*.module");

		// process all framework data
		if (framework_info_files != null) {
			foreach (string fname in framework_info_files) {
				var cfrmw = new Dep.Framework.blank ();
				bool ret = cfrmw.load_from_file (fname);
				if (ret)
					registered_frameworks.set (cfrmw.idname, cfrmw);
				else
					warning ("Unable to load data for framework: %s", fname);
			}
		}

		// process all module data
		if (module_info_files != null) {
			foreach (string fname in module_info_files) {
				var cmod = new Dep.Module.blank ();
				bool ret = cmod.load_from_file (fname);
				if (ret)
					registered_modules.set (cmod.idname, cmod);
				else
					warning ("Unable to load data for module: %s", fname);
			}
		}
	}

	/**
	 * Ensure that the solver pool is set-up. If it isn't,
	 * a new pool is created.
	 */
	private void init_solverpool () {
		if (solver_pool.size > 0)
			return;
		solver_pool.clear ();

		// the order of the solvers defines their priority
		solver_pool.add (new BasicSolver (ssettings));
		solver_pool.add (new PkitSolver (ssettings));
		//solver_pool.add (new PythonSolver (ssettings));
		solver_pool.add (new ZFeedSolver (ssettings));
	}

	public ArrayList<AbstractSolver> get_solverpool () {
		init_solverpool ();

		return solver_pool;
	}

	public void load_extra_modules (string module_dir) {
		HashSet<string>? extra_module_info_files = find_files_matching (module_dir, "*.module");

		// process additional module data
		if (extra_module_info_files != null) {
			foreach (string fname in extra_module_info_files) {
				var cmod = new Dep.Module.blank ();
				bool ret = cmod.load_from_file (fname);
				// installed system modules take precendence before any additional modules
				if (cmod.idname in registered_modules)
					continue;
				if (ret)
					registered_modules.set (cmod.idname, cmod);
				else
					warning ("Unable to load data for module: %s", fname);
			}
		}
	}

	/**
	 * Check for a valid version-compare string
	 */
	private bool version_comp_is_valid (string version_str) {
		string vs = version_str;
		if (vs == "")
			return true;
		return vs.has_prefix ("<< ") || vs.has_prefix (">> ") || vs.has_prefix ("<= ") || vs.has_prefix (">= ") || vs.has_prefix ("== ");
	}

	/**
	 * Function which runs the solver methods on dependencies, to
	 * determine if they are installed.
	 */
	private bool run_solver_framework (AbstractSolver depsolver, Dep.Framework cfrmw) {
		if (!depsolver.usable (cfrmw))
			return true;
		bool ret;
		string reason;
		ret = depsolver.check_framework_items_installed (cfrmw, out reason);
		if (!ret)
			warning ("Framework '%s' could not be found, reason: %s", cfrmw.full_name, reason);

		return ret;
	}

	/**
	 * Function which runs the solver methods on dependencies, to
	 * determine if they are installed and, if not, which parts are missing
	 * to satisfy a dependency.
	 */
	private bool run_solver_module (AbstractSolver depsolver, Dep.Module cmod, out string reason) {
		if (!depsolver.usable (cmod))
			return true;
		bool ret;
		ret = depsolver.check_module_items_installed (cmod, out reason);

		return ret;
	}

	private bool check_framework_installed (Dep.Framework cfrmw) {
		if (cfrmw.installed)
			return true;

		bool ret = false;
		string reason;
		init_solverpool ();

		foreach (AbstractSolver solver in solver_pool) {
			ret = run_solver_framework (solver, cfrmw);
			if (!ret)
				return false;
		}

		return ret;
	}

	private bool check_module_installed (Dep.Module cmod, out string reason = null) {
		if (cmod.installed)
			return true;

		bool ret;
		init_solverpool ();

		foreach (AbstractSolver solver in solver_pool) {
			ret = run_solver_module (solver, cmod, out reason);
			if (!ret)
				return false;
			// update the installed state, in case we resolved everything
			cmod.update_installed_status ();
		}

		if ((!cmod.installed) && (reason == null))
			critical ("Module '%s' is not installed, but we don't know a reason why, since no resolver has failed. This is usually a bug in a resolver, please fix it!", cmod.full_name);

		return cmod.installed;
	}

	public bool framework_installed (string idname) {
		if (registered_frameworks.has_key (idname)) {
			Dep.Framework cfrmw = get_framework (idname);
			check_framework_installed (cfrmw);
			return cfrmw.installed;
		}

		return false;
	}

	public bool component_version_satisfied (string version, string reference_version, string relation) {
		bool ret = false;
		// always satisfied if no required version is set
		if (reference_version == "")
			return true;
		// no version found, but reference-version set? => version can never be satisfied
		if (version == "")
			return false;

		int compare_result = compare_versions (version, reference_version);
		switch (relation) {
				case "==":
				case "<=":
				case ">=":
				ret = compare_result == 0;
				break;
		}
		if (ret)
			return true;

		if (relation.index_of (">") > 0)
			return compare_result > 0;
		if (relation.index_of ("<") > 0)
			return compare_result < 0;

		critical ("Unable to compare versions for relation: %s (this should never happen)", relation);
		return false;
	}

	private bool is_satisfied (string idname, string version_comp, out Dep.Module required_mod = null, out string reason = null) {
		if (!version_comp_is_valid (version_comp)) {
			warning ("Version compare string %s is not valid!", version_comp);
			reason = "Invalid version-compare string! (This is a serious packaging bug)";
			return false;
		}

		bool ret = false;
		// prepare our version
		string[] vparts;
		if (version_comp == "")
			vparts = { "", "" }; // if we don't care about the version, relation and version are empty strings
		else
			vparts = version_comp.split (" ", 2);
		string required_version = vparts[1].strip ();
		string required_version_relation = vparts[0].strip ();

		if (registered_frameworks.has_key (idname)) {
			Dep.Framework cfrmw = get_framework (idname);
			check_framework_installed (cfrmw);
			if (!cfrmw.installed) {
				reason = _("Framework %s is not installed! Please make it available to continue.").printf (cfrmw.full_name);
				return false;
			}

			string c_version;
			try {
				c_version = cfrmw.get_version ();
			} catch (Error e) {
				reason = e.message;
				return false;
			}

			ret = component_version_satisfied (c_version, required_version, required_version_relation);
			if (!ret)
				reason = _("Framework %s is only available in version %s, while a version %s %s is required. This software can not be installed, please notify the original author about it.").printf (cfrmw.full_name,
																										c_version,
																										required_version,
																										required_version_relation);
			return ret;

		} else if (registered_modules.has_key (idname)) {
			// TODO: Resolve module (installed? all dependencies installed?)
			Dep.Module cmod = get_module (idname);
			string ic_reason;
			check_module_installed (cmod, out ic_reason);

			if (!cmod.installed) {
				reason = ic_reason;
				required_mod = cmod;
				return false;
			}

			string c_version;
			try {
				c_version = cmod.get_version ();
			} catch (Error e) {
				reason = e.message;
				return false;
			}

			ret = component_version_satisfied (c_version, required_version, required_version_relation);
			if (!ret) {
				reason = _("Module %s is only available in version %s, while a version %s %s is required. This software can not be installed, please notify the original author about it.").printf (cmod.full_name,
																										c_version,
																										required_version,
																										required_version_relation);
				required_mod = cmod;
			}

			return true;
		} else {
			reason = _("No framework/module matching '%s' (v%s) found!").printf (idname, version_comp);

			return false;
		}
	}

	/*
	 * Get franework dependency by name
	 */
	public Dep.Framework? get_framework (string name) {
		return registered_frameworks.get (name);
	}

	/*
	 * Get Module dependency by name
	 */
	public Dep.Module? get_module (string name) {
		return registered_modules.get (name);
	}

	/**
	 * Determine if a package with the dependencies taken as function argument can be installed, and if not,
	 * return the modules which require installation.
	 *
	 * @param dependencies An IPK package dependency line
	 * @param required_modules Variable to store an ArrayList of required modules, in order to make the package installable
	 * @param reason A reason why the given package cannot be installed, as localized string.
	 *
	 * @returns TRUE if there is a way to install this application, FALSE in any other case
	 */
	public bool can_be_installed (string dependencies, out ArrayList<Dep.Module> required_modules, out string reason = null) {
		// a dependencies listing is comma-separated, so split it
		string[] deps = dependencies.split(",");

		required_modules = new ArrayList<Dep.Module> ();
		foreach (string dep in deps) {
			string name;
			string vcomp;

			if (dep.index_of ("(") > 0) {
				// we have a version number, so extract it
				string[] parts = dep.split ("(", 2);
				name = parts[0].strip ();
				vcomp = parts[1].strip ();
				if (vcomp.has_suffix (")"))
					vcomp = vcomp.substring (0, vcomp.length-1);
				else
					error ("Invalid formatting of dependency line '%s'!", dependencies);
			} else {
				name = dep.strip ();
				vcomp = ""; // no version set, we ignore it
			}

			string s_reason;
			Dep.Module dep_mod;
			bool ret;
			ret = is_satisfied (name, vcomp, out dep_mod, out s_reason);
			if (!ret) {
				// check if we have a module dependency, which can be satisfied
				if (dep_mod == null) {
					reason = s_reason;
					return false;
				}
				required_modules.add (dep_mod);
			}
		}

		return true;
	}

	/**
	 * This function is currently only used for testing purposes.
	 *
	 * @returns TRUE if dependencies in dep_list are installable.
	 */
	internal bool modules_installable (ref ArrayList<Dep.Module> dep_list, out string? reason = null) {
		bool ret = true;
		foreach (Dep.Module dep in dep_list) {
			string ic_reason;
			ret = check_module_installed (dep, out ic_reason);
			if (!ret) {
				reason = ic_reason;
				return false;
			}
		}

		return ret;
	}
}

} // End of namespace: Listaller.Dep
