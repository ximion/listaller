/* component-factory.vala -- Read and process component information
 *
 * Copyright (C) 2012-2014 Matthias Klumpp <matthias@tenstral.net>
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
	private string listaller_metainfo_dir;
	private ArrayList<AbstractSolver> solver_pool;
	private SetupSettings ssettings;

	public HashMap<string, Dependency> registered_deps { get; private set; }

	private HashSet<string>? metainfo_files_sys;
	private HashSet<string>? metainfo_files_listaller;

	public ComponentFactory (SetupSettings? setup_settings = null) {
		listaller_metainfo_dir = PkgConfig.DATADIR + "/listaller/metainfo-templates";

		registered_deps = new HashMap<string, Dependency> ();
		solver_pool = new ArrayList<AbstractSolver> ();

		ssettings = setup_settings;
		if (ssettings == null)
			ssettings = new SetupSettings (IPK.InstallMode.SHARED);
	}

	private void load_dependency_data (HashSet<string> metainfo_files, bool include_optional) {
		foreach (string fname in metainfo_files) {
			var dep = new Dependency.blank ();
			bool ret = dep.load_from_file (fname, include_optional);
			if (ret)
				registered_deps.set (dep.info.id, dep);
			else
				warning ("Unable to load data for component: %s", fname);
		}
	}

	/**
	 * Initialize the component-factory with data from disk.
	 *
	 * @param include_optional Include optional sections in component-definitions. This is usually only used
	 *                         by the package-builder to group dependencies together. The parameter defaults to FALSE.
	 */
	public void initialize (bool include_optional = false) {
		// search for metadata installed on this system
		metainfo_files_sys = find_files_matching (Config.sys_componentdir, "*.metainfo.xml");
		// now add Listaller-provided metadata templates (the system files override these ones)
		metainfo_files_listaller = find_files_matching (listaller_metainfo_dir, "*.metainfo.xml");
		//FIXME: find_files_matching (Path.build_filename (listaller_metainfo_dir, "frameworks", null), "*.framework");

		// process all dependency data
		if (metainfo_files_sys != null)
			load_dependency_data (metainfo_files_sys, include_optional);
		if (metainfo_files_listaller != null)
			load_dependency_data (metainfo_files_listaller, include_optional);
	}

	/**
	 * Find the path which matches the framework name
	 *
	 * This is mainly used while building an Listaller package
	 *
	 * @return NULL if no path was found.
	 */
	public string? find_component_path (string cptname) {
		if (metainfo_files_sys != null) {
			foreach (string fname in metainfo_files_sys) {
				var dep = new Dependency.blank ();
				bool ret = dep.load_from_file (fname, false);
				if (dep.info.id == cptname)
					return fname;
			}
		}
		if (metainfo_files_listaller != null) {
			foreach (string fname in metainfo_files_listaller) {
				var dep = new Dependency.blank ();
				bool ret = dep.load_from_file (fname, false);
				if (dep.info.id == cptname)
					return fname;
			}
		}

		return null;
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
		//! solver_pool.add (new PythonSolver (ssettings));
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
				var dep = new Dependency.blank ();
				bool ret = dep.load_from_file (fname);
				// installed system modules take precendence before any additional modules
				if (dep.info.id in registered_deps)
					continue;
				if (ret)
					registered_deps.set (dep.info.id, dep);
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
	 * determine if they are installed and, if not, which parts are missing
	 * to satisfy a dependency.
	 */
	private bool run_solver_module (AbstractSolver depsolver, Dependency dep, out string reason) {
		if (!depsolver.usable (dep))
			return true;
		bool ret;
		ret = depsolver.check_dependency_installed (dep, out reason);

		return ret;
	}

	private bool check_module_installed (Dependency dep, out string reason = null) {
		if (dep.installed)
			return true;

		bool ret;
		init_solverpool ();

		foreach (AbstractSolver solver in solver_pool) {
			ret = run_solver_module (solver, dep, out reason);
			if (!ret)
				return false;
		}

		if ((!dep.installed) && (reason == null))
			critical ("Module '%s' is not installed, but we don't know a reason why, since no resolver has failed. This is usually a bug in a resolver, please fix it!", dep.info.name);

		return dep.installed;
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

	private bool is_satisfied (string idname, string version_comp, out Dependency required_dep = null, out string reason = null) {
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

		if (registered_deps.has_key (idname)) {
			// TODO: Resolve module (installed? all dependencies installed?)
			Dependency dep = get_dependency (idname);
			string ic_reason;
			check_module_installed (dep, out ic_reason);

			if (!dep.installed) {
				reason = ic_reason;
				required_dep = dep;
				return false;
			}

			string c_version;
			try {
				c_version = dep.get_version ();
			} catch (Error e) {
				reason = e.message;
				return false;
			}

			ret = component_version_satisfied (c_version, required_version, required_version_relation);
			if (!ret) {
				reason = _("Component %s is only available in version %s, while a version %s %s is required. This software can not be installed, please notify the original author about it.").printf (dep.info.name,
																										c_version,
																										required_version,
																										required_version_relation);
				required_dep = dep;
			}

			return true;
		} else {
			reason = _("No component matching '%s' (v%s) found!").printf (idname, version_comp);

			return false;
		}
	}

	/*
	 * Get dependency by name
	 */
	public Dependency? get_dependency (string name) {
		return registered_deps.get (name);
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
	public bool can_be_installed (string dependencies, out ArrayList<Dependency> required_modules, out string reason = null) {
		// a dependencies listing is comma-separated, so split it
		string[] deps = dependencies.split(",");

		required_modules = new ArrayList<Dependency> ();
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
			Dependency dep_mod;
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
	internal bool modules_installable (ref ArrayList<Dependency> dep_list, out string? reason = null) {
		bool ret = true;
		foreach (Dependency dep in dep_list) {
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
