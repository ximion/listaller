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
	private SetupSettings ssettings;

	public DepInstaller (SoftwareDB lidb) {
		base ();
		db = lidb;
		ssettings = lidb.setup_settings;

		// This should never happen!
		if (ssettings == null) {
			error ("Listaller config was NULL in DepManager constructor!");
			ssettings = new SetupSettings ();
		}
		if (!db.database_writeable ()) {
			critical ("Dependency installer received a read-only database! This won't work if write actions have to be performed!");
		}

		// Create a new dependency manager to fetch information about installed dependencies
		depman = new DepManager (db);
		depman.connect_with_object_all (this);
	}

	private void emit_depmissing_error (ErrorItem? inst_error, Dep.Module dep) {
		string text = "";
		if (inst_error != null)
			text = "\n\n%s".printf (inst_error.details);
		emit_error (ErrorEnum.DEPENDENCY_MISSING, "%s%s".printf (
			_("Unable to find valid candidate to satisfy dependency '%s'!").printf (dep.full_name),
			  text));
	}

	/**
	 * This method checks if the dependency is already there
	 *
	 * @returns TRUE if dependency could be resolved, means that it can be installed
	 * via native packages. If it returns FALSE, the dependency is not installable.
	 */
	private bool find_dependency_internal (ref Dep.Module dep, bool force_feedinstall = false) {
		if (dep.installed)
			return true;

		// First of all, check if the dependency is already there
		if (depman.module_is_installed (ref dep))
			return true;

		// If we force feed-install and don't have a feed... This just can't work.
		if ((force_feedinstall) && (dep.feed_url == ""))
			return false;

		bool ret = true;

		// Finish if the dependency is already satisfied
		if (dep.installed)
			return true;

		// If we do a feedinstall anyway, we don't need to solve native pkgs
		if (force_feedinstall)
			return true;

		// Try if we can find native packages providing the dependency
		var pksolv = new PkResolver (ssettings);
		connect_with_object (pksolv, ObjConnectFlags.PROGRESS_TO_SUBPROGRESS);

		ret = pksolv.search_dep_packages (dep);

		// TODO: Implement & call all other solvers, to determine if this dependency can be satisfied

		// If we don't have a feed AND can't install a native package we're doomed. :P
		// The dependency cannot be solved then.
		if ((!ret) && (dep.feed_url == "")) {
			return false;
		}

		// All packages are already there
		if (dep.installed)
			return true;

		return true;
	}

	private bool install_module_dep_internal (PkInstaller pkinst, FeedInstaller finst,
						  ref Dep.Module dep, bool force_feedinstall = false) {

		bool ret;
		ret = find_dependency_internal (ref dep);

		// We cannot install this dependency, as it cannot be solved. So exit here
		if (!ret) {
			emit_depmissing_error (null, dep);
			return false;
		}

		// If dependency is already satisfied, we don't need to run Pk on it
		if (dep.installed)
			return true;

		ErrorItem? error = null;

		/* Now try the PackageKit dependency provider, if feedinstall is not forced
		 * and the previous package dependency searching was successful */
		if ((!force_feedinstall) && (dep.has_installdata ())) {
			ret = pkinst.install_dependency (dep);
			if (!ret)
				error = pkinst.last_error;
		}

		// Finish if the dependency is satisfied
		if (dep.installed)
			return true;

		// Now try to install from dependency-feed
		ret = finst.install_dependency (db, ref dep);
		if (!ret) {
			if (dep.has_feed ())
				error = finst.last_error;
			emit_depmissing_error (error, dep);
			return false;
		}

		return ret;
	}

	/**
	 * Install dependencies from a dependency-string of an IPK package.
	 *
	 * @param dependencies_str Dependency-information as string (comma-separated)
	 * @param cfactory The ComponentFactory instance which belongs to the given package, or an empty, initialized ComponentFactory instance
	 * @param force_feedinstall Enforce installation of ZeroInstall feeds, defaults to FALSE
	 * The function emits an error signal on the DepInstaller instance on failure.
	 *
	 * @returns TRUE if there were no errors.
	 */
	public bool install_dependencies (string dependencies_str, ComponentFactory cfactory, bool force_feedinstall = false) {
		PkInstaller pkinst = new PkInstaller (ssettings);
		pkinst.message.connect ( (m) => { this.message (m); } );

		FeedInstaller finst = new FeedInstaller (ssettings);
		finst.message.connect ( (m) => { this.message (m); } );

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
				ret = install_module_dep_internal (pkinst, finst, ref dep_mod, force_feedinstall);
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
	internal bool install_existing_module_dependency (ref Dep.Module dep_mod) {
		PkInstaller pkinst = new PkInstaller (ssettings);
		pkinst.message.connect ( (m) => { this.message (m); } );

		FeedInstaller finst = new FeedInstaller (ssettings);
		finst.message.connect ( (m) => { this.message (m); } );

		bool ret = true;
		if (!depman.module_is_installed (ref dep_mod)) {
			ret = install_module_dep_internal (pkinst, finst, ref dep_mod);
			if ((ret) && (dep_mod.installed))
				db.add_dependency (dep_mod);
		}

		return ret;
	}

	/**
	 * This function is currently only used for testing purposes.
	 *
	 * @returns TRUE if dependencies in dep_list are installable.
	 */
	internal bool dependencies_installable (ref ArrayList<Dep.Module> dep_list, bool force_feedinstall = false) {
		bool ret = true;
		foreach (Dep.Module dep in dep_list) {
			ret = find_dependency_internal (ref dep, force_feedinstall);
			if (!ret)
				break;
		}
		return ret;
	}

}

} // End of namespace: Listaller
