/* depmanager.vala - Install & remove software dependencies
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
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

private class DepManager : Object {
	private SoftwareDB db;
	private Listaller.Settings conf;

	public signal void error_code (ErrorItem error);
	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public DepManager (SoftwareDB lidb) {
		db = lidb;
		conf = lidb.get_liconf ();
		// This should never happen!
		if (conf == null) {
			error ("Listaller config was NULL in DepManager constructor!");
			conf = new Listaller.Settings ();
		}
		if (!db.database_locked ()) {
			critical ("Dependency manager received a read-only database! This won't work if write actions have to be performed!");
		}
	}

	public SoftwareDB get_sdb () {
		return db;
	}

	#if 0
	internal void receive_provider_progress (int p) {
		prog += p;
		int progress = (int) Math.round (oneprog * prog);
		assert (progress <= 100);
		progress_changed (progress);
	}
	#endif

	private void emit_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		error_code (item);
		li_error (details);
	}

	private void emit_depmissing_error (ErrorItem? inst_error, IPK.Dependency dep) {
		string text = "";
		if (inst_error != null)
			text = "\n\n%s".printf (inst_error.details);
		emit_error (ErrorEnum.DEPENDENCY_MISSING, "%s%s".printf (
					_("Unable to find valid candidate to satisfy dependency '%s'!").printf (dep.full_name),
					text));
	}


	/* This method checks if the dependency is already there
	 * Returns true if dependency could be resolved, means that it can be installed
	 * via native packages. If it returns false, the dependency is not installable.
	 */
	private bool find_dependency_internal (ref IPK.Dependency dep, bool force_feedinstall = false) {
		if (dep.satisfied)
			return true;

		// First of all, check if the dependency is already there
		if (dependency_is_installed (ref dep))
			return true;

		// If we force feed-install and don't have a feed... This just can't work.
		if ((force_feedinstall) && (dep.feed_url == ""))
			return false;

		bool ret = true;

		// Finish if the dependency is already satisfied
		if (dep.satisfied)
			return true;

		/* Search files using "find_library" before calling PackageKit to do this
		 * (this is a huge speed improvement)
		 * This only works for library dependencies!
		 */
		ret = false;
		foreach (string cmp in dep.raw_complist) {
			if (dep.component_get_type (cmp) == ComponentType.SHARED_LIB) {
				string s = dep.component_get_name (cmp);
				if (s.has_suffix (".*"))
					s = s.replace (".*", "");
				ret = find_library (s, conf);
				if (!ret) {
					debug ("Library not found:" + s);
				}
				if (!ret)
					break;
			}
		}

		// If all libraries were found, add them to installdata and exit
		if (ret) {
			dep.clear_installdata ();
			foreach (string s in dep.raw_complist)
				if (dep.component_get_type (s) == ComponentType.SHARED_LIB)
					dep.add_installed_comp (s);
			dep.satisfied = true;
			return true;
		}

		// If we do a feedinstall anyway, we don't need to solve native pkgs
		if (force_feedinstall)
			return true;

		// Try if we can find native packages providing the dependency
		var pksolv = new PkResolver (conf);
		pksolv.message.connect ( (m) => { this.message (m); } );
		// TODO: Forward progress_changed

		ret = pksolv.search_dep_packages (ref dep);

		// If we don't have a feed AND can't install a native package we're doomed. :P
		// The dependency cannot be solved then.
		if ((!ret) && (dep.feed_url == "")) {
			return false;
		}

		// All packages are already there
		if (dep.satisfied)
			return true;

		return true;
	}

	private bool install_dependency_internal (PkInstaller pkinst, FeedInstaller finst,
						ref IPK.Dependency dep, bool force_feedinstall = false) {

		bool ret;
		ret = find_dependency_internal (ref dep);

		// We cannot install this dependency, as it cannot be solved. So exit here
		if (!ret) {
			emit_depmissing_error (null, dep);
			return false;
		}

		// If dependency is already satisfied, we don't need to run Pk on it
		if (dep.satisfied)
			return true;

		ErrorItem? error = null;

		/* Now try the PackageKit dependency provider, if feedinstall is not forced
		 * and the previous package dependency searching was successful */
		if ((!force_feedinstall) && (dep.has_installdata ())) {
			ret = pkinst.install_dependency (ref dep);
			if (!ret)
				error = pkinst.last_error;
		}

		// Finish if the dependency is satisfied
		if (dep.satisfied)
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

	public bool dependency_is_installed (ref IPK.Dependency dep) {
		IPK.Dependency? dbDep = db.get_dependency_by_id (dep.idname);
		if (dbDep != null) {
			debug ("Dependency with id [%s] is already installed :)", dep.idname);
			dep = dbDep;
			return true;
		}
		return false;
	}

	public bool install_dependency (ref IPK.Dependency dep, bool force_feedinstall = false) {
		PkInstaller pkinst = new PkInstaller (conf);
		pkinst.message.connect ( (m) => { this.message (m); } );

		FeedInstaller finst = new FeedInstaller (conf);
		finst.message.connect ( (m) => { this.message (m); } );

		var di = new DepInfo ();
		di.update_dependency_with_system_data (ref dep);

		// If we have a system standard-lib (a minimal distribution dependency), consider it as installed
		if (dep.is_standardlib) {
			dep.satisfied = true;
			return true;
		}

		bool ret = true;
		if (!dependency_is_installed (ref dep)) {
			ret = install_dependency_internal (pkinst, finst, ref dep, force_feedinstall);

			if ((ret) && (dep.satisfied))
				db.add_dependency (dep);
		}

		return ret;
	}

	public bool install_dependencies (ArrayList<IPK.Dependency> depList, bool force_feedinstall = false) {
		PkInstaller pkinst = new PkInstaller (conf);
		pkinst.message.connect ( (m) => { this.message (m); } );

		FeedInstaller finst = new FeedInstaller (conf);
		finst.message.connect ( (m) => { this.message (m); } );

		// Update dependencies with distributor's system data
		var di = new DepInfo ();
		foreach (IPK.Dependency idep in depList) {
			/* Update package dependencies with system data (which might add some additional information here, provided
			 * by the distributor */
			di.update_dependency_with_system_data (ref idep);

			if (idep.is_standardlib) {
				// If we have a system standard-lib, always consider it as installed
				idep.satisfied = true;
				continue;
			}
		}

		bool ret = true;
		foreach (IPK.Dependency dep in depList) {
			// If this is a default lib, just continue
			if (dep.is_standardlib)
				continue;

			ret = true;
			if (!dependency_is_installed (ref dep)) {
				ret = install_dependency_internal (pkinst, finst, ref dep, force_feedinstall);
				if ((ret) && (dep.satisfied))
					db.add_dependency (dep);
			}

			if (!ret)
				break;
		}
		return ret;
	}

	internal bool dependencies_installable (ArrayList<IPK.Dependency> depList, bool force_feedinstall = false) {
		// Update dependencies with distributor's system data
		var di = new DepInfo ();
		foreach (IPK.Dependency idep in depList) {
			/* Update package dependencies with system data (which might add some additional information here, provided
			 * by the distributor */
			di.update_dependency_with_system_data (ref idep);

			if (idep.is_standardlib) {
				// If we have a system standard-lib, always consider it as installed
				idep.satisfied = true;
				continue;
			}
		}

		bool ret = true;
		foreach (IPK.Dependency dep in depList) {
			// If this is a default lib, just continue
			if (dep.is_standardlib)
				continue;

			ret = find_dependency_internal (ref dep, force_feedinstall);
			if (!ret)
				break;
		}
		return ret;
	}

}

} // End of namespace
