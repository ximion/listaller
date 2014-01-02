/* ipk-repo-local.vala -- Manage a local Listaller repository
 *
 * Copyright (C) 2012-2014 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU General Public License Version 3
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using GLib;
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

private class RepoLocal : Repo {
	private string repo_root;
	private string repo_pool_dir;

	public RepoLocal (string dir) {
		base ("local");
		repo_root = dir;
		repo_pool_dir = Path.build_filename (dir, "apps", "pool", null);

		rsettings.open (Path.build_filename (repo_root, "reposetting", null));
	}

	~RepoLocal () {
		rsettings.save (Path.build_filename (repo_root, "reposetting", null));
		save_current_index ();
	}

	/**
	 * Add a completely new package
	 */
	private bool add_package_new (string fname, AppItem app, string arch) {
		bool ret;
		string app_dir = Path.build_filename (repo_pool_dir, app.idname, null);

		string canonical_pkgname = build_canonical_pkgname (app, arch);

		ret = create_dir_structure (app_dir);
		if (!ret) {
			Report.log_error ("Unable to create app-id directory.");
			return false;
		}

		// copy current package to the repo tree
		ret = copy_file (fname, Path.build_filename (app_dir, canonical_pkgname, null));
		if (!ret)
			return false;

		// register package
		cindex.update_application (app);

		return ret;
	}

	/**
	 * Update an existing pacjage to a new version
	 */
	private bool add_package_existing (string fname, AppItem app, string arch) {
		bool ret;
		string app_dir = Path.build_filename (repo_pool_dir, app.idname, null);
		AppItem appExisting = cindex.get_application (app.idname);

		if (appExisting == null)
			error ("FATAL: Application was assumed to be present in index, but it was not found!");

		string canonical_old_pkgname = build_canonical_pkgname (appExisting, arch);
		string canonical_pkgname = build_canonical_pkgname (app, arch);

		// delete existing package
		FileUtils.remove (canonical_old_pkgname);

		// copy current package to the repo tree
		ret = copy_file (fname, Path.build_filename (app_dir, canonical_pkgname, null));
		if (!ret)
			return false;

		// register package
		cindex.update_application (app);

		return ret;
	}

	public bool add_package (string fname) {
		bool ret = false;
		create_dir_structure (repo_pool_dir);

		var ipkp = new IPK.Package (fname);
		ret = ipkp.initialize ();
		if (!ret)
			return false;
		AppItem app = ipkp.control.get_application ();

		// we do the arch split to prevent invalid archs from being added (this is much more failsafe)
		string[] archs = ipkp.control.get_architectures ().split (",");
		if (archs.length > 1) {
			Report.log_error (_("You cannot add multiarch IPK packages to a repository!"));
			return false;
		}
		string arch = archs[0];

		// save current index, in case there is an unsaved one open
		// then open arch-specific index
		save_current_index ();
		open_index_for_arch (repo_root, arch);

		if (cindex.application_exists (app)) {
			int j = cindex.compare_version (app);
			if (j == 0) {
				Report.log_error (_("The package you want to add already exists in the repository (ID: %s, Version: %s).").printf (app.idname, app.version));
				return false;
			} else if (j < 0) {
				Report.log_error (_("A newer version of the package you want to add is already present in the repository."));
				return false;
			}

			add_package_existing (fname, app, arch);

		} else {
			ret = add_package_new (fname, app, arch);
		}

		return ret;
	}

	public ArrayList<AppItem> get_applist () {
		var appList = new ArrayList<AppItem> ();
		//! TODO
		return appList;
	}

}

} // End of namespace: Listaller.IPK
