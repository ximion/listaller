/* ipk-repo-local.vala -- Main file for Listaller repository tool
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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
		base ();
		repo_root = dir;
		repo_pool_dir = Path.build_filename (dir, "pool", null);

		rsettings.open (Path.build_filename (repo_root, "reposetting", null));
		string cindex_fname = Path.build_filename (repo_root, "contents.xz", null);
		if (FileUtils.test (cindex_fname, FileTest.EXISTS))
			cindex.open (cindex_fname);
	}

	~RepoLocal () {
		rsettings.save (Path.build_filename (repo_root, "reposetting", null));
		cindex.save (Path.build_filename (repo_root, "contents.xz", null));
	}

	private string build_canonical_pkgname (IPK.Package ipkp, AppItem app) {
		// we do the arch split to prevent invalid archs from being added (this is much more failsafe)
		string[] archs = ipkp.control.get_architectures ().split ("\n");
		string archs_str = "";
		foreach (string s in archs) {
			if (s == null)
				continue;
			s = s.strip ();
			if (s != "")
				if (archs_str != "")
					archs_str = "%s+%s".printf (archs_str, s);
				else
					archs_str = s;
		}
		string canonical_pkgname = "%s-%s_%s.ipk".printf (app.idname, app.version, archs_str);

		return canonical_pkgname;
	}

	private bool add_package_new (string fname, IPK.Package ipkp, AppItem app) {
		bool ret;
		string app_dir = Path.build_filename (repo_pool_dir, app.idname, null);

		ret = create_dir_parents (app_dir);
		if (!ret) {
			Report.log_error ("Unable to create app-id directory.");
			return false;
		}


		string canonical_pkgname = build_canonical_pkgname (ipkp, app);
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
		create_dir_parents (repo_pool_dir);

		var ipkp = new IPK.Package (fname);
		ret = ipkp.initialize ();
		if (!ret)
			return false;
		AppItem app = ipkp.control.get_application ();

		if (cindex.application_exists (app)) {
			int j = cindex.compare_version (app);
			if (j == 0) {
				Report.log_error (_("The package you want to add already exists in the repository."));
				return false;
			} else if (j < 0) {
				Report.log_error (_("A newer version of the package you want to add is already present in the repository."));
				return false;
			}

			warning ("TODO: Implement this!");
		} else {
			ret = add_package_new (fname, ipkp, app);
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
