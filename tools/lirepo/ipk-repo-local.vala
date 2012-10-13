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
	private Listaller.Repo.Settings rsettings;

	public RepoLocal (string dir) {
		repo_root = dir;

		rsettings = new Listaller.Repo.Settings ();
		rsettings.open (Path.build_filename (repo_root, "reposetting", null));
	}

	~RepoLocal () {
		rsettings.save (Path.build_filename (repo_root, "reposetting", null));
	}

	private bool add_package_new (string fname) {
		bool ret;
		var ipkp = new IPK.Package (fname);
		ret = ipkp.initialize ();
		if (!ret)
			return false;

		AppItem app = ipkp.control.get_application ();

		string app_dir = Path.build_filename (repo_root, app.idname, null);

		ret = create_dir_parents (app_dir);
		if (!ret) {
			Report.log_error ("Unable to create app-id directory.");
			return false;
		}

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

		// copy current package to the repo tree
		ret = copy_file (fname, Path.build_filename (app_dir, canonical_pkgname, null));

		// register package
		//! TODO

		return false;
	}

	public bool add_package (string fname) {
		add_package_new (fname);
		//! TODO
		return false;
	}

	public ArrayList<AppItem> get_applist () {
		var appList = new ArrayList<AppItem> ();
		//! TODO
		return appList;
	}

}

} // End of namespace: Listaller.IPK
