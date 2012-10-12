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

	public bool add_package (string fname) {
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
