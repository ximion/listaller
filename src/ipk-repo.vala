/* ipk-repo.vala - Definition of an (remote) IPK application repository
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.IPK {

/**
 * A Listaller repo has the following structure:
 * [/]                              - root
 *  reposetting                     - basic repository settings (IPK.MetaFile format)
 *  contents.tar.xz                 - DOAP data collected from all projects in the archive
 *  [appid1]                        - directory named after using AppItem appid
 *    apppackage-1.4_amd64+i386.ipk - optional: the multi-arch IPK package
 *    [amd64]                       - architecture dir
 *      apppackage-1.4_amd64.ipk    - IPK package to install
 *      [delta]                     - directory with package deltas
 *        appid-1.0-to-1.4.deltaipk - deltaIPK packages
 *        ...
 *    [i386]
 *    [all]                         - data for all architectures
 *  [appid2]
 *    ...
 */

/**
 * Basic stuff for Listaller software repos
 */
internal class Repo : Object {

	public Repo () {
	}

	private ArrayList<AppItem> get_applist_from_dir (string dir) {
		var appList = new ArrayList<AppItem> ();
		//! TODO
		return appList;
	}

}

/**
 * A remote package repository
 */
internal class RepoRemote : Object {

	public RepoRemote (string url) {

	}
}


} // End of namespace: Listaller.IPK
