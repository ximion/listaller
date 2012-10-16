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
 * [/]                                        - root
 *  | reposetting                             - basic repository settings (MetaFile format)
 *  | contents_amd64.xz                       - contents of this repository (MetaFile format), arch-specific
 *  | contents_ix32.xz
 *  | [pool]                                  - contains the package data
 *  |  \| [appid1]                            - directory named like project's application-id (using AppItem appid)
 *  |   | \| changelog                        - software changelog
 *  |   |  | appid1-1.4_amd64.ipk             - IPK package to install
 *  |   |  | appid1-1.4_ix32.ipk
 *  |   |  | (?)appid1.doap                   - optional: application DOAP data (RDF)
 *  |   |  | [delta]                          - directory with package deltas
 *  |   |  |  \|
 *  |   |  |    [amd64]                       - architecture dir
 *  |   |  |     \| appid-1.0-to-1.4.deltaipk - deltaIPK packages
 *  |   |  |      | ...
 *  |   |  |    [ix86]
 *  |   |  |    [all]                         - data for all architectures
 *  |   | [appid2]
 *  | [meta]                                  - directory for metadata (icons/AppStream info)
 *    ...
 */

/**
 * Basic stuff for Listaller software repos
 */
internal abstract class Repo : Object {
	protected Listaller.Repo.ContentIndex cindex;
	protected Listaller.Repo.Settings rsettings;

	private string current_cindex_fname;

	public Repo () {
		rsettings = new Listaller.Repo.Settings ();
		cindex = new Listaller.Repo.ContentIndex ();
		current_cindex_fname = "";
	}

	private ArrayList<AppItem> get_applist_from_dir (string dir) {
		var appList = new ArrayList<AppItem> ();
		//! TODO
		return appList;
	}

	protected bool open_index_for_arch (string dir, string arch_id) {
		bool ret = true;

		string cindex_fname = Path.build_filename (dir, "contents_%s.xz".printf (arch_id), null);
		if (FileUtils.test (cindex_fname, FileTest.EXISTS))
			ret = cindex.open (cindex_fname);
		current_cindex_fname = cindex_fname;

		return ret;
	}

	protected bool save_current_index () {
		if (current_cindex_fname == "")
			return true;

		return cindex.save (current_cindex_fname);
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
