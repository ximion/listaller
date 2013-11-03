/* ipk-repo.vala - Definition of an (remote) IPK application repository
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

namespace Listaller.IPK {

/**
 * A Listaller repo has the following structure:
 * /                                                 - root
 * |-- reposetting                                   - basic repository settings (MetaFile format)
 * |-- contents_amd64.xz                             - contents of this repository (MetaFile format), arch-specific
 * |-- contents_ia32.xz
 * |-- [apps]
 * |   |-- [pool]                                    - contains the package data
 * |   |   |-- [appid1]                              - directory named like project's application-id (using AppItem appid)
 * |   |   |   |-- changelog                         - software changelog
 * |   |   |   |-- appid1-1.4_amd64.ipk              - IPK package to install
 * |   |   |   |-- appid1-1.4_ia32.ipk
 * |   |   |   |-- (?)appid1.appdata.xml             - optional: application AppStream data (RDF)
 * |   |   |   |-- [delta]                           - directory with package deltas
 * |   |   |   |   |-- [amd64]                       - architecture dir
 * |   |   |   |   |   |-- appid-1.0-to-1.4.deltaipk - deltaIPK packages
 * |   |   |   |   |   |-- ...
 * |   |   |   |   |-- [ia32]
 * |   |   |   |   |-- [all]                         - data for all architectures
 * |   |   |-- [appid2]
 * |   |-- [meta]                                    - directory for metadata (icons/AppStream info)
 * |-- [deps]
 * |   |-- [feeds]                                   - contains the dependency feed files
 * |   |-- [data]                                    - contains dependency-data (if necessary)
 * |   |   |-- [ia32]
 * |   |   |-- [amd64]
 * |   |   |-- [all]
 */

/**
 * Basic stuff for Listaller software repos
 */
internal abstract class Repo : MessageObject {
	protected Listaller.Repo.ContentIndex cindex;
	protected Listaller.Repo.Settings rsettings;

	private string current_cindex_fname;

	public Repo (string origin) {
		rsettings = new Listaller.Repo.Settings ();
		cindex = new Listaller.Repo.ContentIndex (origin);
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
			ret = cindex.add_file_compressed (cindex_fname);
		current_cindex_fname = cindex_fname;

		return ret;
	}

	protected bool save_current_index () {
		if (current_cindex_fname == "")
			return true;

		return cindex.save_compressed (current_cindex_fname);
	}

	protected string build_canonical_pkgname (AppItem app, string arch) {
		string canonical_pkgname = "%s-%s_%s.ipk".printf (app.idname, app.version, arch);

		return canonical_pkgname;
	}
}

/**
 * A remote package repository
 */
internal class RepoRemote : Repo {
	private string repo_url;

	private string tmpdir;
	private string current_arch;

	// architecture-independent index
	private Listaller.Repo.ContentIndex cindex_indep;

	public string url {
		get {
			return repo_url;
		}
		set {
			repo_url = value;
		}
	}

	public RepoRemote (string url) {
		base (url);
		repo_url = url;

		current_arch = system_machine_generic ();
		var conf = new Config ();
		tmpdir = conf.get_unique_tmp_dir ("repo");
		cindex_indep = new Listaller.Repo.ContentIndex (url);
	}

	public void load_server_index () {
		string url;
		string fname;
		Error error = null;

		fname = Path.build_filename (tmpdir, "contents.xz", null);
		url = Path.build_filename (repo_url, "contents_%s.xz".printf (current_arch), null);

		var dl = new Downloader ();

		try {
			dl.download_file_sync (url, fname, true);
		} catch (Error e) {
			error = e;
		}
		if (error == null) {
			cindex.add_file_compressed (fname);
			FileUtils.remove (fname);
		}

		url = Path.build_filename (repo_url, "contents_all.xz", null);
		try {
			dl.download_file_sync (url, fname, true);
		} catch (Error e) {
			if (error != null) {
				string err_msg = e.message;
				if (err_msg != error.message)
					err_msg = "%s\n%s".printf (err_msg, error.message);

				emit_error (ErrorEnum.NETWORK_ERROR,
					    _("Unable to fetch repository contents for: '%s'\nError: %s").printf (repo_url, err_msg));

				return;
			}
			error = e;
		}
		if (error == null) {
			cindex_indep.add_file_compressed (fname);
			FileUtils.remove (fname);
		}
	}

	public string? download_release_package_noindex (AppItem app, string arch) {
		string pkg_name = build_canonical_pkgname (app, arch);


		string url = Path.build_filename (repo_url, "apps", "pool", app.idname, pkg_name, null);
		string fname = Path.build_filename (tmpdir, pkg_name, null);

		var dl = new Downloader ();
		dl.progress.connect ((step) => {
			this.change_item_progress (app.build_pk_package_id (), step);
		});

		try {
			dl.download_file_sync (url, fname);
		} catch (Error e) {
			emit_error (ErrorEnum.NETWORK_ERROR,
					    _("Unable to download package for '%s'\nError: %s").printf (app.full_name, e.message));

			return null;
		}

		return fname;
	}

	public string? download_release_package (AppItem app) {
		string arch;
		// try to find application
		if (cindex.application_exists (app)) {
			arch = current_arch;
		} else if (cindex_indep.application_exists (app)) {
			arch = "all";
		} else {
			return null;
		}

		return download_release_package_noindex (app, arch);
	}

	public ArrayList<AppItem> get_available_applications (bool arch_indep) {
		var appList = new ArrayList<AppItem> ();

		if (arch_indep)
			appList.add_all (cindex_indep.get_application_list ());
		else
			appList.add_all (cindex.get_application_list ());

		return appList;
	}
}


} // End of namespace: Listaller.IPK
