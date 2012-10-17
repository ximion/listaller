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
internal abstract class Repo : MessageObject {
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
			ret = cindex.add_file (cindex_fname);
		current_cindex_fname = cindex_fname;

		return ret;
	}

	protected bool save_current_index () {
		if (current_cindex_fname == "")
			return true;

		return cindex.save (current_cindex_fname);
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

	public RepoRemote (string url) {
		repo_url = url;

		current_arch = system_machine_generic ();
		var conf = new Config ();
		tmpdir = conf.get_unique_tmp_dir ("repo");
		cindex_indep = new Listaller.Repo.ContentIndex ();
	}

	private async void do_download (File remote, File local, FileProgressCallback? on_progress) throws Error {
		try {
			try {
				yield remote.find_enclosing_mount_async (0);
			} catch (IOError e_mount) {
				// Not mounted...
			}

			int64 size = 0;
			try {
				FileInfo info = yield remote.query_info_async (FileAttribute.STANDARD_SIZE,
					FileQueryInfoFlags.NONE, 0);
				size = (int64) info.get_attribute_uint64 (FileAttribute.STANDARD_SIZE);
			} catch (IOError e_query) {
				debug ("Cannot query file size, continuing with an unknown size.");
			}

			FileInputStream input = yield remote.read_async ();
			FileOutputStream output;
			int64 downloaded_size = 0;

			if (input.can_seek () && local.query_exists ()) {
				output = yield local.append_to_async (FileCreateFlags.NONE, 0);
				output.seek (0, SeekType.END);
				downloaded_size = output.tell ();
				input.seek (downloaded_size, SeekType.SET);
			} else {
				output = yield local.replace_async (null, false, FileCreateFlags.NONE, 0);
			}

			uint8[] buf = new uint8[4096];

			ssize_t read = yield input.read_async (buf);
			while (read != 0) {
				yield output.write_async (buf[0:read]);
				if (on_progress != null)
					on_progress (downloaded_size + read, size);
				read = yield input.read_async (buf);
			}

		} catch (Error e) {
			throw e;
		}
	}

	private bool download_file_sync (string remote_url, string local_name) throws Error {
		File local_file = File.new_for_path (local_name);
		File remote_file = File.new_for_uri (remote_url);

		MainLoop main_loop = new MainLoop ();
		Error error = null;
		do_download (remote_file, local_file, null, (obj, res) => {
			try {
				do_download.end (res);
			} catch (Error e) {
				error = e;
			}
			main_loop.quit();
		});

		main_loop.run ();

		if (error == null)
			return true;
		else
			throw error;

		return false;
	}

	public void load_server_index () {
		string url;
		string fname;
		Error error = null;

		fname = Path.build_filename (tmpdir, "contents.xz", null);
		url = Path.build_filename (repo_url, "contents_%s.xz".printf (current_arch), null);
		try {
			download_file_sync (url, fname);
		} catch (Error e) {
			error = e;
		}
		if (error == null) {
			cindex.add_file (fname);
			FileUtils.remove (fname);
		}

		url = Path.build_filename (repo_url, "contents_all.xz", null);
		try {
			download_file_sync (url, fname);
		} catch (Error e) {
			if (error != null) {
				warning ("Unable to fetch repository contents for: '%s'", repo_url);

				//! TODO: Emit Listaller error type!

				return;
			}
			error = e;
		}
		if (error == null) {
			cindex_indep.add_file (fname);
			FileUtils.remove (fname);
		}
	}

	public string? download_release_package_noindex (AppItem app, string arch) {
		string pkg_name = build_canonical_pkgname (app, arch);


		string url = Path.build_filename (repo_url, "pool", app.idname, pkg_name, null);
		string fname = Path.build_filename (tmpdir, pkg_name, null);

		try {
			download_file_sync (url, fname);
		} catch (Error e) {
			warning (e.message);
			//! TODO: Emit Listaller error!
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
			//! TODO: Emit Listaller error!

			return null;
		}

		return download_release_package_noindex (app, arch);
	}

	public ArrayList<AppItem> get_available_applications (bool arch_indep) {
		return null;
	}
}


} // End of namespace: Listaller.IPK
