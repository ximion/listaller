/* repo-manager.vala -- Manage IPK package repositories
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

namespace Listaller.Repo {

private class Manager : MessageObject {
	private Repo.ListFile repo_list;
	private IPK.RepoRemote[] repos;
	private Repo.SoftwareCache cache;

	private string cache_fname;

	public Manager () {
		repo_list = new Repo.ListFile ();

		var conf = new Config ();
		string repolist_fname = Path.build_filename (conf.conf_dir (), "repositories.list", null);
		repo_list.open_file (repolist_fname);
		repos = {};

		string[] repo_urls = repo_list.get_repo_urls ();
		foreach (string url in repo_urls) {
			repos += new IPK.RepoRemote (url);
		}

		cache = new Repo.SoftwareCache (true);
	}

	public bool refresh_cache () {
		// we write on a temporary db, in case something goes wrong
		cache.prepare_safe_refresh ();

		foreach (IPK.RepoRemote repo in repos) {
			repo.load_server_index ();
			ArrayList<AppItem> appList;

			// arch-dependent apps
			appList = repo.get_available_applications (false);
			foreach (AppItem app in appList) {
				cache.update_application (app, Utils.system_machine_generic ());
			}
			// arch-independent apps
			appList = repo.get_available_applications (true);
			foreach (AppItem app in appList) {
				cache.update_application (app, "all");
			}
		}

		// load the new cache
		cache.finish_safe_refresh ();

		//! TODO: Do proper error-handling in the code above!
		return true;
	}

	public ArrayList<AppItem> get_applications (string search = "") {
		ArrayList<AppItem> res;

		//! TODO: implement application filter!
		res = cache.get_applications_available ();

		return res;
	}

	public Setup? get_setup_for_remote_app (string app_idname) {
		string? fname = null;
		AppItem? app = cache.get_application_by_idname (app_idname);
		if (app == null) {
			debug ("Application with id '%s' not found in cache!", app_idname);
			return null;
		}
		string? arch = cache.get_arch_for_app (app.idname);
		if (arch == null) {
			critical ("Database inconsistent: Found application without valid arch in cache!");
			return null;
		}
		string url = app.origin;
		if (!url.has_prefix ("http://") && !url.has_prefix ("ftp://")) {
			warning ("Found application, but origin does not match repo origin.");
			return null;
		}

		var repo = new IPK.RepoRemote (url);
		fname = repo.download_release_package_noindex (app, arch);
		if (fname != null) {
			var setup = new Setup (fname);
			return setup;
		}

		// No repo provides submitted app => no setup object
		return null;
	}
}

} // End of namespace: Listaller.Repo
