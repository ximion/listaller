/* repo-manager.vala -- Manage IPK package repositories
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

namespace Listaller.Repo {

private class Manager : MessageObject {
	private Repo.ListFile repo_list;
	private IPK.RepoRemote[] repos;
	private Repo.ContentCache cache;

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

		cache_fname = Path.build_filename (conf.shared_repo_cache_dir (), "application-cache.list", null);
		cache = new Repo.ContentCache ();
		cache.open_file (cache_fname);
	}

	public bool refresh_cache () {
		var tmpCache = new Repo.ContentCache ();

		foreach (IPK.RepoRemote repo in repos) {
			repo.load_server_index ();
			ArrayList<AppItem> appList;

			// arch-dependent apps
			appList = repo.get_available_applications (false);
			foreach (AppItem app in appList) {
				tmpCache.update_application (app, Utils.system_machine_generic (), repo.url);
			}
			// arch-independent apps
			appList = repo.get_available_applications (true);
			foreach (AppItem app in appList) {
				tmpCache.update_application (app, "all", repo.url);
			}
		}

		cache = tmpCache;
		cache.save_to_file (cache_fname, true);

		//! TODO: Do proper error-handling in the code above!
		return true;
	}
}

} // End of namespace: Listaller.Repo
