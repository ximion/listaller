/* software-cache.vala -- Access the Listaller software cache
 *
 * Copyright (C) 2012-2014 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Repo {

/**
 * Access Listaller's remote repository cache
 */
private class SoftwareCache : MessageObject {
	private RepoCacheDB cache_db;
	private bool writeable;

	private string dbname;
	private string dbname_tmp;

	private bool opened;

	public SoftwareCache (bool shared_mode, bool open_write = false) {
		var conf = new Config ();

		string mpath;
		if (shared_mode)
			mpath = conf.shared_repo_cache_dir ();
		else
			mpath = conf.user_repo_cache_dir ();

		if (__unittestmode) {
			mpath = Path.build_filename (conf.tmp_dir (), "repo-cache", null);
			Utils.create_dir_structure (mpath);
		}

		dbname = Path.build_filename (mpath, "available.db", null);
		dbname_tmp = Path.build_filename (mpath, "available_tmp.db", null);

		cache_db = new RepoCacheDB (dbname);
		writeable = open_write;

		opened = false;
		try {
			if (writeable)
				cache_db.open_rw ();
			else
				cache_db.open_r ();
			opened = true;

		} catch (Error e) {
			// only if we fail write-opening the cache this is a problem
			// (in other cases the cache might be unavailable/broken, which will automatically be fixed)
			if (!writeable)
				GLib.message ("Unable to open software cache! Message: %s", e.message);
			else
				critical ("Unable to open software cache! %s", e.message);
		}
	}

	public void update_application (AppItem app, string arch) {
		if (!opened)
			return;

		AppItem? tmpApp;

		tmpApp = cache_db.get_application_by_unique_name (app.unique_name);
		if (tmpApp != null) {
			cache_db.remove_application (tmpApp);
		}

		cache_db.add_application (app, arch);
	}

	public AppItem? get_application_by_unique_name (string app_idname) {
		return cache_db.get_application_by_unique_name (app_idname);
	}

	/**
	 * This method will open a new tmp database for re-writing the cache
	 */
	public void prepare_safe_refresh () {
		if (!writeable)
			critical ("Tried to refresh non-writeable software cache!");
		if (!opened)
			return;

		// clear existing db, which might be left from a failed cache update
		FileUtils.remove (dbname_tmp);

		cache_db = null;
		cache_db = new RepoCacheDB (dbname_tmp);
		cache_db.open_rw ();
	}

	/**
	 * Finishes a safe-update
	 */
	public void finish_safe_refresh () {
		if (!writeable)
			critical ("Tried to finish refresh of non-writeable software cache!");
		if (!opened)
			return;

		cache_db = null;
		if (FileUtils.test (dbname_tmp, FileTest.EXISTS)) {
			FileUtils.remove (dbname);
			FileUtils.rename (dbname_tmp, dbname);
		}

		cache_db = new RepoCacheDB (dbname);
		cache_db.open_rw ();
	}

	/**
	 * Aborts a safe-refresh action without damaging the original database
	 */
	public void abort_safe_refresh () {
		if (!writeable)
			critical ("Tried to abort refresh of non-writeable software cache!");
		if (!opened)
			return;

		cache_db = null;
		FileUtils.remove (dbname_tmp);

		cache_db = new RepoCacheDB (dbname);
		cache_db.open_rw ();
	}

	public HashMap<string, AppItem>? get_applications_available () {
		if (!opened)
			return null;

		ArrayList<AppItem> apps_available_list = cache_db.get_applications_all ();
		if (apps_available_list == null)
			return null;

		// build hash-map of available applications (large speed improvements when used later)
		var apps_available = new HashMap<string, AppItem> ();
		foreach (AppItem app in apps_available_list) {
			apps_available.set (app.unique_name, app);
		}

		return apps_available;
	}

	/**
	 * Return the architecture of the application with the given idname
	 */
	public string get_arch_for_app (string app_idname) {
		return cache_db.get_arch_for_app (app_idname);
	}
}

} // End of namespace: Listaller.Repo
