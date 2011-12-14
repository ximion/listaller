/* pkit-resolver.vala - Resolving universal dependencies to native distro packages
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Deps {

private class PkResolver : Object {
	private Listaller.Settings conf;
	private PackageKit.Client? pkclient;
	private PkBackendProxy? pkbproxy;

	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public ErrorItem? last_error { get; set; }

	public PkResolver (Listaller.Settings liconf) {
		last_error = null;
		conf = liconf;

		pkbproxy = null;
		pkclient = null;
		if (is_root ()) {
			// Access to the native PackageKit backend
			pkbproxy = get_pk_backend ();
			if (pkbproxy == null) {
				// We don't have a PK backend! This must not happen, if we run as root.
				var msg = _("Could not obtain a PkBackendProxy instance. Maybe the Listaller-PkPlugin is not installed or broken?");
				set_error (ErrorEnum.UNKNOWN, msg);
				critical (msg);
			}
		}
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem (MessageEnum.WARNING);
		item.details = msg;
		message (item);
		li_warning (msg);
	}

	private void emit_info (string msg) {
		// Construct info message
		MessageItem item = new MessageItem (MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void set_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem (id);
		item.details = details;
		last_error = item;
		debug ("PkResolver: <error> %s", details);
	}

	private void pkit_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		// TODO
	}

	private PackageKit.PackageSack? pkit_pkgs_from_depfiles (IPK.Dependency dep) {
		PackageKit.Bitfield filter = PackageKit.filter_bitfield_from_string ("none");

		// We only resolve libraries at time
		// TODO: Resolve other dependencies too
		string[] libs = {};
		foreach (string s in dep.raw_complist) {
			debug (s);
			if (dep.component_get_type (s) == Deps.ComponentType.SHARED_LIB)
				libs += dep.component_get_name (s);
		}
		libs += null;

		PackageKit.Results? res;
		PackageKit.PackageSack? sack;

		if (pkbproxy == null) {
			try {
				res  = pkclient.what_provides (filter, PackageKit.Provides.SHARED_LIB, libs, null, null);
			} catch (Error e) {
				debug (e.message);
				return null;
			}
		} else {
			res = pkbproxy.run_what_provides (filter, PackageKit.Provides.SHARED_LIB, libs);
			if (res == null) {
				debug ("Native backend PkResults was NULL!");
				return null;
			}
		}

		sack = res.get_package_sack ();
		if (sack == null)
			return null;
		string[] packages = sack.get_ids ();

		if ( (res.get_exit_code () != PackageKit.Exit.SUCCESS) || (packages[0] == null) ) {
			set_error (ErrorEnum.UNKNOWN, "%s\n%s".printf (_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())),
						       _("Unable to find native package for '%s'!").printf (dep.full_name)));
			return null;
		}

		return sack;
	}

	private void reset () {
		last_error = null;
		if (pkbproxy == null)
			pkclient = new PackageKit.Client ();
	}

	/* This method searches for dependency packages & stores them in dep.install_data */
	public bool search_dep_packages (ref IPK.Dependency dep) {
		bool ret = true;
		reset ();

		// If there are no files, consider this dependency as "installed"
		// This is usually an ERROR and might indicate a broken package
		if (!dep.has_components ()) {
			li_warning ("Dependency %s has no components assigned!".printf (dep.full_name));
			dep.satisfied = true;
			return true;
		}

		/* We don't solve dependencies when unit tests are running.
		 * Consider everything as satisfied. */
		if (__unittestmode) {
			dep.satisfied = true;
			return true;
		}

		PackageKit.PackageSack? sack = pkit_pkgs_from_depfiles (dep);
		if (sack == null)
			return false;

		string[] packages = sack.get_ids ();

		for (uint i = 0; packages[i] != null; i++) {
			PackageKit.Package? pkg = sack.find_by_id (packages[i]);
			if (pkg == null) {
				ret = false;
				break;
			}

			if (pkg.get_info () == PackageKit.Info.INSTALLED)
				dep.add_install_comp ("pkg:" + pkg.get_id ());
			else
				dep.add_install_comp ("*pkg:" + pkg.get_id ());
		}
		if (!ret) {
			dep.clear_installdata ();
			return false;
		}
		/* Check if there are native packages which need to be installed.
		 * If not, the dependency is already satified. */
		dep.satisfied = true;
		foreach (string pkg in dep.get_installdata ()) {
			if (pkg.has_prefix ("*pkg:")) {
				dep.satisfied = false;
				break;
			}
		}

		return ret;
	}

}

} // End of namespace
