/* pkit-resolver.vala - Resolving universal dependencies to native distro packages
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Dep {

private errordomain PkError {
	TRANSACTION_FAILED;
}

private class PkResolver : MsgObject {
	private Listaller.Settings conf;
	private PackageKit.Client? pkclient;
	private PkBackendProxy? pkbproxy;

	public ErrorItem? last_error { get; set; }

	public PkResolver (Listaller.Settings liconf) {
		base ();
		set_error_hint_str ("PkResolver");
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
		reset ();
	}

	private new void emit_error (ErrorItem item) { }

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
		PackageKit.Bitfield filter = PackageKit.filter_bitfield_from_string ("arch;");

		// We only resolve libraries at time
		// TODO: Resolve other dependencies too
		string[] libs = {};
		foreach (string s in dep.raw_complist) {
			if (dep.component_get_type (s) == Dep.ComponentType.SHARED_LIB)
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
			set_error (ErrorEnum.UNKNOWN, "%s\n%s".printf (_("Unable to find native package for '%s'!").printf (dep.full_name),
				_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())))
			);
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
			li_error ("Dependency %s has no components assigned!".printf (dep.full_name));
			dep.satisfied = true;
			return true;
		}

		PackageKit.PackageSack? sack = pkit_pkgs_from_depfiles (dep);
		if (sack == null)
			return false;

		string[]? packages = sack.get_ids ();

		if (packages == null)
			ret = false;

		ret = false;
		for (uint i = 0; packages[i] != null; i++) {
			PackageKit.Package? pkg = sack.find_by_id (packages[i]);

			if (pkg == null) {
				ret = false;
				break;
			}
			// Skip packages which don't match the arch requirements
			/*var arch = pkg.get_arch ();
			debug ("Package architecture: %s", arch);
			if ((arch != "all") && (arch != Utils.system_machine ()))
				continue;
			*/

			debug ("Found native package: %s", pkg.get_id ());

			ret = true;
			if (pkg.get_info () == PackageKit.Info.INSTALLED)
				dep.add_installed_comp ("pkg:" + pkg.get_id ());
			else
				dep.add_installed_comp ("*pkg:" + pkg.get_id ());
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

	public string? package_name_for_file (string fname) throws PkError {
		PackageKit.Bitfield filter = PackageKit.filter_bitfield_from_string ("installed;");

		PackageKit.Results? res = null;
		PackageKit.PackageSack? sack;

		if (pkbproxy == null) {
			try {
				res  = pkclient.search_files (filter, {fname, null}, null, null);
			} catch (Error e) {
				debug (e.message);
				return null;
			}
		} else {
			debug ("::TODO");
			/*
			res = pkbproxy.run_search_files (filter, {fname, null});
			if (res == null) {
				debug ("Native backend PkResults was NULL!");
				return null;
			}
			*/
		}

		if (res == null)
			return null;

		sack = res.get_package_sack ();
		if (sack == null)
			return null;
		string[] packages = sack.get_ids ();

		if ( (res.get_exit_code () != PackageKit.Exit.SUCCESS)) {
			throw new PkError.TRANSACTION_FAILED (_("PackageKit transaction failed!\nExit message was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())));
		}

		return packages[0];
	}

}

} // End of namespace
