/* pkit-tasks.vala - Perform tasks on native package-db using PackageKit
 *
 * Copyright (C) 2011-2014 Matthias Klumpp <matthias@tenstral.net>
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

/**
 * Abstract class which defines basic things needed by Listaller
 * to access bother PackageKit via DBus when running as user and
 * PackageKit internal API when running as root (and as plugin)
 */
private abstract class PkListallerTask : MessageObject {
	public ErrorItem? last_error { get; set; }

	protected SetupSettings ssettings;
	protected PackageKit.Task? pktask;
	protected PkBackendProxy? pkbproxy;

	private PackageKit.Bitfield supported_roles;

	private new void emit_error (ErrorItem item) { }

	public PkListallerTask (SetupSettings setup_settings) {
		base ();
		ssettings = setup_settings;

		var pkcontrol = new PackageKit.Control ();
		pkcontrol.get_properties (null);
		supported_roles = pkcontrol.roles;

		pkbproxy = null;
		pktask = null;
		if (packagekit_daemon_caller) {
			// Access to the native PackageKit backend
			pkbproxy = get_pk_backend ();
			if (pkbproxy == null) {
				// We don't have a PK backend! This must not happen, if we run as root.
				var msg = _("Could not obtain a PkBackendProxy instance. Maybe the Listaller-PkPlugin is not installed or broken?");
				set_error (ErrorEnum.INTERNAL, msg);
				critical (msg);
			}
		}
	}

	protected void reset () {
		last_error = null;
		if (pkbproxy == null)
			pktask = new PackageKit.Task ();
	}

	protected void set_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem (id);
		item.details = details;
		last_error = item;
		debug ("PkResolver: <error> %s", details);
	}

	protected bool supported (PackageKit.Role role) {
		return (supported_roles & (1 << role)) > 0;
	}
}

private class PkResolver : PkListallerTask {

	public PkResolver (SetupSettings setup_settings) {
		base (setup_settings);
		set_error_hint_str ("PkResolver");
		reset ();
	}

	private void pkit_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		// TODO
	}

	/**
	 * Get package-name (PK package-id) for filename
	 *
	 * @param fname The filename to search for
	 *
	 * @return Resolved package-id or NULL, if none was found
	 */
	public string? find_package_name_for_file (string fname) throws PkError {
		PackageKit.Bitfield filter = PackageKit.Filter.bitfield_from_string ("installed;");

		PackageKit.Results? res = null;
		PackageKit.PackageSack? sack;

		// we can't do anything if PK doesn't support SearchFiles
		if (!supported (PackageKit.Role.SEARCH_FILE)) {
			warning (_("PackageKit backend does not support searching for files - Installer was unable to use native packages to satisfy dependencies of this application."));
			return null;
		}

		if (pkbproxy == null) {
			try {
				res  = pktask.search_files (filter, {fname, null}, null, null);
			} catch (Error e) {
				debug (e.message);
				return null;
			}
		} else {
			res = pkbproxy.run_search_files (filter, {fname, null});
			if (res == null) {
				debug ("Native backend PkResults was NULL!");
				return null;
			}
		}

		if (res == null)
			return null;

		sack = res.get_package_sack ();
		if (sack == null)
			return null;
		string[] packages = sack.get_ids ();

		if ( (res.get_exit_code () != PackageKit.Exit.SUCCESS)) {
			throw new PkError.TRANSACTION_FAILED (_("PackageKit transaction failed!\nExit message was: %s").printf (PackageKit.Exit.enum_to_string (res.get_exit_code ())));
		}

		return packages[0];
	}

	/**
	 * Resolve a package-name
	 *
	 * @param fname The filename to search for
	 *
	 * @return Resolved package-id or NULL, if none was found
	 */
	public string? resolve (string pkg_name) throws PkError {
		PackageKit.Results? res = null;
		PackageKit.PackageSack? sack;

		// we can't do anything if PK doesn't support SearchFiles
		if (!supported (PackageKit.Role.RESOLVE)) {
			warning (_("PackageKit backend does not support resolving packages - Installer was unable to use native packages to satisfy dependencies of this application."));
			return null;
		}

		PackageKit.Bitfield filter = PackageKit.Filter.bitfield_from_string ("");
		if (pkbproxy == null) {
			try {
				res  = pktask.resolve (filter, {pkg_name, null}, null, null);
			} catch (Error e) {
				debug (e.message);
				return null;
			}
		} else {
			res = pkbproxy.run_resolve_packages (filter, {pkg_name, null});
			if (res == null) {
				debug ("Native backend PkResults was NULL!");
				return null;
			}
		}

		if (res == null)
			return null;

		sack = res.get_package_sack ();
		if (sack == null)
			return null;
		string[] packages = sack.get_ids ();

		if ( (res.get_exit_code () != PackageKit.Exit.SUCCESS)) {
			throw new PkError.TRANSACTION_FAILED (_("PackageKit transaction failed!\nExit message was: %s").printf (PackageKit.Exit.enum_to_string (res.get_exit_code ())));
		}

		return packages[0];
	}

}

private class PkInstaller : PkListallerTask {

	public PkInstaller (SetupSettings setup_settings) {
		base (setup_settings);
		set_error_hint_str ("PkInstaller");

		reset ();
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		// TODO
	}

	private bool pkit_install_packages (string[] pkids) {
		PackageKit.Results? res = null;
		PackageKit.Error? pkerror = null;

		if (pkbproxy == null) {
			try {
				res = pktask.install_packages (PackageKit.TransactionFlag.NONE,
								pkids,
								null,
								pk_progress_cb);
			} catch (Error e) {
				set_error (ErrorEnum.DEPENDENCY_INSTALL_FAILED,
					_("Installation of native packages failed with message: %s").printf (e.message));
				return false;
			}
		} else {
			// If we need to use the native backend plugin proxy
			res = pkbproxy.run_install_packages (PackageKit.TransactionFlag.NONE, pkids);
			if (res == null) {
				debug ("Native backend PkResults was NULL!");
				return false;
			}
		}
		if (res == null)
			return false;

		pkerror = res.get_error_code ();
		if (pkerror != null) {
			set_error (ErrorEnum.DEPENDENCY_INSTALL_FAILED,
					_("Installation of native packages failed with message: %s").printf (pkerror.get_details ()));
			return false;
		}

		if ((res != null) && (res.get_exit_code () == PackageKit.Exit.SUCCESS))
			return true;

		warning ("An unknown error occurred while trying to install a native package!");
		/*emit_warning (_("Installation of native package '%s' failed!").printf (pkg.get_id ()) + "\n" +
				_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())));*/

		return false;
	}

	/* This method install a dependency if necessary */
	public bool install_dependency (Dependency dep) {
		bool ret = true;
		// Just to be sure...
		if (last_error != null)
			return false;

		// return if dependency is already satified
		if (dep.installed)
			return true;

		/* If no packages are given, the native installer is
		 * useless and we can leave. */
		if (dep.metainfo.pkgnames.length <= 0) {
			return false;
		} else if (__unittestmode) {
			/* We don't install dependencies via PK when unit tests are running.
			 * Consider everything as satisfied, as long as we know a package name.
			 * (unittests can modify this, of course) */
			debug ("Faking dependency '%s' to be installed.", dep.unique_name);
			dep.installed = true;
			return true;
		}

		// Now do the installing
		emit_message (_("Installing native packages: %s").printf (strv_to_string (dep.metainfo.pkgnames)));
		ret = pkit_install_packages (dep.metainfo.pkgnames);
		if (ret) {
			dep.installed = true;
			return true;
		}

		return false;
	}

}

} // End of namespace
