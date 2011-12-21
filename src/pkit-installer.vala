/* pkit-installer.vala - Install native packages using PackageKit
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

private class PkInstaller : Object {
	private Listaller.Settings conf;
	private PackageKit.Client pkclient;
	private PkBackendProxy? pkbproxy;

	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public ErrorItem? last_error { get; set; }

	public PkInstaller (Listaller.Settings liconf) {
		pkclient = new PackageKit.Client ();
		last_error = null;
		conf = liconf;

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
		debug ("PkInstaller: %s", details);
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		// TODO
	}

	private bool pkit_install_packages (string[] pkids) {
		PackageKit.Results? res = null;
		PackageKit.Error? pkerror = null;

		if (pkbproxy == null) {
			try {
				res = pkclient.install_packages (true, pkids, null, pk_progress_cb);
			} catch (Error e) {
				set_error (ErrorEnum.DEPENDENCY_INSTALL_FAILED,
					_("Installation of native packages failed with message: %s").printf (e.message));
				return false;
			}
		} else {
			// If we need to use the native backend plugin proxy
			res = pkbproxy.run_install_packages (true, pkids);
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

	public void reset () {
		last_error = null;
		if (pkbproxy == null)
			pkclient = new PackageKit.Client ();
	}

	/* This method install a dependency if necessary */
	public bool install_dependency (ref IPK.Dependency dep) {
		bool ret = true;
		// Just to be sure...
		if (last_error != null)
			return false;

		// return if dependency is already satified
		if (dep.satisfied)
			return true;

		/* Exit if we have no install-data: No package can be installed, dependency is not satisfied.
		 * (we might do a feed-install instead */
		if (!dep.has_installdata ())
			return false;


		string[] pkgs = {};
		/* Now install every not-yet-installed package. The asterisk (*pkg) indicates
		 * that this package needs to be installed */
		foreach (string pkg in dep.get_installdata ()) {
			if (pkg.has_prefix ("*pkg:")) {
				pkgs += pkg.substring (5);
			}
		}
		// null-terminate the array
		pkgs += null;

		/* If no elements need to be installed and everything is already there,
		 * the dependency is satisfied and we can leave. */
		if (pkgs[0] == null) {
			dep.satisfied = true;
			return true;
		}

		// Now do the installing
		emit_info (_("Installing native packages: %s").printf (strv_to_string (pkgs)));
		ret = pkit_install_packages (pkgs);
		if (ret) {
			dep.satisfied = true;
			return true;
		}

		return false;
	}

}

} // End of namespace
