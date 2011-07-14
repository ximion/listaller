/* pkit-installer.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
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
	private PackageKit.Client pkit;

	public signal void message (MessageItem message);
	public signal void progress_changed (int progress);

	public ErrorItem? last_error { get; set; }

	public PkInstaller () {
		pkit = new PackageKit.Client ();
		last_error = null;
	}

	private void emit_warning (string msg) {
		// Construct warning message
		MessageItem item = new MessageItem(MessageEnum.WARNING);
		item.details = msg;
		message (item);
		li_warning (msg);
	}

	private void emit_info (string msg) {
		// Construct info message
		MessageItem item = new MessageItem(MessageEnum.INFO);
		item.details = msg;
		message (item);
		GLib.message (msg);
	}

	private void set_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem(id);
		item.details = details;
		last_error = item;
		debug ("PkInstaller: %s", details);
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		// TODO
	}

	private PackageKit.PackageSack? pkit_pkgs_from_depfiles (IPK.Dependency dep) {
		PackageKit.Bitfield filter = PackageKit.filter_bitfield_from_string ("none");

		string[] files = array_list_to_strv (dep.files);

		PackageKit.Results res;
		PackageKit.PackageSack sack;
		try {
			res  = pkit.search_files (filter, files, null, pk_progress_cb);
			sack = res.get_package_sack ();
		} catch (Error e) {
			debug (e.message);
			return null;
		}
		string[] packages = sack.get_ids ();

		if ( (res.get_exit_code () != PackageKit.Exit.SUCCESS) || (packages[0] == null) ) {
			set_error (0, "%s\n%s".printf (_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())),
						       _("Unable to find native package for '%s'!").printf (dep.name)));
			return null;
		}

		return sack;
	}

	private bool pkit_install_packages (string[] pkids) {
		PackageKit.Results res = pkit.install_packages (true, pkids, null, pk_progress_cb);

		if (res.get_exit_code () == PackageKit.Exit.SUCCESS)
			return true;

		/*emit_warning (_("Installation of native package '%s' failed!").printf (pkg.get_id ()) + "\n" +
				_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())));*/
		return false;
	}

	public void reset () {
		last_error = null;
		pkit = new PackageKit.Client ();
	}

	public bool install_dependency (ref IPK.Dependency dep) {
		bool ret = true;
		reset ();

		// If there are no files, consider this dependency as "installed"
		if (dep.files.size <= 0) {
			li_warning ("Dependency %s has no files assigned!".printf (dep.name));
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
				dep.meta_info.add ("pkg:" + pkg.get_id ());
			else
				dep.meta_info.add ("*pkg:" + pkg.get_id ());
		}
		if (!ret) {
			dep.meta_info.clear ();
			return false;
		}

		// Just to be sure...
		if (last_error != null)
			return false;

		/* This should never happen - if PK did not find a dependency, pkit_pkgs_from_depfiles ()
		 * returns null already */
		if (dep.meta_info.size <= 0)
			return false;

		string[] pkgs = {};
		/* Now install every not-yet-installed package. The asterisk (*pkg) indicates
		 * that this package needs to be installed */
		foreach (string pkg in dep.meta_info) {
			if (pkg.has_prefix ("*pkg:")) {
				pkgs += pkg.substring (4);
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
		emit_info (_("Installing native packages %s").printf (pkgs));
		ret = pkit_install_packages (pkgs);
		if (ret)
			dep.satisfied = true;

		return ret;
	}

}

} // End of namespace
