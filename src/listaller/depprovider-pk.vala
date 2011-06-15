/* depprovider-pk.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under the GNU Lesser General Public License Version 3+
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

namespace Listaller.Deps {

private class PkitProvider : Provider {
	private PackageKit.Client pkit;

	public PkitProvider (ArrayList<IPK.Dependency> dep_lst) {
		base (dep_lst);

		pkit = new PackageKit.Client ();
	}

	private void pk_progress_cb (PackageKit.Progress progress, PackageKit.ProgressType type) {
		// TODO
	}

	private PackageKit.Package? pkit_pkg_from_file (string fname, IPK.Dependency dep) {
		PackageKit.Bitfield filter = PackageKit.filter_bitfield_from_string ("none");
		string[] files = { fname, null };

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
			debug (_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())));
			emit_warning (_("Unable to find native package for %s!").printf (dep.name));
			return null;
		}

		PackageKit.Package pkg = sack.find_by_id (packages[0]);

		return pkg;
	}

	private bool pkit_install_package (PackageKit.Package pkg) {
		string[] pkids = { pkg.get_id (), null };
		PackageKit.Results res = pkit.install_packages (true, pkids, null, pk_progress_cb);

		if (res.get_exit_code () == PackageKit.Exit.SUCCESS)
			return true;

		emit_warning (_("Installation of native package '%s' failed!").printf (pkg.get_id ()) + "\n" +
				_("PackageKit exit code was: %s").printf (PackageKit.exit_enum_to_string (res.get_exit_code ())));
		return false;
	}

	public override bool execute () {
		bool ret = true;
		foreach (IPK.Dependency dep in dependency_list) {
			// PK solver can only handle files...
			if (dep.files.size > 0)
				foreach (string s in dep.files) {
					PackageKit.Package pkg = pkit_pkg_from_file (s, dep);
					if (pkg == null) {
						ret = false;
						break;
					}
					if (pkg.get_info () != PackageKit.Info.INSTALLED) {
						emit_info (_("Installing native package %s").printf (pkg.get_id ()));
						ret = pkit_install_package (pkg);
						if (!ret)
							break;
					} else {
						emit_info (_("Native package %s is already installed.").printf (pkg.get_id ()));
					}
					if (ret)
						dep.meta_info.add ("pkg:" + pkg.get_id ());
				}
				if (!ret)
					dep.meta_info.clear ();
		}

		return ret;
	}

}

} // End of namespace
