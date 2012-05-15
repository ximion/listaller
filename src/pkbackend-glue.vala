/* pkbackend-glue.vala
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

namespace Listaller {

/* This unit defines a class which acts as a proxy between the Listaller software manager
 * and a PackageKit native backend, forwarding all signals to the backend.
 * The proxy object is create by Listaller's PkPlugin, and should _only_ be set if Listaller
 * is doing a shared installation. (as root)
 */
private PkBackendProxy? pkit_backend_proxy;

private static bool packagekit_daemon_caller = false;

internal class PkBackendProxy : Object {
	public delegate unowned PackageKit.Results? WhatProvidesCB (PackageKit.Bitfield filters,
							   PackageKit.Provides provides,
							   [CCode (array_length = false, array_null_terminated = true)] string[] search);
	public delegate unowned PackageKit.Results? InstallPackagesCB (bool only_trusted,
								[CCode (array_length = false, array_null_terminated = true)] string[] packages);

	private WhatProvidesCB pk_whatprovides;
	private InstallPackagesCB pk_installpackages;

	public PkBackendProxy () {
		pk_whatprovides = null;
	}

	public void set_what_provides (WhatProvidesCB call) {
		pk_whatprovides = call;
	}

	public void set_install_packages (InstallPackagesCB call) {
		pk_installpackages = call;
	}

	public unowned PackageKit.Results? run_what_provides (PackageKit.Bitfield filters, PackageKit.Provides provides, [CCode (array_length = false, array_null_terminated = true)] string[] values) {
		if (pk_whatprovides == null)
			return null;
		return pk_whatprovides (filters, provides, values);
	}

	public unowned PackageKit.Results? run_install_packages (bool only_trusted, [CCode (array_length = false, array_null_terminated = true)] string[] packages) {
		if (pk_installpackages == null)
			return null;
		return pk_installpackages (only_trusted, packages);
	}

}

internal void set_backend_proxy (PkBackendProxy? pkbproxy) {
	if (!Utils.is_root ()) {
		error ("Tried to set a PackageKit native backend proxy, but application does not run as root (and therefore can not " +
			"have been called from packagekitd) This should NEVER happen, maybe someone is using the API wrong.");
		return;
	}
	pkit_backend_proxy = pkbproxy;
	packagekit_daemon_caller = true;
}

private PkBackendProxy? get_pk_backend () {
	if (!Utils.is_root ())
		return null;
	return pkit_backend_proxy;
}

#if 0
private void test_dummy () {
	var pkbp = new PkBackendProxy ();
	string test = "Hello!";
	int miniint = 42;
	pkbp.set_what_provides ( () => { debug ("%i", miniint); return null; } );
	PackageKit.Results? pkres = pkbp.run_what_provides (0, 0, null);
	pkres.get_package_sack ();
}
#endif

} // End of LI namespace
