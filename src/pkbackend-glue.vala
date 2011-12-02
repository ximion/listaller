/* pkbackend-glue.vala
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

namespace Listaller {

/* This unit defines a class which acts as a proxy between the Listaller software manager
 * and a PackageKit native backend, forwarding all signals to the backend.
 * The proxy object is create by Listaller's PkPlugin, and should _only_ be set if Listaller
 * is doing a shared installation. (as root)
 */
private PkBackendProxy? pkit_backend_proxy;

internal class PkBackendProxy : Object {
	// Workaround for strance plugin behavior
	public void* plugin { get; set; }

	// Used by the PkPlugin
	public signal void error_message ();
	public signal void packages ();

	// Used by Listaller
	public signal PackageKit.Results? request_whatprovides (uint filters, uint provides, [CCode (array_length = false, array_null_terminated = true)] string[] values);

	internal PkBackendProxy () {

	}

	public PackageKit.Results? run_what_provides (PackageKit.Bitfield filters, PackageKit.Provides provides, [CCode (array_length = false, array_null_terminated = true)] string[] values) {
		return request_whatprovides ((uint) filters, (uint) provides, values);
	}

}

internal void set_backend_proxy (PkBackendProxy? pkbproxy) {
	if (!Utils.is_root ()) {
		error ("Tried to set a PackageKit native backend proxy, but application does not run as root (and therefore can not " +
			"have been called from packagekitd) This should NEVER happen, maybe someone is using the API wrong.");
		return;
	}
	pkit_backend_proxy = pkbproxy;
}

private PkBackendProxy? get_pk_backend () {
	if (!Utils.is_root ())
		return null;
	return pkit_backend_proxy;
}

private void test_dummy () {
	var pkbp = new PkBackendProxy ();
	string simple_text = "Hello World!";
	pkbp.request_whatprovides.connect ( () => { debug (simple_text); return new PackageKit.Results (); } );
	PackageKit.Results? pkres = pkbp.request_whatprovides (0, 0, null);
	pkres.get_package_sack ();
}


} // End of LI namespace
