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

/* This stuff is defined to be able to use the PackageKit PkBackend directly in Listaller.
 * It allows us to call the native backend instead of invoking a new PackageKit native transaction,
 * which can never work if there already is a PackageKit Listaller transaction. (it would come to
 * a deadlock then)
 * The PkBackend reference is set by Listaller's PK plugin.and should _only_ be set by it. It can only
 * be used if the user is root (because PK is a root-daemon, if PkBackend is set and we're a unprivileged
 * user, something is going wrong or someone did a foolish thing.
 */
private PkPlugin.Backend pkit_native_backend;

internal void set_pkit_backend (PkPlugin.Backend backend) {
	if (!Utils.is_root ()) {
		error ("Tried to set a PackageKit native backend, but application does not run as root (and therefore can not " +
			"have been called from packagekitd) This should NEVER happen, maybe someone is using the API wrong.");
		return;
	}
	pkit_native_backend = backend;
}

private PkPlugin.Backend? get_pk_backend () {
	if (!Utils.is_root ())
		return null;
	return pkit_native_backend;
}

} // End of LI namespace
