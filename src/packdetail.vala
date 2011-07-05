/* packdetail.vala
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
using Listaller;
using Listaller.Utils;

namespace Listaller.IPK {

public class PackSecurity : Object {
	private weak Package pack;

	public SignStatus signature_status { get; set; }
	public SignValidity signature_validity { get; set; }

	internal PackSecurity () {
	}

	public SecurityLevel get_level () {
		if ((signature_status & SignStatus.VALID) == 0)
			return SecurityLevel.DANGEROUS;
		if (signature_validity == SignValidity.MARGINAL)
			return SecurityLevel.MEDIUM;
		if (signature_validity >= SignValidity.FULL)
			return SecurityLevel.HIGH;
		return SecurityLevel.LOW;
	}
}

public class PackDetails : Object {
	private weak Package pack;

	internal PackDetails (Package ipk) {
		pack = ipk;
	}
}

} // End of namespace
