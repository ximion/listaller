/* security.vala
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
using Listaller;

namespace Listaller {

public enum SecurityLevel {
	UNKNOWN,
	HIGH,
	MEDIUM,
	LOW,
	DANGEROUS;

	public string to_string() {
		switch (this) {
			case UNKNOWN:
				return _("Unknown.");

			case HIGH:
				return _("Should be safe.");

			case MEDIUM:
				return _("Medium security.");

			case LOW:
				return _("Low security.");

			case DANGEROUS:
				return _("This package could be dangerous!");

			default:
				return ("Security level is: [%d]").printf((int) this);
		}
	}
}

public enum SignStatus {
	UNKNOWN,
	VALID,
	KEY_EXPIRED,
	CERT_REVOKED,
	SIG_EXPIRED,
	BAD,
	NO_PUBKEY;


	public string to_string() {
		switch (this) {
			case UNKNOWN:
				return _("Status of this signature is unknown or package is not signed.");

			case VALID:
				return _("Signature is fully valid");

			case KEY_EXPIRED:
				return _("One key has expired");

			case CERT_REVOKED:
				return _("One key has been revoked");

			case SIG_EXPIRED:
				return _("Signature has expired");

			case BAD:
				return _("Signature is bad");

			case NO_PUBKEY:
				return _("Pubkey is missing");

			default:
				return ("Signature status is: [%d]").printf((int) this);
		}
	}
}

public enum SignTrust {
	UNKNOWN,
	BAD_VALUE,
	UNDEFINED,
	NEVER,
	MARGINAL,
	FULL,
	ULTIMATE;

	public string to_string() {
		switch (this) {
			case UNKNOWN:
				return _("Trust level is unknown");

			case UNDEFINED:
				return _("Validity is undefined");

			case NEVER:
				return _("Never trust.");

			case MARGINAL:
				return _("Trust is marginal");

			case FULL:
				return _("Trust is full");

			case ULTIMATE:
				return _("Trust is ultimate.");

			default:
				return ("Signature validity is: [%d]").printf((int) this);
		}
	}
}

} // End of namespace Listaller

namespace Listaller.IPK {

public class PackSecurity : Object {
	private weak Package pack;

	public SignStatus signature_status { get; set; }
	public SignTrust signature_trustlevel { get; set; }

	internal PackSecurity () {
	}

	public SecurityLevel get_level () {
		debug ("SigStatus: %s | SigValidity: %s", signature_status.to_string (), signature_trustlevel.to_string ());

		// No valid or no signature: We can't ensure anything, so flag this package as 'dangerous'
		if (signature_status != SignStatus.VALID)
			return SecurityLevel.DANGEROUS;

		// Marginal trust for medium trust level
		if (signature_trustlevel == SignTrust.MARGINAL)
			return SecurityLevel.MEDIUM;

		// Full & ultimate trust will allow a high trust level for this package
		if ((signature_trustlevel >= SignTrust.FULL) ||
		    (signature_trustlevel >= SignTrust.ULTIMATE))
			return SecurityLevel.HIGH;

		return SecurityLevel.LOW;
	}
}

} // End of namespace Listaller.IPK
