/* security-info.vala
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
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

/**
 * Simple indicator of package security
 */
public enum SecurityLevel {
	DANGEROUS,
	LOW,
	MEDIUM,
	HIGH;

	public string to_string() {
		switch (this) {
			case HIGH:
				return _("high");

			case MEDIUM:
				return _("medium");

			case LOW:
				return _("low");

			case DANGEROUS:
				return _("dangerous");

			default:
				return ("Security level is: [%d]").printf((int) this);
		}
	}

	public static SecurityLevel from_string (string str) {
		string s = str.down ();
		if (s == "high")
			return HIGH;
		if (s == "medium")
			return MEDIUM;
		if (s == "low")
			return LOW;
		if (s == "dangerous")
			return DANGEROUS;

		return LOW;
	}
}

/**
 * Status of a package signature
 */
public enum SignStatus {
	UNKNOWN,
	VALID,
	KEY_EXPIRED,
	KEY_MISSING,
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

			case KEY_MISSING:
				return _("Key is missing");

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

/**
 * Trust level of a signature
 */
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

/**
 * Package security information
 *
 * This class stores data which can be used to display meaningful information
 * about the security level of a 3rd-party software package.
 * It provides information about the state of the package signature.
 */
public class SecurityInfo : Object {
	private weak Package pack;

	public SignStatus signature_status { get; internal set; }
	public SignTrust signature_trustlevel { get; internal set; }

	public string user_names { get; internal set; }
	public string key_fpr { get; internal set; }
	public string trust_reason { get; internal set; }

	internal SecurityInfo () {
	}

	/**
	 * Returns a human-readable general security level, which
	 * was determined for the associated package.
	 */
	public SecurityLevel get_level () {
		debug ("SigStatus: %s | SigValidity: %s", signature_status.to_string (), signature_trustlevel.to_string ());

		// No valid or no signature: We can't ensure anything, so flag this package as 'dangerous'
		if (signature_status != SignStatus.VALID)
			return SecurityLevel.DANGEROUS;

		// Marginal trust for medium trust level
		if ((signature_trustlevel == SignTrust.MARGINAL) ||
		    (signature_trustlevel == SignTrust.UNKNOWN))
			return SecurityLevel.MEDIUM;

		// Full & ultimate trust will allow a high trust level for this package
		if ((signature_trustlevel >= SignTrust.FULL) ||
		    (signature_trustlevel >= SignTrust.ULTIMATE))
			return SecurityLevel.HIGH;

		return SecurityLevel.LOW;
	}

	public string get_level_as_string () {
		SecurityLevel lev = get_level ();
		switch (lev) {
			case SecurityLevel.HIGH:
				return _("Should be safe to use.");

			case SecurityLevel.MEDIUM:
				return _("Medium security.");

			case SecurityLevel.LOW:
				return _("Low security.");

			case SecurityLevel.DANGEROUS:
				return _("This package could be dangerous!");

			default:
				return ("Security level is: [%d]").printf((int) this);
		}
	}
}

} // End of namespace Listaller.IPK
