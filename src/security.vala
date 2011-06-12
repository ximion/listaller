/* security.vala
 *
 * Copyright (C) 2011  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using GLib;
using Listaller;

namespace Listaller {

public enum SignStatus {
	UNKNOWN,
	VALID,
	GREEN,
	RED,
	KEY_REVOKED,
	KEY_EXPIRED,
	SIG_EXPIRED,
	KEY_MISSING,
	CRL_MISSING,
	CRL_TOO_OLD,
	BAD_POLICY,
	SYS_ERROR;

	public string to_string() {
		switch (this) {
			case UNKNOWN:
				return _("Status of this signature is unknown or package is not signed.");

			case VALID:
				return _("Signature is fully valid");

			case GREEN:
				return _("Signature is good.");

			case RED:
				return _("Signature is bad.");

			case KEY_REVOKED:
				return _("One key has been revoked");

			case KEY_EXPIRED:
				return _("One key has expired");

			case SIG_EXPIRED:
				return _("Signature has expired");

			case KEY_MISSING:
				return _("Can't verify: Key is missing");

			case CRL_MISSING:
				return _("CRL not available.");

			case CRL_TOO_OLD:
				return _("Available CRL is too old");

 			case BAD_POLICY:
				return _("A policy was not met.");

			case SYS_ERROR:
				return _("A system error occured.");

			default:
				return ("Signature status is: [%d]").printf((int) this);
		}
	}
}

public enum SignValidity {
	UNKNOWN,
	UNDEFINED,
	NEVER,
	MARGINAL,
	FULL,
	ULTIMATE;

	public string to_string() {
		switch (this) {
			case UNKNOWN:
				return _("Validity is unknown");

			case UNDEFINED:
				return _("Validity is undefined");

			case NEVER:
				return _("Never trust.");

			case MARGINAL:
				return _("Trust is marginal");

			case FULL:
				return _("Trust if full");

			case ULTIMATE:
				return _("Trust is ultimate.");

			default:
				return ("Signature validity is: [%d]").printf((int) this);
		}
	}
}

} // End of namespace
