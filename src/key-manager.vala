/* key-manager.vala - Handle the Listaller GPG keyring
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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
using GPG;
using Listaller;
using Listaller.GPGEx;

namespace Listaller {

/**
 * Manage Listaller's GPG-Key database
 *
 * Listaller uses it's own set of GPG keys to determine package
 * trust levels. Use this class to access the key database and
 * to check package trust levels.
 */
public class KeyManager : MessageObject {
	private Context main_ctx;

	public KeyManager () {
		init_gpgme (Protocol.OpenPGP);

		GPGError.ErrorCode err;
		err = new_context (out main_ctx);
		return_if_fail (check_gpg_err (err));
	}

	internal Key? lookup_key (string key_fpr) {
		GPGError.ErrorCode err;
		string fpr = key_fpr;
		Key key;

		// add 0x prefix, gpg2 needs this...
		if (!fpr.has_prefix ("0x"))
			fpr = "0x%s".printf (fpr);

		/* using LOCAL and EXTERN together doesn't work for GPG 1.X. Ugh. */
		set_context_external (main_ctx);

		err = main_ctx.get_key (fpr, out key, false);
		if (err.code () == GPGError.ErrorCode.EOF) {
			debug ("key lookup failed, unknown key");
			/* Try an alternate lookup using the 8 character fingerprint value, since
			* busted-ass keyservers can't support lookups using subkeys with the full
			* value as of now. This is why 2012 is not the year of PGP encryption. */
			int fpr_length = fpr.length;
			if(fpr_length > 8) {
				string short_fpr = "0x%s".printf (fpr.substring (fpr_length - 8));
				debug (	"looking up key %s remotely", short_fpr);
				err = main_ctx.get_key(short_fpr, out key, false);

				if (err.code () == GPGError.ErrorCode.EOF)
					debug ("key lookup failed, unknown key");
			}
		}

		return_val_if_fail (check_gpg_err (err), null);

		return key;
	}

	public bool import_key () {
		Key[] keyList = {};

		main_ctx.op_import_keys (keyList);

		return false;
	}

}

} // End of namespace: Listaller
