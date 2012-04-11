/* gpgsign.vala - Sign IPK package with a GPK key
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU General Public License Version 3
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
 */

using GLib;
using GPG;
using Listaller;

namespace Listaller {

private class GPGSign : GPGBasic {

	public GPGSign () {
		base (Protocol.OpenPGP);
	}

	private bool check_result (SignResult *result, SigMode type) {
		if (result->invalid_signers != null) {
			stderr.printf ("Invalid signer found: %s\n",
				result->invalid_signers.fpr);
			return false;
		}
		if ((result->signatures == null) || (result.signatures->next != null)) {
			stderr.printf ("Unexpected number of signatures created\n");
			return false;
		}
		if (result->signatures->type != type) {
			stderr.printf ("Wrong type of signature created\n");
			return false;
		}
		/* if (result->signatures->pubkey_algo != PublicKeyAlgorithm.DSA) {
			stderr.printf ("Wrong pubkey algorithm reported: %i\n",
				result.signatures->pubkey_algo);
			return false;
		} */
		if (result->signatures->hash_algo != HashAlgorithm.SHA1) {
			stderr.printf ("Wrong hash algorithm reported: %i\n",
				result.signatures->hash_algo);
			return false;
		}
		if (result->signatures->sig_class != 1) {
			stderr.printf ("Wrong signature class reported: %u\n",
				result.signatures->sig_class);
			return false;
		}
		pkinfo_info ("Signed with fingerprint: %s\n".printf (result->signatures->fpr));
		return true;
	}

	private static GPGError.ErrorCode simple_passphrase_cb (void* hook, string uid_hint, string passphrase_info, bool prev_was_bad, int fd) {
		// IMPORTANT: This method of requesting a passwird is very ugly, replace it with something better soon!
		if (!prev_was_bad)
			stdout.printf ("Please enter your password:\n");
		else
			stdout.printf ("Okay, and now enter the correct password:");
		string? pass = "%s\n".printf (stdin.read_line ());
		Posix.write (fd, pass, pass.size ());
		return GPGError.ErrorCode.NO_ERROR;
	}

	public bool sign_package (string control_fname, string? payload_fname, out string signature_out) {
		GPGError.ErrorCode err;
		bool ret;

		Context ctx;
		err = Context.Context (out ctx);
		ctx.set_protocol (Protocol.OpenPGP);
		return_if_fail (check_gpg_err (err));

		string? agent_info = Environment.get_variable ("GPG_AGENT_INFO");
		if (!((agent_info != null) && (agent_info.index_of_char (':') > 0)))
			ctx.set_passphrase_cb (simple_passphrase_cb);

		ctx.set_textmode (true);
		ctx.set_armor (true);

		Data din;
		err = Data.create (out din);
		return_if_fail (check_gpg_err (err));

		ret = read_file_to_data (control_fname, ref din);
		if (!ret)
			return false;
		ret = read_file_to_data (payload_fname, ref din);
		if ((!ret) && (!__unittestmode))
			return false;

		// detached signature.
		din.seek (0, Posix.SEEK_SET);
		Data *dout;
		err = Data.create (out dout);
		return_if_fail (check_gpg_err (err));
		err = ctx.op_sign (din, dout, SigMode.DETACH);
		// return_if_fail (check_gpg_err (err));
		SignResult *result = ctx.op_sign_result ();
		check_result (result, SigMode.DETACH);

		signature_out = free_data_to_string (&dout);

		return true;
	}

	public bool _dbg_sign_test () {
		GPGError.ErrorCode err;

		Context ctx;
		err = Context.Context (out ctx);
		return_if_fail (check_gpg_err (err));

		string? agent_info = Environment.get_variable ("GPG_AGENT_INFO");
		if (!((agent_info != null) && (agent_info.index_of_char (':') > 0)))
			ctx.set_passphrase_cb (simple_passphrase_cb);

		ctx.set_textmode (true);
		ctx.set_armor (true);

		Data din;
		err = Data.create_from_memory (out din, "Hallo Leute\n", 12, false);
		return_if_fail (check_gpg_err (err));

		// detached signature.
		din.seek (0, Posix.SEEK_SET);
		Data dout;
		err = Data.create (out dout);
		return_if_fail (check_gpg_err (err));
		err = ctx.op_sign (din, dout, SigMode.DETACH);
		return_if_fail (check_gpg_err (err));
		SignResult *result = ctx.op_sign_result ();
		check_result (result, SigMode.DETACH);
		_dbg_print_data (dout);

		return false;
	}
}

} // End of namespace
