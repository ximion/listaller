/* signature.vala
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
using GPG;
using Listaller;
using Listaller.Utils;

namespace Listaller {

private class GPGSignature : GPGBasic {
	private string signtext;

	public SignStatus sigstatus { get; set; }
	public SignValidity validity { get; set; }
	public bool sig_valid { get; set; }

	public GPGSignature (string sig) {
		base (Protocol.OpenPGP);

		signtext = sig;
		sig_valid = false;
		sigstatus = SignStatus.UNKNOWN;
		validity = SignValidity.UNKNOWN;
	}

	private void set_sigvalidity_from_gpgvalidity (Validity val) {
		switch (val) {
			case Validity.UNKNOWN:
				validity = SignValidity.UNKNOWN;
				break;

			case Validity.UNDEFINED:
				validity = SignValidity.UNDEFINED;
				break;

			case Validity.NEVER:
				validity = SignValidity.NEVER;
				break;

			case Validity.MARGINAL:
				validity = SignValidity.MARGINAL;
				break;

			case Validity.FULL:
				validity = SignValidity.FULL;
				break;

			case Validity.ULTIMATE:
				validity = SignValidity.ULTIMATE;
				break;

			default:
				validity = SignValidity.UNKNOWN;
				break;
		}
	}

	private bool process_sig_result (VerifyResult *result) {
		Signature *sig = result->signatures;

		if ((sig == null) || (sig->next != null)) {
			warning ("Unexpected number of signatures!");
			return false;
		}
		sigstatus = (SignStatus) sig->summary;
		set_sigvalidity_from_gpgvalidity (sig->validity);

		if (sig->status != GPGError.ErrorCode.NO_ERROR) {
			warning ("Unexpected signature status: %s", sig->status.to_string ());
			sig_valid = false;
		} else {
			sig_valid = true;
		}
		if (sig->wrong_key_usage) {
			warning ("Unexpectedly wrong key usage");
			return false;
		}

		if (sig->validity_reason != GPGError.ErrorCode.NO_ERROR) {
			li_error ("Unexpected validity reason: %s".printf (sig->validity_reason.to_string ()));
			return false;
		}
		return true;
	}

	private bool verify_package_internal (string ctrlfname, string? payloadfname) {
		Context ctx;
		GPGError.ErrorCode err;
		Data sig, dt;
		VerifyResult *result;
		bool ret;

		err = Context.Context (out ctx);
		return_if_fail (check_gpg_err (err));

		ctx.set_textmode (true);
		ctx.set_armor (true);

		err = Data.create (out dt);
		return_if_fail (check_gpg_err (err));

		ret = read_file_to_data (ctrlfname, ref dt);
		if (!ret)
			return false;
		ret = read_file_to_data (payloadfname, ref dt);
		if ((!ret) && (!__unittestmode))
			return false;

		//err = Data.create_from_memory (out sig, signtext, signtext.length, false);
		err = Data.create (out sig);
		read_string_to_data (signtext, ref sig);
		return_if_fail (check_gpg_err (err));

		sig.seek (0, Posix.SEEK_SET);
		dt.seek (0, Posix.SEEK_SET);

		err = ctx.op_verify (sig, dt, null);
		return_if_fail (check_gpg_err (err));
		result = ctx.op_verify_result ();

		// FIXME: The whole GPGMe code is not working peroperly...
		// This codeblock is for debugging
		Signature *s = result->signatures;
while (s != null) {
        stdout.printf("summary=%d\n", s->summary);
        stdout.printf("fpr=%s\n", s->fpr);
        stdout.printf("status=%d\n", s->status);
        stdout.printf("timestamp=%lu\n", s->timestamp);
        stdout.printf("wrong_key_usage=%u\n", (uint) s->wrong_key_usage);
        stdout.printf("pka_trust=%u\n", s->pka_trust);
        stdout.printf("chain_model=%u\n", (uint) s->chain_model);
        stdout.printf("validity=%d\n", s->validity);
        stdout.printf("validity_reason=%d\n", s->validity_reason);
        stdout.printf("key=%d\n", s->pubkey_algo);
        stdout.printf("hash=%d\n", s->hash_algo);
        s = s->next;
    }

		process_sig_result (result);

		debug ("Signature checked.");
		return true;
	}

	public bool verify_package (string ctrlfname, string payloadfname) {
		bool ret;
		ret = verify_package_internal (ctrlfname, payloadfname);
		if (!ret) {
			debug ("Signature is broken!");
			validity = SignValidity.NEVER;
			sigstatus = SignStatus.RED;
		}
		return ret;
	}

	internal bool _verify_package_test (string fname) {
		bool ret;
		ret = verify_package_internal (fname, null);
		if (!ret) {
			debug ("Signature is broken!");
			validity = SignValidity.NEVER;
			sigstatus = SignStatus.RED;
		}
		return ret;
	}
}

} // End of namespace
