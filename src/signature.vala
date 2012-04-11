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
	public SignTrust trust_level { get; set; }
	public bool sig_valid { get; set; }

	public GPGSignature (string sig) {
		base (Protocol.OpenPGP);

		signtext = sig;
		sig_valid = false;
		sigstatus = SignStatus.UNKNOWN;
		trust_level = SignTrust.UNKNOWN;
	}

	/*
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
	*/

	private bool process_sig_result (VerifyResult *result) {
		Signature *sig = result->signatures;

		if ((sig == null) || (sig->next != null)) {
			warning ("Unexpected number of signatures!");
			return false;
		}
		//sig_summary = sig->summary;
		var sig_estatus = (GPGError.ErrorCode) sig->status;

		switch (sig_estatus) {
			case GPGError.ErrorCode.NO_ERROR:
				sigstatus = SignStatus.VALID;
				break;
			case GPGError.ErrorCode.KEY_EXPIRED:
				sigstatus = SignStatus.KEY_EXPIRED;
				break;
			case GPGError.ErrorCode.CERT_REVOKED:
				sigstatus = SignStatus.CERT_REVOKED;
				break;
			case GPGError.ErrorCode.SIG_EXPIRED:
				time_t t = (time_t) sig->exp_timestamp;
				var time = new DateTime.from_unix_utc (t);

				warning ("Expired signature (since %s)", time.format ("%Y-%m-%d"));
				sigstatus = SignStatus.SIG_EXPIRED;
				break;
			case GPGError.ErrorCode.BAD_SIGNATURE:
				sigstatus = SignStatus.BAD;
				break;
			case GPGError.ErrorCode.NO_PUBKEY:
				sigstatus = SignStatus.NO_PUBKEY;
				break;
			default:
				sigstatus = SignStatus.UNKNOWN;
				warning ("Got unknown return status while processing signature: %s", sig_estatus.to_string ());
				break;
		}

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
		ctx.set_protocol (Protocol.OpenPGP);

		return_if_fail (check_gpg_err (err));

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
		if (!check_gpg_err (err))
			return false;
		result = ctx.op_verify_result ();

		if (result == null) {
			critical ("Error communicating with libgpgme: no result record!");
			return false;
		}

		// FIXME: The whole GPGMe code is not working properly...
		// This codeblock is useful to debug the issue
		if (__unittestmode) {
			Signature *s = result->signatures;
			while (s != null) {
				var t = s->timestamp;
				var time = new DateTime.from_unix_utc (t);

				stdout.printf("SigSum: %i\n", (int) s->summary);
				stdout.printf("fpr=%s\n", s->fpr);
				stdout.printf("status=%d\n", s->status);
				stdout.printf("timestamp=%s\n", time.format ("%Y-%m-%d"));
				stdout.printf("wrong_key_usage=%u\n", (uint) s->wrong_key_usage);
				stdout.printf("pka_trust=%u\n", s->pka_trust);
				stdout.printf("chain_model=%u\n", (uint) s->chain_model);
				stdout.printf("validity=%d\n", s->validity);
				stdout.printf("validity_reason=%d\n", s->validity_reason);
				stdout.printf("key=%d\n", s->pubkey_algo);
				stdout.printf("hash=%d\n", s->hash_algo);
				SigNotation *r;
				for (r = s->notations; r != null; r = r->next) {
					stdout.printf("notation.name=%s\n", r->name);
				}
				s = s->next;
			}
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
			trust_level = SignTrust.NEVER;
			sigstatus = SignStatus.BAD;
		}
		return ret;
	}

	internal bool _verify_package_test (string fname) {
		bool ret;
		ret = verify_package_internal (fname, null);
		if (!ret) {
			debug ("Signature is broken!");
			trust_level = SignTrust.NEVER;
			sigstatus = SignStatus.BAD;
		}
		return ret;
	}
}

} // End of namespace
