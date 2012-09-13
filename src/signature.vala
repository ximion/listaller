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
using Listaller.GPGEx;

namespace Listaller {

private class GPGSignature : Object {
	private string signtext;
	private KeyManager keymgr;

	public SignStatus sigstatus { get; set; }
	public SignTrust trust_level { get; set; }
	public bool sig_valid { get; set; }

	public GPGSignature (string sig) {
		keymgr = new KeyManager ();

		signtext = sig;
		sig_valid = false;
		sigstatus = SignStatus.UNKNOWN;
		// we have a marginal trust level by default
		trust_level = SignTrust.MARGINAL;
	}


	private string signsummary_to_string (Sigsum summary) {
		string str = "";
		if ((summary & Sigsum.VALID) > 0)       str += _(" valid");
		if ((summary & Sigsum.GREEN) > 0)       str += _(" green");
		if ((summary & Sigsum.RED) > 0)         str += _(" red");
		if ((summary & Sigsum.KEY_REVOKED) > 0) str += _(" revoked");
		if ((summary & Sigsum.KEY_EXPIRED) > 0) str += _(" key-expired");
		if ((summary & Sigsum.SIG_EXPIRED) > 0) str += _(" sig-expired");
		if ((summary & Sigsum.KEY_MISSING) > 0) str += _(" key-missing");
		if ((summary & Sigsum.CRL_MISSING) > 0) str += _(" crl-missing");
		if ((summary & Sigsum.CRL_TOO_OLD) > 0) str += _(" crl-too-old");
		if ((summary & Sigsum.BAD_POLICY) > 0)  str += _(" bad-policy");
		if ((summary & Sigsum.SYS_ERROR) > 0)   str += _(" sys-error");
		if (str == "")
			str = " ???";

		return str;
	}

	private SignTrust sigvalidity_to_trustlevel (Validity validity) {
		switch (validity) {
			case Validity.UNKNOWN:   return SignTrust.UNKNOWN;
			case Validity.UNDEFINED: return SignTrust.UNDEFINED;
			case Validity.NEVER:     return SignTrust.NEVER;
			case Validity.MARGINAL:  return SignTrust.MARGINAL;
			case Validity.FULL:      return SignTrust.FULL;
			case Validity.ULTIMATE:  return SignTrust.ULTIMATE;
			default : return SignTrust.BAD_VALUE;
		}

		return SignTrust.BAD_VALUE;
	}

	private string sigvalidity_to_string (Validity validity) {
		SignTrust trust = sigvalidity_to_trustlevel (validity);
		if (trust == SignTrust.BAD_VALUE)
			return _("[bad validity value]");

		return trust.to_string ();
	}


	private string signature_details_as_string (Signature *sig) {
		string keyinfo_format = _("status ....: %s\n" +
			"summary ...:%s\n" +
			"fingerprint: %s\n" +
			"created ...: %s\n" +
			"expires ...: %s\n" +
			"validity ..: %s\n" +
			"val.reason : %s\n" +
			"pubkey algo: %s\n" +
			"digest algo: %s\n" +
			"pka address: %s\n" +
			"pka trust .: %s\n" +
			"other flags:%s%s\n" +
			"notations .: %s");

		string sig_timestamp = _("Unknown");
		string sig_exp_timestamp = _("Unknown");
		if (sig->timestamp > 0)
			sig_timestamp = Time.local ((time_t) sig->timestamp).to_string ();
		if (sig->exp_timestamp > 0)
			sig_exp_timestamp = Time.local ((time_t) sig->exp_timestamp).to_string ();

		string res_text;
		res_text = keyinfo_format.printf (sig->status.to_string (),
							signsummary_to_string (sig->summary),
							(sig->fpr != null) ? sig->fpr : _("[None]"),
							sig_timestamp,
							sig_exp_timestamp,
							sigvalidity_to_string (sig->validity),
							sig->status.to_string (),
							(sig->pubkey_algo > 0) ? get_public_key_algorithm_name (sig->pubkey_algo) : _("Unknown"),
							(sig->hash_algo > 0) ? get_hash_algorithm_name (sig->hash_algo) : _("Unknown"),
							(sig->pka_address != null) ? sig->pka_address : _("[None]"),
							(sig->pka_trust == 0) ? _("n/a") : sig->pka_trust == 1 ? _("bad") : sig->pka_trust == 2 ? _("okay"): _("RFU"),
							(sig->wrong_key_usage) ? _(" wrong-key-usage") : "", sig->chain_model ? _(" chain-model") : "",
							(sig->notations != null) ? _("yes") : _("no"));

		return res_text;
	}

	private bool process_sig_result (VerifyResult *result, Context ctx) {
		GPGError.ErrorCode err;
		Signature *sig = result->signatures;

		if ((sig == null) || (sig->next != null)) {
			warning ("Unexpected number of signatures!");
			return false;
		}

		var sig_estatus = (GPGError.ErrorCode) sig->status;

		// set trust level for this signature
		trust_level = sigvalidity_to_trustlevel (sig->validity);

		debug ("Signature Details:\n%s", signature_details_as_string (sig));

		if (sig_estatus == GPGError.ErrorCode.NO_ERROR) {
			sigstatus = SignStatus.VALID;
			sig_valid = true;
		} else if ((sig_estatus & GPGError.ErrorCode.BAD_SIGNATURE) > 0) {
			sigstatus = SignStatus.BAD;
		} else if ((sig_estatus & GPGError.ErrorCode.KEY_EXPIRED) > 0) {
			sigstatus = SignStatus.KEY_EXPIRED;
		} else if ((sig_estatus & GPGError.ErrorCode.CERT_REVOKED) > 0) {
			sigstatus = SignStatus.CERT_REVOKED;
		} else if ((sig_estatus & GPGError.ErrorCode.SIG_EXPIRED) > 0) {
			time_t t = (time_t) sig->exp_timestamp;
			var time = new DateTime.from_unix_utc (t);

			Report.log_warning ("Expired signature (since %s)".printf (time.format ("%Y-%m-%d")));
			sigstatus = SignStatus.SIG_EXPIRED;
		} else if ((sig_estatus & GPGError.ErrorCode.NO_PUBKEY) > 0) {
			sigstatus = SignStatus.NO_PUBKEY;
		} else {
			sigstatus = SignStatus.UNKNOWN;
			string msg = "Got unknown return status while processing signature: %s | %d".printf (sig_estatus.to_string (), sig_estatus);
			if (__unittestmode)
				Report.log_warning (msg);
			else
				warning (msg);
		}

		if (sig->status != GPGError.ErrorCode.NO_ERROR) {
			string msg = "Unexpected signature status: %s".printf (sig->status.to_string ());
			if (__unittestmode)
				Report.log_warning (msg);
			else
				warning (msg);
			sig_valid = false;
		}

		if (sig->wrong_key_usage) {
			Report.log_warning ("Unexpectedly wrong key usage");
			return false;
		}

		if (sig->validity_reason != GPGError.ErrorCode.NO_ERROR) {
			Report.log_error ("Unexpected validity reason: %s".printf (sig->validity_reason.to_string ()));
			return false;
		}

		return true;
	}

	private bool check_signature_internal (Context ctx, Data sig, Data dat) {
		bool ret;
		GPGError.ErrorCode err;
		VerifyResult *result;

		err = ctx.op_verify (sig, dat, null);
		if (!check_gpg_err (err))
			return false;
		result = ctx.op_verify_result ();

		if (result == null) {
			critical ("Error communicating with libgpgme: no result record!");
			return false;
		}

		if (__unittestmode) {
			Signature *s = result->signatures;
			while (s != null) {
				var t = s->timestamp;
				var time = new DateTime.from_unix_utc (t);

				stdout.printf ("%s\n", signature_details_as_string (s));

				SigNotation *r;
				for (r = s->notations; r != null; r = r->next) {
					stdout.printf("notation.name=%s\n", r->name);
				}
				s = s->next;
			}
		}

		ret = process_sig_result (result, ctx);

		return ret;
	}

	private bool verify_package_internal (string ctrl_fname) {
		unowned Context ctx;
		GPGError.ErrorCode err;
		Data sig, dt;
		bool ret;

		ctx = keymgr.get_main_context ();

		err = Data.create (out dt);
		return_if_fail (check_gpg_err (err));

		ret = read_file_to_data (ctrl_fname, ref dt);
		if (!ret)
			return false;

		//err = Data.create_from_memory (out sig, signtext, signtext.length, false);
		err = Data.create (out sig);
		read_string_to_data (signtext, ref sig);
		return_if_fail (check_gpg_err (err));

		sig.seek (0, Posix.SEEK_SET);
		dt.seek (0, Posix.SEEK_SET);

		check_signature_internal (ctx, sig, dt);

		debug ("Signature checked.");

		return true;
	}

	public bool verify_package (string ctrl_fname) {
		bool ret;
		ret = verify_package_internal (ctrl_fname);
		if (!ret) {
			debug ("Signature is broken!");
			trust_level = SignTrust.NEVER;
			sigstatus = SignStatus.BAD;
		}
		return ret;
	}

}

} // End of namespace: Listaller
