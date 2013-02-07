/* signature.vala
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
	public bool valid { get; set; }

	public string user_names { get; set; }
	public string key_fpr { get; set; }
	public string trust_reason { get; set; }

	public GPGSignature (string sig) {
		keymgr = new KeyManager ();

		signtext = sig;
		valid = false;
		sigstatus = SignStatus.UNKNOWN;
		// we have a marginal trust level by default
		trust_level = SignTrust.MARGINAL;
	}


	private string signsummary_to_string (Sigsum summary) {
		string str = "";
		if ((summary & Sigsum.VALID) > 0)       str += " valid";
		if ((summary & Sigsum.GREEN) > 0)       str += " green";
		if ((summary & Sigsum.RED) > 0)         str += " red";
		if ((summary & Sigsum.KEY_REVOKED) > 0) str += " revoked";
		if ((summary & Sigsum.KEY_EXPIRED) > 0) str += " key-expired";
		if ((summary & Sigsum.SIG_EXPIRED) > 0) str += " sig-expired";
		if ((summary & Sigsum.KEY_MISSING) > 0) str += " key-missing";
		if ((summary & Sigsum.CRL_MISSING) > 0) str += " crl-missing";
		if ((summary & Sigsum.CRL_TOO_OLD) > 0) str += " crl-too-old";
		if ((summary & Sigsum.BAD_POLICY) > 0)  str += " bad-policy";
		if ((summary & Sigsum.SYS_ERROR) > 0)   str += " sys-error";
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
			return "[bad validity value]";

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
							(sig->wrong_key_usage) ? " wrong-key-usage" : "", sig->chain_model ? " %s".printf ("chain-model") : "",
							(sig->notations != null) ? _("yes") : _("no"));

		return res_text;
	}

	private bool set_info_from_key (Key? key) {
		if (key == null) {
			warning ("Key was NULL, cannot set security information!");
			return false;
		}

		key_fpr = (key.subkeys != null) ? key.subkeys.fpr : "?";

		string key_usernames = "";
		UserID *uid = key.uids;
		int nuids = 0;
		while (uid != null) {
			key_usernames += "%d: %s\n".printf (nuids, uid->uid);

			nuids++;
			uid = uid->next;
		}
		user_names = key_usernames;

		return true;
	}

	private bool process_sig_result (Signature *sig, Context ctx) {
		GPGError.ErrorCode err;

		var sig_estatus = (GPGError.ErrorCode) sig->status;

		// set trust level for this signature
		trust_level = sigvalidity_to_trustlevel (sig->validity);

		if (sig_estatus == GPGError.ErrorCode.NO_ERROR) {
			sigstatus = SignStatus.VALID;
			valid = true;
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

		if ((sig->summary & Sigsum.KEY_MISSING) > 0) {
			sigstatus = SignStatus.KEY_MISSING;
		}

		// If we have one of these statuses already, it doesn't make sense to continue here
		if ((sigstatus == SignStatus.KEY_MISSING) ||
		    (sigstatus == SignStatus.NO_PUBKEY)) {
			return false;
		}

		if (sig->status != GPGError.ErrorCode.NO_ERROR) {
			string msg = "Unexpected signature status: %s".printf (sig->status.to_string ());
			if (__unittestmode)
				Report.log_warning (msg);
			else
				warning (msg);
			valid = false;
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

	private VerifyResult *context_verify (Context ctx, Data sig_data, Data dat) {
		GPGError.ErrorCode err;
		VerifyResult *result;

		err = ctx.op_verify (sig_data, dat, null);
		if (!check_gpg_err (err))
			return null;
		result = ctx.op_verify_result ();

		if (result == null) {
			critical ("Error communicating with libgpgme: no result record!");
			return null;
		}

		return result;
	}

	private bool check_signature_internal (Data sig_data, Data dat) {
		bool ret;
		unowned Context ctx;
		VerifyResult *result;

		ctx = keymgr.get_main_context ();

		result = context_verify (ctx, sig_data, dat);
		if (result == null)
			return false;

		Signature *sig = result->signatures;

		if ((sig == null) || (sig->next != null)) {
			warning ("Unexpected number of signatures!");
			return false;
		}

		ret = process_sig_result (sig, ctx);
		if ((!ret) && (sigstatus == SignStatus.KEY_MISSING) && (sig->fpr != null)) {
			// Key seems to be missing, so fetch it and try again!
			TmpContext tmpctx = keymgr.get_tmp_context_with_key (sig->fpr);

			sig_data.seek (0, Posix.SEEK_SET);
			dat.seek (0, Posix.SEEK_SET);
			result = context_verify (tmpctx.context, sig_data, dat);
			if (result == null)
				return false;

			// now check the result
			sig = result->signatures;
			if ((sig == null) || (sig->next != null)) {
				warning ("Unexpected number of signatures!");
				return false;
			}
			ret = process_sig_result (sig, tmpctx.context);

			set_info_from_key (tmpctx.key);

			// ensure tmp context is deleted properly (I hate this hack...)
			keymgr.delete_tmp_context (tmpctx);
		} else {
			// if key has been found in our trusted-db, we can fully trust it
			trust_level = SignTrust.FULL;
			set_info_from_key (keymgr.lookup_key (sig->fpr, true));
		}

		debug ("Signature Details:\n%s", signature_details_as_string (sig));

		return ret;
	}

	private bool verify_package_internal (string ctrl_fname) {
		GPGError.ErrorCode err;
		Data sig, dt;
		bool ret;

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

		check_signature_internal (sig, dt);

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
