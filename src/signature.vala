/* signature.vala
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
using GPG;
using Listaller;

namespace Listaller {

private class GPGSignature : Object {
	private string signtext;

	public SignStatus sigstatus { get; set; }
	public SignValidity validity { get; set; }

	public GPGSignature (string sig) {
		signtext = sig;
		sigstatus = SignStatus.UNKNOWN;
		validity = SignValidity.UNKNOWN;
		init_gpgme (Protocol.OpenPGP);
	}

	private void init_gpgme (Protocol proto) {
		GPG.check_version (null);
		Intl.setlocale (LocaleCategory.ALL, "");
		/* Context.set_locale (null, LocaleCategory.CTYPE, Intl.setlocale (LocaleCategory.CTYPE, null)); */
	}

	private bool check_gpg_err (GPGError.ErrorCode err) {
		if (err != GPGError.ErrorCode.NO_ERROR) {
			stdout.printf ("X: %s".printf (GPGError.strsource (err)));
			return false;
		}
		return true;
	}

	private bool read_file_to_data (Data dt, string fname) {
		dt.set_encoding (DataEncoding.BINARY);

		const int BUFFER_SIZE = 512;
		char buff[512];

		int fd = Posix.open (fname, Posix.O_RDONLY);
		ssize_t len = Posix.read (fd, buff, BUFFER_SIZE);
		while (len > 0) {
			dt.write (buff, len);
			len = Posix.read (fd, buff, BUFFER_SIZE);
		}
		Posix.close (fd);
		//return_val_if_fail (check_gpg_err (errno), false);
		return true;
	}

	private void set_sigstatus_from_gpgsigsum (Sigsum sum) {
		switch (sum) {
			case Sigsum.VALID:
				sigstatus = SignStatus.VALID;
				break;

			case Sigsum.GREEN:
				sigstatus = SignStatus.GREEN;
				break;

			case Sigsum.RED:
				sigstatus = SignStatus.RED;
				break;

			case Sigsum.KEY_REVOKED:
				sigstatus = SignStatus.KEY_REVOKED;
				break;

			case Sigsum.KEY_EXPIRED:
				sigstatus = SignStatus.KEY_EXPIRED;
				break;

			case Sigsum.SIG_EXPIRED:
				sigstatus = SignStatus.SIG_EXPIRED;
				break;

			case Sigsum.KEY_MISSING:
				sigstatus = SignStatus.KEY_MISSING;
				break;

			case Sigsum.CRL_MISSING:
				sigstatus = SignStatus.CRL_MISSING;
				break;

			case Sigsum.CRL_TOO_OLD:
				sigstatus = SignStatus.CRL_TOO_OLD;
				break;

			case Sigsum.BAD_POLICY:
				sigstatus = SignStatus.BAD_POLICY;
				break;

			case Sigsum.SYS_ERROR:
				sigstatus = SignStatus.SYS_ERROR;
				break;

			default:
				sigstatus = SignStatus.UNKNOWN;
				break;
		}
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
			li_warning ("Unexpected number of signatures\n");
			return false;
		}
		set_sigstatus_from_gpgsigsum (sig->summary);
		set_sigvalidity_from_gpgvalidity (sig->validity);

		debug (sig->pka_address);
		if (sig->status != GPGError.ErrorCode.NO_ERROR) {
			li_warning ("Unexpected signature status: %s\n".printf (sig->status.to_string ()));
			return false;
		}
		if (sig->wrong_key_usage) {
			li_warning ("Unexpectedly wrong key usage\n");
			return false;
		}

		if (sig->validity_reason != GPGError.ErrorCode.NO_ERROR) {
			li_error ("Unexpected validity reason: %s\n".printf (sig->validity_reason.to_string ()));
			return false;
		}
		return true;
	}

	public bool verify_package (string ctrlfname, string payloadfname) {
		Context ctx;
		GPGError.ErrorCode err;
		Data sig, dt;
		VerifyResult *result;

		err = Context.Context (out ctx);
		return_if_fail (check_gpg_err (err));

		string ccf = concat_binfiles (ctrlfname, payloadfname);

		/* Checking a valid message.  */
		err = Data.create_from_file (out dt, ccf, true);
		dt.set_encoding (DataEncoding.BINARY);
		read_file_to_data (dt, ctrlfname);
		read_file_to_data (dt, payloadfname);
		return_if_fail (check_gpg_err (err));

		err = Data.create_from_memory (out sig, signtext, Posix.strlen (signtext), false);
		return_if_fail (check_gpg_err (err));

		err = ctx.op_verify (sig, dt, null);
		return_if_fail (check_gpg_err (err));
		result = ctx.op_verify_result ();

		process_sig_result (result);
		debug ("Signature checked!");
		return true;
	}
}

} // End of namespace
