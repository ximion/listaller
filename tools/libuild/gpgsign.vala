/* gpgsign.vala
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

private class GPGSign : Object {

	public GPGSign () {
		init_gpgme (Protocol.OpenPGP);
	}

	private void init_gpgme (Protocol proto) {
		GPG.check_version (null);
		Intl.setlocale (LocaleCategory.ALL, "");
		/* Context.set_locale (null, LocaleCategory.CTYPE, Intl.setlocale (LocaleCategory.CTYPE, null));

		GPGError.ErrorCode err = GPG.check_version (proto);
		return_if_fail (check_gpg_err (err)); */
	}

	private bool check_gpg_err (GPGError.ErrorCode err) {
		if (err != GPGError.ErrorCode.NO_ERROR) {
			stdout.printf ("X: %s".printf (GPGError.strsource (err)));
			return false;
		}
		return true;
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

	private void _dbg_print_data (Data dh)	{
		const uint BUF_SIZE = 512;
		char buf[513];
		long ret;

		ret = dh.seek (0, Posix.SEEK_SET);
		/*if (ret > 0)
			error (errno.to_string ());*/
		while ((ret = dh.read (buf, BUF_SIZE)) > 0)
			stdout.printf ((string) buf, ret, 1);
		/*if (ret < 0)
			error (errno.to_string ());*/
	}

	private string data_to_string (Data dt) {
		const uint BUF_SIZE = 512;
		char buf[513];
		long ret;
		string res = "";

		ret = dt.seek (0, Posix.SEEK_SET);
		/*if (ret > 0)
		 e rror (errno.to_string ());*/
		 while ((ret = dt.read (buf, BUF_SIZE)) > 0)
			 res += ((string) buf).printf (ret, 1);
		 /*if (ret < 0)
		  e rror (errno.to_string ());*/
		 return res;
	}

	private bool read_file_to_data (Data dt, string fname) {
		dt.set_encoding (DataEncoding.BINARY);

		const int BUFFER_SIZE = 8192;
		char buff[8192];

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

	public bool sign_package (string control_fname, string payload_fname, out string signature_out) {
		GPGError.ErrorCode err;

		Context ctx;
		err = Context.Context (out ctx);
		return_if_fail (check_gpg_err (err));

		string? agent_info = Environment.get_variable ("GPG_AGENT_INFO");
		/*if (!(agent_info && strchr (agent_info, ':')))
		 gpgme_set_pa*ssphrase_cb (ctx, passphrase_cb, NULL);*/

		ctx.set_textmode (true);
		ctx.set_armor (true);

		Data din;
		err = Data.create (out din);
		return_if_fail (check_gpg_err (err));

		read_file_to_data (din, control_fname);
		read_file_to_data (din, payload_fname);

		// detached signature.
		din.seek (0, Posix.SEEK_SET);
		Data dout;
		err = Data.create (out dout);
		return_if_fail (check_gpg_err (err));
		err = ctx.op_sign (din, dout, SigMode.DETACH);
		return_if_fail (check_gpg_err (err));
		SignResult *result = ctx.op_sign_result ();
		check_result (result, SigMode.DETACH);
		signature_out = data_to_string (dout);

		return true;
	}

	public bool _dbg_sign_test () {
		GPGError.ErrorCode err;

		Context ctx;
		err = Context.Context (out ctx);
		return_if_fail (check_gpg_err (err));

		string? agent_info = Environment.get_variable ("GPG_AGENT_INFO");
		/*if (!(agent_info && strchr (agent_info, ':')))
			gpgme_set_passphrase_cb (ctx, passphrase_cb, NULL);*/

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
