/* gpgsign.vala
 *
 * Copyright (C) 2011 Matthias Klumpp
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
			stdout.printf ("X: %s\n".printf (err.to_string ()));
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

	private string free_data_to_string (Data *dt) {
		string sig_data;
		size_t signature_len;

		sig_data = dt->release_and_get_mem (out signature_len);
		if (sig_data == null) {
			li_error ("Signature data was NULL!");
			sig_data = "";
		}
		return sig_data;
	}

	private bool read_file_to_data (string fname, ref Data dt) {
		const uint BUFFER_SIZE = 512;
		dt.set_encoding (DataEncoding.BINARY);

		var file = File.new_for_path (fname);
		var fs = file.read ();
		var data_stream = new DataInputStream (fs);
		data_stream.set_byte_order (DataStreamByteOrder.LITTLE_ENDIAN);

		// Seek and read the image data chunk
		uint8[] buffer = new uint8[BUFFER_SIZE];
		fs.seek (0, SeekType.CUR);
		while (data_stream.read (buffer) > 0)
			dt.write (buffer, BUFFER_SIZE);

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

		read_file_to_data (control_fname, ref din);
		read_file_to_data (payload_fname, ref din);

		// detached signature.
		//din.seek (0, Posix.SEEK_SET);
		Data *dout;
		err = Data.create (out dout);
		return_if_fail (check_gpg_err (err));
		err = ctx.op_sign (din, dout, SigMode.DETACH);
		return_if_fail (check_gpg_err (err));
		SignResult *result = ctx.op_sign_result ();
		check_result (result, SigMode.DETACH);
		signature_out = free_data_to_string (dout);

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
