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

private class GPGCreateSign : Object {

	public GPGCreateSign () {

	}

	public bool sign_package (string fname) {
		/*GPG.Context ctx;
		GPG.Error err;
		GPG.Data in;
		GPG.Data out;
		GPG.SignResult res;
		string? agent_info = Environment.get_variable ("GPG_AGENT_INFO");

		GPG.check_version (GPG.Protocol.OpenPGP);

		err = GPG.Context (out ctx);
		fail_if_err (err);

		/ *if (!(agent_info && strchr (agent_info, ':')))
		 gpgme_set_passphras*e_cb (ctx, passphrase_cb, NULL);* /

		ctx.set_textmode (false);
		ctx.set_armor (true);

		err = gpgme_data_new_from_mem (&in, "Hallo Leute\n", 12, 0);
		fail_if_err (err);

		/ * First a normal signature.  * /
		err = gpgme_data_new (&out);
		fail_if_err (err);
		err = gpgme_op_sign (ctx, in, out, GPGME_SIG_MODE_NORMAL);
		fail_if_err (err);
		result = gpgme_op_sign_result (ctx);
		check_result (result, GPGME_SIG_MODE_NORMAL);
		print_data (out);
		gpgme_data_release (out);

		/ * Now a detached signature.  * /
		gpgme_data_seek (in, 0, SEEK_SET);
		err = gpgme_data_new (&out);
		fail_if_err (err);
		err = gpgme_op_sign (ctx, in, out, GPGME_SIG_MODE_DETACH);
		fail_if_err (err);
		result = gpgme_op_sign_result (ctx);
		check_result (result, GPGME_SIG_MODE_DETACH);
		print_data (out);
		gpgme_data_release (out);

		/ * And finally a cleartext signature.  * /
		gpgme_data_seek (in, 0, SEEK_SET);
		err = gpgme_data_new (&out);
		fail_if_err (err);
		err = gpgme_op_sign (ctx, in, out, GPGME_SIG_MODE_CLEAR);
		fail_if_err (err);
		result = gpgme_op_sign_result (ctx);
		check_result (result, GPGME_SIG_MODE_CLEAR);
		print_data (out);
		gpgme_data_release (out);

		gpgme_data_release (in);
		gpgme_release (ctx);
		*/
		return false;
	}
}

} // End of namespace
