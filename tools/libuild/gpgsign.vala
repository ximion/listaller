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

private class GPGSign : Object {

	public GPGSign () {
		init_gpgme (Protocol.OpenPGP);
	}

	private void fail_if_err (GPGError.ErrorCode err) {
		stdout.printf ((string) err);
	}

	private void init_gpgme (Protocol proto) {
		GPG.check_version (null);
		Intl.setlocale (LocaleCategory.ALL, "");
		//Context.set_locale (null, LocaleCategory.CTYPE, Intl.setlocale (LocaleCategory.CTYPE, null));

		//GPGError.ErrorCode err = GPG.check_version (proto);
		//fail_if_err (err);
	}

	private void fail_id_err (GPGError.ErrorCode err) {
		//TODO
	}

	private void _dbg_print_data (Data dh)	{
		const uint BUF_SIZE = 512;
		char buf[513];
		long ret;

		ret = dh.seek (0, Posix.SEEK_SET);
		/*if (ret > 0)
			error (errno.to_string ());*/
		while ((ret = dh.read (buf, BUF_SIZE)) > 0)
			stdout.printf ((string) buf, ret, 1, stdout);
		/*if (ret < 0)
			error (errno.to_string ());*/
	}

	public bool sign_package (string fname) {
		GPGError.ErrorCode err;

		Context ctx;
		err = Context.Context (out ctx);
		//! fail_if_err (err);

		string? agent_info = Environment.get_variable ("GPG_AGENT_INFO");
		/*if (!(agent_info && strchr (agent_info, ':')))
			gpgme_set_passphrase_cb (ctx, passphrase_cb, NULL);*/

		ctx.set_textmode (true);
		ctx.set_armor (true);

		Data din;
		err = Data.create_from_memory (out din, "Hallo Leute\n", 12, false);
		//! fail_if_err (err);

		// detached signature.
		din.seek (0, Posix.SEEK_SET);
		Data dout;
		err = Data.create (out dout);
		//! fail_if_err (err);
		err = ctx.op_sign (din, dout, SigMode.DETACH);
		//! fail_if_err (err);
		SignResult *result = ctx.op_sign_result ();
		//! check_result (result, GPGME_SIG_MODE_DETACH);
		_dbg_print_data (dout);

		return false;
	}
}

} // End of namespace
