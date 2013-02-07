/* gpgex.vala - Common operations to simplify handling of GPGMe
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

/**
 * Namespace for internal stuff to make handling of GPGMe easier.
 *
 * All functions here should be "private" or "internal"!
 */
namespace Listaller.GPGEx {

private void init_gpgme (Protocol proto) {
	GPGError.ErrorCode err;

	GPG.check_version (null);
	Intl.setlocale (LocaleCategory.ALL, "");
	err = GPG.engine_check_version (proto);
	Context.set_locale (null, LocaleCategory.CTYPE, Intl.setlocale (LocaleCategory.CTYPE, null));

	check_gpg_err (err);
}

private bool write_gpg_config_to_homedir (string homedir) {
	bool ret;
	string gpgconf_fname = Path.build_filename (homedir, "gpg.conf", null);

	if (FileUtils.test (gpgconf_fname, FileTest.EXISTS))
		return true;

	if (!Utils.is_root ())
		return false;

	string config = "# Options for GnuPG used by Listaller \n\n" +
			"no-greeting\n" +
			"no-permission-warning\n" +
			"lock-never\n" +
			"keyserver-options timeout=10\n\n" +
			"keyserver hkp://keys.gnupg.net\n" +
			"#keyserver hkp://keyring.debian.org\n\n" +
			"keyserver-options auto-key-retrieve\n";
	ret = save_string_to_file (gpgconf_fname, config);

	return ret;
}

internal static bool check_gpg_err (GPGError.ErrorCode err, bool show_error = true) {
	if (err != GPGError.ErrorCode.NO_ERROR) {
		string msg = "GPGError: %s".printf (err.to_string ());
		if (show_error)
			warning (msg);
		else
			debug (msg);

		return false;
	}

	return true;
}

internal GPGError.ErrorCode new_context (out Context ctx, string? home_dir = null) {
	GPGError.ErrorCode err;

	string home = home_dir;
	if (home_dir == null)
		home = Config.keyring_dir;

	debug ("GPG home dir: %s", home);

	err = Context.Context (out ctx);
	ctx.set_protocol (Protocol.OpenPGP);
	ctx.set_engine_info (Protocol.OpenPGP, null, home);

	return err;
}

private void set_context_local (Context ctx) {
	GPGError.ErrorCode err;
	KeylistMode mode = ctx.get_keylist_mode ();

	mode &= ~KeylistMode.EXTERN;
	mode |= KeylistMode.LOCAL;
	err = ctx.set_keylist_mode (mode);
	check_gpg_err (err);
}

private void set_context_external (Context ctx) {
	GPGError.ErrorCode err;
	KeylistMode mode = ctx.get_keylist_mode ();

	mode &= ~KeylistMode.LOCAL;
	mode |= KeylistMode.EXTERN;
	err = ctx.set_keylist_mode (mode);
	check_gpg_err (err);
}

private string free_data_to_string (Data **dt) {
	string sig_data;
	size_t signature_len;

	sig_data = (*dt)->release_and_get_mem (out signature_len);
	if (sig_data == null) {
		critical ("Signature data was NULL!");
		sig_data = "";
	}
	return sig_data;
}

private void _dbg_print_data (Data dh) {
	const uint BUF_SIZE = 512;
	char buf[513];
	Posix.off_t ret;

	stdout.printf ("## GPGMe DataObj data: BEGIN\n");

	ret = dh.seek (0, Posix.SEEK_SET);

	while ((ret = dh.read (buf, BUF_SIZE)) > 0)
		stdout.printf ((string) buf, ret, 1);

	stdout.printf ("\n## GPGMe DataObj data: END\n");
}

private bool _dbg_save_data (Data dh, string fname, bool overrideExisting = false) throws IOError {
	var file = File.new_for_path (fname);
	if ( (!overrideExisting) && (file.query_exists ()))
		return false;

	int fd = Posix.open (fname, Posix.O_CREAT | Posix.O_WRONLY | Posix.O_TRUNC,
				Posix.S_IRUSR | Posix.S_IWUSR | Posix.S_IRGRP | Posix.S_IROTH);

	const uint BUF_SIZE = 512;
	uint8 buf[513];

	dh.seek (0, Posix.SEEK_SET);
	ssize_t len = dh.read (buf, BUF_SIZE);
	while (len > 0) {
		Posix.write (fd, buf, len);
		len = dh.read (buf, BUF_SIZE);
	}
	Posix.close (fd);

	return true;
}

private int gpgme_data_write_all (Data data, void* buffer, size_t len) {
	uchar *text = (uchar*)buffer;
	ssize_t written = 0;

	if (len < 0)
		len = Posix.strlen ((string)text);

	while (len > 0) {
		written = data.write ((void*)text, len);
		if (written < 0) {
			if (errno == Posix.EAGAIN || errno == Posix.EINTR)
				continue;
			return -1;
	}

		len -= written;
		text += written;
	}

	return (int) written;
}

private bool read_file_to_data (string? fname, ref Data dt) {
	if (fname == null) {
		return false;
	}

	const uint BUFFER_SIZE = 512;
	//dt.set_encoding (DataEncoding.BINARY);

	var file = File.new_for_path (fname);
	FileInputStream fs;
	try {
		fs = file.read ();
	} catch (Error e) {
		warning ("Unable to read file %s: %s", fname, e.message);
		return false;
	}

	// Seek and read the image data chunk
	uint8[] buffer = new uint8[BUFFER_SIZE];
	//fs.seek (0, SeekType.SET);

	size_t bytesRead;
	while (fs.read_all (buffer, out bytesRead)) {
		if (bytesRead <=0)
			break;

		if (gpgme_data_write_all (dt, buffer, bytesRead) < 0)
			return false;
	}

	return true;
}

private bool read_string_to_data (string str_data, ref Data dt) {
	if (gpgme_data_write_all (dt, str_data, str_data.length) < 0)
		return false;
	return true;
}

} // End of namespace: Listaller.GPGExtra
