/* libgpgme.vapi
 *
 * Copyright (C) 2009 Sebastian Reichel <sre@ring0.de>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

/**
 * GPGME is an API wrapper around GnuPG, which parses the output of GnuPG.
 */
[CCode (lower_case_cprefix = "gpgme_", cheader_filename = "gpgme.h")]
namespace GPG {
	/**
	 * EngineInfo as List
	 */
	[CCode (cname = "struct _gpgme_engine_info")]
	public struct EngineInfo {
		/**
		 * Next entry in the list
		 */
		EngineInfo* next;

		/**
		 * The protocol ID
		 */
		Protocol protocol;

		/**
		 * filename of the engine binary
		 */
		string filename;

		/**
		 * version string of the installed binary
		 */
		string version;

		/**
		 * minimum version required for gpgme
		 */
		string req_version;

		/**
		 * home directory to be used or null for default
		 */
		string? home;
	}

	/**
	 * A Key from the Keyring
	 */
	[CCode (cname = "struct _gpgme_key", ref_function = "gpgme_key_ref", ref_function_void = true, unref_function = "gpgme_key_unref", free_function = "gpgme_key_release")]
	public class Key {
		public bool revoked;
		public bool expired;
		public bool disabled;
		public bool invalid;
		public bool can_encrypt;
		public bool can_sign;
		public bool can_certify;
		public bool secret;
		public bool can_authenticate;
		public bool is_qualified;
		public Protocol protocol;

		/**
		 * If protocol is CMS, this string contains the issuer's serial
		 */
		public string issuer_serial;

		/**
		 * If protocol is CMS, this string contains the issuer's name
		 */
		public string issuer_name;

		/**
		 * If protocol is CMS, this string contains the issuer's ID
		 */
		public string issuer_id;

		/**
		 * If protocol is OpenPGP, this field contains the owner trust level
		 */
		public Validity owner_trust;

		/**
		 * The key's subkeys
		 */
		public SubKey* subkeys;

		/**
		 * The key's user ids
		 */
		public UserID* uids;

		public KeylistMode keylist_mode;
	}

	/**
	 * A signature notation
	 */
	[CCode (cname = "struct _gpgme_sig_notation")]
	public struct SigNotation {
		/**
		 * The next SigNotation from the list
		 */
		SigNotation* next;

		/**
		 * If name is a null pointer value contains a policy url rather than a notation
		 */
		string? name;

		/**
		 * The value of the notation data
		 */
		string value;

		/**
		 * The length of the name of the notation data
		 */
		int name_len;

		/**
		 * The length of the value of the notation data
		 */
		int value_len;

		/**
		 * The accumulated flags
		 */
		SigNotationFlags flags;

		/**
		 * notation data is human readable
		 */
		bool human_readable;

		/**
		 * notation data is critical
		 */
		bool critical;
	}

	/**
	 * A subkey from a Key
	 */
	[CCode (cname = "struct _gpgme_subkey")]
	public struct SubKey {
		SubKey* next;
		bool revoked;
		bool expired;
		bool disabled;
		bool invalid;
		bool can_encrypt;
		bool can_sign;
		bool can_certify;
		bool secret;
		bool can_authenticate;
		bool is_qualified;
		bool is_cardkey;
		PublicKeyAlgorithm algo;
		uint length;
		string keyid;

		/**
		 * Fingerprint of the key in hex form
		 */
		string fpr;

		/**
		 * The creation timestamp.
		 * -1 = invalid,
		 * 0 = not available
		 */
		long timestamp;

		/**
		 * The expiration timestamp.
		 * 0 = key does not expire
		 */
		long expires;

		/**
		 * The serial number of the smartcard holding this key or null
		 */
		string? cardnumber;
	}

	/**
	 * A signature on a UserID
	 */
	[CCode (cname = "struct _gpgme_key_sig")]
	public struct KeySig {
		/**
		 * The next signature from the list
		 */
		KeySig* next;
		bool invoked;
		bool expired;
		bool invalid;
		bool exportable;
		PublicKeyAlgorithm algo;
		string keyid;

		/**
		 * The creation timestamp.
		 * -1 = invalid,
		 * 0 = not available
		 */
		long timestamp;

		/**
		 * The expiration timestamp.
		 * 0 = key does not expire
		 */
		long expires;

		GPGError.ErrorCode status;

		string uid;
		string name;
		string email;
		string comment;

		/**
		 * Crypto backend specific signature class
		 */
		uint sig_class;

		SigNotation *notations;
	}

	/**
	 * A UserID from a Key
	 */
	[CCode (cname = "struct _gpgme_user_id")]
	public struct UserID {
		/**
		 * The next UserID from the list
		 */
		UserID* next;

		bool revoked;
		bool invalid;
		Validity validity;
		string uid;
		string name;
		string email;
		string comment;

		KeySig signatures;
	}

	/**
	 * verify result of OP
	 */
	[CCode (cname = "struct _gpgme_op_verify_result")]
	public struct VerifyResult {
		Signature* signatures;

		/**
		 * The original file name of the plaintext message, if available
		 */
		string? file_name;
	}

	/**
	 * sign result of OP
	 */
	[CCode (cname = "struct _gpgme_op_sign_result")]
	public struct SignResult {
		InvalidKey* invalid_signers;
		SignatureNew* signatures;
	}

	/**
	 * encrypt result of OP
	 */
	[CCode (cname = "struct _gpgme_op_encrypt_result")]
	public struct EncryptResult {
		/**
		 * The list of invalid repipients
		 */
		InvalidKey invalid_signers;
	}

	/**
	 * decrypt result of OP
	 */
	[CCode (cname = "struct _gpgme_op_decrypt_result")]
	public struct DecryptResult {
		string unsupported_algorithm;
		bool wrong_key_usage;
		Recipient recipients;
		string filename;
	}

	/**
	 * An receipient
	 */
	[CCode (cname = "struct _gpgme_recipient")]
	public struct Recipient {
		Recipient *next;
		string keyid;
		PublicKeyAlgorithm pubkey_algo;
		GPGError.ErrorCode status;
	}

	/**
	 * list of invalid keys
	 */
	[CCode (cname = "struct _gpgme_invalid_key")]
	public struct InvalidKey {
		InvalidKey *next;
		string fpr;
		GPGError.ErrorCode reason;
	}

	/**
	* A Signature
	*/
	[CCode (cname = "struct _gpgme_signature")]
	public struct Signature {
		/**
		* The next signature in the list
		*/
		Signature *next;

		/**
		* A summary of the signature status
		*/
		Sigsum summary;

		/**
		* Fingerprint or key ID of the signature
		*/
		string fpr;

		/**
		* The Error status of the signature
		*/
		GPGError.ErrorCode status;

		/**
		* Notation data and policy URLs
		*/
		SigNotation *notations;

		/**
		* Signature creation time
		*/
		ulong timestamp;

		/**
		* Signature expiration time or 0
		*/
		ulong exp_timestamp;

		/**
		* Key should not have been used for signing
		*/
		bool wrong_key_usage;

		/**
		* PKA status
		*/
		uint pka_trust;

		/**
		* Validity has been verified using the chain model
		*/
		bool chain_model;

		/**
		* Validity
		*/
		Validity validity;

		/**
		* Validity reason
		*/
		GPGError.ErrorCode validity_reason;

		/**
		* public key algorithm used to create the signature
		*/
		PublicKeyAlgorithm pubkey_algo;

		/**
		* The hash algorithm used to create the signature
		*/
		HashAlgorithm hash_algo;

		/**
		* The mailbox from the PKA information or null
		*/
		string? pka_address;
	}
	/**
	 * A Signature
	 */
	[CCode (cname = "struct _gpgme_new_signature")]
	public struct SignatureNew {
		/**
		 * The next signature in the list
		 */
		SignatureNew *next;

		SigMode type;

		/**
		 * Fingerprint or key ID of the signature
		 */
		string fpr;

		/**
		 * Notation data and policy URLs
		 */
		SigNotation *notations;

		/**
		 * Signature creation time
		 */
		ulong timestamp;

		/**
		 * Signature expiration time or 0
		 */
		ulong exp_timestamp;

		/**
		 * PKA status
		 */
		PKAStatus pka_trust;

		/**
		 * Validity has been verified using the chain model
		 */
		bool chain_model;

		/**
		 * Validity reason
		 */
		GPGError.ErrorCode validity_reason;

		/**
		 * public key algorithm used to create the signature
		 */
		PublicKeyAlgorithm pubkey_algo;

		/**
		 * The hash algorithm used to create the signature
		 */
		HashAlgorithm hash_algo;

		/**
		 * Crypto backend specific signature class.
		 */
		uint sig_class;
	}

	/**
	 * PKA Status
	 */
	public enum PKAStatus {
		NOT_AVAILABLE,
		BAD,
		OKAY,
		RFU
	}

	/**
	 * Flags used for the summary field in a Signature
	 */
	[CCode (cname = "gpgme_sigsum_t", cprefix = "GPGME_SIGSUM_")]
	public enum Sigsum {
		/**
		 * The signature is fully valid
		 */
		VALID       = 0x0001,

		/**
		 * The signature is good
		 */
		GREEN       = 0x0002,

		/**
		 * The signature is bad
		 */
		RED         = 0x0004,

		/**
		 * One key has been revoked
		 */
		KEY_REVOKED = 0x0010,

		/**
		 * One key has expired
		 */
		KEY_EXPIRED = 0x0020,

		/**
		 * The signature has expired
		 */
		SIG_EXPIRED = 0x0040,

		/**
		 * Can't verfiy - missing key
		 */
		KEY_MISSING = 0x0080,

		/**
		 * CRL not available
		 */
		CRL_MISSING = 0x0100,

		/**
		 * Available CRL is too old
		 */
		CRL_TOO_OLD = 0x0200,

		/**
		 * A policy was not met
		 */
		BAD_POLICY  = 0x0400,

		/**
		 * A system error occured
		 */
		SYS_ERROR   = 0x0800;
	}

	/**
	 * Encoding modes of Data objects
	 */
	[CCode (cname = "gpgme_data_encoding_t", cprefix = "GPGME_DATA_ENCODING_")]
	public enum DataEncoding {
		/**
		 * Not specified
		 */
		NONE,
		/**
		 * Binary encoded
		 */
		BINARY,
		/**
		 * Base64 encoded
		 */
		BASE64,
		/**
		 * Either PEM or OpenPGP Armor
		 */
		ARMOR,
		/**
		 * LF delimited URL list
		 */
		URL,
		/**
		 * LF percent escaped, delimited URL list
		 */
		URLESC,
		/**
		 * Nul determined URL list
		 */
		URL0
	}

	/**
	 * Public Key Algorithms from libgcrypt
	 */
	[CCode (cname = "gpgme_pubkey_algo_t", cprefix = "GPGME_PK_")]
	public enum PublicKeyAlgorithm {
		RSA,
		RSA_E,
		RSA_S,
		ELG_E,
		DSA,
		ELG
	}

	/**
	 * Hash Algorithms from libgcrypt
	 */
	[CCode (cname = "gpgme_hash_algo_t", cprefix = "GPGME_MD_")]
	public enum HashAlgorithm {
		NONE,
		MD5,
		SHA1,
		RMD160,
		MD2,
		TIGER,
		HAVAL,
		SHA256,
		SHA384,
		SHA512,
		MD4,
		MD_CRC32,
		MD_CRC32_RFC1510,
		MD_CRC24_RFC2440
	}

	/**
	 * Signature modes
	 */
	[CCode (cname = "gpgme_sig_mode_t", cprefix = "GPGME_SIG_MODE_")]
	public enum SigMode {
		NORMAL,
		DETACH,
		CLEAR
	}

	/**
	 * Validities for a trust item or key
	 */
	[CCode (cname = "gpgme_validity_t", cprefix = "GPGME_VALIDITY_")]
	public enum Validity {
		UNKNOWN,
		UNDEFINED,
		NEVER,
		MARGINAL,
		FULL,
		ULTIMATE
	}

	/**
	 * Protocols
	 */
	[CCode (cname = "gpgme_protocol_t", cprefix = "GPGME_PROTOCOL_")]
	public enum Protocol {
		/**
		 * Default Mode
		 */
		OpenPGP,
		/**
		 * Cryptographic Message Syntax
		 */
		CMS,
		/**
		 * Special code for gpgconf
		 */
		GPGCONF,
		/**
		 * Low-level access to an Assuan server
		 */
		ASSUAN,
		UNKNOWN
	}

	/**
	 * Keylist modes used by Context
	 */
	[CCode (cname = "gpgme_keylist_mode_t", cprefix = "GPGME_KEYLIST_MODE_")]
	public enum KeylistMode {
		LOCAL,
		EXTERN,
		SIGS,
		SIG_NOTATIONS,
		EPHEMERAL,
		VALIDATE
	}

	/**
	 * Export modes used by Context
	 */
	[CCode (cname = "gpgme_export_mode_t", cprefix = "GPGME_EXPORT_MODE_")]
	public enum ExportMode {
		EXTERN
	}

	/**
	 * Audit log function flags
	 */
	[CCode (cprefix = "GPGME_AUDITLOG_")]
	public enum AuditLogFlag {
		HTML,
		WITH_HELP
	}

	/**
	 * Signature notation flags
	 */
	[CCode (cname = "gpgme_sig_notation_flags_t", cprefix = "GPGME_SIG_NOTATION_")]
	public enum SigNotationFlags {
		HUMAN_READABLE,
		CRITICAL
	}

	/**
	 * Encryption Flags
	 */
	[CCode (cname = "gpgme_encrypt_flags_t", cprefix = "GPGME_ENCRYPT_")]
	public enum EncryptFlags {
		ALWAYS_TRUST,
		NO_ENCRYPT_TO
	}

	/**
	 * Edit Operation Stati
	 */
	[CCode (cname = "gpgme_status_code_t", cprefix = "GPGME_STATUS_")]
	public enum StatusCode {
		EOF,
		ENTER,
		LEAVE,
		ABORT,
		GOODSIG,
		BADSIG,
		ERRSIG,
		BADARMOR,
		RSA_OR_IDEA,
		KEYEXPIRED,
		KEYREVOKED,
		TRUST_UNDEFINED,
		TRUST_NEVER,
		TRUST_MARGINAL,
		TRUST_FULLY,
		TRUST_ULTIMATE,
		SHM_INFO,
		SHM_GET,
		SHM_GET_BOOL,
		SHM_GET_HIDDEN,
		NEED_PASSPHRASE,
		VALIDSIG,
		SIG_ID,
		SIG_TO,
		ENC_TO,
		NODATA,
		BAD_PASSPHRASE,
		NO_PUBKEY,
		NO_SECKEY,
		NEED_PASSPHRASE_SYM,
		DECRYPTION_FAILED,
		DECRYPTION_OKAY,
		MISSING_PASSPHRASE,
		GOOD_PASSPHRASE,
		GOODMDC,
		BADMDC,
		ERRMDC,
		IMPORTED,
		IMPORT_OK,
		IMPORT_PROBLEM,
		IMPORT_RES,
		FILE_START,
		FILE_DONE,
		FILE_ERROR,
		BEGIN_DECRYPTION,
		END_DECRYPTION,
		BEGIN_ENCRYPTION,
		END_ENCRYPTION,
		DELETE_PROBLEM,
		GET_BOOL,
		GET_LINE,
		GET_HIDDEN,
		GOT_IT,
		PROGRESS,
		SIG_CREATED,
		SESSION_KEY,
		NOTATION_NAME,
		NOTATION_DATA,
		POLICY_URL,
		BEGIN_STREAM,
		END_STREAM,
		KEY_CREATED,
		USERID_HINT,
		UNEXPECTED,
		INV_RECP,
		NO_RECP,
		ALREADY_SIGNED,
		SIGEXPIRED,
		EXPSIG,
		EXPKEYSIG,
		TRUNCATED,
		ERROR,
		NEWSIG,
		REVKEYSIG,
		SIG_SUBPACKET,
		NEED_PASSPHRASE_PIN,
		SC_OP_FAILURE,
		SC_OP_SUCCESS,
		CARDCTRL,
		BACKUP_KEY_CREATED,
		PKA_TRUST_BAD,
		PKA_TRUST_GOOD,
		PLAINTEXT
	}

	/**
	 * The Context object represents a GPG instance
	 */
	[Compact]
	[CCode (cname = "struct gpgme_context", free_function = "gpgme_release", cprefix = "gpgme_")]
	public class Context {
		/**
		 * Create a new context, returns Error Status Code
		 */
		[CCode (cname = "gpgme_new")]
		public static GPGError.ErrorCode Context(out Context ctx);

		public GPGError.ErrorCode set_protocol(Protocol p);
		public Protocol get_protocol();

		public void set_armor(bool yes);
		public bool get_armor();

		public void set_textmode(bool yes);
		public bool get_textmode();

		public GPGError.ErrorCode set_keylist_mode(KeylistMode mode);
		public KeylistMode get_keylist_mode();

		/**
		 * Include up to nr_of_certs certificates in an S/MIME message,
		 * Use "-256" to use the backend's default.
		 */
		public void set_include_certs(int nr_of_certs = -256);

		/**
		 * Return the number of certs to include in an S/MIME message
		 */
		public int get_include_certs();

		/**
		 * Set callback function for requesting passphrase. hook_value will be
		 * passed as first argument.
		 */
		public void set_passphrase_cb(passphrase_callback cb, void* hook_value = null);

		/**
		 * Get callback function and hook_value
		 */
		public void get_passphrase_cb(out passphrase_callback cb, out void* hook_value);

		public static GPGError.ErrorCode set_locale(Context ctx, int category, string val);

		/**
		 * Get information about the configured engines. The returned data is valid
		 * until the next set_engine_info() call.
		 */
		[CCode (cname = "gpgme_ctx_get_engine_info")]
		public EngineInfo* get_engine_info();

		[CCode (cname = "gpgme_ctx_set_engine_info")]
		public GPGError.ErrorCode set_engine_info(Protocol proto, string? file_name, string? home_dir);

		/**
		 * Delete all signers
		 */
		public void signers_clear();

		/**
		 * Add key to list of signers
		 */
		public GPGError.ErrorCode signers_add(Key key);

		/**
		 * Get the n-th signer's key
		 */
		public Key* signers_enum(int n);

		/**
		 * Clear all notation data
		 */
		public void sig_notation_clear();

		/**
		 * Add human readable notation data. If name is null,
		 * then value val should be a policy URL. The HUMAN_READABLE
		 * flag is forced to be true for notation data and false
		 * for policy URLs.
		 */
		public GPGError.ErrorCode sig_notation_add(string name, string val, SigNotationFlags flags);

		/**
		 * Get sig notations
		 */
		public SigNotation* sig_notation_get();

		/**
		 * Get key with the fingerprint FPR from the crypto backend.
		 * If SECRET is true, get the secret key.
		 */
		public GPGError.ErrorCode get_key (string fpr, out Key key, bool secret);

		/**
		 * process the pending operation and, if hang is true, wait for
		 * the pending operation to finish.
		 */
		public Context* wait(out GPGError.ErrorCode status, bool hang);

		/**
		 * Retrieve a pointer to the results of the signing operation
		 */
		public SignResult* op_sign_result();

		/**
		 * Sign the plaintext PLAIN and store the signature in SIG.
		 */
		public GPGError.ErrorCode op_sign(Data plain, Data sig, SigMode mode);

		/**
		 * Retrieve a pointer to the result of the verify operation
		 */
		public VerifyResult* op_verify_result();

		/**
		 * Verify that SIG is a valid signature for SIGNED_TEXT.
		 */
		public GPGError.ErrorCode op_verify(Data sig, Data? signed_text, Data? plaintext);

		/**
		 * Retrieve a pointer to the result of the encrypt operation
		 */
		public EncryptResult* op_encrypt_result();

		/**
		 * Encrypt plaintext PLAIN for the recipients RECP and store the
		 * resulting ciphertext in CIPHER.
		 */
		public GPGError.ErrorCode op_encrypt([CCode (array_length = false)] Key[] recp, EncryptFlags flags, Data plain, Data cipher);

		/**
		 * Retrieve a pointer to the result of the decrypt operation
		 */
		public DecryptResult* op_decrypt_result();

		/**
		 * Decrypt ciphertext CIPHER and store the resulting plaintext
		 * in PLAIN.
		 */
		public GPGError.ErrorCode op_decrypt(Data cipher, Data plain);

		/**
		 * Export the keys found by PATTERN into KEYDATA. If PATTERN is
		 * NULL all keys will be exported.
		 */
		public GPGError.ErrorCode op_export(string? pattern, ExportMode mode, Data keydata);

		/** Import the keys from the array KEYS into the keyring.  */
		public GPGError.ErrorCode op_import_keys_start ([CCode (array_length = false)] Key[] keys);
		public GPGError.ErrorCode op_import_keys ([CCode (array_length = false)] Key[] keys);


	}

	/**
	 * Data Object, contains encrypted and/or unencrypted data
	 */
	[Compact]
	[CCode (cname = "struct gpgme_data", free_function = "gpgme_data_release", cprefix = "gpgme_data_")]
	public class Data {
		/**
		 * Create a new data buffer, returns Error Status Code.
		 */
		[CCode (cname = "gpgme_data_new")]
		public static GPGError.ErrorCode create(out Data d);

		/**
		 * Create a new data buffer filled with SIZE bytes starting
		 * from BUFFER. If COPY is false, COPYING is delayed until
		 * necessary and the data is taken from the original location
		 * when needed. Returns Error Status Code.
		 */
		[CCode (cname = "gpgme_data_new_from_mem")]
		public static GPGError.ErrorCode create_from_memory (out Data d, char* buffer, size_t size, bool copy);

		[CCode (cname = "gpgme_data_new_from_file")]
		public static GPGError.ErrorCode create_from_file (out Data d, string fname, bool copy);

		[CCode (cname = "gpgme_data_new_from_stream")]
		public static GPGError.ErrorCode create_from_stream (out Data d, Posix.FILE stream);

		[CCode (cname = "gpgme_data_new_from_fd")]
		public static GPGError.ErrorCode create_from_fd (out Data d, int fd);

		/**
		 * Destroy the object and return a pointer to its content.
		 * It's size is returned in R_LEN.
		 */
		[CCode (cname = "gpgme_data_release_and_get_mem")]
		public string release_and_get_mem(out size_t len);

		/**
		 * Read up to SIZE bytes into buffer BUFFER from the data object.
		 * Return the number of characters read, 0 on EOF and -1 on error.
		 * If an error occurs, errno is set.
		 */
		public ssize_t read(void* buf, size_t size);

		/**
		 * Write up to SIZE bytes from buffer BUFFER to the data object.
		 * Return the number of characters written, or -1 on error.
		 * If an error occurs, errno is set.
		 */
		public ssize_t write(void* buf, size_t size);

		/**
		 * Set the current position from where the next read or write
		 * starts in the data object to OFFSET, relativ to WHENCE.
		 */
		public Posix.off_t seek(long offset, int whence=0);

		/**
		 * Get the encoding attribute of the buffer
		 */
		public DataEncoding *get_encoding();

		/**
		 * Set the encoding attribute of the buffer to ENC
		 */
		public GPGError.ErrorCode set_encoding(DataEncoding enc);
	}

	[CCode (cname = "gpgme_get_protocol_name")]
	public unowned string get_protocol_name(Protocol p);

	[CCode (cname = "gpgme_pubkey_algo_name")]
	public unowned string get_public_key_algorithm_name(PublicKeyAlgorithm algo);

	[CCode (cname = "gpgme_hash_algo_name")]
	public unowned string get_hash_algorithm_name(HashAlgorithm algo);

	[CCode (cname = "gpgme_passphrase_cb_t", has_target = false)]
	public delegate GPGError.ErrorCode passphrase_callback(void* hook, string uid_hint, string passphrase_info, bool prev_was_bad, int fd);

#if 0
	/**
	 * Set the default engine info for the protocol PROTO to the file name
	 * FILE_NAME and the home directory HOME_DIR.
	 */
	[CCode (cname = "gpgme_set_engine_info")]
	public GPGError.ErrorCode set_engine_info (Protocol proto,
							string file_name,
							string? home_dir);
#endif

	/**
	 * Get version of libgpgme
	 * Always call this function before using gpgme, it initializes some stuff
	 */
	[CCode (cname = "gpgme_check_version")]
	public unowned string check_version(string? required_version = null);

	/**
	 * Verify that the engine implementing proto is installed and
	 * available.
	 */
	[CCode (cname = "gpgme_engine_check_version")]
	public GPGError.ErrorCode engine_check_version(Protocol proto);

	/**
	 * Get information about the configured engines. The returned data is valid
	 * until the next set_engine_info() call.
	 */
	[CCode (cname = "gpgme_get_engine_information")]
	public GPGError.ErrorCode get_engine_information(out EngineInfo engine_info);

	/**
	 * Return the error string for ERR in the user-supplied buffer BUF
	 * of size BUFLEN. This function is thread-safe, if a thread-safe
	 * strerror_r() function is provided by the system. If the function
	 * succeeds, 0 is returned and BUF contains the string describing
	 * the error. If the buffer was not large enough, ERANGE is returned
	 * and BUF contains as much of the beginning of the error string as
	 * fits into the buffer. Returns Error Status Code.
	 */
	[CCode (cname = "gpgme_strerror_r")]
	public int strerror_r(GPGError.ErrorCode err, char[] buf);

	/**
	 * Like strerror_r, but returns a pointer to the string. This method
	 * is not thread safe!
	 */
	[CCode (cname = "gpgme_strerror")]
	public unowned string strerror(GPGError.ErrorCode err);
}
