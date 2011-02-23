[CCode (prefix = "", lower_case_cprefix = "", cheader_filename = "config.h")]
namespace PkgConfig
{
	/* Package information */
	public const string PACKAGE_NAME;
	public const string PACKAGE_STRING;
	public const string PACKAGE_VERSION;
	public const string VERSION;

	/* Gettext package */
	public const string GETTEXT_PACKAGE;

	/* Configured paths */
	public const string LOCALEDIR;  /* /usr/local/share/locale  */
	public const string PKGDATADIR; /* /usr/local/share/listaller */
	public const string PKGLIBDIR;  /* /usr/local/lib/listaller   */
}
