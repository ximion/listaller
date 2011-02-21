# CMakeLists for Listaller's translations

find_package(Gettext REQUIRED)
if (NOT GETTEXT_MSGMERGE_EXECUTABLE)
MESSAGE(FATAL_ERROR "Please install msgmerge binary")
endif (NOT GETTEXT_MSGMERGE_EXECUTABLE)
if (NOT GETTEXT_MSGFMT_EXECUTABLE)
MESSAGE(FATAL_ERROR "Please install msgmerge binary")
endif (NOT GETTEXT_MSGFMT_EXECUTABLE)

set(LOCALE_INSTALL_DIR "share/locale" CACHE PATH "The install dir for translations")

add_subdirectory(listaller)
if(FRONTENDS)
  add_subdirectory(frontends)
endif(FRONTENDS)

# All available domains.
set(DOMAINS
	listaller
)

# If a source file has not GETTEXT_DOMAIN it belongs to this domain.
set(DEFAULT_DOMAIN listaller)

############ Validation. ###########

find_program(GETTEXT_MSGINIT_EXECUTABLE msginit)
if(NOT GETTEXT_MSGINIT_EXECUTABLE)
      message(FATAL_ERROR "msginit not found")
endif(NOT GETTEXT_MSGINIT_EXECUTABLE)

find_program(GETTEXT_XGETTEXT_EXECUTABLE xgettext)
if(NOT GETTEXT_XGETTEXT_EXECUTABLE )
      message(FATAL_ERROR "xgettext not found")
endif(NOT GETTEXT_XGETTEXT_EXECUTABLE )

find_program(GETTEXT_MSGCAT_EXECUTABLE msgcat)
if(NOT GETTEXT_MSGCAT_EXECUTABLE )
      message(FATAL_ERROR "msgcat not found")
endif(NOT GETTEXT_MSGCAT_EXECUTABLE )

find_program(GETTEXT_MSGATTRIB_EXECUTABLE msgattrib)
if(NOT GETTEXT_MSGATTRIB_EXECUTABLE)
      message(FATAL_ERROR "msgattrib not found")
endif(NOT GETTEXT_MSGATTRIB_EXECUTABLE)
