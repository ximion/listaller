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
