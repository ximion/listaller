# CMakeLists for Listaller source

set(lisrc ${CMAKE_SOURCE_DIR}/src
	  CACHE INTERNAL "Listaller sourcecode dir"
)

set(LISTALLER_BASE_SOURCES ${lisrc}/util.vala
	${lisrc}/status.vala
	${lisrc}/settings.vala
	${lisrc}/appitem.vala
	${lisrc}/database.vala
	${lisrc}/varsolver.vala
	${lisrc}/ipkcdef.vala
	${lisrc}/ipkfilelist.vala
	${lisrc}/gpgsign.vala
	${lisrc}/ipkpackage.vala
	${lisrc}/depman.vala
	CACHE INTERNAL "Listaller core source files"
)

set(LISTALLER_BASE_VALA_DEPS config gee-1.0 gio-2.0 sqlite3 libarchive gpgme libxml-2.0
			     CACHE INTERNAL "Essential Vala package dependencies"
)

# Variables used later to set library versioning
set(LILIBS_FULL_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")
set(LILIBS_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}")
set(LILIBS_SOVERSION "${LISTALLER_VERSION_MAJOR}")

add_subdirectory (listaller)
if(QT)
	add_subdirectory(listaller-qt)
endif(QT)
