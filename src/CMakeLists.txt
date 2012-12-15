# CMakeLists for Listaller-GLib (source)

#
# Some general library & source settings
#
set(lisrc ${CMAKE_SOURCE_DIR}/src
	  CACHE INTERNAL "Listaller sourcecode dir"
)

set(LISTALLER_BASE_SOURCES ${lisrc}/logging.vala
	${lisrc}/utils.vala
	${lisrc}/status.vala
	${lisrc}/message-object.vala
	${lisrc}/config-global.vala
	${lisrc}/reporting.vala
	${lisrc}/downloader.vala
	${lisrc}/setup-settings.vala
	${lisrc}/application.vala
	${lisrc}/database-internal.vala
	${lisrc}/software-db.vala
	${lisrc}/software-cache.vala
	${lisrc}/varsolver.vala
	${lisrc}/varsetter.vala
	${lisrc}/doap.vala
	${lisrc}/ipk-metafile.vala
	${lisrc}/ipk-filelist.vala
	${lisrc}/ipk-control.vala
	${lisrc}/ipk-package.vala
	${lisrc}/gpgex.vala
	${lisrc}/security-info.vala
	${lisrc}/dep-info.vala
	${lisrc}/version.vala
	CACHE INTERNAL "Listaller core source files"
)

set(LISTALLER_BASE_VALA_DEPS config gee-1.0 gio-2.0 sqlite3 libarchive libsoup-2.4 gpgme libxml-2.0 rdf-minimal
			     CACHE INTERNAL "Essential Vala package dependencies"
)

# Variables used later to set library versioning
set(LILIBS_FULL_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")
set(LILIBS_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}" CACHE INTERNAL "int: LiLibs version")

#
# Build Qt4 library if flag is set
#
if(QT)
	message ("The Qt4 interface is outdated and needs to be rewritten.")
	message ("The rewrite was planned for a later release, if a Qt interface is still useful.")
	#add_subdirectory(qt4)
endif(QT)

#
# Listaller-GLib build instructions
#

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 2.23 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
find_package(LibXml2 REQUIRED)
find_package(GPGMe REQUIRED)
find_package(GI REQUIRED)
find_library(M_LIB m)
pkg_check_modules(GEE REQUIRED gee-1.0>=0.6)
pkg_check_modules(GIO2 REQUIRED gio-2.0>=2.26)
pkg_check_modules(REDLAND REQUIRED redland>=0.4)
pkg_check_modules(SOUP REQUIRED libsoup-2.4>=2.40)
pkg_check_modules(PACKAGEKIT REQUIRED packagekit-glib2>=0.8.6)
pkg_check_modules(PACKAGEKIT_PLUGIN REQUIRED packagekit-plugin)
#find_gir(PKGIR "PackageKitGlib-1.0" REQUIRED)

set(LISTALLER_GLIB_SOURCES ${LISTALLER_BASE_SOURCES}
	zfeed.vala
	ipk-repo.vala
	ipk-changelog.vala
	dep-manager.vala
	dep-installer.vala
	pkbackend-glue.vala
	pkit-tasks.vala
	feed-installer.vala
	signature.vala
	key-manager.vala
	repo-utils.vala
	repo-list.vala
	repo-manager.vala
	manager.vala
	installer.vala
	updater.vala
	task.vala
)

list(APPEND VALA_INCLUDE_DIRS ${CMAKE_BINARY_DIR})

vala_add_library(listaller-glib SHARED ${LISTALLER_GLIB_SOURCES}
			BASE_DIR ${lisrc}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2
			GENERATE_VAPI listaller-${LILIBS_VERSION}.vapi
			GENERATE_HEADER listaller.h
			GENERATE_INTERNAL_HEADER listaller_internal.h
			OUTPUT_DIR ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS --gir=Listaller-${LILIBS_VERSION}.gir
				--library Listaller-${LILIBS_VERSION}
				--internal-vapi=listaller_internal.vapi.orig
				--thread
)

find_program (SED_TOOL NAMES sed)
if (NOT SED_TOOL)
	message (FATAL_ERROR "Unable to find sed")
endif (NOT SED_TOOL)

# Workaround to fix Listaller header filename in internal vapi
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/listaller_internal.vapi
	COMMAND ${SED_TOOL}  "'s/\"listaller.h\"/\"listaller_internal.h\"/'" ${CMAKE_CURRENT_BINARY_DIR}/listaller_internal.vapi.orig ">" ${CMAKE_CURRENT_BINARY_DIR}/listaller_internal.vapi
	DEPENDS listaller-glib_precompile ${CMAKE_CURRENT_BINARY_DIR}/listaller_internal.vapi.orig
)
add_custom_target(header_internal_fix DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/listaller_internal.vapi)
add_dependencies (listaller-glib header_internal_fix)

# Compile the introspection data
add_custom_target (gir-compile
	${INTROSPECTION_COMPILER}  "--shared-library=listaller-glib" "--includedir=."
				"${CMAKE_CURRENT_BINARY_DIR}/Listaller-${LILIBS_VERSION}.gir"
				"-o" "${CMAKE_CURRENT_BINARY_DIR}/Listaller-${LILIBS_VERSION}.typelib"
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Listaller-${LILIBS_VERSION}.gir
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

#FIXME: GIR does not compile at time, so we disable this
#add_dependencies (listaller-glib gir-compile)

add_definitions("-DI_KNOW_THE_PACKAGEKIT_GLIB2_API_IS_SUBJECT_TO_CHANGE"
	"-DI_KNOW_THE_PACKAGEKIT_PLUGIN_API_IS_SUBJECT_TO_CHANGE"
)

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"Listaller\"")

target_link_libraries(listaller-glib
		${M_LIB}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${SQLITE_LIBRARIES}
		${LibArchive_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		${SOUP_LIBRARIES}
		${REDLAND_LIBRARIES}
		${PACKAGEKIT_LIBRARIES}
		${PACKAGEKIT_PLUGIN_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		${CMAKE_CURRENT_SOURCE_DIR}
		${GLIB2_INCLUDE_DIR}
		${GEE_INCLUDE_DIRS}
		${GIO2_INCLUDE_DIR}
		${SQLITE_INCLUDE_DIR}
		${LibArchive_INCLUDE_DIR}
		${LIBXML2_INCLUDE_DIR}
		${GPGME_INCLUDES}
		${SOUP_INCLUDE_DIRS}
		${REDLAND_INCLUDE_DIRS}
		${PACKAGEKIT_INCLUDE_DIRS}
		${PACKAGEKIT_PLUGIN_INCLUDE_DIRS}
)

if(NOT LILIBS_FULL_VERSION)
 message(FATAL_ERROR "Library version not defined. (This should never happen)")
endif()
set_target_properties(listaller-glib PROPERTIES VERSION ${LILIBS_FULL_VERSION} SOVERSION ${LISTALLER_API_LEVEL})

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/listaller-glib.pc.in
  ${CMAKE_CURRENT_BINARY_DIR}/listaller-glib.pc
  @ONLY
)

install(TARGETS listaller-glib DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller-glib.pc
	DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
	COMPONENT headers
)
install(FILES ${CMAKE_BINARY_DIR}/src/listaller.h
	DESTINATION include/listaller-glib
	COMPONENT headers
)
install(FILES ${CMAKE_BINARY_DIR}/src/listaller-${LILIBS_VERSION}.vapi
	DESTINATION share/vala/vapi
	COMPONENT headers
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/Listaller-${LILIBS_VERSION}.gir
	DESTINATION share/gir-1.0
	COMPONENT headers
)

#
# Build Listaller PackageKit PlugIn
#
add_subdirectory(pk-plugin)
