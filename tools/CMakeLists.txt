# CMakeLists for Listaller Tools Directory

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
find_package(GPGMe REQUIRED)
find_library(M_LIB m)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)
pkg_check_modules(REDLAND REQUIRED redland>=0.4)
pkg_check_modules(PACKAGEKIT REQUIRED packagekit-glib2>=0.8.4)

set(LI_TOOLS_SHARED_SOURCES toolshared.vala
			console-progress-bar.vala
			reporting.vala
)

vala_precompile(tools-shared LI_TOOLS_SHARED_C ${LI_TOOLS_SHARED_SOURCES}
			PACKAGES config gee-1.0 gio-2.0 posix appstream listaller-${LILIBS_VERSION}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
)

# prepare workspace for tool compilation
include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_BINARY_DIR}/src
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIRS}
		    ${GIO2_INCLUDE_DIR}
		    ${SQLITE_INCLUDE_DIR}
		    ${LibArchive_INCLUDE_DIR}
		    ${LIBXML2_INCLUDE_DIR}
		    ${GPGME_INCLUDES}
		    ${APPSTREAM_INCLUDE_DIRS}
		    ${PACKAGEKIT_INCLUDE_DIRS}
)

add_definitions("-DI_KNOW_THE_PACKAGEKIT_GLIB2_API_IS_SUBJECT_TO_CHANGE")

add_subdirectory(runapp)
add_subdirectory(lipa)
add_subdirectory(likey)
if(TOOLS)
	add_subdirectory(depscan)
	add_subdirectory(lipkgen)
#	add_subdirectory(lilith)
	add_subdirectory(appcompile)
	add_subdirectory(lirepo)
endif(TOOLS)
