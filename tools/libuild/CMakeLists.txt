# CMakeLists for Listaller package builder tool
project(listaller-libuild)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
find_package(GPGMe REQUIRED)
find_library(M_LIB m)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)
pkg_check_modules(REDLAND REQUIRED redland>=0.4)

set(LIBUILD_SOURCES ${LISTALLER_BASE_SOURCES}
			lbutils.vala
			prepare.vala
			gpgsign.vala
			depfind.vala
			ipkbuilder.vala
)

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
		    ${REDLAND_INCLUDE_DIRS}
)

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"Builder\"")

# Listaller build tool
vala_add_executable (libuild ${LIBUILD_SOURCES} main.vala
			PACKAGES ${LISTALLER_BASE_VALA_DEPS}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
)

target_link_libraries(libuild
		${M_LIB}
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${SQLITE_LIBRARIES}
		${LibArchive_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		${PACKAGEKIT_LIBRARIES}
		${REDLAND_LIBRARIES}
)

install(TARGETS libuild DESTINATION bin)
