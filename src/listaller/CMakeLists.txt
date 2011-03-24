# CMakeLists for LibListaller-GLib

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
find_package(LibXml2 REQUIRED)
find_package(GPGme REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(LISTALLER_GLIB_SOURCES ${LISTALLER_BASE_SOURCES}
	manager.vala
	installer.vala
)

list(APPEND VALA_INCLUDE_DIRS ${CMAKE_BINARY_DIR})

vala_add_library(listaller-glib SHARED ${LISTALLER_GLIB_SOURCES}
			     BASE_DIR ${lisrc}
			     VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
			     PACKAGES ${LISTALLER_BASE_VALA_DEPS} #PackageKitGlib-1.0
			     GENERATE_VAPI listaller.vapi
			     GENERATE_HEADER listaller.h
			     OUTPUT_DIR ${CMAKE_BINARY_DIR}/src
)

target_link_libraries(listaller-glib
	       ${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
	       ${SQLITE_LIBRARIES}
	       ${LibArchive_LIBRARIES}
	       ${LIBXML2_LIBRARIES}
	       ${GPGME_VANILLA_LIBRARIES}
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
)

if(NOT LILIBS_FULL_VERSION)
 message(FATAL_ERROR "Library version not defined. (This should never happen)")
endif()
set_target_properties(listaller-glib PROPERTIES VERSION ${LILIBS_FULL_VERSION} SOVERSION ${LILIBS_SOVERSION})

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
