# CMakeLists for LibListaller-GLib

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
find_package(LibXml2 REQUIRED)
find_package(GPGme REQUIRED)
find_package(GI REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)
pkg_check_modules(PACKAGEKIT REQUIRED packagekit-glib2)
#find_gir(PKGIR "PackageKitGlib-1.0" REQUIRED)

set(LISTALLER_GLIB_SOURCES ${LISTALLER_BASE_SOURCES}
	depsolver.vala
	depprovider_pk.vala
	manager.vala
	installer.vala
)

list(APPEND VALA_INCLUDE_DIRS ${CMAKE_BINARY_DIR})

vala_add_library(listaller-glib SHARED ${LISTALLER_GLIB_SOURCES}
			BASE_DIR ${lisrc}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2
			GENERATE_VAPI listaller-${LILIBS_VERSION}.vapi
			GENERATE_HEADER listaller.h
			OUTPUT_DIR ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS --gir=Listaller-${LILIBS_VERSION}.gir
				--library Listaller-${LILIBS_VERSION}
)

add_definitions ("-DI_KNOW_THE_PACKAGEKIT_GLIB2_API_IS_SUBJECT_TO_CHANGE")

target_link_libraries(listaller-glib
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${SQLITE_LIBRARIES}
		${LibArchive_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		${PACKAGEKIT_LIBRARIES}
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
		${PACKAGEKIT_INCLUDE_DIRS}
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
install(FILES ${CMAKE_BINARY_DIR}/src/listaller-${LILIBS_VERSION}.vapi
	DESTINATION share/vala/vapi
	COMPONENT headers
)
