# CMakeLists for Listaller PackageKit PlugIn

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)
pkg_check_modules(PACKAGEKIT REQUIRED packagekit-glib2)
#find_gir(PKGIR "PackageKitGlib-1.0" REQUIRED)

set(PK_LISTALLER_SOURCES pk-listaller.c
		pk-plugin-listaller.c
)

set(PK_LISTALLER_HEADERS pk-listaller.h
)

add_library(pk_plugin_listaller ${PK_LISTALLER_SOURCES})

target_link_libraries(pk_plugin_listaller
		${M_LIB}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${PACKAGEKIT_LIBRARIES}
		listaller-glib
)

include_directories(${CMAKE_BINARY_DIR}
		${CMAKE_BINARY_DIR}/src
		${CMAKE_CURRENT_SOURCE_DIR}
		${GLIB2_INCLUDE_DIR}
		${GEE_INCLUDE_DIRS}
		${GIO2_INCLUDE_DIR}
		${PACKAGEKIT_INCLUDE_DIRS}
)

if(NOT LILIBS_FULL_VERSION)
 message(FATAL_ERROR "Library version not defined. (This should never happen)")
endif()
set_target_properties(pk_plugin_listaller PROPERTIES VERSION ${LILIBS_FULL_VERSION} SOVERSION ${LILIBS_SOVERSION})
