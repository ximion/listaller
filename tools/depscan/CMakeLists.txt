# CMakeLists for Dependency Scanner Tool
project(listaller-depscan)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(DEPSCAN_SOURCES depscan-tool.vala
		   depscan.vala
		   scan_ldd.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
	       ${LIBXML2_LIBRARIES}
	       ${GPGME_VANILLA_LIBRARIES}
	       listaller-glib
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
		    ${APPSTREAM_INCLUDE_DIRS}
)

add_definitions(${GEE_CFLAGS} ${GEE_CFLAGS_OTHER})

vala_add_executable(depscan ${DEPSCAN_SOURCES}
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} listaller_internal packagekit-glib2 appstream
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)

install(TARGETS depscan DESTINATION bin)
