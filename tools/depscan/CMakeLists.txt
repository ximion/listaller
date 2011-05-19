# CMakeLists for Dependency Scanner Tool

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(RUNAPP_SOURCES ${lisrc}/utils.vala
		   main.vala
		   depscan.vala
		   scan_ldd.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
)

add_definitions(${GEE_CFLAGS} ${GEE_CFLAGS_OTHER})

vala_add_executable(depscan ${RUNAPP_SOURCES}
			PACKAGES config glib-2.0 posix gio-2.0 gee-1.0
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi)

install(TARGETS depscan DESTINATION bin)
