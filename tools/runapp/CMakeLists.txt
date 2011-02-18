# CMakeLists for RunApp tool

find_package(Vala REQUIRED)
find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

include(ValaVersion)
include(ValaPrecompile)
ensure_vala_version("0.10" MINIMUM)

set(RUNAPP_SOURCES main.vala
)

vala_precompile(VALA_C ${RUNAPP_SOURCES}
		PACKAGES gio-2.0 gee-1.0 posix
		OPTIONS --thread
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
)
add_definitions(${GEE_CFLAGS} ${GEE_CFLAGS_OTHER})

add_executable("runapp" ${VALA_C})
