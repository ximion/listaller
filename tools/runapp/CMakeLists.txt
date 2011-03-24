# CMakeLists for RunApp tool

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(RUNAPP_SOURCES runapp.vala
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

vala_add_executable(runapp ${RUNAPP_SOURCES})

install(TARGETS runapp DESTINATION bin)
