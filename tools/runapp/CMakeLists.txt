# CMakeLists for RunApp tool
project(listaller-runapp)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(RUNAPP_SOURCES runapp.vala
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIRS}
		    ${GIO2_INCLUDE_DIR}
		    ${CMAKE_BINARY_DIR}/src
)

# We don't use localisation here
remove_definitions ("--include config.h")

vala_add_executable(runapp ${RUNAPP_SOURCES}
			PACKAGES config gee-1.0 gio-2.0 posix listaller-${LILIBS_VERSION}
			OUTPUT_DIR ${CMAKE_BINARY_DIR}/src
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
)

add_dependencies(runapp_precompile listaller-glib)
add_dependencies(runapp_precompile ${CMAKE_BINARY_DIR}/src/listaller-${LILIBS_VERSION}.vapi)

target_link_libraries(runapp
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		listaller-glib
)

install(TARGETS runapp DESTINATION bin)
