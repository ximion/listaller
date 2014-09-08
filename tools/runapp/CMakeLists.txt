# CMakeLists for RunApp tool
project(listaller-runapp)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-0.8)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(RUNAPP_SOURCES runapp.vala
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIRS}
		    ${GIO2_INCLUDE_DIR}
		    ${APPSTREAM_INCLUDE_DIRS}
		    ${CMAKE_BINARY_DIR}
		    ${CMAKE_BINARY_DIR}/src
)

vala_add_executable(runapp ${RUNAPP_SOURCES}
			PACKAGES config gee-0.8 gio-2.0 posix appstream listaller-${LILIBS_VERSION}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)

add_dependencies(runapp_precompile listaller-glib)
add_dependencies(runapp_precompile ${CMAKE_BINARY_DIR}/src/listaller-${LILIBS_VERSION}.vapi)

target_link_libraries(runapp
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${APPSTREAM_LIBRARIES}
		listaller-glib
)

install(TARGETS runapp DESTINATION bin)
