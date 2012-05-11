# CMakeLists for Lipa tool
project(listaller-lipa)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(LIPA_SOURCES main.vala
		../toolshared.vala
		../console-progress-bar.vala
		lipa-module.vala
		lipa-installer.vala
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIRS}
		    ${GIO2_INCLUDE_DIR}
		    ${CMAKE_BINARY_DIR}
		    ${CMAKE_BINARY_DIR}/src
)

vala_add_executable(lipa ${LIPA_SOURCES}
			PACKAGES config gee-1.0 gio-2.0 posix listaller-${LILIBS_VERSION}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
)

add_dependencies(lipa_precompile listaller-glib tools-shared)
add_dependencies(lipa_precompile ${CMAKE_BINARY_DIR}/src/listaller-${LILIBS_VERSION}.vapi)

target_link_libraries(lipa
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		listaller-glib
)

install(TARGETS lipa DESTINATION bin)
