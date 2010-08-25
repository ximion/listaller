#CMakeLists for libListaller build

set(pascal_compiler_flags_cmn "-vm5024" "-fPIC" "-dNoGUI")

find_package(Lazbuild REQUIRED)
find_package(GLIB2 REQUIRED)
pkg_check_modules(DBus REQUIRED dbus-1)
pkg_check_modules(PolKitGObject REQUIRED polkit-gobject-1>=0.90)

if(LCL_VERSION LESS "929")
 message(FATAL_ERROR "LCL should be at least version 0.9.29!")
endif()

set(listallhelperd_project listallhelperd.lpr)

set(listallhelperd_base_sources
	${listallhelperd_project}
)

set(listallhelperd_name "Listaller helper daemon")
set(listallhelperd_build_args "-B" "--ws=nogui")

add_custom_command(OUTPUT ${listallhelperd_name}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${listallhelperd_build_args} ${listallhelperd_project}
	MAIN_DEPENDENCY ${listallhelperd_project}
	DEPENDS ${listallhelperd_base_sources}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

add_custom_target(listallhelperd ALL DEPENDS ${listallhelperd_name})
