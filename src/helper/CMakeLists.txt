# CMakeLists for Listaller Helper Daemon build

find_package(Lazbuild REQUIRED)
find_package(GLIB2 REQUIRED)
find_package(DBus REQUIRED)
find_package(PolKit REQUIRED)

if(LCL_VERSION LESS "929")
 message(FATAL_ERROR "LCL should be at least version 0.9.29!")
endif()

set(lihelperd_project listallhelperd.lpr)

set(lihelperd_base_sources
	${listallhelperd_project}
)

set(listallhelperd_name "${CMAKE_BINARY_DIR}/listallhelperd")
set(lihelperd_build_args "-B" "--ws=nogui")

add_custom_command(OUTPUT ${listallhelperd_name}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${lihelperd_build_args} ${lihelperd_project}
	MAIN_DEPENDENCY ${listallhelperd_project}
	DEPENDS ${listallhelperd_base_sources}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

add_custom_target(liHelperD ALL DEPENDS ${listallhelperd_name})

install(PROGRAMS ${listallhelperd_name} DESTINATION ${LISTALLER_INSTALL_DIR})
