# CMakeLists for Listaller Lipa & Libuild

set(lipa_project lipa.lpr)
set(libuild_project libuild.lpr)

set(lipa_name ${CMAKE_BINARY_DIR}/lipa)
set(libuild_name ${CMAKE_BINARY_DIR}/libuild)
set(cmdtools_build_args "-B" "--ws=nogui")

add_custom_command(OUTPUT ${lipa_name}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${cmdtools_build_args} ${lipa_project}
	MAIN_DEPENDENCY ${lipa_project}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_command(OUTPUT ${libuild_name}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${cmdtools_build_args} ${libuild_project}
	MAIN_DEPENDENCY ${libuild_project}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

add_custom_target(cmdtools ALL DEPENDS ${lipa_name} ${libuild_name})

install(PROGRAMS ${lipa_name} ${libuild_name} DESTINATION bin)
