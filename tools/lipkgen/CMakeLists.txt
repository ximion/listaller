# CMakeLists for Listaller package builder tool
project(listaller-lipkgen)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

set(LIPKGEN_SOURCES lipkgen.vala
		lpkgutils.vala
		${CMAKE_SOURCE_DIR}/tools/reporting.vala
		prepare.vala
		gpgsign.vala
		depfind.vala
		ipkbuilder.vala
)

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"Builder\"")

# Listaller build tool
vala_add_executable (lipkgen ${LIPKGEN_SOURCES}
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} listaller_internal packagekit-glib2 appstream
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)

add_dependencies(lipkgen_precompile listaller-glib)

target_link_libraries(lipkgen
		${M_LIB}
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${LibArchive_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		listaller-glib
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_BINARY_DIR}/src
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${APPSTREAM_INCLUDE_DIRS}
)

install(TARGETS lipkgen DESTINATION bin)
