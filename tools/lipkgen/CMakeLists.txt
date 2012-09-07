# CMakeLists for Listaller package builder tool
project(listaller-lipkgen)

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

set(LIPKGEN_SOURCES lipkgen.vala
		lpkgutils.vala
		prepare.vala
		gpgsign.vala
		depfind.vala
		ipkbuilder.vala
)

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"Builder\"")

# Listaller build tool
vala_add_executable (lipkgen ${LIPKGEN_SOURCES}
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} listaller_internal packagekit-glib2
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
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
		${REDLAND_LIBRARIES}
		listaller-glib
)

install(TARGETS lipkgen DESTINATION bin)
