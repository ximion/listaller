# CMakeLists for Listaller repository manager

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

set(LIREPO_SOURCES lirepo.vala
		ipk-repo-local.vala
		../reporting.vala
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_BINARY_DIR}/src
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIRS}
		    ${GIO2_INCLUDE_DIR}
		    ${SQLITE_INCLUDE_DIR}
		    ${LibArchive_INCLUDE_DIR}
		    ${LIBXML2_INCLUDE_DIR}
		    ${GPGME_INCLUDES}
		    ${APPSTREAM_INCLUDE_DIRS}
)

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"RepoTool\"")

# Listaller repo-tool
vala_add_executable (lirepo ${LIREPO_SOURCES}
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} listaller_internal packagekit-glib2
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)

add_dependencies(lirepo_precompile listaller-glib)

target_link_libraries(lirepo
		${M_LIB}
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${LibArchive_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		listaller-glib
)

install(TARGETS lirepo DESTINATION bin)
