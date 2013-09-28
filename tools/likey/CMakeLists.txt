# CMakeLists for Listaller key manager

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

set(LIKEY_SOURCES likey.vala
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
		    ${REDLAND_INCLUDE_DIRS}
)

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"KeyTool\"")

# Listaller key-manager
vala_add_executable (likey ${LIKEY_SOURCES}
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} listaller_internal packagekit-glib2
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
			COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)

add_dependencies(likey_precompile listaller-glib)

target_link_libraries(likey
		${M_LIB}
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		listaller-glib
)

install(TARGETS likey DESTINATION bin)
