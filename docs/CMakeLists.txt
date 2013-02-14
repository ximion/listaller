# CMakeLists for documentation

add_subdirectory (man)

find_program (XMLTO xmlto)
mark_as_advanced (XMLTO)
if (XMLTO STREQUAL "")
	message (FATAL_ERROR "Xmlto was not found! Please install it to continue!")
endif (XMLTO STREQUAL "")

# Find required tools
find_program (YELP_BUILD yelp-build)
mark_as_advanced (YELP_BUILD)
if (YELP_BUILD STREQUAL "")
	message (FATAL_ERROR "yelp-build was not found! Please install the Yelp tools to continue!")
endif (YELP_BUILD STREQUAL "")

add_custom_target (documentation
	${CMAKE_CURRENT_SOURCE_DIR}/create_html.sh
	DEPENDS pages/index.page
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/html)

if (API_DOCS)
	# find documentation tools
	find_program (VALADOC valadoc)
	mark_as_advanced (VALADOC)
	if (VALADOC STREQUAL "")
		message (FATAL_ERROR "valadoc was not found! Please install Valadoc to continue!")
	endif (VALADOC STREQUAL "")
	find_program(GTKDOC_SCAN_EXE gtkdoc-scan PATH "${GLIB_PREFIX}/bin")
	if(NOT GTKDOC_SCAN_EXE)
		message(FATAL_ERROR "gtkdoc not found")
	endif(NOT GTKDOC_SCAN_EXE)

	find_path (SYSTEM_VAPI_DIR
		NAMES glib-2.0.vapi
		HINTS "/usr/share/vala-0.??/vapi" "/usr/share/vala")
	# ugly hack to help valadoc finding vapi files
	string (SUBSTRING ${VALA_VERSION} 0 4 VALA_VERSION_SHORT)
	set (SYSTEM_VAPI_DIR "${CMAKE_INSTALL_PREFIX}/share/vala-${VALA_VERSION_SHORT}/vapi")

	# find public library sources
	file (GLOB LISTALLER_LIB_SOURCES "${CMAKE_SOURCE_DIR}/src/*.vala")

	add_custom_command (TARGET documentation
			COMMAND "valadoc" "--force" "-b" "${CMAKE_SOURCE_DIR}/src" "-o" "${CMAKE_SOURCE_DIR}/docs/api"
			"--vapidir=${CMAKE_SOURCE_DIR}/vapi" "--vapidir=${SYSTEM_VAPI_DIR}"
			"-X" "${CMAKE_BINARY_DIR}/src/listaller.h" "-X" "-l"
			"-X" "${CMAKE_BINARY_DIR}/liblistaller-glib.so.${LILIBS_FULL_VERSION}"
			"--doclet" "gtkdoc"
			"--pkg=config" "--pkg=gee-1.0" "--pkg=gio-2.0" "--pkg=libsoup-2.4"
			"--pkg=sqlite3" "--pkg=libarchive" "--pkg=gpgme" "--pkg=libxml-2.0"
			"--pkg=rdf-minimal" "--pkg=packagekit-glib2"
			"--package-name=listaller"
			"--package-version=${LISTALLER_VERSION}"
			"--importdir=${CMAKE_SOURCE_DIR}/docs/pages"
			${LISTALLER_LIB_SOURCES}
			VERBATIM)
	set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/api)

endif()
