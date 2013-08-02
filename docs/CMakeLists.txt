# CMakeLists for documentation

add_subdirectory (man)

find_program (XMLTO xmlto)
mark_as_advanced (XMLTO)
if (XMLTO STREQUAL "")
	message (FATAL_ERROR "Xmlto was not found! Please install it to continue!")
endif (XMLTO STREQUAL "")

# Find required tools
find_program (PUBLICAN publican)
mark_as_advanced (PUBLICAN)
if (PUBLICAN STREQUAL "")
	message (FATAL_ERROR "Publican was not found! Please install the it to continue!")
endif (PUBLICAN STREQUAL "")

# build small listaller lib version string
set(LISTALLER_LIB_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")

# we need to assemble our documentation build-directory before we can built it
add_custom_target (assemble-docs
	COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/prepare-docbuild.sh" "source_dir=${CMAKE_CURRENT_SOURCE_DIR}" "build_dir=${CMAKE_BINARY_DIR}/docbuild"
	DEPENDS "publican.cfg"
		"sources/"
		"${CMAKE_BINARY_DIR}/docbuild/"
		"${CMAKE_BINARY_DIR}/src/liblistaller-glib.so.${LISTALLER_LIB_VERSION}"
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	VERBATIM
)

# the actual target to build all documentation
add_custom_target (documentation
	COMMAND ${PUBLICAN} "build" "--langs=en-US" "--publish" "--formats=html"
	DEPENDS "publican.cfg"
		"${CMAKE_BINARY_DIR}/docbuild/publican.cfg"
		assemble-docs
	WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/docbuild/
)
add_custom_command (OUTPUT "${CMAKE_BINARY_DIR}/docbuild/"
		COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/docbuild"
		VERBATIM
)

set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/html)
set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_BINARY_DIR}/docbuild)

# a target to export documentation html to the source tree (useful when building the tarballs)
add_custom_target (documentation-htmlexport
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_BINARY_DIR}/docbuild/Listaller-Docs/en-US/html/" "${CMAKE_CURRENT_SOURCE_DIR}/html"
	DEPENDS documentation
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

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
	file (GLOB LISTALLER_LIB_SOURCES_MAIN "${CMAKE_SOURCE_DIR}/src/*.vala")
	file (GLOB LISTALLER_LIB_SOURCES_SOLVERS "${CMAKE_SOURCE_DIR}/src/solvers/*.vala")
	set (LISTALLER_LIB_SOURCES ${LISTALLER_LIB_SOURCES_MAIN} ${LISTALLER_LIB_SOURCES_SOLVERS})

	add_custom_command (TARGET documentation
			COMMAND "valadoc" "--force" "-b" "${CMAKE_SOURCE_DIR}/src" "-o" "${CMAKE_SOURCE_DIR}/docs/api"
			"--vapidir=${CMAKE_SOURCE_DIR}/vapi" "--vapidir=${SYSTEM_VAPI_DIR}"
			"-X" "${CMAKE_BINARY_DIR}/src/listaller.h" "-X" "-l"
			"-X" "${CMAKE_BINARY_DIR}/src/liblistaller-glib.so.${LISTALLER_LIB_VERSION}"
			"--doclet" "gtkdoc"
			"--pkg=config" "--pkg=gee-1.0" "--pkg=gio-2.0" "--pkg=libsoup-2.4"
			"--pkg=sqlite3" "--pkg=libarchive" "--pkg=gpgme" "--pkg=libxml-2.0"
			"--pkg=rdf-minimal" "--pkg=packagekit-glib2"
			"--package-name=listaller"
			"--package-version=${LISTALLER_VERSION}"
			"--importdir=${CMAKE_SOURCE_DIR}/docs/man"
			${LISTALLER_LIB_SOURCES}
			VERBATIM)
	set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/api/html)
	set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/api/xml)

endif()
