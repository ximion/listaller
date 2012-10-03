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
	find_program (VALADOC valadoc)
	mark_as_advanced (VALADOC)
	if (VALADOC STREQUAL "")
		message (FATAL_ERROR "valadoc was not found! Please install Valadoc to continue!")
	endif (VALADOC STREQUAL "")
	add_custom_target (api-docs
		${CMAKE_CURRENT_SOURCE_DIR}/extract_docs.sh
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
	set_directory_properties (PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/api)
endif()
