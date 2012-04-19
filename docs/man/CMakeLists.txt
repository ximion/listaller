# CMakeLists for manual page docs

include (${CMAKE_SOURCE_DIR}/data/cmake/documentation.cmake)

set(manpages "runapp.1")
if(TOOLS)
	set(manpages ${manpages}
		"lipkgen.1"
		"appcompile.1"
		"depscan.1"
	)
endif()

if(APBUILD)
	set(manpages ${manpages}
		"ligcc.1"
		"lig++.1"
	)
endif()

add_xml_manpages(doc-man "${manpages}" "" "")
