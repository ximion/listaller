# CMakeLists for Listaller Tools Directory

add_subdirectory(runapp)
add_subdirectory(lipa)
if(TOOLS)
	add_subdirectory(depscan)
	add_subdirectory(lipkgen)
	add_subdirectory(appcompile)
endif(TOOLS)
