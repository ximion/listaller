# CMakeLists for Listaller Tools Directory

add_subdirectory(runapp)
if(TOOLS)
	add_subdirectory(depscan)
	add_subdirectory(lipkgen)
	add_subdirectory(appcompile)
endif(TOOLS)
