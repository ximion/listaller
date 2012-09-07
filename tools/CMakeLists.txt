# CMakeLists for Listaller Tools Directory

set(LI_TOOLS_SHARED_SOURCES toolshared.vala
			console-progress-bar.vala
)

vala_precompile(tools-shared LI_TOOLS_SHARED_C ${LI_TOOLS_SHARED_SOURCES}
			PACKAGES config gee-1.0 gio-2.0 posix listaller-${LILIBS_VERSION}
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
)

add_subdirectory(runapp)
add_subdirectory(lipa)
if(TOOLS)
	add_subdirectory(depscan)
	add_subdirectory(lipkgen)
	add_subdirectory(appcompile)
	add_subdirectory(lirepo)
endif(TOOLS)
