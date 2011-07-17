
add_subdirectory(binreloc)
if (APBUILD)
	add_subdirectory(apbuild)
endif (APBUILD)
if (PATCHELF)
	add_subdirectory(patchelf)
endif (PATCHELF)
