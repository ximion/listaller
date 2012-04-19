
add_subdirectory(binreloc)
if (LIXBUILD)
	add_subdirectory(lixbuild)
endif (APBUILD)
if (PATCHELF)
	add_subdirectory(patchelf)
endif (PATCHELF)
