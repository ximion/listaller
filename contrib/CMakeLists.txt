
add_subdirectory(binreloc)
if (LIXBUILD)
	add_subdirectory(lixbuild)
endif (LIXBUILD)
if (PATCHELF)
	add_subdirectory(patchelf)
endif (PATCHELF)
