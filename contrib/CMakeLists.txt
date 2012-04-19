
if (LIBUILD)
	add_subdirectory(libuild)
	add_subdirectory(binreloc)
endif (LIBUILD)
if (PATCHELF)
	add_subdirectory(patchelf)
endif (PATCHELF)
