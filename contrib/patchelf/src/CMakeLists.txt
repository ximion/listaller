# CMakeLists for Patchelf

set(PATCHELF_SOURCES patchelf.cc
)

include_directories(${CMAKE_BINARY_DIR}
		${CMAKE_BINARY_DIR}/src
		${CMAKE_CURRENT_SOURCE_DIR}
)

add_executable(patchelf ${PATCHELF_SOURCES})

install(TARGETS patchelf
	RUNTIME DESTINATION bin
)
