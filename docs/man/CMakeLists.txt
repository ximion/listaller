# CMakeLists for Manpages

include (${CMAKE_SOURCE_DIR}/data/cmake/documentation.cmake)

set(manpages "libuild.1")

add_manpages(doc-man "${manpages}" "")
