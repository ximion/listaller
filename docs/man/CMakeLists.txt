# CMakeLists for manual page docs

include (${CMAKE_SOURCE_DIR}/data/cmake/documentation.cmake)

set(manpages "libuild.1")

add_xml_manpages(doc-man "${manpages}" "" "")
