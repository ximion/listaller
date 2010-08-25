
set(LISTALLER_LIB "liblistaller.so")

set(LIBS_FULL_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")
set(LIBS_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}")

add_subdirectory(listaller)
add_subdirectory(linst-cbind)
add_subdirectory(listaller-qt)
