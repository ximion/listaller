# CMake for lib folder

set(LIPK_LIB "libipk.so")

set(LIBS_FULL_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")
set(LIBS_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}")
set(LIBS_SOVERSION "${LISTALLER_VERSION_MAJOR}")

set(lisrc ${CMAKE_SOURCE_DIR}/src CACHE PATH "Listaller sourcecode dir")

add_subdirectory(ipk)
add_subdirectory(lipk-cbind)
add_subdirectory(listaller)

if (QT)
  add_subdirectory(listaller-qt)
endif(QT)
