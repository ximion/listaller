
set(LISTALLER_LIB "liblistaller.so")

set(LIBS_FULL_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")
set(LIBS_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}")
set(LIBS_SOVERSION "${LISTALLER_VERSION_MAJOR}")

add_subdirectory(listaller)
add_subdirectory(linst-cbind)

if (QT)
  add_subdirectory(listaller-qt)
endif(QT)
