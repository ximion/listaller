#CMake file for Listaller Library Headers

set(llistaller_c_HEADERS
    li-installer.h
    li-manager.h
    li-types.h
    li-updater.h
    listaller
)

install(FILES ${llistaller_c_HEADERS} DESTINATION include/listaller COMPONENT headers)
