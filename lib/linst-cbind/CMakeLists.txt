#CMake file for Listaller Library Headers

set(llistaller_c_HEADERS Listaller
    li-installer.h
    li-manager.h
    li-types.h
    li-updater.h
)

set(llistaller_pas_BIND
    ${CMAKE_SOURCE_DIR}/src/bind/liappmgr.pas
    ${CMAKE_SOURCE_DIR}/src/bind/liappupdate.pas
    ${CMAKE_SOURCE_DIR}/src/bind/liinstaller.pas
    ${CMAKE_SOURCE_DIR}/src/litypes.pas
)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/listaller.pc.in
  ${CMAKE_CURRENT_BINARY_DIR}/listaller.pc
  @ONLY
)
  
install(FILES ${llistaller_c_HEADERS}
	DESTINATION include/listaller
	COMPONENT headers
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller.pc
	DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
	COMPONENT headers
)
install(FILES ${llistaller_pas_BIND}
	DESTINATION share/pascal/listaller
	COMPONENT headers
)
