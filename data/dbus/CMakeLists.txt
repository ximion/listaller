# Install/Configure DBus/PolKit related files

find_package(DBus REQUIRED)
find_package(PolKit REQUIRED)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/org.nlinux.Listaller.service.in"
  "${CMAKE_CURRENT_BINARY_DIR}/org.nlinux.Listaller.service"
  @ONLY)
  
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/org.nlinux.Listaller.policy
	DESTINATION ${POLKIT_POLICY_FILES_INSTALL_DIR})
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/org.nlinux.Listaller.conf
	DESTINATION /etc/dbus-1/system.d/)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/org.nlinux.Listaller.service
	DESTINATION share/dbus-1/system-services/)
