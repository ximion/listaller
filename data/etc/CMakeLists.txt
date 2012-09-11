# Install standard configuration

# not-to-be-edited config files
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/default-dependencies.list
	DESTINATION ${DATA_INSTALL_DIR}
	COMPONENT config
)

# changeable config files
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/dependencies.list
	  ${CMAKE_CURRENT_SOURCE_DIR}/Listaller.conf
	DESTINATION ${SYSCONF_INSTALL_DIR}/listaller
	COMPONENT config
)
