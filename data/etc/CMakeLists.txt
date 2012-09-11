# Install standard configuration

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/default-dependencies.list
	  ${CMAKE_CURRENT_SOURCE_DIR}/dependencies.list
	  ${CMAKE_CURRENT_SOURCE_DIR}/Listaller.conf
	DESTINATION /etc/listaller
	COMPONENT config)
