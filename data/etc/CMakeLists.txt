# Install standard configuration

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/blacklist
	${CMAKE_CURRENT_SOURCE_DIR}/ignore-deps.list
	DESTINATION /etc/lipa
	COMPONENT config)
