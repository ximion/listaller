# CMakeLists to install Listaller's mime data

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/listaller-ipk.xml
	DESTINATION share/mime/packages/
	COMPONENT base)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/listaller-ips.xml
	DESTINATION share/mime/packages/
	COMPONENT base)
