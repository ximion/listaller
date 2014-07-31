# CMakeLists for default component templates

# find metainfo templates and add README
file (GLOB_RECURSE METAINFO_TEMPLATE_DATA "${CMAKE_CURRENT_SOURCE_DIR}/*.metainfo.xml")
set(METAINFO_TEMPLATE_DATA ${METAINFO_TEMPLATE_DATA} README)

install(FILES ${METAINFO_TEMPLATE_DATA}
	DESTINATION ${DATA_INSTALL_DIR}/metainfo-templates
)
