# CMakeLists for default component templates

# find frameworks and add README
file (GLOB_RECURSE FRAMEWORKS_DATA "${CMAKE_CURRENT_SOURCE_DIR}/frameworks/*.framework")
set(FRAMEWORKS_DATA ${FRAMEWORKS_DATA} frameworks/README)

# find modules and add README
file (GLOB_RECURSE MODULES_DATA "${CMAKE_CURRENT_SOURCE_DIR}/*.module")
set(MODULES_DATA ${MODULES_DATA} README)

install(FILES ${FRAMEWORKS_DATA}
	DESTINATION ${DATA_INSTALL_DIR}/components/frameworks
)

install(FILES ${MODULES_DATA}
	DESTINATION ${DATA_INSTALL_DIR}/components
)
