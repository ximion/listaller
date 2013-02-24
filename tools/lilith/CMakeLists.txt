# CMakeLists for Lilith

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(LILITH_SOURCES lilith.vala
		lilith-factory.vala
		lilith-checkrunner.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
		    ${CMAKE_BINARY_DIR}/src
)

# find Lilith checks
file (GLOB LILITH_CHECK_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/checks/*.vala")
set(LILITH_SOURCES ${LILITH_SOURCES} ${LILITH_CHECK_SOURCES})

# Add log domain
add_definitions("-DG_LOG_DOMAIN=\"Lilith\"")

vala_add_executable(lilith ${LILITH_SOURCES}
			PACKAGES ${LISTALLER_BASE_VALA_DEPS} listaller_internal packagekit-glib2
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
)

add_dependencies(lilith_precompile listaller-glib)
add_dependencies(lilith_precompile ${CMAKE_BINARY_DIR}/src/listaller-${LILIBS_VERSION}.vapi)

target_link_libraries(lilith
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		listaller-glib
)

install(TARGETS lilith DESTINATION bin)
