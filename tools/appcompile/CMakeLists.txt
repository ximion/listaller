# CMakeLists for Listaller package builder tool

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(LIBUILD_SOURCES ${lisrc}/util.vala
			../libuild/utils.vala
			main.vala
			autocompiler.vala
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIRS}
		    ${GIO2_INCLUDE_DIR}
)

# Database tests
vala_add_executable (appcompile ${LIBUILD_SOURCES}
			PACKAGES config gee-1.0 gio-2.0 posix
			VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
)

target_link_libraries(appcompile
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
)
